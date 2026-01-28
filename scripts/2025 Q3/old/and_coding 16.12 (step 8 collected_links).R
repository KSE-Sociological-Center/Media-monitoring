# 1. Libraries & Parallel Setup
library(data.table)       
library(readxl)           
library(tidyverse)
library(jsonlite)
library(openxlsx)
library(future.apply)     
library(stringr)
library(scales)
library(rvest)
plan(multisession, workers = parallel::detectCores() - 1)


# Deletes any existing .xlsx files whose names start with `prefix`
delete_old_files <- function(prefix) {
  old <- list.files(pattern = paste0("^", prefix, "_.*\\.xlsx$"))
  if (length(old)) {
    file.remove(old)
    message("✅ Removed ", length(old), " old file(s) with prefix ", prefix)
  }
}

# Saves `df` to Excel, names it prefix_YYYYMMDD_HHMMSS.xlsx
save_partial_results <- function(df, prefix) {
  # ensure no factors
  df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
  # delete old
  delete_old_files(prefix)
  # build filename
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fname <- sprintf("%s_%s.xlsx", prefix, ts)
  # write
  write.xlsx(df, file = fname, rowNames = FALSE)
  message("✅ Saved results to ", fname)
}

# 2. Load & Pre-filter Data
setwd("E:\\work\\kse\\chatgpt coding")
path <- "./Масив згадок опитувань.xlsx"
monitoring_df <- as.data.table(read_excel(path, sheet = "Статті"))[
  !(`Тип ЗМІ` %in% c("Соціальні мережі", "Форум", "Відгук", "Радіо", "ТБ", "Блог"))
]
# drop unused cols, clean dates
monitoring_df[, `Дата виходу` := as.Date(`Дата виходу`)]
monitoring_df[, month_year := format(`Дата виходу`, "%Y-%m")]


# 3. OpenAI Config & Helper
api_key <- ""
hey_chatGPT <- function(prompt, model) {
  response <- httr2::request("https://api.openai.com/v1/chat/completions") %>%
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type`  = "application/json"
    ) %>%
    httr2::req_body_json(list(
      model       = model,
      temperature = 0,
      messages    = list(list(role = "user", content = prompt))
    )) %>%
    # allow up to 5 minutes for the request
    httr2::req_timeout(seconds = 300) %>%
    # retry up to 5 times, waiting exactly 2 s each time
    httr2::req_retry(
      max_tries = 5,
      backoff   = function(retry) 2
    ) %>%
    httr2::req_perform()
  
  parsed <- httr2::resp_body_json(response)
  str_trim(parsed$choices[[1]]$message$content)
}
request_with_retry <- function(prompt, model, retries=5) {
  for (i in seq_len(retries)) {
    Sys.sleep(min(2^(i-1), 10))
    out <- tryCatch(hey_chatGPT(prompt, model), error = function(e) NA)
    if (!is.na(out) && nzchar(out)) return(out)
  }
  NA_character_
}



# Step 1: Identify messages
# one-liner: classify & filter in place

monitoring_df[, survey_focus := unlist(future_lapply(Текст, function(txt) {
  if (is.na(txt) || txt == "") return(NA_character_)
  # build prompt
  prompt <- paste0(
    
    "Persona: Ти — уважний аналітик, що спеціалізується на перевірці джерел.\n\n",
    "Завдання: Проаналізуй наступний текст і визнач, чи згадуються в ньому результати *конкретного*, *вже проведеного* соціологічного опитування або дослідження (хоча б одного).\n\n",
    "Контекст (Правила): 1. Якщо згадується абстрактне опитування ('соціологи кажуть', 'опитування показують'), відповідай 'Ні'. 2. Якщо згадується майбутнє опитування ('ми плануємо опитати'), відповідай 'Ні'. 3. Якщо згадується конкретне опитування (є назва, організатор, дата або чіткі результати), відповідай 'Так'.\n\n",
    "Формат: Відповідай лише одним словом: 'Так' або 'Ні'.\n\n",
    "Текст повідомлення:\n", txt
  )
  res <- request_with_retry(prompt, "gpt-4o-mini")
  if (is.na(res) || !nzchar(res)) return(NA_character_)
  
  # normalize answer: trim, lowercase, strip punctuation
  clean <- str_remove_all(str_to_lower(str_trim(res)), "[[:punct:]]")
  
  # map to canonical labels
  if (clean == "так") {
    "Так"
  } else if (clean == "ні") {
    "Ні"
  } else {
    NA_character_
  }
})
)]


# keep only those with focus == "Так"
filtered_df <- monitoring_df[survey_focus == "Так"]

# save
save_partial_results(filtered_df, prefix = "step1")




# Step 2: Extract organisation & survey topic

# 1) Підготуємо пусті колонки
filtered_df[, `:=`(
  organisation  = NA_character_,
  survey_topic  = NA_character_
)]

# 2) Один промпт для двох задач: вказати організації і тему

prompt_combined <- paste(
  "Ти — аналітичний асистент. Твоє завдання — вилучити з тексту дані про походження соціологічного дослідження.

  КРОК 1. Знайди організації, які відіграють одну з двох ролей:
  1. ВИКОНАВЕЦЬ (хто провів опитування, збирав дані, соціологічна служба/центр).
  2. ЗАМОВНИК (хто ініціював, фінансував або на чиє замовлення робили опитування).

  Критерії пошуку:
  - Шукай фрази: 'на замовлення...', 'проведено компанією..., 'дослідження центру...', 'спільно з...'.
  - Якщо організація лише ОПРИЛЮДНИЛА новину (наприклад, ЗМІ, телеканал, новинний сайт), але не є замовником чи виконавцем дослідження — НЕ включай її.
  - Якщо знайдено більше двох організацій (наприклад, альянс замовників і виконавців) — обери дві найважливіші (першоджерела).

  КРОК 2. Сформулюй тему опитування одним реченням.

  ФОРМАТ ВИВОДУ (суворо два рядки):
  Хто проводив: <Назва Виконавця>; <Назва Замовника>
  Тема: <Тема опитування>

  Якщо в тексті не вказано ні виконавця, ні замовника, поверни: 'Хто проводив:'
  Більше ніяких пояснень.",

  sep = "\n\n"
)


# 3) Run in parallel
n <- nrow(filtered_df)
results <- future_lapply(seq_len(n), function(i) {
  text <- filtered_df$Текст[i]
  full_prompt <- paste(prompt_combined, text, sep = "\n\n\n\n")
  res <- request_with_retry(full_prompt, "gpt-4o-mini")
  
  # Debug first 3
  if (i <= 3) {
    cat("---- GPT raw response for row", i, "----\n")
    cat(res, "\n\n")
  }
  
  # Default empty
  org1 <- NA_character_; org2 <- NA_character_; topic <- NA_character_
  
  if (!is.na(res) && nzchar(res)) {
    lines <- unlist(strsplit(res, "\r?\n"))
    lines <- lines[trimws(lines) != ""]
    who <- grep("^\\s*Хто проводив:", lines, value = TRUE)
    thm <- grep("^\\s*Тема:",       lines, value = TRUE)

    if (length(who) > 0) {
      org_text <- str_trim(sub("^\\s*Хто проводив:\\s*", "", who[1]))
      org_text <- str_replace_all(org_text, "[/\\\\]", "; ") #костиль, щоб розділити назва1/\назва2
      if (nzchar(org_text)) {
        org_list <- str_split(org_text, ";")[[1]] |> str_trim()
        org1 <- ifelse(length(org_list) >= 1, org_list[1], NA_character_)
        org2 <- ifelse(length(org_list) >= 2, org_list[2], NA_character_)
      }
    }
    if (length(thm) > 0) {
      topic <- str_trim(sub("^\\s*Тема:\\s*", "", thm[1]))
    }
  }
        
  
  # checkpoint every 1000
  if (i %% 1000 == 0) {
    save_partial_results(filtered_df, prefix = "step2_checkpoint")
  }
  
  list(organisation = org_text, org1 = org1, org2 = org2, survey_topic = topic)
})

# 4) Assign back
res_dt <- data.table::rbindlist(results)
filtered_df[, organisation  := res_dt$organisation]
filtered_df[, org1 := res_dt$org1]
filtered_df[, org2 := res_dt$org2]
filtered_df[, survey_topic  := res_dt$survey_topic]

# 5) Save
save_partial_results(filtered_df, prefix = "step2")




# Step 3: Нормалізація назв організацій за словником

# 1. Зчитуємо словник організацій
org_dict <- read_excel("Словник.xlsx") |> as.data.table()

# 2. Basic text normalization
for(col in c("org1", "org2")) {
  filtered_df[, (col) := tolower(get(col))]
  filtered_df[, (col) := str_replace_all(get(col), '[“”"«».,<>]', '')]
  filtered_df[, (col) := str_trim(get(col))]
  filtered_df[, (col) := str_remove_all(get(col), '\\s+(україни|україни)$')]
  filtered_df[, (col) := str_replace_all(get(col), '[-/]', ' ')]
}

# 3. шукаємо відповідники та додаємо у змінну створену змінну org_final
filtered_df[, org1_final := org_dict$org_final[match(org1, org_dict$organisationALL)]]
filtered_df[, org2_final := org_dict$org_final[match(org2, org_dict$organisationALL)]]

# 4. якщо не знайдено у словнику - NA
filtered_df[is.na(org1_final), org1_final := NA_character_]
filtered_df[is.na(org2_final), org2_final := NA_character_]

# 5. Save
save_partial_results(filtered_df, prefix = "step3")




# Step 4: Групування подібних назв (поза словником)

filtered_df[, org1_clean_1 := fifelse(
  is.na(org1_final) & !is.na(org1), org1, NA_character_)]
filtered_df[org1_clean_1 == "", org1_clean_1 := NA_character_]

filtered_df[, org2_clean_1 := fifelse(
  is.na(org2_final) & !is.na(org2), org2, NA_character_)]
filtered_df[org2_clean_1 == "", org2_clean_1 := NA_character_]

# 4.1 Об’єднуємо обидві колонки в один вектор без NA
all_orgs <- na.omit(c(filtered_df$org1_clean_1, filtered_df$org2_clean_1))
# Отримуємо відсортований вектор унікальних назв з двох змінних
unique_names <- sort(unique(all_orgs))

# Видаляємо загальні стоп-слова (за потреби)
stop_words <- c("центр", "інститут", "служба", "фонд", "департамент", "відділ")
names_to_group <- setdiff(unique_names, stop_words)

# Функція, яка перевіряє, чи міститься рядок x в будь-якому іншому рядку
is_included <- function(x, vec) {
  any(sapply(vec[vec != x], function(y) {
    grepl(x, y, fixed = TRUE) || grepl(y, x, fixed = TRUE)
  }))
}

# 4) Відбираємо тільки ті назви, які входять одна в одну (кандидати на групування)
candidates <- names_to_group[sapply(names_to_group, is_included, vec = names_to_group)]

# 5) Будуємо групи: кожна група — вектор подібних рядків
groups <- list()
for (nm in candidates) {
  placed <- FALSE
  for (g in seq_along(groups)) {
    if (any(sapply(groups[[g]], function(existing) is_included(existing, nm)))) {
      groups[[g]] <- c(groups[[g]], nm)
      placed <- TRUE
      break
    }
  }
  if (!placed) {
    groups[[length(groups) + 1]] <- nm
  }
}

# 6) Сортуємо кожну групу за довжиною рядка (щоб найкоротша була першою)
groups <- lapply(groups, function(gr) gr[order(nchar(gr))])

# 7) Сортуємо самі групи за першими елементами
groups <- groups[order(sapply(groups, function(gr) gr[1]))]

# 8) Об’єднуємо всі групи в один вектор, зберігаючи порядок
grouped_names <- unlist(groups)


# 4.2: Сортування та об'єднання схожих назв
# Використовуємо grouped_names із попереднього кроку
# Проходимо по кожній парі назв у grouped_names
for (i in 1:(length(grouped_names) - 1)) {
  prev_name <- grouped_names[i]
  next_name <- grouped_names[i + 1]
  
  prompt <- paste0(
    "Порівняйте наступні дві назви організацій:\n",
    "1) '", prev_name, "'\n",
    "2) '", next_name, "'\n\n",
    "Якщо ці назви позначають ту ж саму організацію, і одна з них є коротшою, поверніть коротшу назву. ",
    "Якщо це різні організації, поверніть першу назву."
  )
  
  
  response <- hey_chatGPT(prompt, "gpt-4o")
  response <- trimws(tolower(response))
  
  # Якщо GPT визначив, що друга назва — правильна (коротша або коректніша)
  if (response == next_name) {
    cat("Порівняння:\n  Попередня: ", prev_name,
        "\n  Наступна: ", next_name,
        "\n  Обрана: ", next_name, "\n\n")
    
    # Оновлюємо назви у grouped_names і в датафреймі
    grouped_names[grouped_names == prev_name] <- next_name
    
    filtered_df[org1_clean_1 == prev_name, org1_clean_1 := next_name]
    filtered_df[org2_clean_1 == prev_name, org2_clean_1 := next_name]
    
    # Оновлюємо список групованих назв, щоби уникнути дублів
    grouped_names <- sort(unique(c(filtered_df$org1_clean_1, filtered_df$org2_clean_1)))
  } else {
    cat(i, " — без змін\n")
  }
}


# 4.3: Формування фінального списку назв організацій =============================
# Об’єднуємо обидві колонки в один вектор (без NA) і рахуємо частоти
allorgs <- na.omit(c(filtered_df$org1_clean_1, filtered_df$org2_clean_1))
# Розрахунок частот появи та вибір топ-150 назв
freq_table <- data.table(org_name = allorgs)[, .N, by = org_name][order(-N)]
top_150_names <- freq_table[1:150, org_name]


prompt <- paste0(
  "Ось список 150 назв організацій, впорядкованих за частотою:\n",
  paste(top_150_names, collapse = "\n"),
  "\n\nБудь ласка, проаналізуйте цей список і, якщо знайдете дублікати (назви, що позначають ту ж організацію), ",
  "упорядкуйте їх так, щоб повторювані назви йшли один за одним. \n Вихідний формат: список назв (без нумерації), по одному рядку."
)

response <- hey_chatGPT(prompt, "gpt-4o")
cat("Відповідь API (групування назв):\n", response, "\n")

# Розбиваємо відповідь API на рядки
response_clean <- unlist(strsplit(response, "\n"))
good_lines <- response_clean[!grepl("^\\s*ні\\s*$", response_clean, ignore.case = TRUE)]

# 4.4: Ask GPT to cluster duplicates among the top names
cluster_prompt <- paste0(
  "Ось список назв організацій:\n",
  paste(good_lines, collapse = "\n"),
  "\n\nГрупуйте всі варіанти назв однієї організації в кластери. Для кожного кластера "
  , "оберіть найкоротшу назву як представницьку і виведіть рядок у форматі:\n",
  "  представницька (варіант1, варіант2, …)\n",
  "Якщо кластер містить лише одну назву, виведіть її без дужок.")

# Викликаємо API з використанням функції request_with_retry (яка вже була визначена)
final_grouping_response <- request_with_retry(cluster_prompt, "gpt-4o")
cat("Відповідь API:\n", final_grouping_response, "\n\n")
lines <- str_split(final_grouping_response, "\n")[[1]]
lines <- lines[nzchar(lines)]
lines <- gsub("^\\s*\\d+[\\.)]\\s*|^[-•]\\s*", "", lines)

# Build a mapping table from variant → canonical
mapping <- rbindlist(lapply(lines, function(line) {
  if (str_detect(line, "\\(")) {
    can  <- str_trim(str_extract(line, "^[^\\(]+"))
    vars <- str_split(str_extract(line, "(?<=\\().*(?=\\))"), ",")[[1]] %>% str_trim()
    data.table(variant=vars, canonical=can)
  } else {
    data.table(variant=str_trim(line), canonical=str_trim(line))
  }
}), fill=TRUE)


# Застосування мапінгу до обох колонок
setkey(mapping, variant)
filtered_df[, org1_semifinal := {
  can <- mapping[J(org1_clean_1), canonical]
  fifelse(is.na(can), org1_clean_1, can)
}]
filtered_df[, org2_semifinal := {
  can <- mapping[J(org2_clean_1), canonical]
  fifelse(is.na(can), org2_clean_1, can)
}]



# Перенесення: для всіх рядків, де після словника в org1_final та org2_final вийшло NA 
# присвоюємо значення org1_semifinal та org2_semifinal відповідно
filtered_df[is.na(org1_final), org1_final := org1_semifinal]
filtered_df[is.na(org2_final), org2_final := org2_semifinal]

#уся таблиця організацій ДО перетворень
#filtered_df %>% select(organisation, org1, org2, org1_final, org2_final, org1_semifinal, org2_semifinal, org1_clean_1, org2_clean_1) %>% sample_n(120) %>% view()


#ПЕРЕТВОРЕННЯ
# Прибираємо org2_final, якщо org1_final = org2_final
filtered_df[org1_final == org2_final, org2_final := NA]

# Якщо org1_final == NA, а org2_final має організацію → переносимо значення з org2_final в org1_final
# Відсутність згадок у новині про організацію дивитись по org1_final
filtered_df[is.na(org1_final) & !is.na(org2_final), `:=`(
  org1_final = org2_final,
  org2_final = NA)]


#уся таблиця організацій ПІСЛЯ перетворень
filtered_df %>% select(organisation, org1, org2, org1_final, org2_final, org1_semifinal, org2_semifinal, org1_clean_1, org2_clean_1) %>% sample_n(50) %>% view()

# Save
save_partial_results(filtered_df, prefix = "step4")




# Step 5: Quality of reporting

# Replace with your actual text vector
texts <- filtered_df$Текст

# Define metadata keys and their Ukrainian labels
meta_keys <- c(
  "sponsor","survey_org","dates", "population", "sample_size",
  "sampling_method", "margin_of_error", "weighting",
  "survey_mode", "question_text"
)

meta_labels <- c(
  "Замовник дослідження",
  "Виконавець дослідження",
  "Дати проведення опитування",
  "Генеральна сукупність",
  "Розмір вибірки",
  "Метод вибірки",
  "Похибка вибірки",
  "Застосування вагових коефіцієнтів",
  "Спосіб проведення опитування",
  "Текст запитання"
)
names(meta_labels) <- meta_keys


# Build prompt
make_prompt <- function(texts) {
  # Detailed instructions mapping Ukrainian labels to AAPOR definitions
  instructions <- "
Ти — експерт з методології соціологічних досліджень. Твоє завдання — проаналізувати текст прес-релізу або звіту та визначити, чи містить він інформацію, що відповідає стандартам AAPOR (American Association for Public Opinion Research).

Для кожного з 10 пунктів нижче напиши 'так', якщо інформація наявна в тексті, або 'ні', якщо вона відсутня. Використовуй наведені визначення AAPOR для прийняття рішення:

1. Замовник дослідження (Sponsor / Funding Source):
   - Шукай: чітку вказівку на те, хто фінансував або замовив опитування (наприклад, 'на замовлення...', 'за підтримки фонду...', 'фінансується урядом...').
   - AAPOR Definition: Name the sponsor of the research. If the original source of funding is different than the sponsor, this source will also be disclosed.

2. Виконавець дослідження (Who Conducted the Research):
   - Шукай: назву організації, яка безпосередньо проводила опитування, збирала та обробляла дані (соціологічний центр, дослідницька агенція).
   - AAPOR Definition: Name the party(ies) who conducted the research.

3. Дати проведення опитування (Dates of Data Collection):
   - Шукай: часові межі збору даних.
   - Критерій 'так': вказано конкретні дні (наприклад, '10-15 березня') АБО вказано місяць і рік (наприклад, 'у березні 2024 року').
   - Критерій 'ні': вказано ЛИШЕ рік (наприклад, 'опитування 2023 року') або дати відсутні.
   - AAPOR Definition: Disclose the dates of data collection.

4. Генеральна сукупність (Population Under Study):
   - Шукай: кого саме репрезентує вибірка.
   - Критерій 'так':
     a) Для загальних опитувань: достатньо загальної вказівки на громадянство чи країну (наприклад, 'опитано українців', 'населення України', 'більшість громадян вважає').
     b) Для специфічних груп: вказано конкретні характеристики (наприклад, 'ВПО', 'лікарі', 'молодь 18-25 років').
   - AAPOR Definition: Researchers will be specific about the decision rules used to define the population (location, age, other social or demographic characteristics).
   
5. Розмір вибірки (Sample Sizes):
   - Шукай: кількість опитаних респондентів (загальна або по групах).
   - AAPOR Definition: Provide sample sizes for each mode of data collection (for surveys include sample sizes for each frame, list, or panel used).

6. Метод вибірки (Method Used to Generate and Recruit the Sample):
   - Шукай: як відбирали респондентів (випадкова вибірка, квотна, 'снігова куля', панель тощо).
   - AAPOR Definition: Explicitly state whether the sample comes from a frame selected using a probability-based methodology or if the sample was selected using non-probability methods (opt-in, volunteer). Describe any use of quotas.

7. Похибка вибірки (Precision of the Results):
   - Шукай: згадки про статистичну похибку, маржу помилки (margin of error) або точність.
   - AAPOR Definition: For probability sample surveys, report estimates of sampling error. Reports of non-probability sample surveys will only provide measures of precision if they are defined and accompanied by a detailed description.

8. Застосування вагових коефіцієнтів (How the Data Were Weighted):
   - Шукай: інформацію про зважування даних (weighting) для корекції вибірки.
   - AAPOR Definition: Describe how the weights were calculated, including the variables used and the sources of the weighting parameters.

9. Спосіб проведення опитування (Method(s) and Mode(s) of Data Collection):
   - Шукай: метод контакту (телефон, CATI, онлайн-панель, особисте інтерв'ю, CAPI, пошта).
   - AAPOR Definition: Include a description of all mode(s) used to contact participants or collect data or information (e.g., CATI, CAPI, ACASI, IVR, mail, Web).

10. Текст запитання (Measurement Tools/Instruments):
   - Шукай: точне формулювання запитання, яке ставили респондентам.
   - AAPOR Definition: The exact wording and presentation of any measurement tool from which results are reported.

ФОРМАТ ВІДПОВІДІ (суворо дотримуйся порядку):
Замовник дослідження: [так/ні]
Виконавець дослідження: [так/ні]
Дати проведення опитування: [так/ні]
Генеральна сукупність: [так/ні]
Розмір вибірки: [так/ні]
Метод вибірки: [так/ні]
Похибка вибірки: [так/ні]
Застосування вагових коефіцієнтів: [так/ні]
Спосіб проведення опитування: [так/ні]
Текст запитання: [так/ні]
"
  
  # Final assembly
  paste0(instructions, "\n\nТекст повідомлення для аналізу:\n", texts)
}


# Run GPT
results <- future_lapply(texts, function(txt) {
  prompt <- make_prompt(txt)
  res <- request_with_retry(prompt, "gpt-4o-mini")
  lines <- str_split(res, "\n")[[1]]
  out <- setNames(rep("no", length(meta_keys)), meta_keys)
  for (key in meta_keys) {
    pattern <- paste0(meta_labels[key], ":\\s*(так|ні)")
    match <- str_match(lines, pattern)
    val <- na.omit(match[, 2])[1]
    if (!is.na(val)) {
      out[key] <- ifelse(val == "так", "yes", "no")
    }
  }
  return(out)
})

# Convert to data.table
meta_dt <- rbindlist(lapply(results, as.list), fill = TRUE)
meta_dt <- as.data.table(meta_dt)

# Bind metadata columns back to original dataframe
filtered_df <- cbind(filtered_df, meta_dt)

# Save
save_partial_results(filtered_df, prefix = "step5")




# Step 6: Рівень дотримання стандартів публікації результатів
level_df <- copy(filtered_df)

# перетворення yes/no на 1/0
cols <- c("sponsor", "survey_org",
          "population", "sample_size", "sampling_method", "margin_of_error", "weighting",
          "dates", "survey_mode", "question_text")

level_df[, (cols) := lapply(.SD, function(x) fifelse(x == "yes", 1, 0)), .SDcols = cols]

# обчислення вимірів
level_df[, dim1 := (sponsor + survey_org) * (1/2)]  #джерельна доброчесність; 2інд → 0.5
level_df[, dim2 := (population + sample_size + sampling_method + margin_of_error + weighting) * (1/5)]  #статистична прозорість; 5інд → 0.2
level_df[, dim3 := (dates + survey_mode + question_text) * (1/3)]  #інтерпретаційна точність; 3інд → 0.3(3)

# обчислення концепту 'Рівень дотримання стандартів'
level_df[, compliance_standards := rowMeans(.SD, na.rm = TRUE), .SDcols = c("dim1", "dim2", "dim3")]

# категоризація рівнів ризику
level_df[, compliance_level := fcase(
  compliance_standards >= 0.6, "високий",
  compliance_standards >= 0.3, "середній",
  default = "низький"
)]


# повертаємо в оригінальний масив
filtered_df <- cbind(filtered_df, level_df[, .(compliance_standards, compliance_level)])
# результати
filtered_df[, .N, by = compliance_level][order(compliance_level)]

# Save
save_partial_results(filtered_df, prefix = "step6")




# Step 7: Closed topic coding
# Define the list of topics 

# Define topics and their definitions as a table
topics_definitions <- data.frame(
  Topic = c(
    "Political Trust and Governance",
    "Foreign Relations / International Affairs",
    "Military and Defense",
    "Economic and Business Climate",
    "Employment and Labor Market",
    "Social Issues and Welfare",
    "Healthcare",
    "Education",
    "Infrastructure and Urban Planning",
    "Civic Engagement and Participation",
    "Culture and Identity",
    "Justice and Law Enforcement",
    "Environment and Climate",
    "Media and Information",
    "Technology",
    "Energy",
    "Immigration"
  ),
  
  Definition = c(
    "Domestic surveys assessing citizens' trust in local or national government, political leaders, institutions, or domestic political sentiment and governance.",
    
    "Surveys explicitly about international diplomacy, relations between countries, geopolitical issues, foreign policy, international alliances, or attitudes toward other countries or international organizations.",
    
    "Surveys focused on armed forces, national security, military operations, defense policies, warfare, or attitudes towards military institutions.",
    
    "Surveys focused on economic trends, business confidence, investments, market conditions, entrepreneurship, or overall economic environment.",
    
    "Surveys addressing employment rates, workforce trends, job market conditions, unemployment, labor rights, or working conditions.",
    
    "Surveys covering poverty, inequality, social welfare policies, demographic trends, vulnerable groups, or quality of life.",
    
    "Surveys on medical care, public health services, healthcare quality, medical infrastructure, diseases, health insurance, or medical practices.",
    
    "Surveys related to schools, universities, educational quality, literacy, education policies, student attitudes, educational infrastructure, or training.",
    
    "Surveys on roads, transportation, housing, urban development, local infrastructure projects, public spaces, or communal services.",
    
    "Surveys explicitly focused on civic participation, voter behavior, public activism, community involvement, volunteering, or citizens' engagement in local decision-making.",
    
    "Surveys covering national identity, traditions, historical memory, language use, cultural practices, or public attitudes toward cultural heritage.",
    
    "Surveys related to crime, law enforcement, judicial systems, public security, police activities, courts, prisons, or citizen safety.",
    
    "Surveys focused explicitly on ecological issues, climate change, pollution, conservation efforts, natural resources, sustainability, or public attitudes toward environmental protection.",
    
    "Surveys on media credibility, information consumption habits, press freedom, misinformation, media literacy, or public attitudes toward journalism and information.",
    
    "Surveys on technological developments, digitalization, internet use, cybersecurity, innovation, digital infrastructure, or public attitudes toward technology.",
    
    "Surveys focused on energy production, energy policies, electricity or fuel markets, renewable energy, energy efficiency, or public attitudes toward energy sources.",
    
    "Surveys explicitly about migration, refugees, immigration policies, integration of migrants, public attitudes toward immigrants or migration-related topics."
  )
)


# Classification Function
classify_topic <- function(text) {
  if (!is.na(text) && nzchar(text)) {
    prompt <- paste0(
      "Classify the survey's primary topic from these categories:\n",
      paste(topics_definitions$Topic, collapse = "; "),
      ".\n\nDefinitions:\n",
      paste(paste0(topics_definitions$Topic, ": ", topics_definitions$Definition), collapse = "; "),
      ".\n\nSurvey description:\n", text,
      "\n\nReturn only the best-matching topic or 'None'."
    )
    
    result <- request_with_retry(prompt, "gpt-4o-mini")
    
    if (!is.na(result) && result %in% topics_definitions$Topic) {
      return(result)
    } else {
      return("None")
    }
  } else {
    return("None")
  }
}


# 7.1: Test Classification
cat("Testing classification on 10 rows...\n")

test_indices <- sample(seq_len(nrow(filtered_df)), 10)
test_results <- future_lapply(test_indices, function(i) {
  classify_topic(filtered_df$survey_topic[i])
})

test_df <- data.table(
  message = filtered_df$survey_topic[test_indices],
  classified_topic = unlist(test_results)
)

print(test_df)


# 7.2: Full Classification
cat("Running full classification...\n")

filtered_df[, classified_topic := future_lapply(survey_topic, classify_topic)]

filtered_df[, classified_topic := unlist(classified_topic)]


# Summary
cat("Total classified rows:", sum(!is.na(filtered_df$classified_topic)), "\n")
cat("Total rows:", nrow(filtered_df), "\n")

# Save
save_partial_results(filtered_df, prefix = "step7")




# Step 8: 
# 8.1: збір посилань з новин із фільтром за словником ---------------------------
# 1 Створюємо патерн із даних у Excel та зберігаємо крапки
executor_links_clean <- na.omit(org_dict$executor_links)
executor_links_clean <- gsub("\\.", "\\\\.", executor_links_clean) # екрануємо крапки
dict_links <- paste(executor_links_clean, collapse = "|")

# 2 Функція для збирання посилань
extract_links <- function(url, dict_links) {
  tryCatch({
    # якщо URL новини містить елемент зі словника — повертаємо її
    if (grepl(dict_links, url, ignore.case = TRUE)) {
      return(url)
    }
    
    # інакше парсимо сторінку
    page <- read_html(url)
    links <- html_attr(html_nodes(page, "a"), "href")
    links <- unique(na.omit(links))
    links <- links[grepl("^https?://", links, ignore.case = TRUE)]
    
    # Залишаємо лише ті, які містять хоча б один елемент зі словника
    links <- links[grepl(dict_links, links, ignore.case = TRUE)]
    if (length(links) == 0) return(NA_character_)
    # Об'єднуємо всі посилання через ;
    paste(links, collapse = "; ")
    
  }, error = function(e) NA_character_)
}

# 3 Паралельне збирання посилань
filtered_df[, collected_links := future_lapply(URL, extract_links, dict_links = dict_links)]
filtered_df[, collected_links := unlist(collected_links)]

# 3alt Simpler approach without parallelization
filtered_df[, collected_links := sapply(URL, extract_links, dict_links = dict_links)]

# 4 Відбір .pdf, drive.google та випадків із кількома посиланнями
filtered_df[, checkthelinks := ifelse(
  grepl("\\.pdf|drive\\.google|;", collected_links, ignore.case = TRUE),
  "Перевірити",
  NA_character_
)]



#--------------------------------------------------------------------------
# валідація посилань вручну (checkthelinks = 'Перевірити')
# прибрати помилкові посилання з collected_links
save_partial_results(filtered_df, "step8_check.xlsx")


# зберегти масив та завантажити 'Оновлений масив.xlsx'
new_df <- read_excel("Оновлений масив згадок.xlsx") |> as.data.table()
#--------------------------------------------------------------------------



# 8.2: Наявність посилання на звіт/прес-реліз ----------------------------------
new_df[, report := ifelse(is.na(collected_links), "Ні", "Так")]
table(new_df$report) #розподіл




# Step 9: Ідентифікація висвітлення однакових опитувань
new_df[, survey_id := NA_character_]

# Підготовка дати
new_df[, pub_date := as.Date(substr(`Дата виходу`, 1, 10))]

# Початковий лічильник — максимальний існуючий survey_id
start_id <- ifelse(
  any(!is.na(new_df$survey_id)),
  suppressWarnings(max(as.integer(gsub("\\D", "", new_df$survey_id)), na.rm = TRUE)),
  0
)
counter <- start_id

# Проходимо по всіх рядках, де survey_id ще немає
for (i in seq_len(nrow(new_df))) {
  
  if (!is.na(new_df$survey_id[i])) next
  
  row_i <- new_df[i]
  
  # умови пошуку однакових опитувань
  matches <- which(
    is.na(new_df$survey_id) &
      new_df$classified_topic == row_i$classified_topic &
      abs(as.numeric(new_df$pub_date - row_i$pub_date)) <= 5 &
      (
        (!is.na(new_df$org1_final) & new_df$org1_final %in% c(row_i$org1_final, row_i$org2_final)) |
          (!is.na(new_df$org2_final) & new_df$org2_final %in% c(row_i$org1_final, row_i$org2_final))
      )
  )
  
  # Якщо знайдено більше одного рядка, створюємо нову групу
  if (length(matches) > 1) {
    counter <- counter + 1
    new_df[matches, survey_id := paste0("S", counter)]
  }
}

new_df[, pub_date := NULL] #прибираємо технічну колонку

# Огляд результату
new_df[!is.na(survey_id), .N, by = survey_id][order(-N)]

# Save
save_partial_results(new_df, prefix = "step9")



# -----------------------------------------------------------------------------
# 1. Classification Summary Plot ---------------------------------------------
# -----------------------------------------------------------------------------

# Словник перекладу тем
topic_translation <- c(
  "Civic Engagement and Participation" = "Громадянська участь",
  "Culture and Identity" = "Культура та ідентичність",
  "Economic and Business Climate" = "Економіка та бізнес-клімат",
  "Employment and Labor Market" = "Зайнятість і ринок праці",
  "Energy" = "Енергетика",
  "Foreign Relations / International Affairs" = "Зовнішні справи",
  "Healthcare" = "Охорона здоров’я",
  "Immigration" = "Міграція",
  "Infrastructure and Urban Planning" = "Інфраструктура і містопланування",
  "Media and Information" = "Медіа та інформація",
  "Military and Defense" = "Військо та оборона",
  "None" = "Інше / невизначено",
  "Political Trust and Governance" = "Політична довіра та врядування",
  "Social Issues and Welfare" = "Соціальні питання та добробут",
  "Technology" = "Технології",
  "Education" = "Освіта",
  "Environment and Climate" = "Довкілля та клімат",
  "Justice and Law Enforcement" = "Правосуддя та правоохоронні органи"
)

# -----------------------------------------------------------------------------
# 1. Classified Topics for February ------------------------------------------
# -----------------------------------------------------------------------------

# 1.1. Створюємо підтаблицю лише для лютого 2025 року
feb_df <- filtered_df %>%
  filter(month_year == "2025-02")

# 1.2. Підрахунок кількості за темою в лютому
feb_summary <- feb_df %>%
  count(classified_topic) %>%
  mutate(proportion = n / sum(n))

# 1.3. Додати переклад теми (як було визначено раніше)
feb_summary$topic_ua <- topic_translation[ as.character(feb_summary$classified_topic) ]

# 1.4. Впорядкувати фактори за зростанням частки (щоб найменш популярні були зверху)
feb_summary <- feb_summary %>%
  filter(topic_ua != "Інше / невизначено") %>%
  mutate(topic_ua = fct_reorder(topic_ua, proportion))

# 1.5. Створення палітри
n_colors_feb <- nrow(feb_summary)
colors_feb    <- scales::hue_pal()(n_colors_feb)

# 1.6. Побудова графіка для лютого
feb_classified_plot <- ggplot(
  feb_summary,
  aes(x = proportion, y = topic_ua, fill = "deepskyblue")
) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 1)),
    hjust = -0.1,
    size = 3
  ) +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = colors_feb) +
  labs(
    title = "Найпопулярніші теми опитувань у лютому 2025",
    x = "Частка новин (%)",
    y = NULL,
    fill = "Тема"
  ) +
  theme_classic() +
  theme(
    axis.text.y      = element_text(size = 12),
    axis.text.x      = element_text(size = 10),
    plot.title       = element_text(size = 16, face = "bold"),
    legend.position  = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# 1.7. Збереження PNG
ggsave(
  filename = "feb_classified_topics.png",
  plot     = feb_classified_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# 2. Top 10 News by Views in February -----------------------------------------
# -----------------------------------------------------------------------------

top_10_feb <- feb_df %>%
  filter(!is.na(`Сумарна кількість переглядів разом із дублями`)) %>%
  slice_max(order_by = `Сумарна кількість переглядів разом із дублями`, n = 10) %>%
  arrange(desc(`Сумарна кількість переглядів разом із дублями`)) %>%
  select(
    month_year,
    'Текст',
    Заголовок,
    survey_topic,
    classified_topic,
    `Сумарна кількість переглядів разом із дублями`,
    `Тип ЗМІ`,
    Джерело,
    `Рівень видання`
  )

# Відображаємо результати
print(top_10_feb)

# -----------------------------------------------------------------------------
# 4. Metadata Coverage for February -------------------------------------------
# -----------------------------------------------------------------------------

# 4.1. Відфільтрувати лише лютого та обчислити частки "yes"
feb_meta_summary <- feb_df[, lapply(.SD, function(x) mean(x == "yes")), .SDcols = meta_keys]

# 4.2. Довгий формат
feb_meta_long <- as.data.frame(feb_meta_summary) %>%
  rownames_to_column(var = "meta") %>%
  pivot_longer(
    cols      = -meta,
    names_to  = "metadata",
    values_to = "share"
  )
feb_meta_long$label <- meta_labels[match(feb_meta_long$metadata, meta_keys)]

# 4.3. Побудова графіка
feb_metadata_plot <- ggplot(feb_meta_long, aes(x = reorder(label, share), y = share)) +
  geom_col(fill = "deepskyblue") +
  geom_text(
    aes(label = percent(share, accuracy = 1)),
    hjust = -0.1,
    size  = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.1 * max(feb_meta_long$share))
  ) +
  labs(
    title = NULL,
    x     = NULL,
    y     = "Частка новин (% від кількості лютого)"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13)  )

# 4.4. Збереження
ggsave(
  filename = "feb_metadata_coverage.png",
  plot     = feb_metadata_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# Sampling a Few Example News Items by Topic (for February) ------------------
# -----------------------------------------------------------------------------

# Для кожної теми (classified_topic) випадково вибрати до 10 рядків:
sampled_by_topic <- feb_df[
  , .SD[sample(.N, min(.N, 10))],   # якщо в групі менше 10 рядків, взяти всі
  by = classified_topic
]

# Виведіть на екран результати (загальна кількість у кожному підгрупі може бути ≤ 10):
print(sampled_by_topic)

# Або переглянути у табличному вигляді (відкриється в RStudio Viewer):
sampled_by_topic %>%
  select(classified_topic, Текст) %>%
  View()


# -----------------------------------------------------------------------------
# 3. Побудова Графіка ---------------------------------------------------------
# -----------------------------------------------------------------------------

top20 <- feb_df[
  !is.na(org_final),
  .N,
  by = org_final
][order(-N)][1:20]

org_plot <- ggplot(top20, aes(x = reorder(org_final, N), y = N)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Топ-20 організацій за кількістю згадок",
    x     = NULL,
    y     = "Кількість згадок"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    plot.title  = element_text(size = 14, face = "bold")
  )

# Відобразити графік
print(org_plot)

ggsave(filename = "top20_organizations.png", plot = org_plot, width = 8, 
       height = 6, dpi = 300)



write.csv(final_df, 'final.csv')

final_df <- feb_df %>% 
  rename(
    organisation_name_raw = organisation,
    organisation_name_clean = org_final
  ) %>%
  rename_with(
    .cols = c("survey_focus", "sponsor", "survey_org", "dates", "population", "sample_size",
              "sampling_method", "margin_of_error", "weighting", 
              "survey_mode", "question_text", "percentages"),
    .fn = ~ paste0(.x, "_check")
  ) %>% 
  select(-c(org_clean, org_clean_1))
