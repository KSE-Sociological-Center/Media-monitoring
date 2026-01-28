# 1. Libraries & Parallel Setup
library(data.table)       
library(readxl)           
library(tidyverse)
library(jsonlite)
library(openxlsx)
library(future.apply)     
library(stringr)
library(scales)
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
setwd("D:\\work\\sociological center\\chatgpt coding")
path <- "./Масив згадок опитувань січень-лютий 2025.xlsx"
monitoring_df <- as.data.table(read_excel(path, sheet = "Статті"))[
  !(`Тип ЗМІ` %in% c("Соціальні мережі", "Форум", "Відгук", "Радіо", "ТБ"))
]
# drop unused cols, clean dates
monitoring_df[, `Дата виходу` := as.Date(`Дата виходу`)]
monitoring_df[, month_year := format(`Дата виходу`, "%Y-%m")]


# 3. OpenAI Config & Helper
api_key <- "your-api-key"
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

# 4. Step 1: Identify messages with survey focus

test_df <- monitoring_df %>% sample_n(10)

# one-liner: classify & filter in place
test_df[, survey_focus := unlist(future_lapply(Текст, function(txt) {
  if (is.na(txt) || txt == "") return(NA_character_)
  # build prompt
  prompt <- paste0(
    "Чи є соціологічне опитування або його результати **центральною темою** цього повідомлення?\n\n",
    "Відповідай лише 'Так' або 'Ні'.\n\n",
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




# 5. Step 2: Extract organisation & survey topic

# test_df <- filtered_df[5:10,]

# 1) Подготовим пустые колонки
filtered_df[, `:=`(
  organisation  = NA_character_,
  survey_topic  = NA_character_
)]

# 2) Один промпт для двух задач: вказати виконавця і тему
prompt_combined <- paste(
  "1) Вкажи виконавця опитування у форматі `Хто проводив: <назва>`.",
  "2) Одним реченням дай коротку тему опитування у форматі `Тема: <тема>`.",
  "Відповідай рівно двома рядками, без жодних інших пояснень.",
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
  org <- NA_character_; topic <- NA_character_
  if (!is.na(res) && nzchar(res)) {
    lines <- unlist(strsplit(res, "\r?\n"))
    lines <- lines[trimws(lines) != ""]
    who <- grep("^\\s*Хто проводив:", lines, value = TRUE)
    thm <- grep("^\\s*Тема:",       lines, value = TRUE)
    if (length(who)>0) org   <- str_trim(sub("^\\s*Хто проводив:\\s*", "", who[1]))
    if (length(thm)>0) topic <- str_trim(sub("^\\s*Тема:\\s*",       "", thm[1]))
  }
  
  # checkpoint every 1000
  if (i %% 1000 == 0) {
    save_partial_results(filtered_df, prefix = "filtered_opituvalni_checkpoint")
  }
  
  list(organisation = org, survey_topic = topic)
})

# 4) Assign back
res_dt <- data.table::rbindlist(results)
filtered_df[, organisation  := res_dt$organisation]
filtered_df[, survey_topic  := res_dt$survey_topic]

# 5) Final save
save_partial_results(filtered_df, prefix = "filtered_opituvalni_final")



# Крок 2: Фільтрація організацій =============================

# Step 1: Ask GPT whether each value is a concrete org-name or “ні”

prompt_org <- 
"Чи наведена тут назва установи, організації чи видання? 
Якщо так, то поверни назву, якщо ні, то напиши 'ні' одним словом. 
Output format: Назва установи/організації/видання або 'ні'."

# query + normalize in one go
filtered_df[, org_clean := unlist(future_lapply(organisation, function(txt) {
  res <- request_with_retry(paste(prompt_org, txt, sep = "\n\n"), "gpt-4o-mini")
  str_trim(res)
}))]

filtered_df %>% select(organisation, org_clean) %>% sample_n(200) %>% view()

# Step 2: Basic text normalization
filtered_df[, organisation := tolower(organisation)]
filtered_df[, organisation := str_replace_all(organisation, '[“”"«».,]', '')]
filtered_df[, organisation := str_trim(organisation)]
filtered_df[, organisation := str_remove_all(organisation, '\\s+(україни|україни)$')]
filtered_df[, organisation := str_replace_all(organisation, '[-/]', ' ')]


# Step 2.5: Manual overrides for known exceptions -------------------------

# 1) Список організацій, які хочемо зберегти незважаючи на відповідь GPT
manual_keep <- c(
  "acted",
  "active group",
  "ard",
  "мінфін", "minfincomua",
  "імі", "pollster",
  "dozorro ti ukraine ",
  "grcua", "главком",
  "info sapiens",
  "linkedin",
  "rating lab",
  "socis",
  "znua",
  "главком",
  "група рейтинг",
  "кміс",
  "оперативна соціологія",
  "соціологічна група рейтинг",
  "федерація роботодавців",
  "центр громадської експертизи"
)

# 3) Для всіх рядків, де після GPT вийшло NA або "ні",
#    але normalized original входить у наш список — відновлюємо org_clean
filtered_df[
  # умова: або NA, або "ні", і при цьому our normalized original в manual_keep_norm
  (is.na(org_clean) | org_clean == "ні") &
    organisation %in% manual_keep,
  org_clean := organisation
]



# Step 3: Групування подібних назв =============================
# Отримуємо відсортований вектор унікальних назв (без "ні")
unique_names <- sort(unique(filtered_df$org_clean[!is.na(filtered_df$org_clean)]))

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

# Крок 5: Сортування та об'єднання схожих назв =============================
filtered_df$org_clean_1 <- filtered_df$org_clean
for(i in 1:(length(grouped_names)-1)) {
  prev_name <- grouped_names[i]
  next_name <- grouped_names[i+1]
  prompt <- paste0(
    "Порівняйте наступні дві назви організацій:\n",
    "1) '", prev_name, "'\n",
    "2) '", next_name, "'\n\n",
    "Якщо ці назви позначають ту ж саму організацію, і одна з них є коротшою, поверніть коротшу назву. Якщо це різні організації, поверніть першу назву."
  )
  response <- hey_chatGPT(prompt, "gpt-4o")
  response <- trimws(tolower(response))

  if(response == next_name) {
    cat("Порівняння:\n  Попередня: ", prev_name, "\n  Наступна: ", next_name, "\n  Обрана: ", next_name, "\n\n")
    grouped_names[grouped_names == prev_name] <- next_name
    df[org_clean_1 == prev_name, org_clean_1 := next_name]
    grouped_names <- sort(unique(filtered_df$org_clean_1))
  } else {
    cat(i, "\n")
  }
}
print(filtered_df[, .(org_clean_1)])


# Step 4.5: Manual overrides for known exceptions -------------------------

# 1) Список організацій, які хочемо зберегти незважаючи на відповідь GPT
manual_keep <- c(
  "лун",  "axios",
  "назк",  "forsa",
  "моз",  "оон",
  "апостроф", "dw",
  "армія+",
  "суспільне",
  "dou", "фокус",
  "нбу"
)

# 3) Для всіх рядків, де після GPT вийшло NA або "ні",
#    але normalized original входить у наш список — відновлюємо org_clean
filtered_df[
  # умова: або NA, або "ні", і при цьому our normalized original в manual_keep_norm
  (is.na(org_clean_1) | org_clean_1 == "ні") &
    organisation %in% manual_keep,
  org_clean_1 := organisation
]

# Крок 6: Формування фінального списку назв організацій =============================
# Розрахунок частот появи для колонки trash_cleaned_2 та вибір топ-150 назв
freq_table <- filtered_df[, .N, by = org_clean_1][order(-N)]
top_150_names <- freq_table[1:150, org_clean_1]

prompt <- paste0(
  "Ось список 150 назв організацій, впорядкованих за частотою:\n",
  paste(top_150_names, collapse = "\n"),
  "\n\nБудь ласка, проаналізуйте цей список і, якщо знайдете дублікати (назви, що позначають ту ж організацію), ",
  "упорядкуйте їх так, щоб повторювані назви йшли один за одним. \nВихідний формат: список назв (без нумерації), по одному рядку."
)

response <- hey_chatGPT(prompt, "gpt-4o")
cat("Відповідь API (групування назв):\n", response, "\n")

# Розбиваємо відповідь API на рядки
response_clean <- unlist(strsplit(response, "\n"))

good_lines <- response_clean[!grepl("^\\s*ні\\s*$", response_clean, ignore.case = TRUE)]

# Step 4: Ask GPT to cluster duplicates among the top names
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

# Step 5: Build a mapping table from variant → canonical
mapping <- rbindlist(lapply(lines, function(line) {
  if (str_detect(line, "\\(")) {
    can  <- str_trim(str_extract(line, "^[^\\(]+"))
    vars <- str_split(str_extract(line, "(?<=\\().*(?=\\))"), ",")[[1]] %>% str_trim()
    data.table(variant=vars, canonical=can)
  } else {
    data.table(variant=str_trim(line), canonical=str_trim(line))
  }
}), fill=TRUE)

# Step 6: Apply mapping back to filtered_df
setkey(mapping, variant)
filtered_df[, org_final := {
  can <- mapping[J(org_clean_1), canonical]
  # якщо canonical NA, повертаємо org_clean
  fifelse(is.na(can), org_clean_1, can)
}]

# Step 4.5: Manual overrides for known exceptions -------------------------

# 1) Список організацій, які хочемо зберегти незважаючи на відповідь GPT
manual_keep <- c(
  "Minfin.com.ua", 
  "Opinia24 для RMF FM",
  "Pollster",  "Главком",
  "Мінфін",  "Служба зайнятості", "ІМІ", "Рейтинг"
)

# 3) Для всіх рядків, де після GPT вийшло NA або "ні",
#    але normalized original входить у наш список — відновлюємо org_clean
filtered_df[
  # умова: або NA, або "ні", і при цьому our normalized original в manual_keep_norm
  (is.na(org_final) | org_final == "ні") &
    organisation %in% manual_keep,
  org_final := organisation
]








# Step. Quality of reporting --------------------------------
# Replace with your actual text vector
texts <- filtered_df$Текст

# Define metadata keys and their Ukrainian labels
meta_keys <- c(
  "sponsor", "survey_org", "dates", "population", "sample_size",
  "sampling_method", "margin_of_error", "weighting",
  "survey_mode", "question_text", "percentages"
)

meta_labels <- c(
  "Замовник опитування", 
  "Організація, яка проводила опитування", 
  "Дати проведення опитування", 
  "Генеральна сукупність", 
  "Розмір вибірки", 
  "Метод вибірки", 
  "Похибка вибірки", 
  "Застосування вагових коефіцієнтів", 
  "Спосіб проведення опитування", 
  "Текст запитання", 
  "Про які відсотки йдеться"
)
names(meta_labels) <- meta_keys

# Build prompt
make_prompt <- function(text) {
  paste0(
    "Для кожного з наведених пунктів напиши 'так' або 'ні' — чи згадується ця інформація в повідомленні. Поверни рівно 11 рядків у форматі:\n",
    paste0(meta_labels, ": так/ні", collapse = "\n"),
    "\n\nТекст повідомлення:\n", text
  )
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





# Closed topic coding
# Define the list of topics -------------------------------------------------

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


# Classification Function ---------------------------------------------------

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

# Step 1: Test Classification ------------------------------------------------

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

# Step 2: Full Classification ------------------------------------------------

cat("Running full classification...\n")

filtered_df[, classified_topic := future_lapply(survey_topic, classify_topic)]

filtered_df[, classified_topic := unlist(classified_topic)]
# Step 3: Save Results -------------------------------------------------------

save_partial_results(filtered_df, "final_results")

# Summary --------------------------------------------------------------------

cat("Total classified rows:", sum(!is.na(monitoring_df$classified_topic)), "\n")
cat("Total rows:", nrow(monitoring_df), "\n")





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
