library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Set working directory (adjust as needed)
setwd("C:/Users/Veronila Slavíková/Documents/Analyza_dat/data_synergys")

# Read data
Open_day <- read_delim("Data_Open_day.csv", delim = ";")
Hearing <- read_delim("Data_public_hearing.csv", delim = ";")

# Function to create and save bar plots
create_bar_plot <- function(data, question_code, title, x_label, y_label, recode_labels, source_levels) {
  
  # Process and filter data
  plot_df <- data %>%
    pivot_longer(
      cols = starts_with("otazka_"),
      names_to = "question_code",
      values_to = "response_code"
    ) %>%
    mutate(response_code = str_trim(response_code)) %>%
    filter(question_code == !!question_code, !(response_code %in% c("-", "NA")), !is.na(response_code)) %>%
    mutate(
      response_label = recode(response_code, !!!recode_labels),
      source_label = recode(source, "Open Day" = "Den otevřených dveří", "Public Hearing" = "Veřejné projednání")
    ) %>%
    mutate(
      response_label = factor(response_label, levels = recode_labels),
      source_label = factor(source_label, levels = source_levels)
    )
  
  # Create and print the plot
  plot <- ggplot(plot_df, aes(x = response_label, fill = source_label)) +
    geom_bar(position = position_dodge(width = 0.9), width = 0.8) +
    labs(title = title, x = x_label, y = y_label, fill = "Zdroj dat") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot)
}

# Combine and process data for questions 1-4
combined_df_1_4 <- bind_rows(
  Open_day %>% select(otazka_1, otazka_2, otazka_3, otazka_4) %>% mutate(source = "Open Day"),
  Hearing %>% select(otazka_1, otazka_2, otazka_3, otazka_4) %>% mutate(source = "Public Hearing")
)

# Call the function for each bar plot
create_bar_plot(
  data = combined_df_1_4,
  question_code = "otazka_1",
  title = "Který z těchto obnovitelných zdrojů považujete za nejperspektivnější v našich klimatických podmínkách?",
  x_label = "Typ obnovitelného zdroje",
  y_label = "Počet odpovědí",
  recode_labels = c("a" = "solární", "b" = "větrný", "c" = "vodní", "d" = "geotermální", "e" = "biomasa"),
  source_levels = c("Veřejné projednání", "Den otevřených dveří")
)

create_bar_plot(
  data = combined_df_1_4,
  question_code = "otazka_2",
  title = "Je podle Vás geotermální energie perspektivní zdroj?",
  x_label = "Odpovědi",
  y_label = "Počet odpovědí",
  recode_labels = c("a" = "ano", "b" = "spíše ano", "c" = "spíše ne", "d" = "ne", "e" = "nevím"),
  source_levels = c("Veřejné projednání", "Den otevřených dveří")
)

create_bar_plot(
  data = combined_df_1_4,
  question_code = "otazka_3",
  title = "Jaký je Váš názor na to, že město Litoměřice chce využívat geotermální energii pro vytápění?",
  x_label = "Odpovědi",
  y_label = "Počet odpovědí",
  recode_labels = c("a" = "rozhodně to podporuji", "b" = "nejsem si jistý, ale chci se dozvědět víc", "c" = "obávám se toho", "d" = "zásadně to nepodporuji", "e" = "nevím"),
  source_levels = c("Veřejné projednání", "Den otevřených dveří")
)

create_bar_plot(
  data = combined_df_1_4,
  question_code = "otazka_4",
  title = "Víte, jaká je teplota zemské kůry v Litoměřicích v hloubce 1 km?",
  x_label = "Odpovědi",
  y_label = "Počet odpovědí",
  recode_labels = c("a" = "3,2 °C", "b" = "32 °C", "c" = "320 °C"),
  source_levels = c("Veřejné projednání", "Den otevřených dveří")
)

# Function to create and save pie charts
create_pie_chart <- function(data, question_code, title, recode_labels, source_label) {
  
  # Process and filter data
  plot_df <- data %>%
    select(!!question_code) %>%
    mutate(source = source_label) %>%
    pivot_longer(
      cols = starts_with("otazka_"),
      names_to = "question_code",
      values_to = "response_code"
    ) %>%
    mutate(response_code = str_trim(response_code)) %>%
    filter(!(response_code %in% c("-", "NA")), !is.na(response_code)) %>%
    mutate(response_label = recode(response_code, !!!recode_labels)) %>%
    mutate(source_label = recode(source, "Open Day" = "Den otevřených dveří", "Public Hearing" = "Veřejné projednání"))
  
  # Reorder levels
  response_levels <- plot_df %>% distinct(response_label) %>% pull(response_label) %>% setdiff("jiné") %>% sort() %>% c("jiné")
  plot_df <- plot_df %>% mutate(response_label = factor(response_label, levels = response_levels))
  
  # Summarize data
  plot_summary <- plot_df %>%
    group_by(source_label, response_label) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(source_label) %>%
    mutate(percent = count / sum(count), label = paste0(round(percent * 100), "%"))
  
  # Create and print the plot
  plot <- ggplot(plot_summary, aes(x = "", y = count, fill = response_label)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    facet_wrap(~ source_label) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 3
    ) +
    labs(title = title, fill = "Odpovědi") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  print(plot)
}

# Call the function for each pie chart
create_pie_chart(
  data = Hearing,
  question_code = "otazka_5",
  title = "Jaké jsou důvody Vaší účasti na veřejném projednání?",
  recode_labels = c("a" = "dozvědět se něco o geotermální energii v Litoměřicích", "b" = "možnost vyjádřit se k vývoji událostí", "c" = "možnost profesního rozvoje, nebo navázání kontaktů", "d" = "setkat se s podobně smýšlejícími jednotlivci nebo organizacemi", "e" = "zvědavost, nebo obecný zájem", "f" = "zúčastnit se slosování o ceny", "g" = "akci doporučili přátelé / rodina / kolegové", "h" = "jiné"),
  source_label = "Public Hearing"
)

create_pie_chart(
  data = Open_day,
  question_code = "otazka_6",
  title = "O dnu otevřených dveří jsem se dozvěděl/a z tohoto zdroje:",
  recode_labels = c("a" = "plakátek", "b" = "video", "c" = "osobní pozvání", "d" = "Radniční zpravodaj", "e" = "infocentrum Litoměřice", "f" = "pozvánka České spořitelny", "g" = "jiné"),
  source_label = "Open Day"
)

create_pie_chart(
  data = Open_day,
  question_code = "otazka_7",
  title = "Na den otevřených dveří jsem přišel / přišla protože:",
  recode_labels = c("a" = "fandím využití geotermální energie", "b" = "mám obavy z využití GTE", "c" = "zajímá mne projekt SYNERGYS a chci o něm vědět víc", "d" = "chci vidět geotermickou laboratoř a vrtná jádra", "e" = "geotermální energie mne nezajímá, jsem tu jen ze zvědavosti", "f" = "jiné"),
  source_label = "Open Day"
)