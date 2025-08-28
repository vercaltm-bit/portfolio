install.packages(dplyr)
CZT <- read_csv("/cloud/project/CZT_statistika/Data_CZT_1.csv")
as_tibble(CZT)
# Example: Clean the 'Vykon_MW' column by removing commas and spaces
CZT$Vykon_MW <- gsub("[ ,]", "", CZT$Vykon_MW)

# Now, convert the cleaned column to a numeric type
CZT$Vykon_MW <- as.numeric(CZT$Vykon_MW)

# Check the new data type
str(CZT$Vykon_MW)

ustecky <- CZT[, c("Kraj", "Uhli", "ZP", "Biom_OZE", "Topne_oleje", "Jina_paliva", "Vykon_MW")] %>% 
  filter(Kraj == "U")
View(ustecky)

# Calculation and printing of the percentage
total_rows <- nrow(ustecky)
rows_less_than_5k <- ustecky %>%
  filter(Vykon_MW < 5.000) %>%
  nrow()

percentage <- (rows_less_than_5k / total_rows) * 100

cat("Podíl CZT v Ústeckém kraji s výkonem < 5.000:", round(percentage, 2), "%\n")

# This new chunk of code filters out the 'Biomasa/OZE' column
ustecky_bez_OZE <- ustecky %>%
  select(-Biom_OZE)


average_uhli <- ustecky_bez_OZE %>% 
  filter(Vykon_MW < 5.000) %>% 
  summarize(average_uhli = mean(Uhli, na.rm = TRUE))

print(average_uhli)

# pipe
# average_uhli <- CZT[, c("Kraj", "Uhli", "ZP", "Topne_oleje", "Jina_paliva", "Vykon_MW")] %>% 
#  filter(Kraj == "U") %>% 
#  filter(Vykon_MW < 5.000) %>% 
#  summarize(average_uhli = mean(Uhli, na.rm = TRUE))




