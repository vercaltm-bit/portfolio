library(dplyr)
library(readr)
library(stringr)

# Data loading
df_fuel <- read_delim("CZT_fuel.csv", delim = ";",
  locale = locale(encoding = "UTF-8", decimal_mark = ",")
)

# Removing a column 'Lokalita'
df_fuel$Lokalita <- NULL

# Removing empty columns
df_fuel <- df_fuel[, !sapply(df_fuel, function(x) all(is.na(x) | x == ""))]

# Changing the data type of a column "Instalovany_vykon_MW"
df_fuel <- df_fuel %>%
  mutate(
    `Instalovany_vykon_MW` = str_replace(`Instalovany_vykon_MW`, ",", "."),
    `Instalovany_vykon_MW` = as.double(`Instalovany_vykon_MW`)
  )

# Saving the modified file to a new CSV file
write.csv(df_fuel, "CZT_fuel_upraveno1.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Data type checking
str(df_fuel)

# Data display
View(df_fuel)
