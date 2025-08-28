library(tidyverse)
library(readxl)
library(writexl)
library(tools)
library(data.table)

# Set the path to your Excel file and read the data
setwd("/cloud/project/Katarina_data")
data1 <- read_excel(path = "HG_vrt.xlsx", range = "AB2:AH360")

# Convert Metraz to numeric and round down, and rename columns
data1$Metraz <- floor(as.numeric(data1$Metraz))
data1 <- data1 %>% rename(TC_1 = TC...4, TD_1 = TD...5, TC_2 = TC...6, TD_2 = TD...7)

# --- Find all DAT files in the working directory ---
# This line lists all files ending with ".dat"
dat_files <- list.files(path = ".", pattern = "\\.dat$", full.names = FALSE)

# Check if any DAT files were found
if (length(dat_files) == 0) {
  cat("No DAT files found in the working directory.\n")
} else {
  # --- Loop through each DAT file ---
  for (input_file in dat_files) {
    cat(paste("Processing file:", input_file, "\n"))
    
    # Extract numbers from filename
    filename_no_ext <- tools::file_path_sans_ext(input_file)
    parts <- strsplit(filename_no_ext, "_")
    first_number <- as.double(parts[[1]][1])
    second_number <- as.double(parts[[1]][2])
    
    # Read the data from the current DAT file and select the first 6 columns
    dat_data <- read.table(input_file, header = FALSE, sep = "", stringsAsFactors = FALSE)
    selected_data <- dat_data[, 1:6]
    
    # --- Function to calculate average and update data frame ---
    # This function now takes the target column names as arguments
    update_data <- function(row_indices, col_index_A, col_index_F, metraz_val, col_name_tc, col_name_td) {
      # Calculate the average for 'TC' (column 1 in selected_data)
      avg_tc <- (selected_data[row_indices[1], col_index_A] + selected_data[row_indices[2], col_index_A]) / 2
      
      # Calculate the average for 'TD' (column 6 in selected_data)
      avg_td <- (selected_data[row_indices[1], col_index_F] + selected_data[row_indices[2], col_index_F]) / 2
      
      # Find the row in data1 to update
      row_to_update <- which(data1$Metraz == metraz_val)
      
      # Update the data1 data frame with the calculated averages
      data1[row_to_update, col_name_tc] <<- avg_tc
      data1[row_to_update, col_name_td] <<- avg_td
      
      # Return the updated data frame (though we are using <<- to modify it directly)
      return(data1)
    }
    
    # --- Apply the function for the first number (first_number) ---
    data1 <- update_data(c(1, 3), 1, 6, first_number, "TC_1", "TD_1") # For TC_1 and TD_1
    data1 <- update_data(c(5, 7), 1, 6, first_number, "TC_2", "TD_2") # For TC_2 and TD_2
    
    # --- Apply the function for the second number (second_number) ---
    data1 <- update_data(c(2, 4), 1, 6, second_number, "TC_1", "TD_1") # For TC_1 and TD_1
    data1 <- update_data(c(6, 8), 1, 6, second_number, "TC_2", "TD_2") # For TC_2 and TD_2
  }
}

# --- Final Step: Write the updated data frame to a new Excel file ---
# We'll save the modified data frame back to a new Excel file using writexl.
output_data1_file <- "HG_vrt_updated.xlsx"
writexl::write_xlsx(data1, output_data1_file)

# Print a confirmation message
cat(paste("Successfully inserted averages and saved updated data to", output_data1_file, "\n"))