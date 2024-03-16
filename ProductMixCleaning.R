# Parker Sweeney 03/15/2024
# Code will show parsing warnings in the console, can be disregarded

# Load the necessary libraries
library(readr)
library(dplyr)

# Specify the root file path
file_RootBaseRel <- "C:/Users/sween/OneDrive/Documents/DelRioReport/ProductMix/"
report_year <- 2020
report_month <- "Jan"
rel_file_path <- paste0(file_RootBaseRel,as.character(report_year),"/Raw/",report_month,"/")

# Get a list of all CSV files in the directory
file_names <- list.files(path = rel_file_path, pattern = "\\.csv$", full.names = TRUE)

# Loop through each file in the directory
for(file_path in file_names) {
  
  # Read the report date from line 6
  lines <- read_lines(file_path, n_max = 10)
  reportDate <- lines[6]
  editedReportDate <- gsub("/","_",reportDate)
  
  # Read the header lines (9 and 10), which will be concatenated
  header_line_1 <- strsplit(lines[9], ",")[[1]]
  header_line_2 <- strsplit(lines[10], ",")[[1]]
  
  # Clean and concatenate the headers from lines 9 and 10
  concatenated_header <- mapply(FUN = function(x, y) {
    paste0(trimws(gsub('"', '', x)), trimws(gsub('"', '', y)))
  }, header_line_1, header_line_2, SIMPLIFY = TRUE)
  concatenated_header <- gsub(" ", "", concatenated_header)
  
  # Ensure concatenated_header has no NAs or empty strings
  concatenated_header[is.na(concatenated_header)] <- "Unknown"
  concatenated_header[concatenated_header == ""] <- "Unknown"
  
  # Read the data, skipping the first 10 lines to start reading from the data part
  data <- read_csv(file_path, skip = 11, show_col_types = FALSE, col_names = FALSE)
  
  # Assign the concatenated headers as column names
  colnames(data) <- concatenated_header
  
  colnames(data)[which(is.na(colnames(data)))] <- "Unknown"
  data$Unknown <- NULL
  colnames(data)[which(colnames(data) == "")] <- "Unknown"
  data$Unknown <- NULL
  
  # Add New SalesType Column
  data$SalesType <- ''
  
  # Find Line where Non Sales Categories Start
  nonSalesStartLine <- which(data$DayPart == "NON-SALES CATEGORIES")
  
  # Define NonSales Categories Columns
  data <- data %>%
    mutate(SalesType = ifelse(row_number() >= nonSalesStartLine, "Non-Sales", "Sales"))
  
  # Drop entries with NA in the ItemNum column
  data <- filter(data, !is.na(ItemNum))
  
  # Edit Sales Entries' DayPart Column
  LunchStartLine = which(data$SalesType == "Sales" & data$DayPart == "Lunch")
  DinnerStartLine = which(data$SalesType == "Sales" & data$DayPart == "Dinner")
  LateNightStartLine = which(data$SalesType == "Sales" & data$DayPart == "Late Night")
  
  data <- data %>%
    mutate(DayPart = ifelse(SalesType == "Sales",
                            ifelse(row_number() < LunchStartLine, "Breakfast",
                                   ifelse(row_number() < DinnerStartLine, "Lunch",
                                          ifelse(row_number() < LateNightStartLine, "Dinner",
                                                 "Late Night"))),
                            DayPart))
  
  # Edit Non-Sales Entries' DayPartColumn
  NSLunchStartLine = which(data$SalesType == "Non-Sales" & data$DayPart == "Lunch")
  NSDinnerStartLine = which(data$SalesType == "Non-Sales" & data$DayPart == "Dinner")
  NSLateNightStartLine = which(data$SalesType == "Non-Sales" & data$DayPart == "Late Night")
  
  data <- data %>%
    mutate(DayPart = ifelse(SalesType == "Non-Sales",
                            ifelse(row_number() < NSLunchStartLine, "Breakfast",
                                   ifelse(row_number() < NSDinnerStartLine, "Lunch",
                                          ifelse(row_number() < NSLateNightStartLine, "Dinner",
                                                 "Late Night"))),
                            DayPart))
  
  # Change Data Types
  data$DayPart <- as.character(data$DayPart)
  data$ItemNum <- as.character(data$ItemNum)
  data$ItemName <- as.character(data$ItemName)
  data$NumSold <- as.numeric(data$NumSold)
  data$PriceSold <- as.numeric(data$PriceSold)
  data$Amount <- as.numeric(data$Amount)
  data$Cost <- as.numeric(data$Cost)
  data$Profit <- as.numeric(data$Profit)
  data$`FoodCost%` <- as.numeric(data$`FoodCost%`)
  data$`%Sales` <- as.numeric(data$`%Sales`)
  data$SalesType <- as.character(data$SalesType)
  
  # Change Column Names
  colnames(data) <- c("TimeOfDay","ItemID","ItemName","QuantitySold","SellingPrice","SalesTotal","RestaurantTotalCost","TotalProfit","FoodCostPercentage","DailySalesPercentage","SalesType")
  
  # Determine the new filename based on the original, for saving the cleaned data
  new_file_name_path <- paste0(file_RootBaseRel,report_year,"/Cleaned/",report_month,"/",editedReportDate,"_cleaned.csv")
  
  # Save the cleaned data to a new file
  write_csv(data, new_file_name_path)
}
