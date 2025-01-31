# Load required libraries
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# URL of the page to scrape
url <- "https://elderscrolls.fandom.com/wiki/Created_Potions_(Skyrim)"

# Read the HTML content of the webpage
page <- read_html(url)

# Find all tables with class "wikitable" (this is the class of the tables on the page)
tables <- page %>%
  html_nodes("table.wikitable")

# Function to extract and clean the table data
parse_table <- function(table) {
  # Extract column names (header) from the first row
  col_names <- table %>%
    html_nodes("th") %>%
    html_text() %>%
    trimws()
  
  # Extract the table rows
  rows <- table %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_text() %>%
    matrix(ncol = length(col_names), byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # Assign the column names to the data frame
  colnames(rows) <- col_names
  return(rows)
}

#Tables as list 
tables_data <- lapply(tables, parse_table)
tables_data <- tables_data %>% keep(~ ncol(.x) == 5)

# Concatenate and remove expansion/pack information from names
combined_data <- bind_rows(tables_data) %>%
  unique()

combined_data_clean <- combined_data %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\n|Creation Club|DR|HF|DG", "")), 
         across(where(is.character), ~ str_trim(., side = "right"))) %>%
  arrange(Ingredient)

# Save 
write_csv(combined_data_clean, 'data/ingredients-effects.csv')
