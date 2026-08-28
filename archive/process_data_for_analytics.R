library(tidyverse)
library(arrow)
library(ggplot2)
library(plotly)
library(purrr)
library(lubridate)

# Define the path to the folder containing the datasets
data_folder <- 'C:/Users/qgoens/dev/repos/opdi-portal/data/01_raw_data/flight_list/'

# Generate a list of file names for datasets from 012022 to 062024
file_list <- list.files(path = data_folder, pattern = "flight_list_\\d{6}.parquet", full.names = TRUE)

# Function to process each file
process_file <- function(file_path) {
  print(file_path)
  # Load the parquet file
  df <- read_parquet(file_path)
  df <- df %>%
    mutate(month = floor_date(as.Date(DOF), "month"))
  # Process departures
  adep_df <- df %>%
    group_by(adep, month) %>%
    summarize(detected_departures = n(), .groups = 'drop') %>%
    rename(APT = adep)

  # Process arrivals
  ades_df <- df %>%
    group_by(ades, month) %>%
    summarize(detected_arrivals = n(), .groups = 'drop') %>%
    rename(APT = ades)

  # Merge departures and arrivals data
  flight_counts <- adep_df %>%
    full_join(ades_df, by = c('APT', 'month')) %>%
    mutate(
      detected_arrivals = if_else(is.na(detected_arrivals), 0, detected_arrivals),
      detected_departures = if_else(is.na(detected_departures), 0, detected_departures),
      detected_flights = detected_departures + detected_arrivals,
      APT = as.character(APT),   # Ensure APT is always a character
      month = as.Date(month)     # Ensure month is always a Date
    )

  return(flight_counts)
}

# Use purrr::map_dfr to apply the function to each file and combine the results
combined_flight_counts <- map_dfr(file_list, process_file)
combined_flight_counts %>% write_parquet('data/02_analytic_data/total_flights_per_airport_pm.parquet')


###### COUNTS...




# Load necessary libraries
library(tidyverse)
library(arrow)
library(duckdb)

# Define the path to the folder containing the datasets
data_folder <- 'C:/Users/qgoens/dev/repos/opdi-portal/data/01_raw_data/flight_events/'

# Generate a list of file names for datasets from 012022 to 062024 with the correct format
file_list <- list.files(path = data_folder, pattern = "flight_events_\\d{8}_\\d{8}\\.parquet$", full.names = TRUE)


read_parquet(file_list[1])$type %>% unique()

# Initialize DuckDB connection
con <- dbConnect(duckdb::duckdb())

# Function to get row count for each file
get_row_count <- function(file_path) {
  query <- paste0("SELECT COUNT(*) AS row_count FROM parquet_scan('", file_path, "')")
  dbGetQuery(con, query)$row_count
}

# Apply the function to each file and create a data frame with results
row_counts <- map_df(file_list, ~ tibble(file = ., rows = get_row_count(.)))

# Print the row counts for each file
print(row_counts)

# Sum all the number of rows for all files
total_rows <- sum(row_counts$rows)

# Print the total row count
print(total_rows)

# Disconnect from DuckDB
dbDisconnect(con, shutdown = TRUE)







# Load necessary libraries
library(tidyverse)
library(arrow)
library(duckdb)

# Define the path to the folder containing the datasets
data_folder <- 'C:/Users/qgoens/dev/repos/opdi-portal/data/01_raw_data/flight_list/'

# Generate a list of file names for datasets from 012022 to 062024 with the correct format
file_list <- list.files(path = data_folder, pattern = "flight_list_\\d{6}\\.parquet$", full.names = TRUE)

# Initialize DuckDB connection
con <- dbConnect(duckdb::duckdb())

# Function to get row count for each file
get_row_count <- function(file_path) {
  query <- paste0("SELECT COUNT(*) AS row_count FROM parquet_scan('", file_path, "')")
  dbGetQuery(con, query)$row_count
}

# Apply the function to each file and create a data frame with results
row_counts <- map_df(file_list, ~ tibble(file = ., rows = get_row_count(.)))

# Print the row counts for each file
print(row_counts)

# Sum all the number of rows for all files
total_rows <- sum(row_counts$rows)

# Print the total row count
print(total_rows)

# Disconnect from DuckDB
dbDisconnect(con, shutdown = TRUE)









# Load necessary libraries
library(tidyverse)
library(arrow)
library(duckdb)

# Define the path to the folder containing the datasets
data_folder <- 'C:/Users/qgoens/Downloads/opdi (2)/data/OPDI/v002/measurements_clean/'
# Generate a list of file names for datasets from 012022 to 062024 with the correct format
file_list <- list.files(path = data_folder, pattern = "measurements_\\d{8}_\\d{8}\\.parquet$", full.names = TRUE)

# Initialize DuckDB connection
con <- dbConnect(duckdb::duckdb())

# Function to get row count for each file
get_row_count <- function(file_path) {
  query <- paste0("SELECT COUNT(*) AS row_count FROM parquet_scan('", file_path, "')")
  dbGetQuery(con, query)$row_count
}

# Apply the function to each file and create a data frame with results
row_counts <- map_df(file_list, ~ tibble(file = ., rows = get_row_count(.)))

# Print the row counts for each file
print(row_counts)

# Sum all the number of rows for all files
total_rows <- sum(row_counts$rows)

# Print the total row count
print(total_rows)

# Disconnect from DuckDB
dbDisconnect(con, shutdown = TRUE)








library(dplyr)
library(arrow)  # for reading parquet files
library(plotly)

# Read parquet files
events <- read_parquet("C:/Users/qgoens/dev/repos/opdi-portal/data/01_raw_data/flight_events/flight_events_20220101_20220111.parquet")
measurements <- read_parquet("C:/Users/qgoens/dev/repos/opdi-portal/data/01_raw_data/measurements/measurements_20220101_20220111.parquet")

# Merge datasets on 'id' and 'event_id'
df <- merge(events %>%
              filter(flight_id == 'db3777eb2c69e9c2138fd6d7ce69c5a67fed05ed714130209ac9f734dd5c980c_5_2022_1'), measurements, by.x = 'id', by.y = 'event_id')

df %>% write_parquet('data/02_analytic_data/example_events.parquet')











library(arrow)     # For reading parquet files
library(jsonlite)  # For working with JSON
library(dplyr)     # For data manipulation
library(lubridate) # For working with date-time data
library(purrr)     # For applying functions over lists/vectors

# Read parquet file
df <- read_parquet("C:/Users/qgoens/dev/repos/opdi-portal/data/01_raw_data/flight_events/flight_events_20220101_20220111.parquet")

# Filter dataframe based on 'entry_time'
df <- df %>%
  filter(event_time <= ymd("2022-01-02", tz = "UTC"))

# Filter rows where 'type' contains 'entry'
df <- df %>%
  filter(grepl('entry', type))

safe_fromJSON <- function(json_string) {
  tryCatch({
    jsonlite::fromJSON(json_string)
  }, error = function(e) {
    NA
  })
}

# Convert the 'info' column from JSON string to a list
df$info <- map(df$info, safe_fromJSON)

# Flatten the 'info' column and combine with the original dataframe
df_info <- df %>%
  select(-info) %>%
  bind_cols(do.call(rbind, df$info))



# Create 'entry_time' and 'exit_time' columns
df <- df %>%
  mutate(entry_time = as.POSIXct(event_time, origin="1970-01-01"),
         exit_time = entry_time + seconds(time_in_use_s))

# Function to clean apron timeline
clean_apron_timeline <- function(df, col_id = 'id') {

  # Function to check if a row should be dropped
  to_drop <- function(row, df) {
    same_flight <- df %>%
      filter((!!sym(col_id) == row[[col_id]]) & (osm_id != row[['osm_id']]))

    for(i in 1:nrow(same_flight)) {
      other_row <- same_flight[i, ]
      if(row[['osm_aeroway']] %in% c('runway', 'stand')) {
        return(FALSE)
      }
      if(row[['entry_time']] >= other_row[['entry_time']] && row[['exit_time']] < other_row[['exit_time']]) {
        return(TRUE)
      }
      if(row[['entry_time']] > other_row[['entry_time']] && row[['exit_time']] <= other_row[['exit_time']]) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  # Applying the function to each row to mark rows to drop
  df <- df %>%
    rowwise() %>%
    mutate(to_drop = to_drop(cur_data(), df))

  # Filtering out rows marked for dropping and rows with 'time_in_use_s' != 0
  df_clean <- df %>%
    filter(!to_drop & time_in_use_s != 0) %>%
    select(-to_drop)

  return(df_clean)
}

# Apply cleaning function to 'tmp'
tmp <- clean_apron_timeline(tmp, col_id = 'track_id')

# Filter top 30 track_ids
tmp <- tmp %>%
  filter(track_id %in% (tmp %>%
                          count(track_id, sort = TRUE) %>%
                          top_n(30) %>%
                          pull(track_id)))

# Assuming 'visualize_gantt' is a custom function you have defined for visualization
# fig <- visualize_gantt(tmp, '2022-01-01')

# Print the final figure (assuming the 'visualize_gantt' function is defined elsewhere in your code)
# print(fig)
