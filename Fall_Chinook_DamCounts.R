library(glue)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)

# Function to retrieve data from the website and process it
website_pounder <- function(project, year, startdate, enddate){
  link <- glue::glue("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php",
                     "?sc=1",
                     "&outputFormat=csv",
                     "&year={year}",
                     "&proj={project}",
                     "&startdate={startdate}",
                     "&enddate={enddate}")
  
  readr::read_csv(link) |>
    dplyr::filter(!is.na(Date)) |>
    dplyr::mutate(Project = project) # Add a Project column
}

# Define the years to process
years <- 2024

# List of projects and corresponding date ranges
projects <- list(
  list(project = "BON", startdate = "8/1", enddate = "12/31"),
  list(project = "TDA", startdate = "8/4", enddate = "10/31"),
  list(project = "JDA", startdate = "8/6", enddate = "10/31"),
  list(project = "MCN", startdate = "8/9", enddate = "10/31"),
  list(project = "PRD", startdate = "8/14", enddate = "11/15"),
  list(project = "WAN", startdate = "8/14", enddate = "11/15"),
  list(project = "RIS", startdate = "8/18", enddate = "11/14"),
  list(project = "RRH", startdate = "8/20", enddate = "11/14"),
  list(project = "WEL", startdate = "8/29", enddate = "11/15"),
  list(project = "IHR", startdate = "8/12", enddate = "12/15"),
  list(project = "LMN", startdate = "8/14", enddate = "10/31"),
  list(project = "LGS", startdate = "8/16", enddate = "10/31")
)

# Loop through the projects and years to collect data
combined_data <- purrr::map_dfr(projects, function(proj) {
  purrr::map_dfr(years, function(yr) {
    suppressWarnings(website_pounder(proj$project, yr, proj$startdate, proj$enddate))
  })
})

# Replace all NAs in the Chin column with 0
combined_data <- combined_data |> 
  replace_na(list(Chin = 0))

# Clean the Date column and remove rows with parsing errors
combined_data <- combined_data |> 
  mutate(Date = parse_date_time(Date, orders = c("mdy", "ymd", "dmy"))) |>
  filter(!is.na(Date))

# Extract Year and MonthDay from Date
combined_data <- combined_data |> 
  mutate(
    Year = year(Date),
    MonthDay = format(Date, "%m/%d")
  )

# Summarize the total Chin count per project and year
final_data <- combined_data |> 
  group_by(Year, Project) |> 
  summarise(count = sum(Chin, na.rm = TRUE), .groups = 'drop')

# Write the final combined data to a CSV file
write_csv(final_data, "fallCH_counts.csv")

# View the final data
final_data