library(yaml)
library(readxl)
library(dplyr)

# Load the CSV
schedule <- read_excel("syllabus/AREC 615 Class Schedule.xlsx",sheet = "FA2025") %>%
  select(-week) %>%
  mutate(across(topic:due, ~ ifelse(is.na(.), "", .)))

# Convert date to character for YAML compatibility
schedule$date <- format(schedule$date, "%Y-%m-%d")

# Convert to list of lists
schedule_list <- apply(schedule, 1, as.list)

# Write to YAML
write_yaml(schedule_list, "_data/schedule.yml")
