install.packages("actel")  # Install if necessary
library(actel)

#load in biometrics, detections, and spatial datasets

library(ggplot2)
library(dplyr)
library(lubridate)

exampleWorkspace("/fish tracks")

# move into the newly created folder
setwd('/fish tracks')

# Run analysis. Note: This will open an analysis report on your web browser.
results <- explore(tz = "Europe/Copenhagen", report = TRUE)

results <- migration(tz = "Europe/Copenhagen", report = TRUE)

results <- residency(tz = "Europe/Copenhagen", report = FALSE)

# extract residency list
resident <- results$residency.list

### create df that combines list and adds tag id column
resident <- lapply(names(resident), function(name) {
  df <- resident[[name]]
  df$tag_id <- name
  return(df)
})

### combine into one data frame
resident <- bind_rows(resident)
### remove NA column
resident$Index <- NULL

### remove transition rows
resident <- resident %>%
  filter(!(Section %in% c("River-Fjord", "Fjord-Sea")))

### create df of when unique tags reached Sea section
fish_sea_times <- resident %>%
  filter(Section == "Sea") %>%
  group_by(tag_id) %>%
  summarise(arrival_time = min(First.time, na.rm = TRUE))

# Arrange data by tag_id and First.time to ensure chronological order
df_sorted <- resident %>%
  arrange(tag_id, First.time) 

# Identify movement paths for each fish
fish_paths <- df_sorted %>%
  group_by(tag_id) %>%
  summarise(path = paste(unique(Section), collapse = " -> "))  # Create movement sequence

# Categorize the movement paths
fish_paths <- fish_paths %>%
  mutate(path_type = case_when(
    grepl("River -> Sea", path) & !grepl("Fjord", path) ~ "Direct: River to Sea",
    grepl("River -> Fjord", path) & grepl("Fjord -> Sea", path) ~ "Via Fjord",
    grepl("River -> Fjord", path)& !grepl("Fjord -> Sea", path) ~ "Incomplete",
    grepl("River", path) ~ "No movement",
    TRUE ~ "Other"
  ),
  ,
  detected_in_sea = ifelse(grepl("Sea", path), 1, 0)
  )

#add date time column
fish_combined <- fish_paths %>%
  left_join(fish_sea_times, by = "tag_id")

#add date column
fish_combined <- fish_combined %>%
  mutate(arrival_date = as.Date(arrival_time))

#only list fish that made it to the sea
successful_fish <- fish_combined %>%
  filter(detected_in_sea == 1)

successful_fish$detected_in_sea <- NULL

### average environmental_data, adjust to include max and min if necessary
mean_environmental_data <- environmental_data %>%
  mutate(date = as.Date(datetime)) %>%  # Extract the date part from datetime
  group_by(date) %>%  # Group by the 'date'
  summarize(
    avg_dissolved_oxygen = mean(dissolved_oxygen, na.rm = TRUE),
    avg_water_temperature = mean(water_temperature, na.rm = TRUE),
    avg_stage_height = mean(stage_height, na.rm = TRUE),
    avg_turbidity = mean(turbidity, na.rm = TRUE)
  )
