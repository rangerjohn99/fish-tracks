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

resident <- results$residency.list

### creat df that combines list and adds tag id column
df_list <- lapply(names(resident), function(name) {
  df <- resident[[name]] # Extract the data frame
  df$tag_id <- name # Add new column with the name of the data frame
  return(df)
})

combined_df <- bind_rows(df_list)

combined_df$Index <- NULL
