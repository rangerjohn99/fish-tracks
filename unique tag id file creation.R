library(tidyverse)

data <- March.detection.data %>% distinct()

write.csv(data, "~/march_detection_data.csv")
