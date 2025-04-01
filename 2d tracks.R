# Load required libraries
library(leaflet)
library(htmlwidgets)
library(webshot)
library(magick)

# Ensure webshot is installed properly
webshot::install_phantomjs()

# Convert df to a proper dataframe
df <- as.data.frame(df)

# Ensure time is in POSIXct format
df$time <- as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")

# Get unique time steps
time_steps <- unique(df$time)

# Function to create and capture map frame
create_map_frame <- function(df, time_point) {
  # Subset data for current timestamp
  df_subset <- df[df$time == time_point, , drop = FALSE]
  
  if (nrow(df_subset) == 0) return(NULL)  # Skip if no data
  
  # Create leaflet map
  map <- leaflet(df_subset) %>%
    addTiles() %>%
    addCircleMarkers(lng = ~lon, lat = ~lat, 
                     color = "blue", radius = 2, fillOpacity = 1) %>%
    addControl(
      html = paste("<h3>MDC Lake Sturgeon (55592), Date/Time: ", as.character(time_point), "CST</h3>"),
      position = "topright"
    ) %>%
    setView(lng = mean(df$lon), lat = mean(df$lat), zoom = 15)
  
  # Save temporary HTML file
  html_file <- tempfile(fileext = ".html")
  saveWidget(map, html_file, selfcontained = TRUE)
  
  # Capture screenshot as image
  img_file <- tempfile(fileext = ".png")
  webshot(html_file, file = img_file)
  
  # Read image into R and return
  img <- image_read(img_file)
  
  # Delete temporary files
  file.remove(html_file, img_file)
  
  return(img)
}

# Generate images for each time step without storing files
image_list <- lapply(time_steps, function(t) create_map_frame(df, t))

# Create and save GIF
gif <- image_animate(image_join(image_list), fps = 10)
image_write(gif, "animated_map.gif")

# Convert to MP4 video
temp_files <- paste0(tempdir(), "/frame_", seq_along(image_list), ".png")
purrr::walk2(image_list, temp_files, image_write)  # Save images to disk

av::av_encode_video(temp_files, output = "animated_map.mp4", framerate = 12)

# Clean up temporary files after creating the video
file.remove(temp_files)