filter_data <- function(data, age_filter, bmi_filter) {
  # Check if age_filter is valid
  if (length(age_filter) != 2 || !is.numeric(age_filter)) {
    stop("age_filter must be a numeric vector of length 2.")
  }
  
  # Check if bmi_filter is valid
  if (length(bmi_filter) != 2 || !is.numeric(bmi_filter)) {
    stop("bmi_filter must be a numeric vector of length 2.")
  }
  
  # Convert filters to numeric (if they aren't already)
  age_filter <- as.numeric(age_filter)
  bmi_filter <- as.numeric(bmi_filter)
  
  # Filter the dataset based on age range
  filtered_data <- data %>%
    filter(age >= age_filter[1] & age <= age_filter[2]) %>%
    filter(bmi >= bmi_filter[1] & bmi <= bmi_filter[2])
  
  # Check if filtered_data is not empty
  if (nrow(filtered_data) == 0) {
    warning("No data available for the specified filters.")
    return(NULL)
  }
  
  # Return the filtered data
  filtered_data
}


scatter <- function(data, age_filter, bmi_filter) {
  filtered_data <- filter_data(data, age_filter, bmi_filter)
  ggplot(data = filtered_data,   # set data
         mapping = aes(     # map aesthetics to column values
           x = age,           # map x-axis to age            
           y = bmi,         # map y-axis to weight
           color = outcome,
           ))+      # map size to age
    geom_point(             # display data as points      # points display as diamonds
      alpha = 0.3)            # point transparency at 30%
}

bar <- function(data, age_filter, bmi_filter) {
  filtered_data <- filter_data(data, age_filter, bmi_filter)
  ggplot(data = filtered_data,   # set data
         mapping = aes(     # map aesthetics to column values
           x = gender,           # map x-axis to age            
         ))+      # map size to age
    geom_bar()    
}

hist_bmi <- function(data, age_filter, bmi_filter) {
  filtered_data <- filter_data(data, age_filter, bmi_filter)
  ggplot(data = filtered_data,   # set data
         mapping = aes(     # map aesthetics to column values
           x = bmi,           # map x-axis to age            
         ))+      # map size to age
    geom_histogram()    
  
}

hist_age <- function(data, age_filter, bmi_filter) {
  filtered_data <- filter_data(data, age_filter, bmi_filter)
  ggplot(data = filtered_data,   # set data
         mapping = aes(     # map aesthetics to column values
           x = age,           # map x-axis to age            
         ))+      # map size to age
    geom_histogram() 
  
}

map <- function(data, age_filter, bmi_filter) {
  filtered_data <- filter_data(data, age_filter, bmi_filter)
  
  # Define Freetown bounding box
  freetown_bbox <- getbb("Freetown, Sierra Leone")
  
  # Get streets data for Freetown
  freetown_streets <- opq(freetown_bbox) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()
  
  ggplot() +
    geom_sf(data = freetown_streets$osm_lines, color = "black", size = 0.3) +  # Fill and boundary colors
    geom_point(data = filtered_data, 
               aes(x = lon, y = lat), 
               color = "red", 
               size = 3) +
    labs(title = "Cases in Sierra Leone")
}

leaflet_map <- function(data, age_filter, bmi_filter) {
    leaflet(data = data) %>%
      addTiles() %>%  # Add OpenStreetMap base tiles
      addCircleMarkers(lng = ~lon, lat = ~lat, 
                       color = ~hospital, radius = 5, fillOpacity = 0.8, 
                       popup = ~case_id) %>%  # Add points with a popup
      setView(lng = -13.231722, lat = 8.460555, zoom = 12)
}