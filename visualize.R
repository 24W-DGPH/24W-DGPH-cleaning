# scatterplot
scatter <- function(data, age_filter, bmi_filter) {
  # Check if age_filter is valid
  if (length(age_filter) != 2 || !is.numeric(age_filter)) {
    stop("age_filter must be a numeric vector of length 2.")
  }
  
  if (length(bmi_filter) != 2 || !is.numeric(bmi_filter)) {
    stop("age_filter must be a numeric vector of length 2.")
  }
  
  age_filter <- as.numeric(age_filter)
  bmi_filter <- as.numeric(bmi_filter)

  # Filter the dataset based on age range
  filtered_data <- data %>%
    filter(age >= age_filter[1] & age <= age_filter[2]) %>%
    filter(bmi >= bmi_filter[1] & bmi <= bmi_filter[2])
  
  # Check if filtered_data is not empty
  if (nrow(filtered_data) == 0) {
    warning("No data available for the specified age range.")
    return(NULL)
  }
  
  ggplot(data = filtered_data,   # set data
         mapping = aes(     # map aesthetics to column values
           x = age,           # map x-axis to age            
           y = wt_kg,         # map y-axis to weight
           color = age,       # map color to age
           size = bmi))+      # map size to age
    geom_point(             # display data as points      # points display as diamonds
      alpha = 0.3)            # point transparency at 30%
}