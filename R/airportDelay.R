#' visualize_airport_delay
#' @description
#' Visualize airport delay use ridge regression to predict the delays in each airport.
#' @import ggplot2 
#' @import dplyr
#' @importFrom dplyr mutate filter
#' 
#' @import nycflights13
#' @export visualize_airport_delay



visualize_airport_delay <- function(){
  formula <- delay~lat+lon
  data <- data_processing()
  delaly_pre <- linreg(formula, data, model = "ridge")
  delaly_pre$ridgereg(lambda = 0.2, method = 2)
  predicted_delays <- delaly_pre$pred()
  name <- data$name
  predicted_data <- data.frame(name =  name[1:5], pre = predicted_delays[1:5])
  ggplot(predicted_data, aes(x = predicted_data$name, y = predicted_data$pre)) +
    geom_bar(stat = "identity", fill = "steelblue") + 
    labs(title = "Predicted Delays In Each Airport", x = "Name Of Airport", y = "Predicted Of Delayy Value") + 
    theme_minimal()
}

data_processing <- function(){
  # globalVariables(c("mean_origin_delay", "mean_dest_delay", "delay", "airport_delays", "pre"))
  flights <- flights
  airports <- airports
  mean_origin_delay <- list()
  mean_dest_delay <- list()
  delay <- list()
  airport_delays <- list()
  origin_delay <- flights %>%
    group_by(flights$origin) %>%
    summarize(mean_origin_delay = mean(flights$dep_delay, na.rm = TRUE))
  
  destination_delay <- flights %>%
    group_by(flights$dest) %>%
    summarize(mean_dest_delay = mean(flights$arr_delay, na.rm = TRUE))
  
  airport_delays <- airports %>%
    left_join(origin_delay, by = c("faa" = "origin")) %>%
    left_join(destination_delay, by = c("faa" = "dest")) %>%
    mutate(
      mean_origin_delay = ifelse(is.na(mean_origin_delay), 0, mean_origin_delay), 
      mean_dest_delay = ifelse(is.na(mean_dest_delay), 0, mean_dest_delay),
      delay = mean_origin_delay + mean_dest_delay                                
    ) %>%
    select(-mean_origin_delay, -mean_dest_delay) %>%  
    filter(delay != 0) 
  return(airport_delays)
}                         




