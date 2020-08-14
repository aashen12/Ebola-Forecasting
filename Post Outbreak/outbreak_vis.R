# Functions to visualize Hawkes predictions vs. actual Ebola outbreak results

## The full_forecast function plots the result from a single Hawkes forecast 
## with respect to the entire outbreak.

full_forecast <- function(rdate, forecast) {
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  df <- data.frame(Prediction, forecast)
  
  corresp_total <- true$total[true$date==as.Date(rdate)] # running total on that date
  
  p + geom_point(data = df, 
                 mapping = aes(as.Date(rdate)+c(7,14,21),
                               corresp_total + forecast, col = Prediction),
                 size = 2.3) + theme(legend.position = "bottom")
}

######################################################################################################################

## The full_forecast function plots the result from a single Hawkes forecast with respect to the cases 
## up to the forecasted days.

plot_forecast <- function(rdate, forecast) {
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  df <- data.frame(Prediction, forecast)
  corresp_total <- true$total[true$date==as.Date(rdate)] # running total on that date
  g <- ggplot(
    data = true[true$date < as.Date(rdate) + 28,], #an extra week past the last day for readability
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    theme_light() + 
    geom_vline(aes(xintercept = as.Date(rdate)), col = "navy") #line at the date
  
  g + geom_point(
    data = df, 
    mapping = aes(
      x = as.Date(rdate)+c(7,14,21),
      y = corresp_total + forecast, 
      col = Prediction
    ),
    size = 2.3
  ) + 
  theme(legend.position = "bottom")
}

######################################################################################################################

## Function that accepts a vector of dates to plot and their corresponding forecasts
## organized BY COLUMN. Each column in forecast_mat is a 3x1 vector.
## This function can do the same thing as plot_forecast(), but is also optimized to plot 
## multiple forecasts.

add_weeks <- function(x) x + c(7,14,21)

multi_forecast <- function(date_vec, forecast_mat, title = NULL) {
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  l <- length(date_vec)
  max_date <- max(ymd(date_vec))
  date_vecl <- as.list(as.Date(date_vec))
  forc_dates <- lapply(date_vecl, add_weeks)
  l2 <- length(forc_dates)
  date_list <- c()
  for(i in 1:l2) {
    date_list <- c(date_list,as.list(forc_dates[[i]]))
  }
  dfdate <- t(data.frame(date_list))
  
  total_vec <- rep(NA, l)
  for(i in seq_len(l)) {
    total_vec[i] <- true$total[true$date == as.Date(date_vec[i])]
  } #generating totals for that day
  
  forecast_total <- t(t(forecast_mat) + total_vec)
  forecast_total <- as.vector(forecast_total)
  df <- data.frame(dfdate, forecast_total, Prediction)
  #browser()
  g <- ggplot(
    data = true[true$date < as.Date(max_date) + 28,],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    theme_light() + 
    geom_vline(xintercept = as.Date(date_vec), col = "navy") #line at the dates
  #return(g)
  g + geom_point(
    data = df,
    mapping = aes(
      x = as.Date(dfdate),
      y = forecast_total,
      col = Prediction
    ),
    size = 2.3
  ) + theme(legend.position = "bottom") + labs(title = title)
}

######################################################################################################################