# Functions to visualize Hawkes predictions vs. actual Ebola outbreak results
## Andy Shen, Sarita Lee, Frederic Schoenberg
## UCLA Department of Statistics

library(tidyverse)
library(lubridate)

######################################################################################################################

## The full_forecast function plots the result from a single Hawkes forecast 
## with respect to the entire outbreak.

full_forecast <- function(rdate, forecast) {
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  df <- data.frame(Prediction, forecast)
  
  corresp_total <- true$total[true$date == as.Date(rdate)] # running total on that date
  
  p + geom_point(
    data = df, 
    mapping = aes(
      x = as.Date(rdate) + c(7,14,21),
      y = corresp_total + forecast,
      col = Prediction
    ),
    size = 2.3
  ) + theme(legend.position = "bottom")
}

######################################################################################################################

## The plot_forecast function plots the result from a single Hawkes forecast with respect to the cases 
## up to the forecasted days +/- 28.

### THIS FUNCTION IS OBSOLETE. USE multi_forecast() INSTEAD ###

plot_forecast <- function(rdate, forecast) {
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  df <- data.frame(Prediction, forecast)
  corresp_total <- true$total[true$date == as.Date(rdate)] # running total on that date
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
  ) + theme(legend.position = "bottom")
}

######################################################################################################################

## Function that accepts a vector of dates (date_vec) and 
## their corresponding 7, 14 and 21 day forecasted values, respectively (forecast_mat).
## The title argument is the title for the corresponding plot.
## The function returns a visualization of confirmed cases vs
## the Hawkes model projections.
## The matrix of forecasted values is organized BY COLUMN. Each column in forecast_mat is a 3x1 vector.
## This function can do the same thing as plot_forecast() and return the results from one forecast, 
## but is also optimized to plot multiple forecasts.

## NOTE: if you are just visualizing a single forecast, you DO NOT 
## need to cbind() your projections, just the vector is enough.

add_weeks <- function(x) {x + c(7,14,21)} #adds 7, 14 and 21 days to each date

multi_forecast <- function(date_vec, forecast_mat, title = NULL) {
  
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  
  l <- length(date_vec)
  max_date <- max(ymd(date_vec)) #latest date using lubridate
  min_date <- min(ymd(date_vec)) #latest date using lubridate
  date_vecl <- as.list(as.Date(date_vec)) #put dates in list to preserve date structure
  forc_dates <- lapply(date_vecl, add_weeks) # add c(7,14,21) days to each date
  l2 <- length(forc_dates)
  
  date_list <- c()
  for(i in 1:l2) {
    date_list <- c(date_list,as.list(forc_dates[[i]])) 
    #group all dates into their own list
  } 
  dfdate <- t(data.frame(date_list)) #data.frame to preserve date structure
  
  total_vec <- rep(NA, l)
  for(i in seq_len(l)) {
    total_vec[i] <- true$total[true$date == as.Date(date_vec[i])]
  } #generating cumulative case counts for each desired day
  
  forecast_total <- t(t(forecast_mat) + total_vec) # adding forecasted values to cumulative totals
  forecast_total <- as.vector(forecast_total)
  df <- data.frame(dfdate, forecast_total, Prediction) # data frame for ggplot

  g <- ggplot(
    data = true[(true$date < as.Date(max_date) + 28) & (true$date > as.Date(min_date) - 14),],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    theme_light() + 
    geom_vline(xintercept = as.Date(date_vec), col = "navy") #line at the dates

  g + geom_point(
    data = df,
    mapping = aes(
      x = as.Date(dfdate),
      y = forecast_total,
      col = Prediction
    ),
    size = 2.7
  ) + theme(legend.position = "bottom") + labs(title = title)
}

######################################################################################################################

add_7 <- function(x) {x + 7} #adding a 7 day forecast
add_14 <- function(x) {x + 14} #adding a 7 day forecast
add_21 <- function(x) {x + 21} #adding a 7 day forecast

## Function that accepts a vector of dates (date_vec) and 
## their corresponding 7, 14 and 21 day forecasted values, respectively (forecast_mat),
## as well as the number of forecasted days you want to visualize (days).
## The title argument is the title for the corresponding plot.
## The same inputs as multi_forecast().
## But it returns the x-day forecasts for all dates specified, where x is either
## the 7, 14 or 21-day forecast.
## The function returns a visualization of confirmed cases vs
## the Hawkes model projections for that indicated day.

single_forecast <- function(date_vec, forecast_mat, days = 21, title = NULL, res = FALSE) {
  size <- 3.2
  l <- length(date_vec)
  max_date <- max(ymd(date_vec)) #latest date using lubridate
  min_date <- min(ymd(date_vec)) #latest date using lubridate
  date_vecl <- as.list(as.Date(date_vec)) #put dates in list to preserve date structure
  
  if(days == 7) {
    forecast_vec <- forecast_mat[1,] # 7 day projections
    forc_dates <- lapply(date_vecl, add_7) # add 7 days to each date
    col <- "dodgerblue1"
  } else if(days == 14) {
    forecast_vec <- forecast_mat[2,] # 14 day projections
    forc_dates <- lapply(date_vecl, add_14) # add 14 days to each date
    col <- "red2"
  } else {
    forecast_vec <- forecast_mat[3,] # 21 day projections
    forc_dates <- lapply(date_vecl, add_21) # add 21 days to each date
    col <- "forestgreen"
  }
  
  l2 <- length(forc_dates)
  date_list <- rep(NA, l2)
  for(i in 1:l2) {
    date_list[i] <- as.list(forc_dates[[i]])
    #group all dates into their own list
  } 
  dfdate <- t(data.frame(date_list)) #data.frame to preserve date structure
  
  total_vec <- rep(NA, l)
  for(i in seq_len(l)) {
    total_vec[i] <- true$total[true$date == as.Date(date_vec[i])]
  } #generating cumulative case counts for each desired day
  
  forecast_total <- forecast_vec + total_vec # adding forecasted values to cumulative totals
  # this recycles, so order is very important here!
  df <- data.frame(dfdate, forecast_total) # data frame for ggplot
  
  g <- ggplot(
    data = true[(true$date < as.Date(max_date) + 28) & (true$date > as.Date(min_date) - 14),],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    theme_light() + 
    geom_vline(xintercept = as.Date(date_vec), col = "navy") +  #line at the dates  
    geom_point(
      data = df,
      mapping = aes(
        x = as.Date(dfdate),
        y = forecast_total,
      ),
      color = col,
      size = size
      
    ) + theme(legend.position = "bottom") + labs(title = title) +
    geom_path(
      data = df,
      aes(x = as.Date(dfdate), y = forecast_total),
      color = col,
      size = size - 2.2
    )
 
  if(res == TRUE) {
    
    if(days == 7) {
      
      fdate <- as.Date(date_vec) + 7
      actual <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        actual[i] <- true$total[true$date == as.Date(fdate[i])]
      }
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          total_vec + forecast_mat[1,],
          actual
        )
      )
      row.names(df_show) <- 1:nrow(df_show)
      colnames(df_show) <- c("prior_date","prior total","forecast_date","forecast.total.7","actual.total.7")
      
    } 
    
    else if(days == 14) {
      
      fdate <- as.Date(date_vec) + 14
      actual <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        actual[i] <- true$total[true$date == as.Date(fdate[i])]
      }
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          total_vec + forecast_mat[2,],
          actual
        )
      )
      row.names(df_show) <- 1:nrow(df_show)
      colnames(df_show) <- c("prior_date","prior total","forecast_date","forecast.total.14","actual.total.14")
      
    }
    
    else {
      fdate <- as.Date(date_vec) + 21
      actual <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        actual[i] <- true$total[true$date == as.Date(fdate[i])]
      }
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          total_vec + forecast_mat[3,],
          actual
        )
      )
      row.names(df_show) <- 1:nrow(df_show)
      colnames(df_show) <- c("prior_date","prior total","forecast_date","forecast.total.21","actual.total.21")
    }
    return(list(g, df_show))
    
  } 
  
  else{
    return(g)
  }
}
######################################################################################################################