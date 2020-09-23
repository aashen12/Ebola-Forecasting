# Functions to visualize Hawkes and/or predictions vs. actual Ebola outbreak results
## Andy Shen, Sarita Lee, Frederic Schoenberg
## UCLA Department of Statistics

library(tidyverse)
library(lubridate)
library(knitr)
library(Metrics)

add_7 <- function(x) {x + 7} #adding on 7 days
add_14 <- function(x) {x + 14} #adding on 14 days
add_21 <- function(x) {x + 21} #adding on 21 days
add_weeks <- function(x) {x + c(7,14,21)} #adds 7, 14 and 21 days to each date
# these are necessary because they are used in lapply() for the list of dates

######################################################################################################################

### MAIN FUNCTION ###

## Function that accepts a vector of dates (date_vec) and 
## their corresponding 7, 14 and 21 day forecasted values, respectively (forecast_mat),
## as well as the number of forecasted days you want to visualize (days).
## Each column of forecast_mat must contain the c(7-day projection, 14-day projection, 21-day projection)
## The data argument accepts a data frame with the dataset representing the true case count (solid black line in ggplot).
## The res argument decides whether to output table of results and RMSE value, along with a plot. Returns just the plot by default.
## The point argument decides whether you want your ggplot to have points at each date or just a smooth line.
## The title argument is the title for the corresponding plot.
## The same inputs as multi_forecast().
## But it returns the x-day forecasts for all dates specified, where x is either
## the 7, 14 or 21-day forecast.
## The function returns a visualization of confirmed cases vs
## the Hawkes model projections for that indicated day.

single_forecast <- function(date_vec, forecast_mat, days = 21, title = NULL, data = true, res = TRUE, point = FALSE) {
  size <- 3.0 #point size
  l <- length(date_vec)
  max_date <- max(ymd(date_vec)) #latest date using lubridate
  min_date <- min(ymd(date_vec)) #latest date using lubridate
  date_vecl <- as.list(as.Date(date_vec)) #put dates in list to preserve date structure
  
  if(days == 7) {
    forecast_vec <- forecast_mat[1,] # 7 day projections are row 1
    forc_dates <- lapply(date_vecl, add_7) # add 7 days to each date
    col <- "dodgerblue1"
  } else if(days == 14) {
    forecast_vec <- forecast_mat[2,] # 14 day projections are row 2
    forc_dates <- lapply(date_vecl, add_14) # add 14 days to each date
    col <- "red2"
  } else {
    forecast_vec <- forecast_mat[3,] # 21 day projections are row 3
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
    total_vec[i] <- data$total[data$date == as.Date(date_vec[i])]
  } #generating cumulative case counts for each desired day
  
  forecast_total <- forecast_vec + total_vec # adding forecasted values to cumulative totals
  # this recycles, so order is very important here!
  df <- data.frame(dfdate, forecast_total) # data frame for ggplot
  
  #complete ggplot with all points, WITH VERTICAL LINE AT DATES, THIS GRAPH CONTAINS EVERYTHING
  gfull <- ggplot(
    data = data[(data$date < as.Date(max_date) + 28) & (data$date > as.Date(min_date) - 14),], #range is 28 days ahead and 14 days behind
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    theme_light() + 
    geom_vline(xintercept = as.Date(date_vec), col = "gray75") +  #line at each date forecasted
    geom_point( #adds points at each date
      data = df,
      mapping = aes(
        x = as.Date(dfdate),
        y = forecast_total,
      ),
      color = col,
      size = size
    ) +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5)) + 
    labs(caption = title) +
    geom_path( #no dashed line here
      data = df,
      aes(x = as.Date(dfdate), y = forecast_total),
      color = col,
      #linetype = "dashed",
      size = size - 2.2
    ) + scale_x_date(date_breaks = "5 months", date_labels = "%b-%y")
  
  
  #Plot with no vertical lines at dates, CONTAINS POINTS AT EACH FORECASTED DATE INSTEAD OF JUST A DASHED LINE
  gfull_ref <- ggplot( 
    data = data[(data$date < as.Date(max_date) + 28) & (data$date > as.Date(min_date) - 14),],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    theme_light() + 
    #geom_vline(xintercept = as.Date(date_vec), col = "gray75") +  # NO line at the dates
    geom_point(
      data = df,
      mapping = aes(
        x = as.Date(dfdate),
        y = forecast_total,
      ),
      color = col,
      size = 2.5
    ) + 
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5)) + 
    labs(caption = title) +
    geom_path(
      data = df,
      aes(x = as.Date(dfdate), y = forecast_total),
      color = col,
      linetype = "dashed",
      size = 0.65
    ) + scale_x_date(date_breaks = "5 months", date_labels = "%b-%y")
  
  #Graph with NO point markers, and trend line is dashed
  gsimp <- ggplot(
    data = data[(data$date < as.Date(max_date) + 28) & (data$date > as.Date(min_date) - 14),],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + #line for true forecasts
    theme_light() + 
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5)) + 
    labs(caption = title) +
    geom_path(
      data = df,
      aes(x = as.Date(dfdate), y = forecast_total),
      color = col,
      linetype = "dashed",
      size = size - 2.2
    ) + scale_x_date(date_breaks = "5 months", date_labels = "%b-%y")
  
  
  if(res == TRUE) { # CREATES ADDITIONAL OUTPUT BY CALCULATING RMSE AND GIVES DATA FRAME OF RESULTS
    if(days == 7) {
      fdate <- as.Date(date_vec) + 7 #add 7 for 7-day forecast
      actual <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
          actual[i] <- NA # accounting for blank arguments
        } else{
          actual[i] <- data$total[data$date == as.Date(fdate[i])]
        }
      } #record the actual number of cases for the given increment (days)
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          actual,
          total_vec + forecast_mat[1,]
        )
      )
      row.names(df_show) <- 1:nrow(df_show)
      colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
      df_show$prior.total <- as.numeric(df_show$prior.total)
      df_show$actual.total <- as.numeric(df_show$actual.total)
      df_show$forecast.total <- as.numeric(df_show$forecast.total)
      df_show <- df_show %>% mutate(
        resids = actual.total - forecast.total
      ) 
      RMSE <- df_show %>% summarise(
        RMSE = sqrt( mean(na.omit(resids)^2) )
      )
      RMSE <- RMSE[1,1]
    } 
    else if(days == 14) {
      fdate <- as.Date(date_vec) + 14
      actual <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
          actual[i] <- NA # accounting for blank arguments
        } else{
          actual[i] <- data$total[data$date == as.Date(fdate[i])]
        }
      } #record the actual number of cases for the given increment (days)
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          actual,
          total_vec + forecast_mat[2,]
        )
      )
      row.names(df_show) <- 1:nrow(df_show)
      colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
      df_show$prior.total <- as.numeric(df_show$prior.total)
      df_show$actual.total <- as.numeric(df_show$actual.total)
      df_show$forecast.total <- as.numeric(df_show$forecast.total)
      df_show <- df_show %>% mutate(
        resids = actual.total - forecast.total
      )
      RMSE <- df_show %>% summarise(
        RMSE = sqrt( mean(na.omit(resids)^2) )
      )
      RMSE <- RMSE[1,1]
    }
    else {
      fdate <- as.Date(date_vec) + 21
      actual <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
          actual[i] <- NA # accounting for blank arguments
        } else{
          actual[i] <- data$total[data$date == as.Date(fdate[i])]
        }
      }#record the actual number of cases for the given increment (days)
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          actual,
          total_vec + forecast_mat[3,]
        )
      )
      row.names(df_show) <- 1:nrow(df_show)
      colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
      df_show$prior.total <- as.numeric(df_show$prior.total)
      df_show$actual.total <- as.numeric(df_show$actual.total)
      df_show$forecast.total <- as.numeric(df_show$forecast.total)
      df_show <- df_show %>% mutate(
        resids = actual.total - forecast.total
      )
      RMSE <- df_show %>% summarise(
        RMSE = sqrt( mean(na.omit(resids)^2) )
      )
      RMSE <- RMSE[1,1] #calculate rmse
    }  #21 DAYS
    if(point == TRUE) { # IF YOU WANT THE PLOT WITH POINTS OR A DASHED LINE
      return(list(plot = gfull_ref, results = df_show, rmse = RMSE)) #plot with points marking forecast projections
    } else {
      return(list(plot = gsimp, results = df_show, rmse = RMSE)) # no points, just a dashed line
    }
  } 
  else {
    if(point == TRUE) { # IF YOU WANT THE PLOT WITH POINTS OR A DASHED LINE
      return(gfull_ref)
    } else {
      return(gsimp)
    }
  }
}

######################################################################################################################
## Function that takes the same inputs as single_forecast() and 
## returns only the RMSE value.

forecast_rmse <- function(date_vec, forecast_mat, days = 21, data = true) {
  l <- length(date_vec)
  max_date <- max(ymd(date_vec)) #latest date using lubridate
  min_date <- min(ymd(date_vec)) #earliest date using lubridate
  date_vecl <- as.list(as.Date(date_vec)) #put dates in list to preserve date structure
  
  if(days == 7) {
    forecast_vec <- forecast_mat[1,] # 7 day projections are row 1 of matrix
    forc_dates <- lapply(date_vecl, add_7) # add 7 days to each date
    col <- "dodgerblue1"
  } else if(days == 14) {
    forecast_vec <- forecast_mat[2,] # 14 day projections are row 2
    forc_dates <- lapply(date_vecl, add_14) # add 14 days to each date
    col <- "red2"
  } else {
    forecast_vec <- forecast_mat[3,] # 21 day projections are row 3
    forc_dates <- lapply(date_vecl, add_21) # add 21 days to each date
    col <- "forestgreen"
  }
  
  l2 <- length(forc_dates)
  date_list <- rep(NA, l2)
  for(i in 1:l2) {
    date_list[i] <- as.list(forc_dates[[i]]) #group all dates into their own list
  } 
  dfdate <- t(data.frame(date_list)) #data.frame to preserve date structure
  
  total_vec <- rep(NA, l)
  for(i in seq_len(l)) {
    total_vec[i] <- data$total[data$date == as.Date(date_vec[i])]
  } #generating cumulative case counts for each desired day
  
  forecast_total <- forecast_vec + total_vec # adding forecasted values to cumulative totals
  # this recycles, so order is very important here!
  df <- data.frame(dfdate, forecast_total) # data frame for ggplot

  if(days == 7) {
    fdate <- as.Date(date_vec) + 7 #add 7 for 7-day forecast
    actual <- rep(NA,length(fdate))
    for(i in 1:length(fdate)) {
      if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
        actual[i] <- NA # accounting for blank arguments
      } else{
        actual[i] <- data$total[data$date == as.Date(fdate[i])]
      }
    } #record the actual number of cases for the given increment (days)
    
    df_show <- data.frame(
      cbind(
        date_vec, #vector of input dates
        total_vec, #total number of cases on that date
        dfdate, # +7 days from date_vec
        actual, #actual number of cases on that day
        total_vec + forecast_mat[1,] #forecasted total
      )
    )
    row.names(df_show) <- 1:nrow(df_show)
    colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
    df_show$prior.total <- as.numeric(df_show$prior.total)
    df_show$actual.total <- as.numeric(df_show$actual.total)
    df_show$forecast.total <- as.numeric(df_show$forecast.total)
    df_show <- df_show %>% mutate(
      resids = actual.total - forecast.total
    ) 
    RMSE <- df_show %>% summarise(
      RMSE = sqrt( mean(na.omit(resids)^2) )
    )
    RMSE <- RMSE[1,1]
    RMSE2<-Metrics::rmse(df_show$actual.total,df_show$forecast.total) #built-in
    if(RMSE == RMSE2) {
      return(RMSE)
    } else{
      return(cat("RMSE does not match"))
    }
  } 
  else if(days == 14) {
    fdate <- as.Date(date_vec) + 14
    actual <- rep(NA,length(fdate))
    for(i in 1:length(fdate)) {
      if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
        actual[i] <- NA # accounting for blank arguments
      } else{
        actual[i] <- data$total[data$date == as.Date(fdate[i])]
      }
    } #record the actual number of cases for the given increment (days)
    
    df_show <- data.frame(
      cbind(
        date_vec,
        total_vec,
        dfdate,
        actual,
        total_vec + forecast_mat[2,]
      )
    )
    row.names(df_show) <- 1:nrow(df_show)
    colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
    df_show$prior.total <- as.numeric(df_show$prior.total)
    df_show$actual.total <- as.numeric(df_show$actual.total)
    df_show$forecast.total <- as.numeric(df_show$forecast.total)
    df_show <- df_show %>% mutate(
      resids = actual.total - forecast.total
    )
    RMSE <- df_show %>% summarise(
      RMSE = sqrt( mean(na.omit(resids)^2) )
    )
    RMSE <- RMSE[1,1]
    return(RMSE)
  }
  else {
    fdate <- as.Date(date_vec) + 21
    actual <- rep(NA,length(fdate))
    for(i in 1:length(fdate)) {
      if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
        actual[i] <- NA # accounting for blank arguments
      } else{
        actual[i] <- data$total[data$date == as.Date(fdate[i])]
      }
    }#record the actual number of cases for the given increment (days)
    
    df_show <- data.frame(
      cbind(
        date_vec,
        total_vec,
        dfdate,
        actual,
        total_vec + forecast_mat[3,]
      )
    )
    row.names(df_show) <- 1:nrow(df_show)
    colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
    df_show$prior.total <- as.numeric(df_show$prior.total)
    df_show$actual.total <- as.numeric(df_show$actual.total)
    df_show$forecast.total <- as.numeric(df_show$forecast.total)
    df_show <- df_show %>% mutate(
      resids = actual.total - forecast.total
    )
    RMSE <- df_show %>% summarise(
      RMSE = sqrt( mean(na.omit(resids)^2) )
    )
    RMSE <- RMSE[1,1] #calculate rmse
    return(RMSE)
  }  #21 DAYS

}

######################################################################################################################

## The full_forecast function plots the result from a single Hawkes or Recursive forecast 
## with respect to the entire outbreak.

## This function requires the ggplot() object p to be declared

full_forecast <- function(rdate, forecast) {
  Prediction <- c("7-Day Forecast","14-Day Forecast","21-Day Forecast") #for legend
  df <- data.frame(Prediction, forecast)
  
  corresp_total <- true$total[true$date == as.Date(rdate)] # running total on that date
  p <- ggplot(data = true, mapping = aes(x = date, y = total)) + 
    geom_line() + theme_light() + 
    labs(title = "Actual Recorded DRC Ebola Cases")# graph of running total of cases
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

## Function that accepts a vector of dates (date_vec) and 
## their corresponding 7, 14 and 21 day forecasted values, respectively (forecast_mat).
## The title argument is the title for the corresponding plot.
## The function returns a visualization of confirmed cases vs
## the model projections.
## The matrix of forecasted values is organized BY COLUMN. Each column in forecast_mat is a 3x1 vector.

## NOTE: if you are just visualizing a single forecast, you DO NOT 
## need to cbind() your projections in forecast_mat, just the vector is enough.

multi_forecast <- function(date_vec, forecast_mat, data = true, title = NULL) {
  
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
    total_vec[i] <- data$total[data$date == as.Date(date_vec[i])]
  } #generating cumulative case counts for each desired day
  
  forecast_total <- t(t(forecast_mat) + total_vec) # adding forecasted values to cumulative totals
  forecast_total <- as.vector(forecast_total)
  df <- data.frame(dfdate, forecast_total, Prediction) # data frame for ggplot

  g <- ggplot(
    data = data[(data$date < as.Date(max_date) + 28) & (data$date > as.Date(min_date) - 14),],
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
