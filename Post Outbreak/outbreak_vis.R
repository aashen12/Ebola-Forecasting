# Functions to visualize Hawkes and/or predictions vs. actual Ebola outbreak results
## Andy Shen, Sarita Lee, Frederic Schoenberg
## UCLA Department of Statistics

# load all libraries needed
library(tidyverse)
library(lubridate)
library(knitr)
library(Metrics)

# functions for the 1-, 2-, and 3- week forecast dates
# these are necessary because they are used in lapply() for the list of dates
add_7 <- function(x) {x + 7} #adding on 7 days
add_14 <- function(x) {x + 14} #adding on 14 days
add_21 <- function(x) {x + 21} #adding on 21 days
add_weeks <- function(x) {x + c(7,14,21)} #adds 7, 14 and 21 days to each date

# define the RMSE function to be used later on 
my_rmse <- function(v){
  # Function that calculates and returns the Root Mean Squared Error
  # omitting any NA's from the input vector, v
  return( sqrt(mean(na.omit(v)^2)) )
}

######################################################################################################################

### MAIN FUNCTION ###

single_forecast <- function(date_vec, forecast_mat, data = actual, days = 21, title = NULL, point = FALSE, res = TRUE) {
  
  # Function that returns plot of actual Ebola cases vs. their 7, 14 or 21-day forecasts,
  # along with corresponding RMSE values and dataset used to produce the plots.
  #
  # Args:
  #   date_vec: a vector of dates for the outbreak datasets
  #   forecast_mat: the matrix of the outbreak datasets' 1-, 2-, and 3-week forecasts 
  #                 (matrix must be in that order)
  #   data: a dataframe of the actual case count
  #         NOTE TO ANDY: what do you think about changing this to "data_actual"
  #         or "actual_case_counts" so the variable name is more descriptive?
  #   days: number of forecasted days to visualize (valid inputs are 7, 14, or 21)
  #   title: title of the returned plot
  #   point: if FALSE (default), makes the ggplot have points at each date rather than the
  #          a smooth line
  #   res: if TRUE (default), function returns just the plot. If FALSE, function returns
  #        the table of results and calculated RMSE in addition to the plot
  #
  # Returns:
  #   plot:
  #     gfull_ref: Complete plot with points denoting the forecast for each date that was forecasted.
  #     gsimp: Complete plot without vertical lines and points, instead returns a colored dashed line showing the overall forecast trend
  #   rmse: RMSE of data
  #   results: data frame with actual and forecasted values for each date (this data frame is used to create the plots)
  

  ### DATA CLEANING AND MANIPULATION ###
  
  # specify constants
  ## days in 1, 2, and 3 weeks
  n_days_week <- 7
  one_week <- 1 * n_days_week
  two_weeks <- 2 * n_days_week
  three_weeks <- 3 * n_days_week
  
  l <- length(date_vec) #number of datasets
  max_date <- max(ymd(date_vec)) #latest date using lubridate
  min_date <- min(ymd(date_vec)) #latest date using lubridate
  date_vecl <- as.list(as.Date(date_vec)) #put dates in list to preserve date structure

  # specify information for plotting later
  size_point <- 3.0 #point size
  colors_plot <- c("dodgerblue1", "red2", "forestgreen")

  # depending on how many days entered to forecast, 
  # extract the correct forecast
  # assign the corresponding dates
  # specify their line color
  if(days == one_week) {
    forecast_vec <- forecast_mat[1,] # 7 day projections are row 1
    forc_dates <- lapply(date_vecl, add_7) # add 7 days to each date
    col <- colors_plot[1]
  } else if(days == two_weeks) {
    forecast_vec <- forecast_mat[2,] # 14 day projections are row 2
    forc_dates <- lapply(date_vecl, add_14) # add 14 days to each date
    col <- colors_plot[2]
  } else if(days == three_weeks) {
    forecast_vec <- forecast_mat[3,] # 21 day projections are row 3
    forc_dates <- lapply(date_vecl, add_21) # add 21 days to each date
    col <- colors_plot[3]
  } else {stop("Invalid number of days specified. 
               Check that the number of days is either 7, 14, or 21.")}
  
  # Grouping all dates from a list into a vector while preserving date structure
  l2 <- length(forc_dates) #number of dates used in the analysis
  date_list <- rep(NA, l2) #group all dates into their own list
  for(i in 1:l2) { date_list[i] <- as.list(forc_dates[[i]]) } 
  dfdate <- t(data.frame(date_list)) #data.frame to preserve date structure
  
  # For each date in date_vec (list of dates initially accepted by the function),
  # extract the cumulative total at that date
  total_vec <- rep(NA, l)
  #generating cumulative case counts for each desired day
  for(i in seq_len(l)) { total_vec[i] <- data$total[data$date == as.Date(date_vec[i])] } 
  
  # adding forecasted values to cumulative totals
  forecast_total <- forecast_vec + total_vec 
  # this recycles, so order is very important here!
  df <- data.frame(dfdate, forecast_total) # data frame for ggplot
  

  ### DATA VISUALIZATION ###
  
  back <- 14 # how many days back to plot
  ahead <- 28 #how many days ahead to plot
  
  #complete ggplot with all points
  #with vertical lines at dates (this graph contains everything)
  gfull <- ggplot(
    data = data[(data$date < (as.Date(max_date) + ahead)) & (data$date > (as.Date(min_date) - back)),], #range is 28 days ahead and 14 days behind
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    geom_vline(xintercept = as.Date(date_vec), col = "gray75") +  #line at each date forecasted
    #adds points at each date
    geom_point(data = df, 
               mapping = aes(x = as.Date(dfdate), y = forecast_total),
               color = col,
               size = size_point) +
    #no vertical lines here
    geom_path(data = df,
              aes(x = as.Date(dfdate), y = forecast_total),
              color = col,
              #linetype = "dashed",
              size = size_point - 2.2) + 
    labs(caption = title) +
    scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") +
    theme_light() +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5, face = "italic"))
  
  #Plot with no vertical lines at dates
  # contains points at each forecasted date instead of dashed line
  gfull_ref <- ggplot(
    data = data[(data$date < (as.Date(max_date) + ahead)) & (data$date > (as.Date(min_date) - back)),],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + 
    #geom_vline(xintercept = as.Date(date_vec), col = "gray75") +  # NO line at the dates
    geom_point(data = df,
               mapping = aes(x = as.Date(dfdate), y = forecast_total),
               color = col,
               size = 2.5) + 
    geom_path(data = df,
              aes(x = as.Date(dfdate), y = forecast_total),
              color = col,
              linetype = "dashed",
              size = 0.65) + 
    labs(caption = title) +
    scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") +
    theme_light() + 
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5, face = "italic"))
    
  #Plot with no point markers, and trend line is dashed
  gsimp <- ggplot(
    data = data[(data$date < (as.Date(max_date) + ahead)) & (data$date > (as.Date(min_date) - back)),],
    mapping = aes(x = date, y = total)
  ) + 
    geom_line() + #line for true forecasts
    geom_path(
      data = df,
      aes(x = as.Date(dfdate), y = forecast_total),
      color = col,
      linetype = "dashed",
      size = size_point - 2.2
    ) + 
    labs(caption = title) +
    scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") +
    theme_light() +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5, face = "italic"))

  ### RMSE CALCULATION ###
  
  # creates additional output by calculating RMSE and the dataframe of results
  
  if(res == TRUE) { # This argument generates RMSEs and data frame of results
    if(days == one_week) {
      fdate <- as.Date(date_vec) + one_week #add 7 for 7-day forecast
      actual_cases <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
          actual_cases[i] <- NA # accounting for blank arguments
        } else{
          actual_cases[i] <- data$total[data$date == as.Date(fdate[i])]
        }
      } #record the actual number of cases for the given increment (days)
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          actual_cases,
          total_vec + forecast_mat[1,]
        )
      )
    } else if(days == 14) {
      fdate <- as.Date(date_vec) + 14
      actual_cases <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
          actual_cases[i] <- NA # accounting for blank arguments
        } else{
          actual_cases[i] <- data$total[data$date == as.Date(fdate[i])]
        }
      } #record the actual number of cases for the given increment (days)
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          actual_cases,
          total_vec + forecast_mat[2,]
        )
      )
    } else {
      fdate <- as.Date(date_vec) + 21
      actual_cases <- rep(NA,length(fdate))
      for(i in 1:length(fdate)) {
        if(length(data$total[data$date == as.Date(fdate[i])]) == 0) {
          actual_cases[i] <- NA # accounting for blank arguments
        } else{
          actual_cases[i] <- data$total[data$date == as.Date(fdate[i])]
        }
      }#record the actual number of cases for the given increment (days)
      
      df_show <- data.frame(
        cbind(
          date_vec,
          total_vec,
          dfdate,
          actual_cases,
          total_vec + forecast_mat[3,]
        )
      )
    }  #21 DAYS
    
    row.names(df_show) <- 1:nrow(df_show)
    colnames(df_show) <- c("prior.date","prior.total","forecast.date","actual.total","forecast.total")
    
    df_show$prior.total <- as.numeric(df_show$prior.total)
    df_show$actual.total <- as.numeric(df_show$actual.total)
    df_show$forecast.total <- as.numeric(df_show$forecast.total)
    
    df_show <- df_show %>% mutate(resids = actual.total - forecast.total) # calculate residuals
    
    RMSE <- df_show %>% summarise(RMSE = my_rmse(resids)) # calculate RMSE
    RMSE <- RMSE[1,1] #extract the RMSE value due to setup of dplyr
    
    ### RETURN ###
    
    ifelse(
      point == TRUE,
      return(list(plot = gfull_ref, results = df_show, rmse = RMSE)),
      return(list(plot = gsimp, results = df_show, rmse = RMSE))
    )
  } else { #if res == FALSE, simply just return the plots with nothing else (no RMSE or results table)
    if(point == TRUE) { # IF YOU WANT THE PLOT WITH POINTS OR A DASHED LINE
      return(gfull_ref)
    } else {
      return(gsimp)
    }
  }
}
######################################################################################################################


forecast_rmse <- function(date_vec, forecast_mat, days = 21, data = actual) {
  
  # Function that returns the RMSE value of outbreak forecasts
  
  # Args:
  #   date_vec: a vector of dates for the outbreak datasets
  #   forecast_mat: the matrix of the outbreak datasets' 1-, 2-, and 3-week forecasts 
  #                 (matrix must be in that order)
  #   days: number of forecasted days to visualize (valid inputs are 7, 14, or 21)
  #   data: a dataframe of the actual case count
  #
  # Returns:
  #   RMSE: the RMSE for the dataset

  # specify constants
  ## days in 1, 2, and 3 weeks
  n_days_week <- 7
  one_week <- 1*n_days_week
  two_weeks <- 2*n_days_week
  three_weeks <- 3*n_days_week
  
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
    ) #creating residuals 
    
    RMSE <- df_show %>% summarise(
      RMSE = sqrt( mean(na.omit(resids)^2) )
    ) #calculating RMSE
    
    RMSE <- RMSE[1,1] #extracts the singular value due to dplyr formatting
    RMSE2 <- Metrics::rmse(df_show$actual.total,df_show$forecast.total) #built-in
    if(RMSE == RMSE2) { #double-checking that built-in is the same
      return(RMSE)
    } else{
      stop(cat("RMSE does not match"))
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
    return(RMSE) #we don't need the RMSE2 argument check because the built-in can't handle NAs
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



full_forecast <- function(rdate, forecast) {
  
  # The full_forecast function plots the result from a single Hawkes or Recursive forecast
  # with respect to the entire outbreak.
  # This function requires the ggplot() object p to be declared
  
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



multi_forecast <- function(date_vec, forecast_mat, data = true, title = NULL) {
  
  # Function that accepts a vector of dates (date_vec) and
  # their corresponding 7, 14 and 21 day forecasted values, respectively (forecast_mat).
  # The title argument is the title for the corresponding plot.
  # The function returns a visualization of confirmed cases vs
  # the model projections.
  # The matrix of forecasted values is organized BY COLUMN. Each column in forecast_mat is a 3x1 vector.

  # NOTE: if you are just visualizing a single forecast, you DO NOT
  # need to cbind() your projections in forecast_mat, just the vector is enough.
  
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
