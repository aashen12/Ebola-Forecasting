

## Complete Outbreak Dataset 
## Used to plot the actual number of infections over time
rm(list=ls())
source("/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Grayscale_Plots.R") #script with functions
actual <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/15UGkfREtfqH3LdfHmCsSpFJ5SrTnSeyt/ebola/2020-05-04_ebola/2020-05-04_data.csv")
rgx <- "\\d{1,2}\\/\\d{1,2}\\/\\d{4}" #date structure regex
actual <- actual[str_detect(actual$Date, rgx),] #omits rows without a date
colnames(actual) <- c("date", "cases")
actual$date <- mdy(actual$date) #converts into a consistent date format
actual$cases[is.na(actual$cases)] <- 0
actual <- actual %>% mutate(total = cumsum(cases))
last_date <- actual$date[length(actual$date)]
last_case <- actual$total[length(actual$total)]

# Recursive Projections Dataset
# read in data
rproj <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1LaD1nL_OAOposW2fr2XDcs6BLHVBC-jA/2019 Ebola/Post-Outbreak Analysis/recursive_post_proj.csv") #forecasted values
# data manipulation
rproj <- rproj %>% select(date_last_case, pred.7, pred.14, pred.21) 
rproj$date_last_case <- mdy(rproj$date_last_case)
rproj <- na.omit(rproj) # some dates have no projections
# rproj <- rproj %>% distinct()
rownames(rproj) <- 1:nrow(rproj)
rdates <- as.character(rproj$date_last_case)
rpreds <- t(rproj %>% select(pred.7,pred.14,pred.21))
# note the dates of the datasets are missing forecasts
# these are the ones to omit from Hawkes
omit_h <- c(8,17,22,33,47:49,51,52,55,56,60,64,69,77) 


# Hawkes Projections Dataset
# read in data 
hproj <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1LaD1nL_OAOposW2fr2XDcs6BLHVBC-jA/2019 Ebola/Post-Outbreak Analysis/hawkes_post_proj.csv") #forecasted values
hproj <- hproj %>% select(date_last_case, pred.7, pred.14, pred.21) #selecting only the cols with forecasts
hproj$date_last_case <- mdy(hproj$date_last_case)
hproj <- hproj[-omit_h,] #omit the rows that aren't also in recursive
# hproj <- hproj %>% distinct()
rownames(hproj) <- 1:nrow(hproj)
hdates <- as.character(hproj$date_last_case) #for working with dates
hpreds <- t(hproj %>% select(pred.7,pred.14,pred.21)) #take t() for the function

dim <- 3.7

fig <- 1 # for numbering the figures in the plots
# create the plot
mod <- single_forecast(hdates, hpreds, days = 7, res = TRUE)
mod$plot
ggsave(
  "hawkes7_all_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)

# update numbering the figures
fig <- fig + 1
# create and display the plot
mod <- single_forecast(hdates, hpreds, days = 14, res = TRUE)
mod$plot
ggsave(
  "hawkes14_all_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


# update numbering the figures
fig <- fig + 1
# create and display the plot
mod <- single_forecast(hdates, hpreds, days = 21,  res = TRUE)
mod$plot
ggsave(
  "hawkes21_all_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)

# update numbering the figures
fig <- fig + 1
# create and display the plot 
mod <- single_forecast(rdates, rpreds, days = 7, res = TRUE)
mod$plot
ggsave(
  "rec7_all_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


# update numbering the figures
fig <- fig + 1
# create and display the plot
mod <- single_forecast(rdates, rpreds, days = 14, res = TRUE)
mod$plot
ggsave(
  "rec14_all_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


# update numbering the figures
fig <- fig + 1
# create and display the plot
mod <- single_forecast(rdates, rpreds, days = 21, res = TRUE)
mod$plot
ggsave(
  "rec21_all_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)

