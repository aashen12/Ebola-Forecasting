## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- include = FALSE----------------------------------------------------------------------------------------------------------------
## Complete Outbreak Dataset 
## Used to plot the actual number of infections over time
rm(list=ls())
actual <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/15UGkfREtfqH3LdfHmCsSpFJ5SrTnSeyt/ebola/2020-05-04_ebola/2020-05-04_data.csv")
source("outbreak_vis.R") #script with functions
rgx <- "\\d{1,2}\\/\\d{1,2}\\/\\d{4}" #date structure regex
actual <- actual[str_detect(actual$Date, rgx),] #omits rows without a date
colnames(actual) <- c("date", "cases")
actual$date <- mdy(actual$date) #converts into a consistent date format
actual$cases[is.na(actual$cases)] <- 0
actual <- actual %>% mutate(total = cumsum(cases))
last_date <- actual$date[length(actual$date)]
last_case <- actual$total[length(actual$total)]


## ---- include = FALSE----------------------------------------------------------------------------------------------------------------
## Recursive Projections Dataset
rproj <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1LaD1nL_OAOposW2fr2XDcs6BLHVBC-jA/2019 Ebola/Post-Outbreak Analysis/recursive_post_proj.csv") #forecasted values
rproj <- rproj %>% select(date_last_case, pred.7, pred.14, pred.21) 
rproj$date_last_case <- mdy(rproj$date_last_case)
rproj <- na.omit(rproj) # some dates have no projections
#rproj <- rproj %>% distinct()
rownames(rproj) <- 1:nrow(rproj)
rdates <- as.character(rproj$date_last_case)
rpreds <- t(rproj %>% select(pred.7,pred.14,pred.21))
omit_h <- c(8,17,22,33,47:49,51,52,55,56,60,64,69,77) #omit from Hawkes because Recursive has no data for these dates


## ---- include = FALSE----------------------------------------------------------------------------------------------------------------
## Hawkes Projections Dataset
hproj <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1LaD1nL_OAOposW2fr2XDcs6BLHVBC-jA/2019 Ebola/Post-Outbreak Analysis/hawkes_post_proj.csv") #forecasted values
hproj <- hproj %>% select(date_last_case, pred.7, pred.14, pred.21) #selecting only the cols with forecasts
hproj$date_last_case <- mdy(hproj$date_last_case)
hproj <- hproj[-omit_h,] #omit the rows that aren't also in recursive
#hproj <- hproj %>% distinct()
hdates <- as.character(hproj$date_last_case) #for working with dates
hpreds <- t(hproj %>% select(pred.7,pred.14,pred.21)) #take t() for the function
rownames(hproj) <- 1:nrow(hproj)


## ------------------------------------------------------------------------------------------------------------------------------------
dim <- 7


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------
actual <- actual %>% mutate(seven_day_avg = zoo::rollmean(cases, k = 7,fill = NA))
ggplot(
  data = actual,
  mapping = aes(x = date, y = cases)
) + geom_col(color = "gray70") + theme_classic() + 
  geom_path(
    data = actual, 
    mapping = aes(x = date, y = seven_day_avg),
    color = "black",
    size = 0.75
  ) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") + 
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46") + 
  theme(
    axis.title.x = element_blank()
  )
ggsave(
  "daily_new_cases_GS.jpg",
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = 6.5, height = 5.5, units = "in"
)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------
cap <- paste0("Residual plot of Hawkes and Recursive 7-day models")
h7 <- (single_forecast(hdates, hpreds, days = 7)$results)
r7 <- (single_forecast(rdates, rpreds, days = 7)$results)
df <- data.frame(
  date = h7$forecast.date,
  hawkes = h7$resids,
  recursive = r7$resids
)
df <- pivot_longer(df, cols = 2:3, names_to = "forecast", values_to = "resid")
p <- ggplot(
  data = actual,
  mapping = aes(x = date, y = total)
) + 
  theme_classic() + 
  theme(plot.caption = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") + 
  geom_hline(aes(yintercept=0))
  
p + geom_point(
  data = df,
  mapping = aes(x = as.Date(date), y = resid, shape = forecast, color = forecast)
) + scale_color_manual(
  values = c(
    "gray50", "gray10"
  )
) +
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_vs_rec_7_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------
cap <- paste0("Residual plot of Hawkes and Recursive 14-day models")
h14 <- single_forecast(hdates, hpreds, days = 14)$results
r14 <- single_forecast(rdates, rpreds, days = 14)$results
df <- data.frame(
  date = h14$forecast.date,
  hawkes = h14$resids,
  recursive = r14$resids
)
df <- pivot_longer(df, cols = 2:3, names_to = "forecast", values_to = "resid")
p <- ggplot(
  data = actual,
  mapping = aes(x = date, y = total)
) + 
  theme_classic() + 
  theme(plot.caption = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") + 
  geom_hline(aes(yintercept=0))
  
p + labs(y = "cases") + geom_point(
  data = df,
  mapping = aes(
    x = as.Date(date), y = resid, shape = forecast, color = forecast
  ) 
) + scale_color_manual(values = c("gray50", "gray10")) +
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_vs_rec_14_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------
h21 <- single_forecast(hdates, hpreds, days = 21)$results
r21 <- single_forecast(rdates, rpreds, days = 21)$results
df <- data.frame(
  date = h21$forecast.date,
  hawkes = h21$resids,
  recursive = r21$resids
)
df <- pivot_longer(df, cols = 2:3, names_to = "forecast", values_to = "resid")
p <- ggplot(
  data = actual,
  mapping = aes(x = date, y = total)
) + 
  theme_classic() + 
  theme(plot.caption = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") + 
  geom_hline(aes(yintercept=0))
  
p + labs(y = "cases") + geom_point(
  data = df,
  mapping = aes(x = as.Date(date), y = resid, shape = forecast, color = forecast)
) + scale_color_manual(values = c("gray50", "gray10")) +
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_vs_rec_21_GS.jpg", 
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


## ----warning=F-----------------------------------------------------------------------------------------------------------------------
p + labs(y = "cases") + 
  geom_point(
    data = h7, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "black"), shape = 15
  ) +   
  geom_point(
    data = h14, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "gray40"), shape = 16
  ) + 
  geom_point(
    data = h21, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "gray70"), shape = 17
  ) + 
  scale_color_identity(
    name = "",
    breaks = c("black", "gray40", "gray70"),
    labels = c("7-day", "14-day", "21-day"),
    guide = "legend"
  ) + geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_resids_GS.jpg",
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


## ----warning=F-----------------------------------------------------------------------------------------------------------------------
p + labs(y = "cases") + 
  geom_point(
    data = r7, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "black"), shape = 15
  ) +   
  geom_point(
    data = r14, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "gray40"), shape = 16
  ) + 
  geom_point(
    data = r21, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "gray70"), shape = 17
  ) + 
  scale_color_identity(
    name = "",
    breaks = c("black", "gray40", "gray70"),
    labels = c("7-day", "14-day", "21-day"),
    guide = "legend"
  ) + geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "rec_resids_GS.jpg",
  path = "/Users/andyshen/Desktop/Ebola Modeling",
  width = dim, height = dim, units = "in"
)


