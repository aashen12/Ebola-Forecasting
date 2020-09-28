## Complete Outbreak Dataset 
## Used to plot the actual number of infections over time
rm(list=ls())
source("/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/outbreak_vis.R") #script with functions
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

fig <- 1 # for numbering the figures in the plots
# create the plot
title <- paste0("Hawkes 7-Day Forecasts for All Datasets")
mod <- single_forecast(hdates, hpreds, days = 7,  title = title, res = TRUE)
mod$plot
ggsave(
  "hawkes7_all.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)

# update numbering the figures
fig <- fig + 1
# create and display the plot
title <- paste0("Hawkes 14-Day Forecasts for All Datasets")
mod <- single_forecast(hdates, hpreds, days = 14, title = title, res = TRUE)
mod$plot
ggsave(
  "hawkes14_all.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)


# update numbering the figures
fig <- fig + 1
# create and display the plot
title <- paste0("Hawkes 21-Day Forecasts for All Datasets")
mod <- single_forecast(hdates, hpreds, days = 21,  title = title, res = TRUE)
mod$plot
ggsave(
  "hawkes21_all.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)

# update numbering the figures
fig <- fig + 1
# create and display the plot 
title <- paste0("Recursive 7-Day Forecasts for All Datasets")
mod <- single_forecast(rdates, rpreds, days = 7, title = title, res = TRUE)
mod$plot
ggsave(
  "rec7_all.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)


# update numbering the figures
fig <- fig + 1
# create and display the plot
title <- paste0("Recursive 14-Day Forecasts for All Datasets")
mod <- single_forecast(rdates, rpreds, days = 14, title = title, res = TRUE)
mod$plot
ggsave(
  "rec14_all.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)


# update numbering the figures
fig <- fig + 1
# create and display the plot
title <- paste0("Recursive 21-Day Forecasts for All Datasets")
mod <- single_forecast(rdates, rpreds, days = 21, title = title, res = TRUE)
mod$plot
ggsave(
  "rec21_all.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)


# New Ebola Cases by Day
# update numbering the figures
fig <- fig + 1
actual <- actual %>% mutate(seven_day_avg = zoo::rollmean(cases, k = 7,fill = NA))
title <- paste0("Ebola Cases by Day in West Africa with 7-day Rolling Average")
ggplot(
  data = actual,
  mapping = aes(x = date, y = cases)
) + geom_col(color = "lightblue") + theme_classic() + 
  geom_path(
    data = actual, 
    mapping = aes(x = date, y = seven_day_avg),
    color = "black",
    size = 0.75
  ) + labs(caption = title) + 
  theme(plot.caption = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "5 months", date_labels = "%b-%y") + 
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "daily_new_cases.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)


# Hawkes vs Recursive 7-Day Residual Plot
fig <- fig + 1
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
  mapping = aes(x = as.Date(date), y = resid, color = forecast)
) + labs(caption = cap) + 
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_vs_rec_7.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)


# Hawkes vs Recursive 14-Day Residual Plot
fig <- fig + 1
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

p + geom_point(
  data = df,
  mapping = aes(x = as.Date(date), y = resid, color = forecast)
) + labs(caption = cap) + 
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_vs_rec_14.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)

# Hawkes vs Recursive 21-Day Residual Plot

fig <- fig + 1
cap <- paste0("Residual plot of Hawkes and Recursive 21-day models")
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

p + geom_point(
  data = df,
  mapping = aes(x = as.Date(date), y = resid, color = forecast)
) + labs(caption = cap) + 
  geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_vs_rec_21.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)

# Hawkes-Only Residual Plot
fig <- fig + 1
cap <- paste0("Residual plot of Hawkes 7-, 14-, and 21-day models")
p + labs(caption = cap) + 
  geom_point(
    data = h7, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "dodgerblue")
  ) +   
  geom_point(
    data = h14, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "red2")
  ) + 
  geom_point(
    data = h21, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "forestgreen")
  ) + 
  scale_color_identity(
    name = "",
    breaks = c("dodgerblue", "red2", "forestgreen"),
    labels = c("7-day", "14-day", "21-day"),
    guide = "legend"
  ) + geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "hawkes_resids.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)

# Recursive-Only Residual Plot
fig <- fig + 1

cap <- paste0("Residual plot of Recursive 7-, 14-, and 21-day models")

p + labs(caption = cap) + 
  geom_point(
    data = r7, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "dodgerblue")
  ) +   
  geom_point(
    data = r14, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "red2")
  ) + 
  geom_point(
    data = r21, 
    mapping = aes(x = as.Date(forecast.date), y = resids, col = "forestgreen")
  ) + 
  scale_color_identity(
    name = "",
    breaks = c("dodgerblue", "red2", "forestgreen"),
    labels = c("7-day", "14-day", "21-day"),
    guide = "legend"
  ) + geom_vline(aes(xintercept = as.Date("2018-09-04")), col = "grey46") +
  geom_vline(aes(xintercept = as.Date("2019-08-25")), col = "grey46")
ggsave(
  "rec_resids.jpg", 
  path = "/Users/andyshen/Desktop/Git/Ebola-Forecasting/Post Outbreak/Images",
  width = 5.5, height = 5.5, units = "in"
)

