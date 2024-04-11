# Load in Libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

# Read in Data
Crime <- read.csv("Crimes.csv")
lunar <- read.csv("full_moon.csv")
holiday <- read.csv("holidays.csv")
weather <- read.csv("weather.csv")

# Prep and Clean Data
Crime$datetime <- Crime$Date

Crime$Date <- substr(Crime$datetime, 1, 10)  # Extract first 10 characters (assuming MM/DD/YYYY format)
Crime$Date <- as.Date(Crime$Date, format = "%m/%d/%Y")  # Uncomment if needed

Crime$Time <- strptime(Crime$datetime, format = "%m/%d/%Y %I:%M:%S %p")
Crime$Time <- format(Crime$Time, format = "%H:%M:%S")

crimes <- Crime |>
  select(Date, Time, Primary.Type, Description, Location.Description, Arrest, Domestic,
         Latitude, Longitude)

holiday <- holiday |>
  select(Holiday, Date) |>
  mutate(Holiday = TRUE)

holiday$Date <- as.Date(holiday$Date)

lunar <- lunar |>
  mutate(Year = TRUE) |>
  rename(Full.Moon = Year, Date = FullMoonDates)
lunar$Date <- as.Date(lunar$Date, format = "%d %B %Y")

weather <- weather |>
  rename(Date = datetime) |>
  select(Date, tempmax, tempmin, temp, humidity, precip, preciptype, snow,
         windgust, windspeed, cloudcover, visibility, solarradiation, uvindex,
         conditions)
weather$preciptype <- replace(weather$preciptype, is.na(weather$preciptype), "none")
weather <- replace(weather, is.na(weather), 0)
for (i in 1:nrow(weather)) {
  if (weather[i, 'precip'] == 0) {
    weather[i, 'preciptype'] <- "none"
  }
}

weather$Date <- as.Date(weather$Date)

# Combine data into one data frame
crimes <- merge(crimes, holiday, by = "Date", all.x = TRUE)
crimes <- merge(crimes, lunar, by = "Date", all.x = TRUE)
crimes <- merge(crimes, weather, by = "Date", all.x = TRUE)

filter_date <- "2009-12-31"
crimes <- crimes[crimes$Date > filter_date, ]
crimes <- replace(crimes, is.na(crimes), FALSE)

file_path <- "Crimes2.csv"
# Save the data frame as a CSV file
write.csv(crimes, file_path, row.names = FALSE)

# Load in complete Data Frames
crimes <- read.csv('Crimes2.csv')

crimes3 <- read.csv('crimes3.csv')
crime_lm <- lm(Num.Crimes ~ Holiday + Full.Moon + conditions, data = crimes3)
crime_lm

crime_temp <- read.csv("Crime_Temp.csv")
temp_lm <- lm(Num.Crimes ~ temp, data = crime_temp)
temp_lm

crime_full <- read.csv("Crime_Full.csv")
crime_full$Holiday <- as.integer(as.logical(crime_full$Holiday))
crime_full$Full.Moon <- as.integer(as.logical(crime_full$Full.Moon))
crime_full$preciptype <- as.factor(crime_full$preciptype)
crime_full_lm <- lm(Num.Crimes ~ temp + Holiday + Full.Moon + preciptype + humidity,
               data = crime_full)
crime_full_glm <- crime_full |>
  select(temp, Holiday, Full.Moon, preciptype, humidity, Num.Crimes)

ggplot(temp_lm, aes(x = temp, y = Num.Crimes)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE, color = 'red') +
  theme_minimal() +
  labs(x = "Average Temperature in Fahrenheit", y = "Number of Crimes",
       title = "Crimerate by Average Temperature")

ggplot(crime_full, aes(x = humidity, y = Num.Crimes)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE, color = 'red') +
  theme_minimal() +
  labs(x = "Average Temperature in Fahrenheit", y = "Number of Crimes",
       title = "Crimerate by Average Temperature")

library(car)  # needed for VIFs
library(bestglm)  # for all subsets / stepwise methods
library(glmnet)  # for ridge, lasso, and elastic net
library(corrplot) # for a nice neat correlation plot

crime_temp$Holiday <- as.integer(as.logical(crime_temp$Holiday))
crime_temp$Full.Moon <- as.integer(as.logical(crime_temp$Full.Moon))
crime_temp_glm <- crime_temp |>
  select(Holiday, Full.Moon, temp, Num.Crimes)

best_subsets_bic <- bestglm(crime_full_glm,
                            IC = "BIC",
                            method = "exhaustive")
best_subsets_aic <- bestglm(crime_full_glm,
                            IC = "AIC",
                            method = "exhaustive")

best_subsets_bic$BestModels
best_subsets_aic$BestModels
aic_model <- best_subsets_aic$BestModel
bic_model <- best_subsets_bic$BestModel
bic_model
aic_model

# Only looking at violent crime
unique(crimes$Primary.Type)

# Master Data Frame
