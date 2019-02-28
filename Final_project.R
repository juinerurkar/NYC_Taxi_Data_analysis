##
library(lubridate)
library(dplyr)
library(ggmap)
library(leaflet)
library(ggplot2)

#COMMON FILES
taxi_zone <- read.csv("taxi _zone_lookup.csv")

#DATA - TAXI
taxi_jan <- read.csv("Final_Project/Final_Project/yellow_tripdata_2018-01.csv")
taxi_feb <- read.csv("Final_Project/Final_Project/yellow_tripdata_2018-02.csv")
taxi_march <- read.csv("Final_Project/Final_Project/yellow_tripdata_2018-03.csv")
taxi_april <- read.csv ("Final_Project/Final_Project/yellow_tripdata_2018-04.csv")
taxi_may <- read.csv("Final_Project/Final_Project/yellow_tripdata_2018-05.csv")
taxi_june <- read.csv("Final_Project/Final_Project/yellow_tripdata_2018-06.csv")


##date
taxi_jan$Pick_up_Date <- as.Date(taxi_jan$tpep_pickup_datetime)
taxi_feb$Pick_up_Date <- as.Date(taxi_feb$tpep_pickup_datetime)
taxi_march$Pick_up_Date <- as.Date(taxi_march$tpep_pickup_datetime)
taxi_april$Pick_up_Date <- as.Date(taxi_april$tpep_pickup_datetime)
taxi_may$Pick_up_Date <- as.Date(taxi_may$tpep_pickup_datetime)
taxi_june$Pick_up_Date <- as.Date(taxi_june$tpep_pickup_datetime)



##select only one month
taxi_jan <- taxi_jan %>% filter(Pick_up_Date <= "2018-01-31")%>% filter(Pick_up_Date >= "2018-01-01")
taxi_feb <- taxi_feb %>% filter(Pick_up_Date <= "2018-02-28")%>% filter(Pick_up_Date >= "2018-02-01")
taxi_march <- taxi_march %>% filter(Pick_up_Date <= "2018-03-31")%>% filter(Pick_up_Date >= "2018-03-01")
taxi_april <- taxi_april %>% filter(Pick_up_Date <= "2018-04-30")%>% filter(Pick_up_Date >= "2018-04-01")
taxi_may <- taxi_may %>% filter(Pick_up_Date <= "2018-05-31")%>% filter(Pick_up_Date >= "2018-05-01")
taxi_june <- taxi_june %>% filter(Pick_up_Date <= "2018-06-30")%>% filter(Pick_up_Date >= "2018-06-01")


#Separate hours
taxi_jan$Pick_up_Hour <- format(as.POSIXct(taxi_jan$tpep_pickup_datetime),format = "%H")
taxi_feb$Pick_up_Hour <- format(as.POSIXct(taxi_feb$tpep_pickup_datetime),format = "%H")
taxi_march$Pick_up_Hour <- format(as.POSIXct(taxi_march$tpep_pickup_datetime),format = "%H")
taxi_april$Pick_up_Hour <- format(as.POSIXct(taxi_april$tpep_pickup_datetime),format = "%H")
taxi_may$Pick_up_Hour <- format(as.POSIXct(taxi_may$tpep_pickup_datetime),format = "%H")
taxi_june$Pick_up_Hour <- format(as.POSIXct(taxi_june$tpep_pickup_datetime),format = "%H")

#rename pick up date as "DATE"
taxi_jan <- taxi_jan %>% rename(DATE = Pick_up_Date)
taxi_feb <- taxi_feb %>% rename(DATE = Pick_up_Date)
taxi_march <- taxi_march %>% rename(DATE = Pick_up_Date)
taxi_april <- taxi_april %>% rename(DATE = Pick_up_Date)
taxi_may <- taxi_may %>% rename(DATE = Pick_up_Date)
taxi_june <- taxi_june %>% rename(DATE = Pick_up_Date)

#Rename Pick location ID as Location ID (if merging taxi_zone)
taxi_jan <- taxi_jan %>% rename(LocationID = PULocationID)
taxi_feb <- taxi_feb %>% rename(LocationID = PULocationID)
taxi_march <- taxi_march %>% rename(LocationID = PULocationID)
taxi_april <- taxi_april %>% rename(LocationID = PULocationID)
taxi_may <- taxi_may %>% rename(LocationID = PULocationID)
taxi_june <- taxi_june %>% rename(LocationID = PULocationID)


#Separate drop off date and time from the taxi data drop up date time column
taxi_jan$Drop_off_Date <- as.Date(taxi_jan$tpep_dropoff_datetime)
taxi_feb$Drop_off_Date <- as.Date(taxi_feb$tpep_dropoff_datetime)
taxi_march$Drop_off_Date <- as.Date(taxi_march$tpep_dropoff_datetime)
taxi_april$Drop_off_Date <- as.Date(taxi_april$tpep_dropoff_datetime)
taxi_may$Drop_off_Date <- as.Date(taxi_may$tpep_dropoff_datetime)
taxi_june$Drop_off_Date <- as.Date(taxi_june$tpep_dropoff_datetime)

#Separate drop off time and date
taxi_jan$Drop_off_Time <- format(as.POSIXct(taxi_jan$tpep_dropoff_datetime),format = "%H:%M:%S")
taxi_feb$Drop_off_Time <- format(as.POSIXct(taxi_feb$tpep_dropoff_datetime),format = "%H:%M:%S")
taxi_march$Drop_off_Time <- format(as.POSIXct(taxi_march$tpep_dropoff_datetime),format = "%H:%M:%S")
taxi_april$Drop_off_Time <- format(as.POSIXct(taxi_april$tpep_dropoff_datetime),format = "%H:%M:%S")
taxi_may$Drop_off_Time <- format(as.POSIXct(taxi_may$tpep_dropoff_datetime),format = "%H:%M:%S")
taxi_june$Drop_off_Time <- format(as.POSIXct(taxi_june$tpep_dropoff_datetime),format = "%H:%M:%S")


#Remove data where the difference between pick up dates and drop off dates is greater than 1
taxi_jan$Diff_date <- taxi_jan$Drop_off_Date - taxi_jan$DATE
taxi_jan <- taxi_jan %>% filter(Diff_date <= 1)
taxi_feb$Diff_date <- taxi_feb$Drop_off_Date - taxi_feb$DATE
taxi_feb <- taxi_feb %>% filter(Diff_date <= 1)
taxi_march$Diff_date <- taxi_march$Drop_off_Date - taxi_march$DATE
taxi_march <- taxi_march %>% filter(Diff_date <= 1)
taxi_april$Diff_date <- taxi_april$Drop_off_Date - taxi_april$DATE
taxi_april <- taxi_april %>% filter(Diff_date <= 1)
taxi_may$Diff_date <- taxi_may$Drop_off_Date - taxi_may$DATE
taxi_may <- taxi_may %>% filter(Diff_date <= 1)
taxi_june$Diff_date <- taxi_june$Drop_off_Date - taxi_june$DATE
taxi_june <- taxi_june %>% filter(Diff_date <= 1)

#Remove trips where trip distance is less than or equal to 0 and greater than 50 miles
taxi_jan <- taxi_jan %>% filter(trip_distance > 0)
taxi_jan <- taxi_jan %>% filter(trip_distance < 50)
taxi_feb <- taxi_feb %>% filter(trip_distance > 0)
taxi_feb <- taxi_feb %>% filter(trip_distance < 50)
taxi_march <- taxi_march %>% filter(trip_distance > 0)
taxi_march <- taxi_march %>% filter(trip_distance < 50)
taxi_apri <- taxi_april %>% filter(trip_distance > 0)
taxi_april <- taxi_apri
taxi_april <- taxi_april %>% filter(trip_distance < 50)
taxi_may <- taxi_may %>% filter(trip_distance > 0)
taxi_may <- taxi_may %>% filter(trip_distance < 50)
taxi_june <- taxi_june %>% filter(trip_distance > 0)
taxi_june <- taxi_june %>% filter(trip_distance < 50)


#Calculate trip duration (in seconds)
taxi_jan$Trip_duration <- ymd_hms(taxi_jan$tpep_dropoff_datetime) - ymd_hms(taxi_jan$tpep_pickup_datetime)
taxi_jan <- taxi_jan %>% filter(!Trip_duration <= 0)

taxi_feb$Trip_duration <- ymd_hms(taxi_feb$tpep_dropoff_datetime) - ymd_hms(taxi_feb$tpep_pickup_datetime)
taxi_feb <- taxi_feb %>% filter(!Trip_duration <= 0)

taxi_march$Trip_duration <- ymd_hms(taxi_march$tpep_dropoff_datetime) - ymd_hms(taxi_march$tpep_pickup_datetime)
taxi_march <- taxi_march %>% filter(!Trip_duration <= 0)

taxi_april$Trip_duration <- ymd_hms(taxi_april$tpep_dropoff_datetime) - ymd_hms(taxi_april$tpep_pickup_datetime)
taxi_april <- taxi_april %>% filter(!Trip_duration <= 0)

taxi_may$Trip_duration <- ymd_hms(taxi_may$tpep_dropoff_datetime) - ymd_hms(taxi_may$tpep_pickup_datetime)
taxi_may <- taxi_may %>% filter(!Trip_duration <= 0)

taxi_june$Trip_duration <- ymd_hms(taxi_june$tpep_dropoff_datetime) - ymd_hms(taxi_june$tpep_pickup_datetime)
taxi_june <- taxi_june %>% filter(!Trip_duration <= 0)


# Delete rows where number of passengers is 0 or greater than 4
taxi_jan <- taxi_jan %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)
taxi_feb <- taxi_feb %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)
taxi_march <- taxi_march %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)
taxi_april <- taxi_april %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)
taxi_may <- taxi_may %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)
taxi_june <- taxi_june %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)


#Select payment type - only credit and cash(if required)
taxi_jan <- taxi_jan %>% filter(payment_type %in% c(1,2))
taxi_feb <- taxi_feb %>% filter(payment_type %in% c(1,2))
taxi_march <- taxi_march %>% filter(payment_type %in% c(1,2))
taxi_april <- taxi_april %>% filter(payment_type %in% c(1,2))
taxi_may <- taxi_may %>% filter(payment_type %in% c(1,2))
taxi_june <- taxi_june %>% filter(payment_type %in% c(1,2))



#Remove rate code type == 99 from data
taxi_jan <- taxi_jan %>% filter(!RatecodeID == 99)
taxi_feb <- taxi_feb %>% filter(!RatecodeID == 99)
taxi_march <- taxi_march %>% filter(!RatecodeID == 99)
taxi_april <- taxi_april %>% filter(!RatecodeID == 99)
taxi_may <- taxi_may %>% filter(!RatecodeID == 99)
taxi_june <- taxi_june %>% filter(!RatecodeID == 99)


#Since the initial charge for a yellow taxi is 2.5, restrict the total amount to greater than or equal to 2.5
taxi_jan <- taxi_jan %>% filter(total_amount >= 2.50)
taxi_feb <- taxi_feb %>% filter(total_amount >= 2.50)
taxi_march <- taxi_march %>% filter(total_amount >= 2.50)
taxi_april <- taxi_april %>% filter(total_amount >= 2.50)
taxi_may <- taxi_may %>% filter(total_amount >= 2.50)
taxi_june <- taxi_june %>% filter(total_amount >= 2.50)


#Adding weekdays to the data
taxi_jan$Day <- weekdays(as.Date(taxi_jan$DATE))
taxi_feb$Day <- weekdays(as.Date(taxi_feb$DATE))
taxi_march$Day <- weekdays(as.Date(taxi_march$DATE))
taxi_april$Day <- weekdays(as.Date(taxi_april$DATE))
taxi_may$Day <- weekdays(as.Date(taxi_may$DATE))
taxi_june$Day <- weekdays(as.Date(taxi_june$DATE))

#write.csv(taxi_jan, file = "Taxi_jan.csv")
#write.csv(taxi_feb, file = "Taxi_feb.csv")
#write.csv(taxi_march, file = "Taxi_march.csv")
#write.csv(taxi_april, file = "Taxi_april.csv")
#write.csv(taxi_may, file = "Taxi_may.csv")
#write.csv(taxi_june, file = "Taxi_june.csv")

#Read and merge taxi and weather datasets
weather <- read.csv("Weather.csv")
weather$DATE <- as.character(weather$DATE)
weather$DATE <- as.Date(weather$DATE)


taxi_Jan <- merge(taxi_jan, weather, by = "DATE")
taxi_Feb <- merge(taxi_feb, weather, by = "DATE")
taxi_March <- merge(taxi_march, weather, by = "DATE")
taxi_April <- merge(taxi_april, weather, by = "DATE")
taxi_May <- merge(taxi_may, weather, by = "DATE")
taxi_June <- merge(taxi_june, weather, by = "DATE")
###Write as csv later

# Take samples
sample_Jan <- taxi_Jan[sample(nrow(taxi_Jan), 10000), ]
sample_Jan$Month <- rep("January", 10000)

sample_Feb <- taxi_Feb[sample(nrow(taxi_Feb), 10000), ]
sample_Feb$Month <- rep("February", 10000)

sample_March <- taxi_March[sample(nrow(taxi_March), 10000), ]
sample_March$Month <- rep("March", 10000)

sample_April <- taxi_April[sample(nrow(taxi_April), 10000), ]
sample_April$Month <- rep("April", 10000)

sample_May <- taxi_May[sample(nrow(taxi_May), 10000), ]
sample_May$Month <- rep("May", 10000)

sample_June <- taxi_June[sample(nrow(taxi_June), 10000), ]
sample_June$Month <- rep("June", 10000)


#Bind all samples together
train.data <- rbind(sample_Jan, sample_Feb, sample_March, sample_April, sample_May, sample_June)
train.data <- train.data %>% mutate(Average_speed = trip_distance/as.integer(Trip_duration))

#(if required)
train.data <- train.data %>% mutate(time.period = case_when(
  as.numeric(Pick_up_Hour) >= 00 & as.numeric(Pick_up_Hour) < 06 ~ '1',
  as.numeric(Pick_up_Hour) >= 06 & as.numeric(Pick_up_Hour) < 12 ~ '2',
  as.numeric(Pick_up_Hour) >= 12 & as.numeric(Pick_up_Hour) < 18 ~ '3',
  as.numeric(Pick_up_Hour) >= 18 & as.numeric(Pick_up_Hour) <= 23 ~ '4'))



##Test data
taxi_test <- read.csv("yellow_tripdata_2017-01.csv")


taxi_test$Pick_up_Date <- as.Date(taxi_test$tpep_pickup_datetime)
taxi_test <- taxi_test %>% filter(Pick_up_Date <= "2017-01-31")%>% filter(Pick_up_Date >= "2017-01-01")
taxi_test$Pick_up_Hour <- format(as.POSIXct(taxi_test$tpep_pickup_datetime),format = "%H")
taxi_test <- taxi_test %>% rename(DATE = Pick_up_Date)
taxi_test$Drop_off_Date <- as.Date(taxi_test$tpep_dropoff_datetime)
taxi_test$Drop_off_Time <- format(as.POSIXct(taxi_test$tpep_dropoff_datetime),format = "%H:%M:%S")
taxi_test$Diff_date <- taxi_test$Drop_off_Date - taxi_test$DATE
taxi_test <- taxi_test %>% filter(Diff_date <= 1)
taxi_test <- taxi_test %>% filter(trip_distance > 0)
taxi_test <- taxi_test %>% filter(trip_distance < 50)
taxi_test$Trip_duration <- ymd_hms(taxi_test$tpep_dropoff_datetime) - ymd_hms(taxi_test$tpep_pickup_datetime)
taxi_test <- taxi_test %>% filter(!Trip_duration <= 0)
taxi_test <- taxi_test %>% filter(passenger_count > 0) %>% filter(passenger_count < 5)
taxi_test <- taxi_test %>% filter(!RatecodeID == 99)
taxi_test <- taxi_test %>% filter(total_amount >= 2.50)
taxi_test$Day <- weekdays(as.Date(taxi_test$DATE))
test.data <- taxi_test %>% mutate(Average_speed = trip_distance/as.integer(Trip_duration))

weather_test <- read.csv("weather_2017.csv")
weather_test$DATE <- as.character(weather_test$DATE)
weather_test$DATE <- as.Date(weather_test$DATE)

test.data <- merge(test.data, weather_test, by ="DATE")

test_data <- test.data[sample(nrow(test.data), 10000), ]

#(if required)
test.data <- test.data %>% mutate(time.period = case_when(
  as.numeric(Pick_up_Hour) >= 00 & as.numeric(Pick_up_Hour) < 06 ~ '1',
  as.numeric(Pick_up_Hour) >= 06 & as.numeric(Pick_up_Hour) < 12 ~ '2',
  as.numeric(Pick_up_Hour) >= 12 & as.numeric(Pick_up_Hour) < 18 ~ '3',
  as.numeric(Pick_up_Hour) >= 18 & as.numeric(Pick_up_Hour) <= 23 ~ '4'))



#Fit the linear regression model with n as the response variable
#Relation between number of taxis and weather
train_byDate <- train.data %>% group_by(DATE, TMAX, TMIN, SNOW, AWND, SNWD) %>% count() %>% rename(nTaxi = n) 

lm.fit1 <- lm(nTaxi ~ TMAX+TMIN+SNOW+SNWD, data = train_byDate)
summary(lm.fit1)

#Backward stepwise regression
full_model <- lm(Average_speed ~. , data = train.data)
AIC_model <- stepAIC(full_model, direction = "backward")
AIC_model



test_byDate <- test_data %>% group_by(DATE, TMAX, TMIN, SNOW, AWND, SNWD) %>% count() %>% rename(nTaxi = n) 

#Predict 
test_byDate$Predictions <- predict(lm.fit1, newdata = test_byDate, type = "response")
PRMSE <- sqrt(mean((test_byDate$Predictions - test_byDate$nTaxi)^2))
PRMSE

plot(train_byDate$SNOW, train_byDate$nTaxi, main = "Relationship between snow and number of taxis", xlab = "Amount of snowfall", ylab = "Number of taxis per day")
abline(a = coef(lm.fit1)[1], b = coef(lm.fit1)[4], col = 1, lwd = 1)

#Regression to check relation between days and average speed
lm.fit2 <- lm(Average_speed ~ SNOW+SNWD+TMAX+TMIN, data = train.data)
summary(lm.fit2)



#Plots
hourly_data <- train.data %>% group_by(Pick_up_Hour) %>% count() %>% rename(N_Taxis = n) 

day_night <- train.data %>% mutate(time.period = case_when(
  as.numeric(Pick_up_Hour) >= 00 & as.numeric(Pick_up_Hour) < 06 ~ '1',
  as.numeric(Pick_up_Hour) >= 06 & as.numeric(Pick_up_Hour) < 12 ~ '2',
  as.numeric(Pick_up_Hour) >= 12 & as.numeric(Pick_up_Hour) < 18 ~ '3',
  as.numeric(Pick_up_Hour) >= 18 & as.numeric(Pick_up_Hour) <= 23 ~ '4'))

day_night_time <- day_night %>% group_by(Day, time.period) %>% count() %>% rename(Number_of_taxis = n)

ggplot(data = day_night_time, aes(x= Day, y=Number_of_taxis, fill = time.period))+ geom_bar(stat="identity")
lm.fit2 <- lm(Number_of_taxis ~ Day+time.period, data = day_night_time)




daily_data <- train.data %>% group_by(Day) %>% summarise(sum(N_Taxis))

plot(x=hourly_data$Pick_up_Hour, y= hourly_data$`sum(N_Taxis)`/1000, main = "Taxi ridership(in thousands) per Hour", xlab = "Pick up Hour", ylab = "Total number of taxi ridership(in 1000s)", pch = 16)

test_data <- test.data %>% group_by(Pick_up_Hour) %>% summarise(NTaxis = sum(N_Taxis))


#Precipitation graph
precip <- train.data %>% group_by(DATE,PRCP) %>% count() 
precip <- precip %>% rename(nTaxi = n)
precip <- precip %>% rename(precipitation = PRCP)
precip <- precip %>% mutate(precip_in_inches = case_when
(as.numeric(precipitation) == 00  ~ '1',
as.numeric(precipitation) > 00 & as.numeric(precipitation) < 0.2 ~ '2',
as.numeric(precipitation) >= 0.2 & as.numeric(precipitation) < 0.4 ~ '3',
as.numeric(precipitation) >= 0.4 & as.numeric(precipitation) < 0.6 ~ '4',
as.numeric(precipitation) >= 0.6  ~ '5'))
precip <- precip %>% group_by(precip_in_inches) %>% summarise("Average daily trips" = mean(nTaxi))

ggplot(precip, aes(precip$precip_in_inches, precip$`Average daily trips`))+geom_col()+ xlab("Precipitation(tenths of mm)") + ylab ("Average Daily Taxi Trips")+ ggtitle("Precipitation versus Average Daily Taxi Trips")



##Snow effect
snow <- train.data %>% group_by(DATE,SNOW) %>% count() 
snow <- snow %>% rename(nTaxi = n)

snow <- snow %>% mutate(snow_in_inches = case_when
(as.numeric(SNOW) == 00  ~ '1',
as.numeric(SNOW) > 00 & as.numeric(SNOW) < 2 ~ '2',
as.numeric(SNOW) >= 2 & as.numeric(SNOW) < 4 ~ '3',
as.numeric(SNOW) >= 4 & as.numeric(SNOW) < 6 ~ '4',
as.numeric(SNOW) >= 6 & as.numeric(SNOW) < 8 ~ '5',
as.numeric(SNOW) >= 8 & as.numeric(SNOW) < 10 ~ '6'))
snow <- snow %>% group_by(snow_in_inches) %>% summarise("Average daily trips" = mean(nTaxi))

precipitation$`Average daily trips` <- floor(precipitation$`Average daily trips`)

ggplot(snow, aes(snow$snow_in_inches, snow$`Average daily trips`))+geom_col()+ xlab("Snowfall in mm") + ylab ("Average Daily Taxi Trips")+ ggtitle("Snowfall versus Average Daily Taxi Trips")


###Whether factors affect tip/no tip
train_tip <- train.data %>% mutate(Threshold = 0.2*fare_amount)

train_tip <- train_tip %>% filter(payment_type == 1)

train_tip <- train_tip %>% mutate(tip = ifelse(tip_amount > Threshold, "tip", "less_tip"))



test_tip <- test_data %>% mutate(Threshold = 0.2*fare_amount)

test_tip <- test_tip %>% filter(payment_type == 1)

test_tip <- test_tip %>% mutate(tip = ifelse(tip_amount > Threshold, "tip", "less_tip"))


sum(train_tip$tip_amount==0)

new <- train.data %>% dplyr::select(trip_distance, RatecodeID, Pick_up_Hour, Day, Month, tip_amount, fare_amount, payment_type)
new <- new %>% mutate(Threshold = 0.2*fare_amount)
new <- new %>% mutate(distance = ifelse(trip_distance > 10, 1, 0))
new <- new %>% mutate(day = ifelse(Day %in% c("Friday", "Saturday", "Sunday"), 1, 0))
new <- new %>% mutate(hour = ifelse (as.numeric(Pick_up_Hour) > 17 | as.numeric(Pick_up_Hour) == 0 | as.numeric(Pick_up_Hour) < 6, 1,0))
new <- new %>% mutate(ratecode = ifelse(RatecodeID %in% c(1,2,3),1,0))
new <- new %>% filter(payment_type == 1)
new <- new %>% mutate(tip = ifelse(tip_amount > Threshold, "tip", "less_tip"))



new_test <- test_data %>% dplyr::select(trip_distance, RatecodeID, Pick_up_Hour, Day, tip_amount, fare_amount, payment_type)
new_test <- new_test %>% mutate(Threshold = 0.2*fare_amount)
new_test <- new_test %>% mutate(distance = ifelse(trip_distance > 10, 1, 0))
new_test <- new_test %>% mutate(day = ifelse(Day %in% c("Friday", "Saturday", "Sunday"), 1, 0))
new_test <- new_test %>% mutate(hour = ifelse (as.numeric(Pick_up_Hour) > 17 | as.numeric(Pick_up_Hour) == 0 | as.numeric(Pick_up_Hour) < 6, 1,0))
new_test <- new_test %>% mutate(ratecode = ifelse(RatecodeID %in% c(1,2,3),1,0))
new_test <- new_test %>% filter(payment_type == 1)
new_test <- new_test %>% mutate(tip = ifelse(tip_amount > Threshold, "tip", "less_tip"))


#load the library for applying Naive Bayes method
library(naivebayes)

#Apply Naive Bayes model to the train dataset
model <- naive_bayes(tip ~ distance + ratecode + hour + day, new)
#Assess the model on the test dataset
new_test$predictions <- predict(model, new_test, type = "class")

#Generate summary of the model
summary(model)

#Check for accuracy
mean(ifelse(new_test$tip == new_test$predictions, 1,0))

train.data <- train.data %>% rename(LocationID = PULocationID)

data <- merge(train.data, taxi_zone, by = "LocationID")

location_data <- data %>% group_by(Zone, Day, Pick_up_Hour) %>% count() %>% arrange(desc(n))
location_hour <- data %>% group_by(Zone, Pick_up_Hour) %>% count() %>% arrange(desc(n))



lm.data <- merge(train.data, taxi_zone, by = "LocationID")
lm.data <- lm.data %>% group_by(Zone, SNOW, SNWD, TMIN, TMAX) %>% count() %>% arrange(desc(n)) %>% rename(nTaxi = n)
  
  
lm.fit1 <- lm(nTaxi ~ TMAX+TMIN+SNOW+SNWD, data = train_byDate)
summary(lm.fit1)




taxiJan <- taxi_Jan %>% filter(SNOW > 0)
taxiFeb <- taxi_Feb %>% filter(SNOW > 0)
taxiMarch <- taxi_March %>% filter(SNOW > 0)
taxiApril <- taxi_April %>% filter(SNOW > 0)
taxiMay <- taxi_May %>% filter(SNOW > 0)
taxiJune <- taxi_June %>% filter(SNOW > 0)


snow_data <- rbind(taxiJan, taxiFeb, taxiMarch, taxiApril)
snow_data <- snow_data %>% rename(LocationID = PULocationID)
final <- merge(snow_data, taxi_zone, by = "LocationID")
snowData <- final %>% group_by(Zone) %>% count() %>% arrange(desc(n))
last <- inner_join(final, snowData, by = "Zone")


cor(last$SNOW, last$n)

lm1 <- lm(n ~ Zone+SNOW+PRCP+Borough, data = last)
summary(lm1)
