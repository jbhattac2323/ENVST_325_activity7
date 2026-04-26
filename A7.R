#Tutorial 6----
# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

#Get intuitive spread of ch4 levels as shown in tutorial
ggplot(ghg, aes(x = ch4)) +
  geom_histogram(color = "black") +
  labs(
    title = "Distribution of CH4",
    x = "CH4",
    y = "Count"
  ) +
  theme_minimal()

#Transform to linear
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

unique(ghg$Region)

# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe
summary(mod.full)

#checking assumptions
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
# shapiro-wilks test
shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)
#Checking for multicollinearity shouldnt exceed 0.7-.8
# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 
# check full model
full.step$model

# plot AIC over time
plot(full.step )


# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

# look at prediction with 95% confidence interval of the mean

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")


#Tutorial 7----
#Time Series
ETdat <- read.csv("/cloud/project/activity07/ETdata.csv")
unique(ETdat$crop)
install.packages(c("lubridate", "ggplot2", "forecast", "dplyr"))
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

#almonds
# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

almondTrend <- almond_dec$trend
almondSeason <- almond_dec$seasonal

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))

almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")


#Homework----
#Q1 AND Q2
# transform CO2 (given in prompt)
ghg$trans.co2 <- 1/(ghg$co2 + 1000)

# log transform
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP + 1)
ghg$log.precip <- log(ghg$precipitation)

# binary variable
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)

# regression model (same structure as tutorial)
mod.co2 <- lm(trans.co2 ~ airTemp +
                log.age + mean.depth +
                log.DIP +
                log.precip + BorealV,
              data = ghg)

summary(mod.co2)

# check assumptions
res <- rstandard(mod.co2)
fit <- fitted.values(mod.co2)

# QQ plot
qqnorm(res, pch=19, col="grey50")
qqline(res)

# normality test
shapiro.test(res)

# residual plot
plot(fit, res, pch=19, col="grey50")
abline(h=0)

# check multicollinearity
reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,
                       ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

chart.Correlation(reg.data, histogram=TRUE, pch=19)

# stepwise model selection
ols_step_forward_aic(mod.co2)


#Q3
# Almonds
almond <- ETdat %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

almond_ts <- ts(almond$ET.in, start=c(2016,1), frequency=12)
almond_dec <- decompose(almond_ts)
plot(almond_dec)


# Pistachios
pistachio <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

pistachio_ts <- ts(pistachio$ET.in, start=c(2016,1), frequency=12)
pistachio_dec <- decompose(pistachio_ts)
plot(pistachio_dec)


# Fallow/Idle Cropland
fallow <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

fallow_ts <- ts(fallow$ET.in, start=c(2016,1), frequency=12)
fallow_dec <- decompose(fallow_ts)
plot(fallow_dec)


# Corn
corn <- ETdat %>%
  filter(crop == "Corn") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

corn_ts <- ts(corn$ET.in, start=c(2016,1), frequency=12)
corn_dec <- decompose(corn_ts)
plot(corn_dec)


# Table Grapes
grapes <- ETdat %>%
  filter(crop == "Grapes (Table/Raisin)") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

grapes_ts <- ts(grapes$ET.in, start=c(2016,1), frequency=12)
grapes_dec <- decompose(grapes_ts)
plot(grapes_dec)

#Q4

# PISTACHIOS

pistachio <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

pistachio_ts <- ts(pistachio$ET.in, start=c(2016,1), frequency=12)

p_y <- na.omit(pistachio_ts)

model_p <- arima(p_y, order=c(4,0,0))

newP <- forecast(model_p)

newPF <- data.frame(newP)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPF$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() +
  geom_line(data = pistachio, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachio$date[1]), newPF$dateF[24])+
  geom_line(data = newPF, aes(x = dateF, y = Point.Forecast), col="red") +
  geom_ribbon(data=newPF, 
              aes(x=dateF,ymin=Lo.95,ymax=Hi.95),
              fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(title="Pistachios Forecast", x="year", y="Evapotranspiration (in)")

# FALLOW

fallow <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

fallow_ts <- ts(fallow$ET.in, start=c(2016,1), frequency=12)

f_y <- na.omit(fallow_ts)

model_f <- arima(f_y, order=c(4,0,0))

newF <- forecast(model_f)

newFF <- data.frame(newF)
newFF$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow$date[1]), newFF$dateF[24])+
  geom_line(data = newFF, aes(x = dateF, y = Point.Forecast), col="red") +
  geom_ribbon(data=newFF, 
              aes(x=dateF,ymin=Lo.95,ymax=Hi.95),
              fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(title="Fallow Forecast", x="year", y="Evapotranspiration (in)")



