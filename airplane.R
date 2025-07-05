#02-07-25
library(tidyverse)
airplane <- read.csv("airline_2m.csv")
head(airplane)
names(airplane)

sub_airplane <- airplane %>%
  select(DayOfWeek,DayofMonth,Reporting_Airline,Origin,Dest,ArrDelay,DepDelay,ArrDelayMinutes,DepDelayMinutes,CarrierDelay,WeatherDelay,SecurityDelay,LateAircraftDelay) %>% filter(Origin =="LAX", Dest=="JFK")
  

head(sub_airplane)
summary(sub_airplane)
clean_sub_airplane <- drop_na(sub_airplane)
summary(clean_sub_airplane)
head(clean_sub_airplane)
summarise(clean_sub_airplane, mean(clean_sub_airplane$ArrDelay))

clean_sub_airplane %>% group_by(Reporting_Airline) %>% summarise(avg_delay =mean(ArrDelay),
                                                                 avg_dayofweek = mean(DayOfWeek))
clean_sub_airplane %>% group_by(DayOfWeek) %>% summarise(mean(ArrDelay))
clean_sub_airplane %>% group_by(DayofMonth) %>% summarise(mean(ArrDelay))

summary(clean_sub_airplane)

#scaling_coursera_mod2_normalization

simple_scale <- clean_sub_airplane$ArrDelay/ max(clean_sub_airplane$ArrDelay)
summary(simple_scale)

min_max_scale <- (clean_sub_airplane$ArrDelay - min(clean_sub_airplane$ArrDelay))/ (max(clean_sub_airplane$ArrDelay)-min(clean_sub_airplane$ArrDelay))
summary(min_max_scale)

Z_score_scale <- (clean_sub_airplane$ArrDelay - mean(clean_sub_airplane$ArrDelay))/sd(clean_sub_airplane$ArrDelay)
summary(Z_score_scale)

#plotting
ggplot(data = clean_sub_airplane, mapping = aes(x= ArrDelay))+
  geom_histogram(bins = 150,color = "white",fill = "red")+
  coord_cartesian(xlim = c(-73,682))


#binning in tidy
binning <- clean_sub_airplane %>% mutate(quantile_rank = ntile(clean_sub_airplane$ArrDelay,4))

#03-07-25
summarise(clean_sub_airplane,avgdelay=mean(clean_sub_airplane$ArrDelay),deviation=sd(clean_sub_airplane$ArrDelay))
head(clean_sub_airplane)
clean_sub_airplane %>% group_by(clean_sub_airplane$Reporting_Airline) %>% summarise(avgdelay=mean(ArrDelay),deviation=sd(ArrDelay))
count(clean_sub_airplane,clean_sub_airplane$Reporting_Airline)

clean_sub_airplane %>% group_by(clean_sub_airplane$Reporting_Airline,clean_sub_airplane$DayOfWeek) %>% summarise(mean_delay=mean(ArrDelay),sd(ArrDelay)) %>% 
  arrange(desc(mean_delay))
#heatplot ref the ipynb for that

#ANOVA
aa_as_subset <- clean_sub_airplane %>% 
  select(ArrDelay, Reporting_Airline) %>% 
  filter(Reporting_Airline =="AA"| Reporting_Airline == "AS")
ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

#correl
ggplot(clean_sub_airplane, aes(DepDelay, ArrDelay))+
  geom_point()+geom_smooth(method = "lm")
clean_sub_airplane %>% select(DepDelay, ArrDelay) %>% 
  cor(method = "pearson")
clean_sub_airplane %>% 
cor.test(~DepDelay+ ArrDelay,data = .)

library(Hmisc)
numeric_airline <- clean_sub_airplane %>% 
  select(ArrDelayMinutes,DepDelayMinutes,CarrierDelay,WeatherDelay,SecurityDelay,LateAircraftDelay)

airline_cor <- rcorr(as.matrix(numeric_airline),type = "pearson")


#04-07-2025
aa_delay <- clean_sub_airplane %>% 
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")
linear_model <- lm(aa_delay$ArrDelayMinutes ~ aa_delay$DepDelayMinutes)
summary(linear_model)

new_depdelay <- data.frame(DepDels = c(12,19,24))
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
pred
plot(linear_model)

#polynomial regressions

time <- 6:19
temp <- c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)

plot(time,temp)
polyfit2 <- lm(temp ~ poly(time,2,raw = TRUE))
summary(polyfit2)
ggplot(data = NULL, aes(time, temp))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2))
mse <- mean(linear_model$residuals^2)
rmse <-  sqrt(mse)
rmse


#05-07-2025
#model analysis

