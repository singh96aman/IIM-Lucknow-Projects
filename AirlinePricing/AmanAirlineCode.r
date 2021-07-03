# Analysis of Airline Ticket Pricing
# NAME: Aman Singh Thakur
# EMAIL: singh96aman@gmail.com
# COLLEGE / COMPANY: MIT Manipal

setwd("~/Data Analytics/workingdirectory/Airlines")

airlines.df <- read.csv(paste("SixAirlines.csv",sep = ""))
View(airlines.df)

#summary of airline features
summary(airlines.df)

#Corrgram to show relationship between different features of airline
library(corrgram)
corrgram(airlines.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="AirlinesPricing")

library(psych)
library(car)

#Converting all features to numeric from factor to operate on them 
Airline=as.numeric(airlines.df$AIRLINE)
Seats_eco=as.numeric(airlines.df$SEATS_ECONOMY)
Seats_pre=as.numeric(airlines.df$SEATS_PREMIUM)
Pitch_eco=as.numeric(airlines.df$PITCH_ECONOMY)
Pitch_pre=as.numeric(airlines.df$PITCH_PREMIUM)
Width_eco=as.numeric(airlines.df$WIDTH_ECONOMY)
Width_pre=as.numeric(airlines.df$WIDTH_PREMIUM)
Price_rel=as.numeric(airlines.df$PRICE_RELATIVE)
Price_pre=as.numeric(airlines.df$PRICE_PREMIUM)
Price_eco=as.numeric(airlines.df$PRICE_ECONOMY)

#Boxplot to give the relationship of each feature with feature 'AIRLINE'

boxplot(Airline~Seats_eco,data=airlines.df, main="Airline Vs Seats Economy", 
        xlab="Seats In Economy", ylab="Airline")

boxplot(Airline~Seats_pre,data=airlines.df, main="Airline Vs Seats Premium", 
        xlab="Seats In Premium", ylab="Airline")

boxplot(Airline~Pitch_eco,data=airlines.df, main="Airline Vs Pitch Economy", 
        xlab="Pitch in Economy", ylab="Airline")

boxplot(Airline~Pitch_pre,data=airlines.df, main="Airline Vs Pitch Premium", 
        xlab="Pitch in Premium", ylab="Airline")

boxplot(Airline~Price_rel,data=airlines.df, main="Airline Vs Price Relative",
        xlab="Price Relative", ylab="Airline")

boxplot(Airline~Price_pre,data=airlines.df, main="Airline Vs Price Premium",
        xlab="Price Premium", ylab="Airline")

boxplot(Airline~Price_eco,data=airlines.df, main="Airline Vs Price Economy",
        xlab="Price Economy", ylab="Airline")

boxplot(Airline~Width_pre,data=airlines.df, main="Airline Vs Width Premium",
        xlab="Width Premium", ylab="Airline")

boxplot(Airline~Width_eco,data=airlines.df, main="Airline Vs Width Economy",
        xlab="Width Economy", ylab="Airline")

library(UsingR)

#t-test score to find solutions to difference in economy things vs premium things hypothesis
t.test(Seats_pre - Seats_eco)

t.test(Pitch_pre - Pitch_eco)

t.test(Width_pre - Width_eco)

t.test(Price_pre - Price_eco)

t.test(Price_rel)

#Regression Model with coefficients to show relationship pf PRICE_PREMIUM with other relevant factors
fitPremium <- lm(Price_pre~Airline+Seats_pre+Pitch_pre+Width_pre+Price_rel)
coef(fitPremium)

#Regression Model with coefficients to show relationship pf PRICE_ECONOMY with other relevant factors
fitEconomy <- lm(Price_eco~Airline+Seats_eco+Pitch_eco+Width_eco+Price_rel)
coef(fitEconomy)


