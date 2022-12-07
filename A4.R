#Question 1 
library(dplyr)
library(odbc)
library(tidyverse)
library(lubridate)
library(DBI)
install.packages('forecast', dependencies = TRUE)
library(forecast)
NYC_HISTORICAL_1_ <- left_join(NYC_HISTORICAL_1_,BUILDING_CLASS, by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID"))
my_neighborhood <- filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID == 71)
my_neighborhood <- filter(my_neighborhood, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)
date <- as.Date(my_neighborhood$SALE_DATE, format = '%Y/%m/%d')
my_neighborhood <- my_neighborhood %>% mutate(YEAR = isoyear(date), CalenderQuarter=quarter(SALE_DATE, with_year = FALSE))
my_neighborhood2 <- filter(my_neighborhood, YEAR>=2009, SALE_ID!=681929, SALE_ID!=682211)
#adding a new row "t" representing cumulative quarter
my_neighborhood2 <- my_neighborhood2 %>% mutate(t = YEAR*4+CalenderQuarter-2009*4-0)
View(my_neighborhood2)
#creating a new data table
df.NY <- my_neighborhood2 %>% group_by(t)%>%summarise(SalePrice=sum(SALE_PRICE))
View(df.NY)
#running exponential smoothing
ts.NY <- ts(df.NY$SalePrice, start=c(2009,1), frequency = 4)
View(ts.NY)
ets(ts.NY) #M,N,M (error type, trend type, season type)(multiplicative error, none trend type, multiplicative season)
ts.model <- ets(ts.NY)
View(ts.model)
forecast(ts.model,8)
plot(df.NY,type="l", col="green", lwd=4, xlab="t", ylab="Sale Price", xlim=c(0,50), ylim=c(1000000,15248910*3), main='SalePrice/Quarter')

#question 2
df.NY <- cbind(df.NY, c("Q1","Q2","Q3","Q4"))
names(df.NY)[3] <- "Quarter"
reg.NY <- lm(data = df.NY, formula = SalePrice~t+Quarter)
reg.Ny2 <- lm(data = df.NY, formula = SalePrice~t)
summary(reg.NY)
summary(reg.Ny2)
x <- data.frame(t=c(45,46,47,48), SalePrice=c(0,0,0,0), Quarter=c("Q1","Q2","Q3","Q4"))
predict.lm(reg.NY,x, interval = "confidence")

#Question3
table.NY3 <- my_neighborhood2 %>% mutate(units=RESIDENTIAL_UNITS,area=GROSS_SQUARE_FEET,date=SALE_DATE,buildYear=YEAR_BUILT,price=SALE_PRICE)
reg.NY3 <- lm(data=table.NY3, formula = price~area)
reg.NY4 <- lm(data=table.NY3, formula = price~date)
reg.NY5 <- lm(data=table.NY3, formula = price~units)
reg.NY6 <- lm(data=table.NY3, formula = price~buildYear)

summary(reg.NY3)
summary(reg.NY4)
summary(reg.NY5)
summary(reg.NY6)

#residuals
df.DataforLM <- my_neighborhood2 %>% ungroup() %>% select(BUILDING_CLASS_FINAL_ROLL,RESIDENTIAL_UNITS,GROSS_SQUARE_FEET,SALE_DATE,YEAR_BUILT,SALE_PRICE,ADDRESS)
model <- lm(formula=SALE_PRICE~.,data=df.DataforLM)
summary(model)
df.DataforLM["residuals"] <-model$residuals
df.DataforLM["addresses"]<- model$address
View(df.DataforLM)
summary(model)
plot(model)
