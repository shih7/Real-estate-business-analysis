
>library(dplyr)
#extract only assigned neighborhood using filter()
> my_neighborhood <- filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID==71)
> library(lubridate)
#clean data to only residential units using filter()
> my_neighborhood <- filter(my_neighborhood, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0)
#change data type to df
> as.data.frame(my_neighborhood)
#adding a new column of price per sqft by using mutate() and assign it to my_neighborhood
> my_neighborhood <- my_neighborhood %>% mutate(average_price = SALE_PRICE/GROSS_SQUARE_FEET)
#formatting sale date to date format (be aware to use upper Y since the year is 4 digits)
> my_date <- as.Date(my_neighborhood$SALE_DATE, format = '%Y/%m/%d')
#transform the date to year classification
> isoyear(my_date)
#adding a new column of only year using isoyear()
> my_neighborhood <- my_neighborhood %>% mutate(YEAR = isoyear(my_date))
#creating new groups grouping by year (no need this step)
> new_group <- group_by(my_neighborhood, YEAR)
#group the year and get average
> my_neighborhood %>% group_by(YEAR) %>% summarise(mean(average_price, na.rm = T))
#notice we get many errors in the result

#Question 3/4: 
> my_neighborhood <- filter(my_neighborhood, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)
> my_neighborhood %>% group_by(YEAR) %>% summarise(mean(average_price, na.rm = T))
> a <- my_neighborhood %>% group_by(YEAR) %>% summarise(mean(average_price, na.rm = T))
> as.list(a)
> View(a)

>#question 6 for #40 bulls head
  > bulls <-filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID==9)
  bulls <- filter(bulls, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)  
  > date2 <- as.Date(bulls$SALE_DATE, format = '%Y/%m/%d')
  > isoyear(date2)
  > bulls <- bulls %>% mutate(YEAR = isoyear(date2))
  > bulls <- bulls%>% mutate(average_price2 = SALE_PRICE/GROSS_SQUARE_FEET)
  > b <- bulls %>% group_by(YEAR) %>% summarise(mean(average_price2, na.rm = T))
  > as.list(b)
  View(b)
  #question 6 for #20 Bedford STUYVESANT
  Bedford <-filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID==20)
  Bedford <- filter(Bedford, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)
    + date3 <- as.Date(Bedford$SALE_DATE, format = '%Y/%m/%d')
    + isoyear(date3)
    + Bedford <- Bedford %>% mutate(YEAR = isoyear(date3))
    + Bedford <- Bedford%>% mutate(average_price3 = SALE_PRICE/GROSS_SQUARE_FEET)
    + c <- Bedford %>% group_by(YEAR) %>% summarise(mean(average_price3, na.rm = T))
    + as.list(c)
    + View(c)
  
#6> plot(a1,type="l", col="green", lwd=4, xlab="year", ylab="price per sqft", xlim=c(2002,2021), ylim=c(100,800), main='Custom Axes')
lines(b1, col="blue",lwd=5)
lines(c1, col="red", lwd = 5)

