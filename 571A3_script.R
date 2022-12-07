#Question 1 What is the total number units sold in your neighborhood since 2009?
library(dplyr)
library(lubridate)
NYC_HISTORICAL_1_ <- left_join(NYC_HISTORICAL_1_,BUILDING_CLASS, by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID"))
my_neighborhood <- filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID == 71)
my_neighborhood <- filter(my_neighborhood, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)
date <- as.Date(my_neighborhood$SALE_DATE, format = '%Y/%m/%d')
my_neighborhood <- my_neighborhood %>% mutate(YEAR = isoyear(date))
my_neighborhood2 <- filter(my_neighborhood, YEAR>=2009)
as.table(my_neighborhood2)
View(my_neighborhood2)
total_units <- sum(my_neighborhood2$RESIDENTIAL_UNITS)
#There are 1473 residential units sold since 2009.

my_commercial <- filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID == 71)
my_commercial <- filter(my_commercial, RESIDENTIAL_UNITS== 0, COMMERCIAL_UNITS != 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)
date <- as.Date(my_commercial$SALE_DATE, format = '%Y/%m/%d')
my_commercial <- my_commercial %>% mutate(YEAR = isoyear(date))
my_commercial <- filter(my_commercial, YEAR>=2009)
as.table(my_commercial)
View(my_commercial)
total_commercial <- sum(my_commercial$COMMERCIAL_UNITS)
total_commercial
#There are 20 commercial units sold since 2009.

#Question 2 What is the mean sale price and gross square footage for residential properties in your neighborhood since 2009?

mean_squarefeet <- mean(my_neighborhood2$GROSS_SQUARE_FEET)
#The mean of gross square feet is 2881.681

mean_saleprice <- sum(my_neighborhood2$SALE_PRICE)/total_units
mean_saleprice
#The mean of sale price is 484079.5

#Question 3 Get the five-number summary for both sale price and gross square footage for residential properties in your neighborhood since 2009
fivenum(my_neighborhood2$SALE_PRICE)
#10   670000   850000  1065000 28500000
fivenum(my_neighborhood2$GROSS_SQUARE_FEET)
# 299   1512   2049   2656 167032

#Question 4 What is the proportion of units sold of residential, commercial, mixed, to all properties in your neighborhood since 2009?

mix <- filter(NYC_HISTORICAL_1_, NEIGHBORHOOD_ID == 71)
mix <- filter(mix, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS != 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0)
date <- as.Date(mix$SALE_DATE, format = '%Y/%m/%d')
mix <- mix %>% mutate(YEAR = isoyear(date))
mix <- filter(mix, YEAR>=2009)
as.table(mix)
View(mix)
mix_sum <- sum(mix$RESIDENTIAL_UNITS)+sum(mix$COMMERCIAL_UNITS)
mix_sum
#There are 12 mixed units sold since 2009.
total <- 1473+20+12
res_pro <- 1473/total
com_pro <- 20/total
mix_pro <- 12/total
mix_pro
res_pro
com_pro
#The proportion of mixed units is 0.797%, the residential proportion is 97.87%, the commercial proportion is 1.329%.

#Question 5 What is the standard deviation of sale prices for residential properties in your neighborhood since 2009?
sd(my_neighborhood2$SALE_PRICE)
#sd is 1104242

#Question 6 What is the correlation between sale price and gross square feet for residential properties in your neighborhood since 2009?
cor(my_neighborhood2[c(9,11)])
# the correlation is 0.3948

#Question 7 Perform k-means clustering using at least 3 of the following KPI’s.
library(factoextra)

#Step1: filter all NYC historical in valid date of residential since 2009:
NYC_HISTORICAL_1_ <- left_join(NYC_HISTORICAL_1_, NEIGHBORHOOD, by=c("NEIGHBORHOOD_ID"="NEIGHBORHOOD_ID"))
all_neighborhood <- filter(NYC_HISTORICAL_1_, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0) 
date <- as.Date(all_neighborhood$SALE_DATE, format = '%Y/%m/%d')
all_neighborhood <- all_neighborhood %>% mutate(YEAR = isoyear(date))
all_neighborhood <- filter(all_neighborhood, YEAR>=2009)
as.data.frame(all_neighborhood)
all_neighborhood <- mutate(all_neighborhood, price_per_sqft = SALE_PRICE/GROSS_SQUARE_FEET)
#Step 2(getting Median Sale Price, number of sales, price/sqrt): get a list by neighborhood name:
clusterData <- all_neighborhood %>% group_by(NEIGHBORHOOD_NAME.x) %>% summarise(median(SALE_PRICE), mean(price_per_sqft), sum(RESIDENTIAL_UNITS))
View(clusterData)
#Step 3: scale the data and find distance
my_sacle <- scale(clusterData[2:4])
my_distance <- dist(my_sacle)
#Step 4: find how many clusters I need
fviz_nbclust(my_sacle, kmeans, method = "wss") + labs(subtitle = "Neighborhood cluster")
#From the plot we can see the best is 5-7. I am using 6 clusters.
#Step 5: kmeans and visualization 
my_kmeans <- kmeans(my_sacle, centers = 6, nstart = 300)
print(my_kmeans)
visualize <- my_kmeans$cluster
fviz_cluster(list(data=my_sacle, cluster = visualize))
#Step 6: 
cluster.table <- table(visualize, clusterData$NEIGHBORHOOD_NAME.x)
View(cluster.table)
#My neighborhood Douglaston is in cluster 6.

#Question 8: 
all_neighborhood <- filter(NYC_HISTORICAL_1_, RESIDENTIAL_UNITS!= 0, COMMERCIAL_UNITS == 0, SALE_PRICE != 0 & GROSS_SQUARE_FEET!=0) 
date <- as.Date(all_neighborhood$SALE_DATE, format = '%Y/%m/%d')
all_neighborhood <- all_neighborhood %>% mutate(YEAR = isoyear(date))
all_neighborhood <- filter(all_neighborhood, YEAR>=2009)
all_neighborhood <- mutate(all_neighborhood, price_per_sqft = SALE_PRICE/GROSS_SQUARE_FEET)

neighbor_32 <- filter(all_neighborhood, NEIGHBORHOOD_ID == 32)
neighbor_71 <- filter(all_neighborhood, NEIGHBORHOOD_ID == 71)
View(neighbor_32)

t.test(x = neighbor_32$price_per_sqft , y = neighbor_71$price_per_sqft, alternative = "t")
#we cannot conclude under 95% confidence that Brighton Beach (#32) average cost is higher than Douglaston (#71) average cost.
#Mean of Brighton Beach is 2389 and mean of Douglaston is 448.
t.test(x = neighbor_32$price_per_sqft , y = neighbor_71$price_per_sqft, alternative = "g", conf.level=0.9)

