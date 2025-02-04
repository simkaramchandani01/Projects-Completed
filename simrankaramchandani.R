#load packages
install.packages('dplyr')
install.packages("stringr")
install.packages("ggplot2")
install.packages("car")
install.packages("lmtest")
install.packages("lubridate")
install.packages("Metrics")
install.packages("Lahman")
require(dplyr)
require(stringr)
require(ggplot2)
require(car)
require(lmtest)
require(lubridate)
require(Metrics)
require(Lahman)

#Question 1-Reading in files
data_1 <- read.csv("C:/Users/svkar/Downloads/2016_brooklyn.csv", skip=4)
data_2 <- read.csv("C:/Users/svkar/Downloads/2017_brooklyn.csv", skip=4)
data_3 <- read.csv("C:/Users/svkar/Downloads/2018_brooklyn.csv", skip=4)
data_4 <- read.csv("C:/Users/svkar/Downloads/2019_brooklyn.csv", skip=4)
data_5 <- read.csv("C:/Users/svkar/Downloads/2020_brooklyn.csv", skip=6)
summary(data_1)

#Rename columns for standardization
new_col_name <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#Assignment of new column names in all datasets
data_1<-setNames(data_1,new_col_name)
data_2<-setNames(data_2,new_col_name)
data_3<-setNames(data_3,new_col_name)
data_4<-setNames(data_4,new_col_name)
data_5<-setNames(data_5,new_col_name)

#Question 1.2, clean data

clean_all <- function(data)
{
  names(data) <- make.names(gsub("[^[:alnum:]]","_",names(data)))
  data <- data %>% mutate_if(is.character,trimws)
  return(data)
}
#clean data 1
clean_data_1 <- clean_all(data_1) #character
summary(clean_data_1)
clean_data_1$resunits <- as.numeric(clean_data_1$resunits)
clean_data_1$comunits <- as.numeric(clean_data_1$comunits)
clean_data_1$totunits <- as.numeric(clean_data_1$totunits)
clean_data_1$price <- as.numeric(gsub(',','',clean_data_1$price))
clean_data_1$grosssqft <- gsub(',','',clean_data_1$grosssqft)
clean_data_1$landsqft <- gsub(',','',clean_data_1$landsqft)
clean_data_1$grosssqft <- as.numeric(clean_data_1$grosssqft)
clean_data_1$landsqft <- as.numeric(clean_data_1$landsqft)
#clean data 2
clean_data_2 <- clean_all(data_2)
summary(clean_data_2)
clean_data_2$resunits <- as.numeric(clean_data_1$resunits)
clean_data_2$comunits <- as.numeric(clean_data_2$comunits)
clean_data_2$totunits <- as.numeric(clean_data_2$totunits)
clean_data_2$grosssqft <- gsub(',','',clean_data_2$grosssqft)
clean_data_2$landsqft <- gsub(',','',clean_data_2$landsqft)
clean_data_2$grosssqft <- as.numeric(clean_data_2$grosssqft)
clean_data_2$landsqft <- as.numeric(clean_data_2$landsqft)
clean_data_2$price <- as.numeric(gsub(',','',clean_data_2$price))
#clean data 3
clean_data_3 <- clean_all(data_3) #character
summary(clean_data_3)
clean_data_3$resunits <- as.numeric(clean_data_3$resunits)
#clean_data_3$comunits <- as.numeric(clean_data_3$comunits)
clean_data_3$totunits <- as.numeric(clean_data_3$totunits)
clean_data_3$grosssqft <- gsub(',','',clean_data_3$grosssqft)
clean_data_3$landsqft <- gsub(',','',clean_data_3$landsqft)
clean_data_3$grosssqft <- as.numeric(clean_data_3$grosssqft)
clean_data_3$landsqft <- as.numeric(clean_data_3$landsqft)
clean_data_3$price <- as.numeric(gsub('\\$|,','',clean_data_3$price))
#clean data 4
clean_data_4 <- clean_all(data_4)
summary(clean_data_4)
clean_data_4$resunits <- as.numeric(clean_data_4$resunits)
clean_data_4$comunits <- as.numeric(clean_data_4$comunits)
clean_data_4$totunits <- as.numeric(clean_data_4$totunits)
clean_data_4$price <- as.numeric(gsub(',','',clean_data_4$price))
clean_data_4$grosssqft <- gsub(',','',clean_data_4$grosssqft)
clean_data_4$landsqft <- gsub(',','',clean_data_4$landsqft)
clean_data_4$grosssqft <- as.numeric(clean_data_4$grosssqft)
clean_data_4$landsqft <- as.numeric(clean_data_4$landsqft)
clean_data_4_final <- clean_data_4 %>%
  filter(
    is.na(borough)==FALSE
  )
summary(clean_data_4_final)
#clean data 5
clean_data_5 <- clean_all(data_5)
summary(clean_data_5)
clean_data_5$resunits <- as.numeric(clean_data_5$resunits)
clean_data_5$comunits <- as.numeric(clean_data_5$comunits)
clean_data_5$totunits <- as.numeric(clean_data_5$totunits)
clean_data_5$price <- as.numeric(gsub(',','',clean_data_5$price))
clean_data_5$grosssqft <- gsub(',','',clean_data_5$grosssqft)
clean_data_5$landsqft <- gsub(',','',clean_data_5$landsqft)
clean_data_5$grosssqft <- as.numeric(clean_data_5$grosssqft)
clean_data_5$landsqft <- as.numeric(clean_data_5$landsqft)

#Cleaned bound datasets 
total_clean <- bind_rows(clean_data_1, clean_data_2, clean_data_3, clean_data_4_final, clean_data_5)
nrow(total_clean) #117,151 rows
summary(total_clean)

#Question 1.3, filter data

#filter function 
filtered_data <- total_clean %>%
  filter(
      (str_detect(substr(bldclasssale,1,1),"A")==TRUE | str_detect(substr(bldclasssale,1,1),"R")==TRUE) &
      totunits==1 &
      resunits==1 &
      grosssqft>0 &
      is.na(price)==FALSE
  )
nrow(filtered_data)#19,640

summary(filtered_data)

#Question 2

#removal of prices with $0, distribution of price
filtered_data <- filtered_data %>% arrange(price)
mean(filtered_data$price)
sd(filtered_data$price)
max(filtered_data$price)
min(filtered_data$price)

filtered_data2 <- filtered_data %>%
  filter(price != 0)
filtered_data2 <- filtered_data2 %>% arrange(price)
min(filtered_data2$price)
hist(filtered_data2$price)
nrow(filtered_data2) #13,985

#Question 2.2/2.3

#threshold <- 1.5 
#price_iqr <- IQR(filtered_data2$price)
#upper_threshold <- quantile(filtered_data2$price, 0.75) + threshold * price_iqr
filtered_data3 <- filtered_data2 %>%
  subset(price >500) #setting for values above 500 since values below imply un-competitive sales
nrow(filtered_data3) #13,636
#Logarithmic transformation of housing sale prices 
filtered_data3$price_log <- log(filtered_data3$price)
hist(filtered_data3$price_log,col="lightblue",breaks=40,main="Distribution of Log-Transformed Sale Prices",xlab="Sale Price (Log)")

#Reformatting of date column 
filtered_data3$date <- as.Date(filtered_data3$date, format='%m/%d/%Y')
summary(filtered_data3)

#Segmentation of dates into seasons for grouping
filtered_data3$season <- ifelse(month(filtered_data3$date) %in% c(12, 1, 2), "Winter",
                                ifelse(month(filtered_data3$date) %in% c(3, 4, 5), "Spring",
                                       ifelse(month(filtered_data3$date) %in% c(6, 7, 8), "Summer",
                                              ifelse(month(filtered_data3$date) %in% c(9, 10, 11), "Fall", NA))))

#Segmentation of dates into quarters for grouping
filtered_data3$quarters <- ifelse(month(filtered_data3$date) %in% c(1, 2, 3), "Q1",
                                ifelse(month(filtered_data3$date) %in% c(4,5,6), "Q2",
                                       ifelse(month(filtered_data3$date) %in% c(7, 8, 9), "Q3",
                                              ifelse(month(filtered_data3$date) %in% c(10, 11, 12), "Q4", NA))))

#Extraction and Segmentation of year from date column for grouping
filtered_data3$year_of_sale <- year(filtered_data3$date)

filtered_data3$year_of_sale <- ifelse(filtered_data3$year_of_sale == "17", "2017",
                           ifelse(filtered_data3$year_of_sale == "16", "2016",
                                  ifelse(filtered_data3$year_of_sale=="18","2018",
                                  ifelse(filtered_data3$year_of_sale == "19", "2019", filtered_data3$year_of_sale))))

class(filtered_data3$year_of_sale)
summary(filtered_data3$year_of_sale)
filtered_data3$year_of_sale <- as.numeric(filtered_data3$year_of_sale)
filtered_data3$yrdifference <- abs(filtered_data3$year_of_sale-filtered_data3$yrbuilt)


nrow(filtered_data3) #13,636, final dataset

#Grouping of variables for model 

#Neighborhood
neighborhood_summary <- filtered_data3 %>% 
  group_by(neighborhood) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(neighborhood2=ntile(average_price, 4))

neighborhood_summary$neighborhood2 <- as.factor(neighborhood_summary$neighborhood2)
class(neighborhood_summary$neighborhood2)

#Block
block_summary <- filtered_data3 %>%
  group_by(block) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(block2=ntile(average_price,22))

block_summary$block2 <- as.factor(block_summary$block2)
class(block_summary$block2)

#Grosssqft
filtered_data3$grosssqft_log <- log(filtered_data3$grosssqft)

grosssqft_summary <- filtered_data3 %>%
  group_by(grosssqft_log) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(grosssqft2=ntile(average_price,4))

#grosssqft_summary$grosssqft2 <- as.factor(grosssqft_summary$grosssqft2)
#class(grosssqft_summary$grosssqft2)

#Year Difference
yrdifference_summary <- filtered_data3 %>%
  group_by(yrdifference) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(yrdifference2=ntile(average_price,4))

yrdifference_summary$yrdifference2 <- as.factor(yrdifference_summary$yrdifference2)
class(yrdifference_summary$yrdifference2)

#Season
season_summary <- filtered_data3 %>%
  group_by(season) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(season2=ntile(average_price,2))

season_summary$season2 <- as.factor(season_summary$season2)
class(season_summary$season2)

#Bldclass
bldclass_summary <- filtered_data3 %>%
  group_by(bldclasscat) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(bldclasscat2=ntile(average_price,3))

bldclass_summary$bldclasscat2 <- as.factor(bldclass_summary$bldclasscat2)
class(bldclass_summary$bldclasscat2)

#Quarters
quarters_summary <- filtered_data3 %>%
  group_by(quarters) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(quarters2=ntile(average_price,4))

quarters_summary$quarters2 <- as.factor(quarters_summary$quarters2)
class(quarters_summary$quarters2)

#Taxclass
taxclass_summary <- filtered_data3 %>%
  group_by(taxclasssale) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(taxclasssale2=ntile(average_price,2))

taxclass_summary$taxclasssale2 <- as.factor(taxclass_summary$taxclasssale2)
class(taxclass_summary$taxclasssale2)

#Taxclasscurr
taxclasscurr_summary <- filtered_data3 %>%
  group_by(taxclasscurr) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(taxclasscurr2=ntile(average_price,2))

taxclasscurr_summary$taxclasscurr2 <- as.factor(taxclasscurr_summary$taxclasscurr2)
class(taxclasscurr_summary$taxclasscurr2)

#Year
year_summary <- filtered_data3 %>%
  group_by(year_of_sale) %>%
  #summarize(house_count=n(),average_price=mean(price),median_price=median(price)) %>%
  summarize(house_count=n(),average_price=mean(price_log),median_price=median(price_log)) %>%
  mutate(year2=ntile(average_price,4))

year_summary$year2 <- as.factor(year_summary$year2)
class(year_summary$year2)

#Merging groups to final dataset
filtered_data3 <- merge(filtered_data3, neighborhood_summary[, c("neighborhood", "neighborhood2")], by = "neighborhood", all.x = TRUE)
filtered_data3 <- merge(filtered_data3, block_summary[, c("block", "block2")], by = "block", all.x = TRUE)
filtered_data3 <- merge(filtered_data3,grosssqft_summary[,c("grosssqft_log","grosssqft2")],by="grosssqft_log",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,yrdifference_summary[,c("yrdifference","yrdifference2")],by="yrdifference",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,season_summary[,c("season","season2")],by="season",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,bldclass_summary[,c("bldclasscat","bldclasscat2")],by="bldclasscat",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,taxclass_summary[,c("taxclasssale","taxclasssale2")],by="taxclasssale",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,taxclasscurr_summary[,c("taxclasscurr","taxclasscurr2")],by="taxclasscurr",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,quarters_summary[,c("quarters","quarters2")],by="quarters",all.x=TRUE)
filtered_data3 <- merge(filtered_data3,year_summary[,c("year_of_sale","year2")],by="year_of_sale",all.x=TRUE)

#Linear Model 

lm_neighborhood <- lm(price_log ~ grosssqft2+neighborhood2+block2+(quarters2*year2), data=filtered_data3)
summary(lm_neighborhood)
unique(filtered_data3$neighborhood2)
unique(filtered_data3$block2)
unique(filtered_data3$grosssqft2)
unique(filtered_data3$yrdifference2)
unique(filtered_data3$season2)
unique(filtered_data3$quarters2)
#Anova output for significance
anova(lm_neighborhood)

#OLS assumptions
qqnorm(residuals(lm_neighborhood)) #normality
qqline(residuals(lm_neighborhood))
bptest(lm_neighborhood) #heteroskedasticity
durbinWatsonTest(lm_neighborhood) #autocorrelation residuals

#Calculation of RMSE

fitted_values <- exp(fitted(lm_neighborhood))
filtered_data3$sale_price <- exp(filtered_data3$price_log) #Re-transformation back from log-scale to sale prices 
mean_fitted_values <- mean(fitted_values)
print(mean_fitted_values)
residuals_test <- exp(residuals(lm_neighborhood))
print(residuals_test)
squared_residuals <- (residuals_test)^2
mean_squared_residuals <- mean(squared_residuals)
print(mean_squared_residuals)
data_final <- cbind(filtered_data3,fitted_values)
head(data_final)
nrow(filtered_data3)
length(fitted_values)
data_final$residual <- data_final$fitted_values-data_final$sale_price
data_final$residual_abs <- abs(data_final$fitted_values-data_final$sale_price)
data_final$residual_abs_squared <- sqrt((data_final$residual_abs)^2)
mean(data_final$residual_abs_squared)

#Plot between fitted and residual values of model
plot(fitted_values, residuals_test, main = "Fitted Values vs. Residuals", 
     xlab = "Fitted Values", ylab = "Residuals")

#line of reference for plot 
abline(h = 0, col = "red", lty = 2)

#RDS file
saveRDS(list(model=lm_neighborhood, data=filtered_data3), file='simrankaramchandani.RDS') 
readRDS('simrankaramchandani.RDS')
getwd() 

#Part 2
new_predictions <- data.frame(quarters = c("Q3", "Q4"))

filtered_data_2020 <- filtered_data3 %>%
  subset(year_of_sale=="2020")

nrow(filtered_data_2020) #1,607 rows

#Creation of predicted prices of all data within year 2020
predictions_total <- predict(lm_neighborhood, newdata=filtered_data_2020)
new_prices <- exp(predictions_total)

#Creation of column of predicted prices with new prices
filtered_data_2020$predicted_prices <- new_prices

#Q3 Predicted Prices
q3_data <- filtered_data_2020 %>%
  subset(quarters=="Q3")

min(q3_data$predicted_prices) #minimum predicted price
max(q3_data$predicted_prices) #maximum predicted price
average_q3 <- mean(q3_data$predicted_prices) #average predicted price
print(average_q3)

#Q4 Predicted Prices
q4_data <- filtered_data_2020 %>%
  subset(quarters=="Q4")

min(q4_data$predicted_prices) #minimum predicted price
max(q4_data$predicted_prices) #maximum predicted price

average_q4 <- mean(q4_data$predicted_prices) #average predicted price
print(average_q4)

print(average_q4-average_q3) #difference of predicted prices between Q3 and Q4 of year 2020

percent_change <- ((average_q4-average_q3)/average_q3)*100
print(percent_change) #final percent change between Q3 and Q4 of year 2020

t_test <- t.test(q3_data$predicted_prices,q4_data$predicted_prices)
print(t_test) #t-test of significance between both quarters

par(mfrow = c(1, 2)) #allows for window of side by side box plots of Q3 and Q4

boxplot_q3 <- boxplot(q3_data$predicted_prices, main = "Boxplot for Q3",
        ylab = "Predicted Prices", col = "lightblue")

outliers_Q3 <- boxplot_q3$out
print(outliers_Q3)
length(outliers_Q3)

boxplot_q4 <- boxplot(q4_data$predicted_prices, main = "Boxplot for Q4",
        ylab = "Predicted Prices", col = "lightgreen")

outliers_Q4 <- boxplot_q4$out
print(outliers_Q4)
length(outliers_Q4)

#scatterplot of all quarters

ggplot(filtered_data_2020, aes(x = quarters, y = predicted_prices, color = quarters)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  
  labs(title = "Scatter Plot of Predicted Prices per Quarter in 2020",
       x = "Quarters",
       y = "Predicted Prices",
       caption = "Trend lines represent linear fits") +
  scale_color_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(legend.position = "bottom")  

