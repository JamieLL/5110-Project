## Trial on hourly time analysis——None obvious pattern occurs.
"Initial plan was guess that time closer to midnight, people can be more impetuous than daytime. 
Leading to lower review score at midnight than average. While it is not fully proved by the plot."
library(tidyverse)
setwd("/Users/mac/Desktop/DS 5110/Final project/datasets/brazilian-ecommerce")

geo <- read_csv("geolocation_olist_public_dataset.csv")
# "zip_code_prefix","city","state","lat","lng"
public_dataset <- read_csv("olist_public_dataset_v2.csv")
# order- product- customer- review
public_dataset_cust <- read_csv("olist_public_dataset_v2_customers.csv")
# customerID
public_dataset_classified <- read_csv("olist_classified_public_dataset.csv")
# order- product from raw data and votes information
payment <- read_csv("payments_olist_public_dataset.csv")
# order- payment- value
cate <- read_csv("product_category_name_translation.csv")
# category name
meas <- read_csv("product_measures_olist_public_dataset_.csv")
# product_3D
seller <- read_csv("sellers_olist_public_dataset_.csv")
# order- product- seller- location
state <- read_csv("brazil state info.csv")
unique(geo$state)

by_state_sales_distribution <-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(customer_state, product_category_name) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  select(1,4,3) %>%
  left_join(state, by = c("customer_state" = "Abbreviation")) %>%
  select(1,4,6,2,3) %>%
  group_by(customer_state) %>%
  mutate(total_value = sum(sales_value)) %>%
  arrange(desc(total_value),desc(sales_value)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3)
write.csv(by_state_sales_distribution, 
          "geo_sales_distr.csv",
          row.names = F)
library(lubridate)
by_month_sales_pattern <-
  filter(public_dataset, order_status == "delivered") %>%
  mutate(month = month(order_purchase_timestamp)) %>%
  group_by(month, product_category_name) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  select(1,4,3) %>%
  group_by(month) %>%
  mutate(total_value = sum(sales_value)) %>%
  arrange(desc(total_value),desc(sales_value)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3)

write.csv(by_month_sales_pattern, 
          "season_sales_pattern.csv",
          row.names = F)

score3<-filter(public_dataset_classified,public_dataset_classified$review_score == "3")

by_cate_review <-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(product_category_name) %>%
  summarise(medre = median(review_score, na.rm = TRUE),
            meanre = mean(review_score, na.rm = TRUE),
            count = n()) %>%
  left_join(cate, by = "product_category_name") %>%
  select(5,2,3,4) %>%
  filter(count >= 100)

write.csv(by_cate_review, 
          "by_cate_review.csv",
          row.names = F)

## Date
library(dplyr)
library(ggplot2)
Time1 <- data.frame(time=public_dataset$order_purchase_timestamp)
Time2<-format(Time1, format="%h-%m")
install.packages("zoo")
library(zoo)
library(lubridate)
Time3<-transform(Time1,time.str = format(time,'%H:%M:%S'),
                 hour = hour(time),
                 minute = minute(time),
                 second= second(time))
Time4<-Time3[1:2]
head(public_dataset)
colnames(Time4)[1] <- "order_purchase_timestamp"
Review_time_distribution<-
  filter(public_dataset, order_status == "delivered") %>%
  inner_join(Time4,by = "order_purchase_timestamp") %>%
  left_join(cate, by = "product_category_name") %>%
  dplyr::select(1,3,4,7,26,8:10,17,18,21,27) 

Review_time_distribution$time.str <- as.POSIXct(strptime(Review_time_distribution$time.str,"%H:%M:%S"))
class(Review_time_distribution$time.str)
typeof(Review_time_distribution$time.str)
Review_time_distribution <- Review_time_distribution[!duplicated(Review_time_distribution$time.str),]
Review_time_distribution %>%   
  group_by(Review_time_distribution$product_category_name_english)%>%
  ggplot(aes(x=time.str,y=review_score))+
  geom_point(alpha=0.3)+geom_smooth()+facet_wrap(~Review_time_distribution$product_category_name_english)
scale_x_datetime(date_breaks = "30 mins",
                 date_labels = '%H:%M:%S',
                 date_minor_breaks = "1 min") 
library(scales)  
Review_time_distribution %>%  
  group_by(Review_time_distribution$product_category_name_english)%>%
  ggplot(aes(x=time.str,y=review_score))+
  geom_line(alpha=0.3)+facet_wrap(~Review_time_distribution$product_category_name_english)+
  scale_x_datetime(breaks = date_breaks("10 min"),labels=date_format("%H:%M"))+
  xlab("Time 00.00 ~ 24:00 ")

cor(Review_time_distribution[c(10:12)])