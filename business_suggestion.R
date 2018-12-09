library(tidyverse)
library(ggplot2)
library (MASS)
library (dplyr)
#install.packages('dplyr')
library(readr)

rm(list = ls())
setwd("D:/class records/R 5110/project/brazilian-ecommerce---")

geo <- read_csv("geolocation_olist_public_dataset.csv")
# "zip_code_prefix","city","state","lat","lng"
public_dataset <- read_csv("olist_public_dataset_v2.csv")
View(public_dataset)
# order- product- customer- review
public_dataset_cust <- read_csv("olist_public_dataset_v2_customers.csv")
# customerID
public_dataset_classified <- read_csv("olist_classified_public_dataset.csv")
# order- product from raw data and votes information
payment <- read_csv("payments_olist_public_dataset.csv")
# order- payment- value
cate <- read_csv("product_category_name_translation.csv")
# category name
View(cate)
meas <- read_csv("product_measures_olist_public_dataset_.csv")
# product_3D
seller <- read_csv("sellers_olist_public_dataset_.csv")
View(seller)
# order- product- seller- location
state <- read_csv("brazil state info.csv")
View(state)
unique(geo$state)


# proportion of freight cost
freight_cost<- public_dataset %>%
   filter(order_status=='delivered') %>%
  summarise(freight_cost=sum(order_freight_value), total_product_value=sum(order_products_value)
            ,avg_freight=mean(order_freight_value,na.rm = TRUE) ) %>%
  mutate(prop_freight=freight_cost/total_product_value)



freight_cost_samest<-public_dataset %>%
  left_join(seller) %>%
  filter(customer_state==seller_state)%>%
  filter(order_status=='delivered') %>%
  summarise(freight_cost=sum(order_freight_value), total_product_value=sum(order_products_value)
            ,avg_freight=mean(order_freight_value,na.rm = TRUE) ) %>%
  mutate(prop_freight=freight_cost/total_product_value)

freight_cost_difst<-public_dataset %>%
  left_join(seller) %>%
  filter(!customer_state==seller_state)%>%
  filter(order_status=='delivered') %>%
  summarise(freight_cost=sum(order_freight_value), total_product_value=sum(order_products_value)
            ,avg_freight=mean(order_freight_value,na.rm = TRUE) ) %>%
  mutate(prop_freight=freight_cost/total_product_value)

freight_cost1<-union_all(freight_cost,freight_cost_samest)

freight_all<-union_all(freight_cost1,freight_cost_difst)
freight_all$source<-as.factor(c('all_state','same_state','dif_state'))

ggplot(freight_all)+
  geom_col(aes(x=source,y=avg_freight,fill=source))+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size = 18),
        plot.title = element_text(size=18))+
  ylab('Average freight per order: \n dollars')+
  ggtitle('Comparing the freight per order of "whether sellers \n and customers are in the same state" ')
  
# proportion of freight
ggplot(freight_all)+
  geom_col(aes(x=source,y=prop_freight,fill=source))+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size = 18),
        plot.title = element_text(size=18))+
  ylab('Average freight proportion per order')+
  ggtitle('Comparing the freight proportion of "whether sellers \n and customers are in the same state" ')

# all freight
ggplot(freight_all)+
  geom_col(aes(x=source,y=freight_cost,fill=source))+
  theme(axis.title = element_text(size = 18),axis.text.x = element_text(size = 18),
        plot.title = element_text(size=18))+
  ylab('Freight in all')+
  ggtitle('Comparing the total freight of "whether sellers \n and customers are in the same state" ')

write.csv(freight_all,'freight_all.csv',row.names = T)


# time cost of freight

time_cost<-public_dataset %>%
  filter(order_status=='delivered') %>%
  mutate(time_freight=difftime(order_delivered_customer_date,order_aproved_at,units = c('days'))) %>%
  select(time_freight) %>%
  summarise(avg_time_freight=mean(time_freight, na.rm = TRUE))

time_cost_same_st<-public_dataset %>%
  left_join(seller) %>%
  filter(customer_state==seller_state)%>%
  mutate(time_freight=difftime(order_delivered_customer_date,order_aproved_at,units = c('days'))) %>%
  select(time_freight) %>%
  summarise(avg_time_freight=mean(time_freight, na.rm = TRUE))


time_cost_dif_st<-public_dataset %>%
  left_join(seller) %>%
  filter(!customer_state==seller_state)%>%
  mutate(time_freight=difftime(order_delivered_customer_date,order_aproved_at,units = c('days'))) %>%
  select(time_freight) %>%
  summarise(avg_time_freight=mean(time_freight, na.rm = TRUE))

#write.csv(time_cost_dif_st,'time_cost_dif_st.csv',row.names = T)

time_cost_comp<-read.csv('time_cost_dif_st.csv')

time_cost_comp%>%
ggplot()+
  geom_col(aes(x=type,y=time_cost..days.,fill=type))+
  theme(axis.title = element_text(size = 18),axis.text.x = element_text(size = 18),
        plot.title = element_text(size=18))+
  ylab('Average Shipping Days Per Order:\n Days')+
  ggtitle('Comparing the shipping days of "whether sellers \n and customers are in the same state" ')


# state_cate_sales
by_state_sales_distribution <-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(customer_state, product_category_name) %>%
  summarise(sales_value_cate = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  select(1,4,3) %>%
  left_join(state, by = c("customer_state" = "Abbreviation")) %>%
  select(1,4,6,2,3) %>%
  group_by(customer_state) %>%
  mutate(total_value = sum(sales_value_cate)) %>%
  arrange(desc(total_value),desc(sales_value_cate)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3)%>%
  mutate(cate_of_total=sales_value_cate/total_value)

write.csv(by_state_sales_distribution, 
          "geo_sales_distr3.csv",
          row.names = T)
View(by_state_sales_distribution)


# state_cate_sales
by_custstate_cate_distribution <-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(customer_state, product_category_name) %>%
  summarise(sales_value_cate = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  select(1,4,3) %>%
  left_join(state, by = c("customer_state" = "Abbreviation")) %>%
  select(1,4,6,2,3) %>%
  group_by(customer_state) %>%
  mutate(total_value = sum(sales_value_cate)) %>%
  arrange(desc(total_value),desc(sales_value_cate)) %>%
  mutate(rank = row_number()) %>%
  mutate(cate_of_total=sales_value_cate/total_value)

write.csv(by_custstate_cate_distribution, 
          "by_custstate_cate_distribution.csv",
          row.names = T)

View(by_state_sales_distribution)

# sell_cust_state_cate_sales
by_custstate_cate_distribution <-
  filter(public_dataset, order_status == "delivered") %>%
  left_join(seller) %>%
  group_by(customer_state,seller_state, product_category_name) %>%
  summarise(sales_value_cate = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  select(1,2,5,4) %>%
  left_join(state, by = c("customer_state" = "Abbreviation")) %>%
  select(1,2,3,4,6) %>%
  group_by(product_category_name_english) %>%
  mutate(total_value = sum(sales_value_cate)) %>%
  arrange(desc(total_value),desc(sales_value_cate)) %>%
  mutate(rank = row_number()) %>%
  mutate(cate_of_total=sales_value_cate/total_value) 

write.csv(by_custstate_cate_distribution, 
          "by_custstate_cate_distribution.csv",
          row.names = T)


# sell_cust_state_cate_sales
by_custstate_cate_distribution1 <-
  filter(public_dataset, order_status == "delivered") %>%
  left_join(seller) %>%
  group_by(customer_state,seller_state) %>%
  summarise(sales_value_cate = sum(order_products_value)) %>%
  left_join(state, by = c("customer_state" = "Abbreviation")) %>%
  select(2,1,3,5) %>%
  group_by(customer_state) %>%
  mutate(total_custstate=sum(sales_value_cate)) %>%
  arrange(desc(total_custstate),desc(sales_value_cate)) %>%
  ungroup()%>%
  mutate(total=sum(sales_value_cate)) %>%
  mutate(rat1= sales_value_cate/total_custstate)  %>%
  mutate(rat2=total_custstate /total)


write.csv(by_custstate_cate_distribution1, 
          "by_custstate_cate_distribution1.csv",
          row.names = T)

by_custstate_cate_distribution1  %>%
  mutate(total=sum(sales_value_cate))


# state_cate_sales
by_cate_sales_distribution <-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(product_category_name) %>%
  summarise(sales_value_cate = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  mutate(total_value = sum(sales_value_cate)) %>%
  arrange(desc(total_value),desc(sales_value_cate)) %>%
  select(3,2,4) %>%
  mutate(rat=sales_value_cate/total_value) %>%
  mutate(rank = row_number()) 

write.csv(by_cate_sales_distribution, 
          "by_cate_sales_distribution.csv",
          row.names = T)

# graph_cate
bar <- ggplot(data = by_cate_sales_distribution) +
  geom_bar(
    mapping = aes(x =customer_state, y=sales_value,fill=customer_state),
    stat = "identity",
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

View( by_cate_sales_distribution)

#state_sales_cust
by_state_sales_cust<-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(customer_state) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  arrange(desc(sales_value))
View(by_state_sales_cust)



by_state_sales_cust1<-by_state_sales_cust %>%
left_join(state,by=c("customer_state"="Abbreviation")) %>%
 dplyr::select(1,2,4) %>%
  mutate(total=sum(sales_value)) %>%
  mutate(rat=sales_value/total)

write.csv(by_state_sales_cust1, 
          "by_state_sales_cust1.csv",
          row.names = T)

# graph
bar <- ggplot(data = by_state_sales_cust) +
  geom_bar(
    mapping = aes(x = customer_state, y=sales_value,fill=customer_state),
    stat = "identity",
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()



#state_sales_seller
by_state_sales_seller<-
  seller %>%
  left_join(public_dataset) %>%
  filter( order_status == "delivered") %>%
  group_by(seller_state) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  arrange(desc(sales_value))

View(by_state_sales_seller)

by_state_sales_seller1<-by_state_sales_seller %>%
  left_join(state,by=c("seller_state"="Abbreviation")) %>%
  dplyr::select(1,2,4) %>%
  mutate(total=sum(sales_value)) %>%
  mutate(rat=sales_value/total)

write.csv(by_state_sales_seller1, 
          "by_state_sales_seller1.csv",
          row.names = T)


bar <- ggplot(data = by_state_sales_seller) +
  geom_bar(
    mapping = aes(x = seller_state, y=sales_value,fill=seller_state),
    stat = "identity",
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()


#cate_sales_view
by_cate_sales_view <-
  filter(public_dataset, order_status == "delivered") %>%
  group_by(product_category_name) %>%
  summarise(sales_value_cate = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  dplyr::select(3,2) %>%
  left_join(by_cate_review) %>%
  arrange(desc(sales_value_cate)) %>%
  filter(count >= 100)%>%
dplyr::select(1,2,4,5)

write.csv(by_cate_sales_view, 
          "by_cate_sales_view.csv",
          row.names = T)
library(dplyr)
?as.tibble
# cluster
cate_sales_view <- as_tibble(by_cate_sales_view)
head(cate_sales_view)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
cate_sales_view1<-as_tibble(lapply(cate_sales_view[-1], normalize))

fit_kmeans <- kmeans(cate_sales_view1, centers=3)
fit_kmeans
library(digest)

cate_sales_view1 %>%
  mutate(cluster = factor(fit_kmeans$cluster)) %>%
  ggplot(aes(x=meanre, y=count, color=cluster)) +
  geom_point()+
  xlab('Average review score(normalized)')+
  ylab('Number of reviews')+
  ggtitle('Cluster categories into three groups')

cate_sales_view1 %>%
  mutate(cluster = factor(fit_kmeans$cluster)) %>%
  ggplot(aes(x=meanre, y=sales_value_cate, color=cluster)) +
  geom_point(size=3)+
  xlab('Average review score(normalized)')+
  ylab('Sales')+
  ggtitle('Cluster categories into three groups')


cate_sales_view1 %>%
  mutate(cluster = factor(fit_kmeans$cluster)) %>%
  ggplot(aes(x=sales_value_cate, y=count, color=cluster)) +
  geom_point(size=1)+
  xlab('Average review score(normalized)')+
  ylab('Sales')+
  ggtitle('Cluster categories into three groups')



by_state_sales_distribution%>%
  select(1,6) %>%
  distinct(.keep_all = T)%>%
  mutate(total=sum(total_value))
  

#install.packages("stringi")
library(lubridate)
#install.packages('lubridate')
?month()
x <- ymd("2012-03-26")
month(x)

# month_cate_sales
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


month_sales <-
  filter(public_dataset, order_status == "delivered") %>%
  mutate(month = month(order_purchase_timestamp)) %>%
  group_by(month, product_category_name) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  left_join(cate, by = "product_category_name") %>%
  select(1,4,3) %>%
  group_by(month) %>%
  mutate(total_value = sum(sales_value)) %>%
  arrange(desc(total_value),desc(sales_value))
library(ggrepel)

ggplot(data=month_sales[-4],aes(x=factor(month), y=sales_value))+
  geom_point(color = "blue", size = 0.1)+
  geom_label_repel(aes(label=ifelse(sales_value>100000
                                    ,as.character(product_category_name_english),''),
                       color=ifelse(sales_value>100000
                                    ,as.character(product_category_name_english),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'red',
                   show.legend=FALSE)+
  theme_classic()


# month_sales
by_month_sales_pattern1 <-
  filter(public_dataset, order_status == "delivered") %>%
  mutate(month = substr(order_purchase_timestamp,1,7)) %>%
  group_by(month) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  arrange(month)
  
View(by_month_sales_pattern1)
write.csv(by_month_sales_pattern1, 
          "season_sales_pattern1.csv",
          row.names = T)
ggplot(data=by_month_sales_pattern1,aes(x=month, y=sales_value,group=1))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# time series 
library(forecast)
Amtrak.data <- by_month_sales_pattern1
# create time series object using ts()
# ts() takes three arguments: start, end, and freq.
# with monthly data, the frequency of periods per season is 12 (per year).
# arguments start and end are (season number, period number) pairs.
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
ridership.ts <- ts(Amtrak.data$sales_value,
                   start = c(2016, 09), end = c(2018, 09), freq = 12)
# plot the series
plot(ridership.ts, xlab = "Time", ylab = "sales_value", ylim = c(0, 800000))

library(forecast)
# create short time series
# use window() to create a new, shorter time series of ridership.ts
# for the new three-year series, start time is Jan 1997 and end time is Dec 1999
ridership.ts.3yrs <- window(ridership.ts, start = c(2017, 1), end = c(2018, 06))
# fit a linear regression model to the time series
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))
# shorter and longer time series
par(mfrow = c(2, 1))
plot(ridership.ts.3yrs, xlab = "Time", ylab = "Ridership (in 000s)",
     ylim = c(0, 1000000))
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(0, 1000000))
# overlay the fitted values of the linear model
lines(ridership.lm$fitted, lwd = 2)


nValid <- 16
nTrain <- length(ridership.ts) - nValid
# partition the data
train.ts <- window(ridership.ts, start = c(2017, 01), end = c(2017, nTrain))
valid.ts <- window(ridership.ts, start = c(2017, nTrain + 1),
                   end = c(2017, nTrain + nValid))
# generate the naive and seasonal naive forecasts
naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
# plot forecasts and actuals in the training and validation sets
plot(train.ts, ylim = c(1300, 2600), ylab = "Ridership", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(naive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500))
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)
Time




#day_sales
by_day_sales_pattern <-
  filter(public_dataset, order_status == "delivered") %>%
  mutate(day = substr(order_purchase_timestamp,1,10)) %>%
  group_by(day) %>%
  summarise(sales_value = sum(order_products_value)) %>%
  arrange(day)

View(by_day_sales_pattern)
write.csv(by_day_sales_pattern, 
          "day_sales_pattern .csv",
          row.names = T)
ggplot(data=by_day_sales_pattern ,aes(x=day, y=sales_value,group=1))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=0.01))+
coord_flip()



#cate_view
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
          row.names = T)

ggplot(data=by_cate_review,mapping = aes(x=count,y=meanre),col='black')+
  geom_point()+
  geom_text(aes(label=ifelse(meanre>4.3|meanre<3.9,as.character(product_category_name_english),'')),hjust=0, vjust=0)
  
  
  #when the itemid is the same, we take the biggest price as the standard price.

# every day

public_dataset %>%
  group_by(product_id) %>%
  mutate(stand_price=max(order_products_value)) %>%
  dplyr::select(product_id,order_products_value,stand_price,product_category_name,order_id,order_aproved_at)%>%
  arrange(product_id) %>%
  mutate(time=substr(order_aproved_at,1,10)) %>%
  ungroup() %>%
  left_join(cate, by = "product_category_name") %>%
  dplyr::select(c(1:3),8,7,5)%>%
  group_by(product_category_name_english,time) %>%
  mutate(rate=sum(order_products_value)/sum(stand_price),sales=sum(order_products_value)) %>%
  ungroup() %>%
  group_by(product_category_name_english) %>%
  mutate(cate_sales=sum(order_products_value)) %>%
  filter(cate_sales>500000) %>%
  ungroup() %>%
  #count(n_distinct(product_category_name_english))
  arrange(time,product_category_name_english)%>%
  ggplot() +
  geom_point(aes(x=rate,y=sales,color=product_category_name_english)) +
  facet_wrap(~product_category_name_english, scales='free_y')
  

#when the itemid is the same, we take the biggest price as the standard price.

# every month

public_dataset %>%
  group_by(product_id) %>%
  mutate(stand_price=max(order_products_value)) %>%
  dplyr::select(product_id,order_products_value,stand_price,product_category_name,order_id,order_aproved_at)%>%
  arrange(product_id) %>%
  mutate(time=substr(order_aproved_at,1,7)) %>%  # every month
  ungroup() %>%
  left_join(cate, by = "product_category_name") %>%
  dplyr::select(c(1:3),8,7,5)%>%
  group_by(product_category_name_english,time) %>%
  mutate(discount=sum(order_products_value)/sum(stand_price),sales=sum(order_products_value)) %>%
  ungroup() %>%
  group_by(product_category_name_english) %>%
  mutate(cate_sales=sum(order_products_value)) %>%
  filter(cate_sales>500000) %>%
  ungroup() %>%
  #count(n_distinct(product_category_name_english))
  arrange(time,product_category_name_english)%>%
  ggplot() +
  geom_point(aes(x=discount,y=sales,color=product_category_name_english),show.legend = FALSE) +
  facet_wrap(~product_category_name_english, scales='free_y')

# no categories + month 
public_dataset %>%
  group_by(product_id) %>%
  mutate(stand_price=max(order_products_value)) %>%
  dplyr::select(product_id,order_products_value,stand_price,product_category_name,order_id,order_aproved_at)%>%
  arrange(product_id) %>%
  mutate(time=substr(order_aproved_at,1,7)) %>%  # every month
  ungroup() %>%
  left_join(cate, by = "product_category_name") %>%
  dplyr::select(c(1:3),8,7,5)%>%
  group_by(time) %>%
  mutate(discount=sum(order_products_value)/sum(stand_price),sales=sum(order_products_value)) %>%
  ungroup() %>%
  #count(n_distinct(product_category_name_english))
  arrange(time) %>%
ggplot() +
  geom_point(aes(x=discount,y=sales),show.legend = FALSE)


sales_discount_month<-public_dataset %>%
  group_by(product_id) %>%
  mutate(stand_price=max(order_products_value)) %>%
  dplyr::select(product_id,order_products_value,stand_price,product_category_name,order_id,order_aproved_at)%>%
  arrange(product_id) %>%
  mutate(time=substr(order_aproved_at,1,7)) %>%  # every month
  ungroup() %>%
  left_join(cate, by = "product_category_name") %>%
  mutate(time_day=substr(order_aproved_at,1,10)) %>%
  dplyr::select(c(1:3),7,5,9)%>%
  group_by(time_day) %>%
  mutate(discount=sum(order_products_value)/sum(stand_price),sales=sum(order_products_value)) %>%
  ungroup() %>%
  dplyr::select(sales,discount,time,time_day) %>%
  filter(sales>1000) %>%
  filter(!(time_day=='2017-11-24'|time_day=='2017-11-25')) %>%
  distinct(time_day,.keep_all=TRUE)%>%
   arrange(time_day)
  

  # day_discount_sales graph
sales_discount_month%>%
ggplot() +
  geom_point(aes(x=discount,y=sales),show.legend = FALSE,alpha=0.5)

#View(sales_discount_month)

sales_discount_month%>%
  arrange(desc(sales))


        

## set up model--linear regression
#Build a model for predicting sales. Use plots to justify your choice of predictor variables.
library(mlbench)
library(tidyverse)
library(MASS)
library(car)
library(modelr)
library(purrr)
library(ggplot2)
library(corrplot)

sales_discount_month_new <- sales_discount_month %>%
               mutate(time_factor=substr(time,6,7))

newdata<-sales_discount_month_new [,c(1,2,5)]
newdata1<-newdata
newdata1$time_factor<-as.numeric(newdata$time_factor)
corrplot(cor(newdata1))
newdata$time_factor<-as.factor(newdata$time_factor)
scatterplotMatrix(newdata,regLine = list(method=lm, lty=1, lwd=2, col='red') ,smooth = FALSE)

ggplot(data=newdata,mapping = aes(x=discount,y=sales))+
  geom_point()


#Model 1
fit1<-lm(sales~discount, data=newdata)
summary(fit1)
#But the ajusted R-squared is . So we use visualization to see the relationship between the residual in model 1 and other variables.

#Plot the residuals of the fitted model 1 from Problem 3 against the predictor variables already in the model: 

newdata %>%
  mutate(resid=resid(fit1)) %>%
  ggplot(mapping = aes(x=discount,y=resid))+
  geom_point()
# The residual plot shows simple random points without obvious patterns.

#Plot the residuals of the fitted model from Problem 3 against other potential predictor variables in the dataset.
newdata %>%
  mutate(resid=resid(fit1)) %>%
  ggplot(mapping = aes(x=time_factor,y=resid))+
  geom_boxplot()

#Plot the predictions
 newdata %>%
  add_predictions(fit1, "lpred") %>%
  mutate(pred = lpred) %>%
  ggplot(aes(x=discount)) +
  geom_point(aes(y=sales), alpha=0.1) +
  geom_line(aes(y=pred), color="blue") +
  coord_cartesian(xlim=range(newdata$discount),
                  ylim=range(newdata$sales))



# model 2
fit2<-lm(sales~discount+time_factor,data=newdata)
summary(fit2)
  

# model 3
fit3<-lm(sales~discount+time_factor+discount:time_factor,data=newdata)
summary(fit3)

confint(fit3)

stepAIC(fit3, direction="backward")

AIC(fit2,fit3)


#rmse
rmse(fit1, newdata)
rmse(fit2, newdata)
rmse(fit3, newdata)


fits_rmse <- tibble(nvar = 1:3,
                    rmse = c(rmse(fit1, newdata),
                             rmse(fit2, newdata),
                             rmse(fit3, newdata)))

ggplot(fits_rmse) + geom_line(aes(x=nvar, y=rmse))


set.seed(1)
newdata %>%
  crossv_kfold(5) %>%
  mutate(fit = map(train,
                   ~lm(sales~discount+time_factor+discount:time_factor,data = .))) %>%
  mutate(rmse_train = map2_dbl(fit, train, ~ rmse(.x, .y)),
         rmse_test = map2_dbl(fit, test, ~ rmse(.x, .y)))%>%
  summarise(arg_rmse_train=mean(rmse_train),arg_rmse_test=mean(rmse_test))
