library(tidyverse)
library(ggplot2)
library (MASS)
library (dplyr)
library(readr)

rm(list = ls())

setwd("/Users/mac/Desktop/DS 5110/Final project/Version2/brazilian-ecommerce")
geo <- read_csv("geolocation_olist_public_dataset.csv")
#caculate purchase in cities costumers live in
customers_dataset <- read_csv("olist_customers_dataset.csv")
#View(customers_dataset)
#customer_state
items_dataset <- read_csv("olist_order_items_dataset.csv")
#View(items_dataset)
#order_seller_value
orders_dataset <- read_csv("olist_orders_dataset.csv")
#View(orders_dataset)
#order_customer_time
geolocation_dataset <- read_csv("olist_geolocation_dataset.csv")
View(geolocation_dataset)
#location_state
order_payments_dataset <- read_csv("olist_order_payments_dataset.csv")
#View(order_payments_dataset)
#order_payment
sellers_dataset <- read_csv("olist_sellers_dataset.csv")
##product_cate
product_dataset <- read_csv("olist_products_dataset.csv")
##review
review<- read_csv("olist_order_reviews_dataset.csv")
##translation
trans_cate<- read_csv("product_category_name_translation.csv")

##product_order
product_all<-items_dataset %>%
  left_join(orders_dataset,by = "order_id") %>%
  left_join(product_dataset,by = "product_id") %>%
  right_join(order_payments_dataset, by = "order_id") %>%
  left_join(trans_cate, by = "product_category_name")
  filter(order_status == "delivered")
## voucher group
product_voucher <- product_all %>%
  filter(payment_type == "voucher")
## Voucher_Sum
voucher_sum<-product_voucher %>% 
  count(product_category_name_english) %>%
  mutate(Sum=sum(n)) %>% 
  mutate(proportion = n/Sum)  %>%
  group_by(product_category_name_english)
product_all$product_category_name_english
##  Product_Sum
all_sum<-  product_all %>%
  count(product_category_name_english) %>%
  mutate(Sum=sum(n)) %>% 
  mutate(proportion = n/Sum)  %>%
  group_by(product_category_name_english) 
### Boleto

product_boleto <- product_all %>%
  filter(payment_type == "boleto")
## Voucher_Sum
boleto_sum<-product_boleto %>% 
  count(product_category_name_english) %>%
  mutate(Sum=sum(n)) %>% 
  mutate(proportion = n/Sum)  %>%
  group_by(product_category_name_english)
## Difference between voucher 
C<-all_sum %>% full_join(voucher_sum, by = "product_category_name_english") %>%
  full_join(boleto_sum, by = "product_category_name_english")

C <- replace(C, is.na(C), 0)
colnames(C)[colnames(C) == 'proportion.x'] <- 'all_paytype' 
colnames(C)[colnames(C) == 'proportion.y'] <- 'voucher_pay' 
colnames(C)[colnames(C) == 'proportion'] <- 'boleto_pay' 
## voucher百分比
C1<-C[1:71,]
C2<-gather(C1, 'all_paytype' , 'voucher_pay' ,'boleto_pay' , key = "all_voucher", value = "proportion")


C2 %>% group_by(all_voucher) %>%
ggplot()+geom_col(aes(product_category_name_english,proportion,fill= all_voucher),position = 'dodge')+
  coord_flip()+ggtitle("Sales distribution in payment group")

"From the graphs we can observe that at housewares, fashion-bags and bed-bath-table items voucher sales performs significantly 
higher than others. It illustrate that buying demands in those fields are more likely to motivated by discount strategy.
Also housewares products tend to use vouchers as discount, which is more conventional method. 
On the other hand, vouchers proportion is lower than average in watches-gifts, health-beauty and computers fields. This
could result from more directly discount on price at products like computers and beauty. "


## Difference between Boleto


product_all %>%
  filter(order_status == "delivered") %>%
  dplyr::select(5,23) %>%
  group_by(payment_type) %>%
  ggplot()+ geom_boxplot(aes(payment_type,price,fill=payment_type))+coord_cartesian(ylim = c(0, 400))

"According to the plots, the price is usually higher than others by $25 than paid by debit and boleto, while voucher showed $10 
less than the other two in terms of meadian price."


product_all %>%
  filter(order_status == "delivered") %>%
  dplyr::select(5,23) %>%
  group_by(payment_type) %>%
  ggplot( aes(payment_type,price,color=payment_type), alpha(0.05)) +
  layer(geom = "point",
        stat = "identity",
        position = "jitter")



"review by payment method"
product_all %>% left_join(review, by = "order_id")%>%
  filter(order_status == "delivered") %>%
  dplyr::select(23,28) %>%
  group_by(payment_type) %>%
  ggplot( aes(payment_type,review_score) ) + geom_point(position = "jitter")

product_all %>% left_join(review, by = "order_id")%>%
  filter(order_status == "delivered") %>%
  dplyr::select(23,26,28) %>%
  group_by(payment_type,product_category_name_english) %>%summarise( meanre = mean(review_score, na.rm = TRUE),
                                      count = n()) %>%
ggplot( aes(x=count,y=meanre,color=payment_type))+
  geom_point()+
  geom_text(aes(label=ifelse(meanre>4.6|meanre<3.5,as.character(product_category_name_english),'')),hjust=0, vjust=0)+
  facet_wrap(~payment_type,scales = "free")
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


##Review upon description length 1?
Review_time_distribution %>%  
  group_by(product_category_name_english)%>%
  summarise(score=mean(review_score)) %>%
  ggplot(aes(x=reorder(product_category_name_english,score),y=score,fill=score))+
  geom_col()+coord_flip()

## Review upon decription photo ?
Review_time_distribution %>%
  ggplot(aes(x=factor(review_score),y=product_photos_qty,color=review_score))+
  layer(geom = "point",
        stat = "identity",
        position = "jitter")


Review_time_distribution %>%
  ggplot(aes(x=review_score,y=product_description_lenght,
             color=review_score))+layer(geom = "point", stat = "identity", position = "jitter")


Review_time_distribution %>%
  ggplot(aes(x=review_score,y=product_photos_qty,
             color=factor(review_score)))+layer(geom = "point",stat = "identity", position = "jitter")
