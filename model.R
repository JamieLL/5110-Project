##category by obs
cate_rank <- left_join(orders, item, by = "order_id") %>% 
  left_join(product, by = "product_id") %>%
  left_join(cate, by = "product_category_name") %>%
  filter(order_status=="delivered") %>%
  select(product_category_name_english, order_id) %>%
  unique() %>%
  count(product_category_name_english) %>%
  arrange(desc(n)) %>%
  top_n(15) %>%
  mutate(category = reorder(product_category_name_english,n))

ggplot(cate_rank) +
  geom_col(aes(x=category, y= n))+
  coord_flip()

##feature processing
##payment clean
pay_cl_1 <- select(payment, order_id, payment_type) %>%
  unique() 

pay_cl_2 <- select(payment, order_id, payment_type) %>%
  filter(payment_type %in% c("voucher")) %>%
  unique() 
pay_cl_3 <- anti_join(pay_cl_1, pay_cl_2, by = "order_id") %>%
  filter(payment_type %in% c("boleto")) %>%
  unique() 

pay_cl_4 <- anti_join(pay_cl_1, pay_cl_2, by = "order_id") %>%
  anti_join(pay_cl_3, by = "order_id") %>%
  filter(payment_type %in% c("credit_card")) %>%
  unique() 

pay_cl_5 <- anti_join(pay_cl_1, pay_cl_2, by = "order_id") %>%
  anti_join(pay_cl_3, by = "order_id") %>%
  anti_join(pay_cl_4, by = "order_id") %>%
  filter(payment_type %in% c("debit_card")) %>%
  unique() 

pay_cl_6 <- anti_join(pay_cl_1, pay_cl_2, by = "order_id") %>%
  anti_join(pay_cl_3, by = "order_id") %>%
  anti_join(pay_cl_4, by = "order_id") %>%
  anti_join(pay_cl_5, by = "order_id") %>%
  filter(payment_type %in% c("not_defined")) %>%
  unique() 

pay_cl_7 <- group_by(payment, order_id) %>%
  summarise(value = sum(payment_value))

pay_cl <- rbind(pay_cl_6,pay_cl_5,pay_cl_4,pay_cl_3,pay_cl_2) %>% 
  inner_join(pay_cl_7, by ="order_id")

##review clean
review_cl <- group_by(reviews, order_id) %>%
  summarise(score = mean(review_score, na.rm = TRUE))
library(lubridate)
##product clean
pro_cl <- unique(select(item, order_id,product_id)) %>%
  left_join(product, by = "product_id") %>%
  select(order_id,product_description_lenght, product_photos_qty) %>%
  group_by(order_id) %>%
  summarise(desc_l = mean(product_description_lenght, na.rm = TRUE),
            ph_q = mean(product_photos_qty, na.rm = TRUE))

cate_cl <- left_join(orders, item, by = "order_id") %>% 
  left_join(product, by = "product_id") %>%
  left_join(cate, by = "product_category_name") %>%
  filter(order_status=="delivered") %>%
  select(product_category_name_english, order_id) %>%
  unique()

reg_price_new <- group_by(item, product_id) %>% summarise(regular_price = max(price))
item_new <- left_join(product, cate) %>% left_join(item)
##calculate sales and discount
dis <- left_join(orders, item_new) %>%
  left_join(reg_price_new) %>%
  filter(order_status == "delivered") %>%
  mutate(date = substring(order_purchase_timestamp, 1, 10)) %>%
  group_by(product_category_name_english, order_id, date) %>%
  summarise(value = sum(price*order_item_id), 
            reg_value = sum(regular_price*order_item_id),
            discount = 1-value/reg_value,
            sales = sum(order_item_id))

##dataset generated
df <- filter(orders, order_status=="delivered") %>% 
  left_join(customer, by = "customer_id") %>% 
  mutate(state = ifelse(customer_state %in% c("MG", "SP", "RJ"),
                        customer_state, "other")) %>% #add states
  left_join(pay_cl,  by ="order_id") %>% #add payment type and value
  left_join(review_cl, by ="order_id") %>% #add review score
  mutate(month = month(order_purchase_timestamp)) %>% #add month
  left_join(cate_cl, by ="order_id") %>% #add category
  left_join(pro_cl, by = "order_id") %>% #add photo quantities and desc length
  left_join(dis, by = "order_id") %>%
  select(order_id,state,payment_type, value.y, score,
         month, product_category_name_english.x, desc_l, ph_q,
         date, reg_value, sales)#add price discount sales

names(df) <- c("order_id","state","payment_type","value","score","month",
               "category","desc_l","ph_q","date","reg_value","sales" )

df <- mutate(df, discount = 1- value/reg_value)
df$state <- as.factor(df$state)
df$payment_type <- as.factor(df$payment_type)
df$month <- as.factor(df$month)

#######feature importance#######
library(randomForest)

##bed_bath_table
bed <- filter(df, category == "bed_bath_table", substr(date,1,4) == "2017")
model_bed <- randomForest(log(sales)~ discount + state + payment_type + score + month + desc_l + ph_q,
                          data = bed, ntree = 10, mtry = 5)
importance <- importance(model_bed)

##health_beauty
bea <- filter(df, category == "health_beauty", substr(date,1,4) == "2017")

model_bea <- randomForest(log(sales)~ discount + state + payment_type + score + month + desc_l + ph_q,
                          data = bea, ntree = 10, mtry = 5)
importance  <- cbind(importance, importance(model_bea))

##sports_leisure
spt <- filter(df, category == "sports_leisure", substr(date,1,4) == "2017")

model_spt <- randomForest(log(sales)~ discount + state + payment_type + score + month + desc_l + ph_q,
                          data = spt, ntree = 10, mtry = 5)
importance  <- cbind(importance, importance(model_spt))

##computers_accessories
acc <- filter(df, category == "computers_accessories", substr(date,1,4) == "2017")

model_acc <- randomForest(log(sales)~ discount + state + payment_type + score + month + desc_l + ph_q,
                          data = acc, ntree = 10, mtry = 5)
importance  <- cbind(importance, importance(model_acc))

##furniture_decor
fur <- filter(df, category == "furniture_decor", substr(date,1,4) == "2017")

model_fur <- randomForest(log(sales)~ discount + state + payment_type + score + month + desc_l + ph_q,
                          data = fur, ntree = 10, mtry = 5)
importance  <- cbind(importance, importance(model_fur))

##importance
importance <- as.data.frame(importance)
names(importance) <- c("bed_bath_table", "health_beauty", "sports_leisure", 
                       "computers_accessories","furniture_decor")
importance$feature <- row.names(importance)
imp <- gather(importance, bed_bath_table, health_beauty, sports_leisure, 
              computers_accessories,furniture_decor, key = "category", value = "importance") %>%
  group_by(category) %>%
  mutate(feature = reorder(feature, importance))

ggplot(imp) + 
  geom_col(aes(x = feature, y = importance)) + 
  facet_wrap(~ category, scales = "free") + 
  coord_flip()

dis_new <- dis %>% inner_join(filter(count, n>600)) %>% 
  arrange(product_category_name_english, desc(sales)) %>%
  group_by(product_category_name_english) %>%
  mutate( rank = row_number()) %>%
  filter(rank > 5) %>%
  arrange(product_category_name_english, desc(discount)) %>%
  mutate( rank2 = row_number()) %>%
  filter(rank2 > 5)

ggplot(dis_new) +
  geom_point(aes(x=discount, y=sales, color = product_category_name_english),
             alpha = 0.5) +
  facet_wrap(~ product_category_name_english, scales = "free")

ggplot(watches_gifts, aes(x = date)) + 
  geom_col(aes(y = sales)) + 
  geom_line(aes(y = discount)) 

watches_gifts <- filter(dis_new, product_category_name_english == "watches_gifts")
computers_accessories <- filter(dis, product_category_name_english == "computers_accessories")

ggplot(data = watches_gifts) +
  geom_boxplot(aes(x=season, y = sales))

model_gift <- lm(log(sales) ~ log(1-discount), data = watches_gifts)
summary(model_gift)

add_predictions(watches_gifts, model_gift) %>%
  ggplot(aes(x=discount)) + 
  geom_point(aes(y= sales), color = "black", alpha = 0.5, size = 3, position = "jitter") +
  geom_line(aes(y=exp(pred)),color = "brown", size = 1.2) 

