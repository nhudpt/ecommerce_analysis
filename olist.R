getwd()
setwd("C:/Users/Thao Nhu/R")

#------------------------------------------------------------------------------

### CÀI ĐẶT VÀ KHỞI TẠO CÁC PACKAGE CẦN THIẾT ###
install.packages("dplyr")
library("dplyr")
install.packages("lubridate")
library(lubridate)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)

#------------------------------------------------------------------------------

### ĐỌC VÀ XỬ LÝ DỮ LIỆU ###

#------------------------------------------------------------------------------

# Đọc dữ liệu 
order_df <- read.csv("olist_order_items_dataset.csv")
product_df <- read.csv("olist_products_dataset.csv")
seller_df <- read.csv("olist_sellers_dataset.csv")
reviews_df <- read.csv("olist_order_reviews_dataset.csv")
customers_df <- read.csv("olist_customers_dataset.csv")
orders_df <- read.csv("olist_orders_dataset.csv")

#------------------

# Kiểm tra các giá trị thiếu trong bộ dữ liệu
sum(is.na(order_df))
sum(is.na(product_df))
sum(is.na(seller_df))
sum(is.na(reviews_df))
sum(is.na(customers_df))
sum(is.na(orders_df))

#------------------

# Loại bỏ các giá trị thiếu
order_df <- order_df[complete.cases(order_df), ]
product_df <- product_df[complete.cases(product_df), ]
seller_df <- seller_df[complete.cases(seller_df), ]
reviews_df <- reviews_df[complete.cases(reviews_df), ]
customers_df <- customers_df[complete.cases(customers_df), ]
orders_df <- orders_df[complete.cases(orders_df), ]

#------------------

# Kết hợp các bảng
df <- left_join(order_df, product_df, by = "product_id") %>%
  left_join(seller_df, by = "seller_id") %>%
  left_join(reviews_df, by = "order_id") %>%
  left_join(orders_df, by = "order_id")
df <- left_join(df, customers_df, by = "customer_id")
df
str(df)

#------------------

# Chuyển đổi các cột của df để chuẩn bị cho phân tích
df$order_purchase_timestamp <- ymd_hms(df$order_purchase_timestamp)
df$order_approved_at <- ymd_hms(df$order_approved_at)
df$order_delivered_carrier_date <- ymd_hms(df$order_delivered_carrier_date)
df$order_delivered_customer_date <- ymd_hms(df$order_delivered_customer_date)
df$order_estimated_delivery_date <- ymd_hms(df$order_estimated_delivery_date)

#------------------

# Tạo biến Order_Delivery_Time tính thời gian giao hàng thực tế
install.packages("crayon")
library(crayon)

df <- df %>%
  mutate(Order_Delivery_Time = difftime(order_delivered_customer_date, 
                                        order_approved_at, 
                                        units = "days"))

#------------------------------------------------------------------------------

### KHÁM PHÁ DỮ LIỆU ###

#------------------------------------------------------------------------------

# Đếm số dòng và cột của df
dim(df)

# Xem dữ liệu mẫu của df
head(df)

#Xem kiểu dữ liệu của các cột trong df
str(df)

# Tóm tắt thống kê mô tả của df
summary(df)

# Kiểm tra và xử lý các giá trị trống (NA) trong df
df <- df %>% drop_na()

# Kiểm tra lại số dòng và cột của df sau khi xử lý giá trị trống
dim(df)

# Trực quan hóa biểu đồ phân phối thời gian giao hàng thực tế
ggplot(df, aes(x = Order_Delivery_Time)) +
  geom_histogram(fill = "orange", color = "black", binwidth = 2, ) +
  labs(title = "Phân phối thời gian giao hàng thực tế", 
       x = "Thời gian giao hàng thực tế (ngày)", 
       y = "Số lượng đơn hàng")
?geom_histogram

#------------------------------------------------------------------------------

### PHÂN TÍCH DỮ LIỆU KHÁM PHÁ ###

################################################################################

## Phân tích hành vi khách hàng

#  ----------------------------
# 1. Thống kê tần suất và phân phối các đặc tính của khách hàng

# Đếm số lượng khách hàng trong từng thành phố
city_count <- customers_df %>% 
  group_by(customer_city) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:20)

# Biểu đồ cho lượng khách hàng trong từng thành phố
#    Vẽ được nhưng do Brazi có quá nhiều thành phố nên không nên vẽ
ggplot(city_count, aes(x=customer_city, y=count)) + 
  geom_bar(stat="identity", fill="#69b3a2") +
  labs(title="Số lượng khách hàng theo thành phố", 
       x="Thành phố", y="Số lượng") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

# Tỉ lệ phần trăm của lượng khách hàng từ từng thành phố
city_percentage <- city_count %>% mutate(percentage=(count/sum(count))*100)

# 1d. Thành phố có lượng khách đông nhất
top_city <- city_count %>% arrange(desc(count)) %>% slice(1)
top_percent_city <- city_percentage %>% arrange(desc(percentage)) %>% slice(1)
cat("Thành phố có số lượng khách nhiều nhất là", 
    toupper(top_city$customer_city), "với", top_city$count, "khách hàng,",
    "chiếm", top_percent_city$percentage, "%")

# 1e. Tần suất mua hàng của từng khách hàng
order_count <- customers_df %>% group_by(customer_unique_id) %>% 
  summarize(count=n())
top_order <- order_count %>% arrange(desc(count)) %>% slice(1)
cat("Khách hàng có tần suất mua hàng cao nhất là",
    top_order$count, "lần")

#------------------
# 2. Biểu đồ Tổng giá trị đơn hàng theo thời gian

# Kết hợp dữ liệu
merged <- orders_df %>% inner_join(order_df, by = "order_id")
merged <- merged %>% drop_na()

# Chuyển kiểu dữ liệu
merged$order_purchase_timestamp <- as.Date(merged$order_purchase_timestamp)

# Tóm tắt dữ liệu bởi ngày và giá trị đơn hàng
agg_data <- merged %>%
  group_by(order_purchase_timestamp) %>%
  summarize(total_order_value = sum(price))

# Ngày có tổng giá trị đơn hàng cao nhất
top_order_value <- agg_data %>% arrange(desc(total_order_value)) %>% slice(1)
cat("Ngày có tổng giá trị đơn hàng cao nhất là ngày",
    as.character(format(top_order_value$order_purchase_timestamp, '%d/%m/%Y')) ,
    "với", top_order_value$total_order_value, "R$ (đồng Real Brazil)")

# Biểu đồ
ggplot(data = agg_data, aes(x = order_purchase_timestamp, y = total_order_value)) +
  geom_line() +
  ggtitle("Tổng giá trị đơn hàng theo thời gian") +
  xlab("Ngày đặt đơn") +
  ylab("Tổng giá trị đơn")

#------------------
# 4. Phân tích việc đặt hàng theo ngày và theo giờ

orders_1 = orders_df
orders_1$order_purchase_timestamp <- as.Date(orders_1$order_purchase_timestamp)
daily_visits <- orders_1 %>%
  group_by(day = day(order_purchase_timestamp)) %>%
  summarise(visits = n())

# Vẽ biểu đồ số lượng truy cập trang web theo ngày trong tháng
ggplot(daily_visits, aes(x = day, y = visits)) +
  geom_line(color = "#17395C") +
  labs(title = "Số lượng truy cập trang web theo ngày trong tháng") +
  xlab("Ngày trong tháng") +
  ylab("Số lượt truy cập")

# số lượng truy cập theo giờ trong ngày:
hourly_visits <- orders_df %>%
  group_by(hour = hour(order_purchase_timestamp)) %>%
  summarise(visits = n())

# Vẽ biểu đồ phân phối truy cập theo giờ trong ngày
ggplot(hourly_visits, aes(x = hour, y = visits)) +
  geom_bar(stat = "identity", fill = "#688F4E") +
  labs(title = "Phân phối truy cập theo giờ trong ngày") +
  xlab("Giờ trong ngày") +
  ylab("Số lượt truy cập")


#------------------
# 3. Phân tích đối tác, sản phẩm và vùng đặt hàng

geolocations_df <- read.csv("olist_geolocation_dataset.csv")

# Kết hợp các dataframe
order_products <- inner_join(order_df, product_df, by = "product_id")
order_products_sellers <- inner_join(order_products, seller_df, by = "seller_id")
order_products_sellers_geo <- inner_join(
  order_products_sellers, 
  geolocations_df, 
  by = c("seller_zip_code_prefix"="geolocation_zip_code_prefix"))

# Phân tích đối tác
partner_analysis <- order_products_sellers_geo %>%
  group_by(seller_state) %>%
  summarise(total_revenue = sum(price))
top_seller <- partner_analysis %>% arrange(desc(total_revenue)) %>% slice(1)
cat("Đối tác có doanh thu cao nhất đến từ bang",
    top_seller$seller_state,
    "(São Paulo) với", top_seller$total_revenue, "R$")

ggplot(data = partner_analysis, aes(x = seller_state, y = total_revenue)) +
  geom_bar(stat="identity", fill="#69b3a2") +
  ggtitle("Doanh thu từ các đối tác ở các bang") +
  xlab("Bang") +
  ylab("Doanh thu (R$)")

# Phân tích sản phẩm
product_analysis <- order_products_sellers_geo %>%
  group_by(product_category_name) %>%
  summarise(total_revenue = sum(price))
translate <- read.csv("product_category_name_translation.csv")
product_merge <- merge(product_analysis, translate, by = "product_category_name")
top_product <- product_merge %>% arrange(desc(total_revenue)) %>% slice(1)
cat("Sản phẩm có doanh thu cao nhất là các sản phẩm",
    top_product$product_category_name_english,
    "với", top_product$total_revenue, "R$")

ggplot(data = product_merge, aes(x = product_category_name_english, y = total_revenue)) +
  geom_bar(stat="identity", fill="#73B2F1") +
  ggtitle("Doanh thu các loại sản phẩm") +
  xlab("Loại sản phẩm") +
  ylab("Doanh thu (R$)") +
  scale_x_discrete(breaks = c("bed_bath_table",
                              "health_beauty",
                              "watches_gifts",
                              "computers_accessories",
                              "sports_leisure"))

# Phân tích vùng
product_customer <- inner_join(customers_df, orders_df, by = "customer_id") 
order_product_customer <- inner_join(product_customer, order_df, by = "order_id")
region_analysis <- order_product_customer %>%
  group_by(customer_state) %>%
  summarise(total_spend = sum(price))

top_customer <- region_analysis %>% arrange(desc(total_spend)) %>% slice(1)
cat("Chi tiêu cao nhất của khách hàng thuộc về bang",
    top_customer$customer_state,
    "với số tiền là", top_customer$total_spend, "R$")

ggplot(data = region_analysis, aes(x = customer_state, y = total_spend)) +
  geom_bar(stat="identity", fill="#C581F7") +
  ggtitle("Chi tiêu của khách hàng ở các bang") +
  xlab("Bang") +
  ylab("Chi tiêu (R$)")

# Vẽ bản đồ mức độ chi tiêu của khách hàng theo từng bang
install.packages("remotes")
remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

brazil_states = ne_states(country = "brazil", returnclass = "sf")
spending_map = left_join(brazil_states, region_analysis, by = c("postal" = "customer_state"))
top5 <- spending_map %>% 
  arrange(desc(total_spend)) %>% 
  head(5)

ggplot() +
  geom_sf(data = spending_map, aes(fill = total_spend)) +
  scale_fill_gradient(low = "#F3FDFE", high = "#2C6975", 
                      labels = label_number_si(scale = 1e+6, suffix = " R$"),
                      name = "Tổng chi tiêu") +
  geom_text(data = top5, aes(x = longitude, y = latitude, label = str_wrap(paste(name, comma(total_spend)), width = 10)), 
            size = 2.8, fontface = "bold", color = "black") +
  ggtitle("Mức chi tiêu của khách hàng ở từng bang")
  theme_void()

#------------------
# 5. Phân tích xu hướng bán hàng

orders_df$order_purchase_timestamp <- ymd_hms(orders_df$order_purchase_timestamp)

# Đếm số lượng đơn hàng mỗi tháng
orders_per_month <- orders_df %>%
  mutate(month = floor_date(order_purchase_timestamp, "month")) %>%
  count(month)

# Biểu đồ số lượng đơn hàng mỗi tháng
ggplot(orders_per_month, aes(month, n)) +
  geom_col() +
  scale_x_datetime(date_labels = "%m/%y", date_breaks = "1 month") +
  labs(x = "Tháng", y = "Số lượng đơn hàng", 
       title = "Số lượng đơn hàng hằng tháng")

#------------------
# 6. Xác định Các từ xuất hiện nhiều trong các tiêu đề và nội dung bình luận của khách hàng để đánh giá chất lượng sản phẩm và dịch vụ của Olist.

library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("tm")
library(tm)
install.packages("googleAuthR")
library(googleAuthR)
install.packages("googleLanguageR")
library(googleLanguageR)
install.packages("tidytext")
library(tidytext)
library(dplyr)

stopwords = stopwords('es')
reviews_words <- reviews_df %>% 
  select(review_comment_title, review_comment_message) %>% 
  unnest_tokens(word, review_comment_title) %>% 
  unnest_tokens(word, review_comment_message) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Lấy top 50 từ lặp lại nhiều nhất
reviews_words$word = as.character(reviews_words$word)
df_test = reviews_words %>% arrange(desc(n)) %>% slice(1:50)

# Chuyển đổi dữ liệu sang tiếng Anh
gar_auth_service("C:/Users/Thao Nhu/R/translate-383309-b437cb17e140.json")
trans = gl_translate(df_test$word, target = "en", source = "es")
df_test_1 = as.data.frame(cbind(trans$translatedText, df_test$n))    
names(df_test_1) <- c("word", "fre")

# Vẽ biểu đồ
df_test_1$group <- ifelse(seq_len(nrow(df_test_1)) %% 2 == 0, "even", "odd")
ggplot(df_test_1[1:20,], aes(x = word, y = fre)) +
  geom_col(fill = "#77C8BF") +
  coord_flip() +
  labs(x = "Từ", y = "Số lần xuất hiện", 
       title = "Các từ xuất hiện nhiều nhất trong phần đánh giá") +
  theme_minimal()

#------------------
# 7. Phân tích top sản phẩm theo mùa
  
  # Nối dữ liệu và chuyển đổi dữ liệu ngày tháng
  orders_order_items <- orders_df %>% inner_join(order_df, by = "order_id")
  
  orders_order_items_shipdate <- orders_order_items %>%
    mutate(order_delivered_month = month(parse_date_time(order_delivered_customer_date, "ymd HMS")))
  
  orders_order_items_shipdate_product <- orders_order_items_shipdate %>% 
    inner_join(product_df, by = "product_id")
  
  # Tính toán top sản phấm bán chạy trong từng mùa
  # Định nghĩa mùa: Xuân (T9-T11), Hè (T12-T2), Thu (T3-T5), Đông (T6-T8)
  top_products_by_season <- orders_order_items_shipdate_product %>% 
    mutate(season = case_when(
      order_delivered_month %in% c(9, 10, 11) ~ "spring",
      order_delivered_month %in% c(12, 1, 2) ~ "summer",
      order_delivered_month %in% c(3, 4, 5) ~ "autumn",
      order_delivered_month %in% c(6, 7, 8) ~ "winter"
    )) %>%
    filter(!is.na(season)) %>%
    group_by(product_id, season) %>%
    summarise(total_sold = n()) %>%
    ungroup() %>%
    group_by(season) %>%
    top_n(10, total_sold)
  
  translate_pro <- merge(translate, product_df, by = "product_category_name")
  
  top_products_by_season
  top_sp_mua <- merge(top_products_by_season, translate_pro, by = "product_id") %>%
    select("product_id", "season", "total_sold","product_category_name_english")
  top_sp_mua_sp <- top_sp_mua[top_sp_mua$season == "spring",]
  top_sp_mua_su <- top_sp_mua[top_sp_mua$season == "summer",]
  top_sp_mua_au <- top_sp_mua[top_sp_mua$season == "autumn",]
  top_sp_mua_wi <- top_sp_mua[top_sp_mua$season == "winter",]
  
  
  ggplot(top_sp_mua_sp, aes(x=product_category_name_english, y=total_sold)) + 
    geom_bar(stat="identity", fill = "#6495ED") +
    labs(title="Top 10 sản phẩm được mua nhiều nhất trong mùa xuân", 
         x="Tên sản phẩm", y="Số lượng") +
    theme(axis.text.x = element_text(size = 16, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16, margin = margin(t = 10)),
          plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1)) 
  
  ggplot(top_sp_mua_su, aes(x=product_category_name_english, y=total_sold)) + 
    geom_bar(stat="identity", fill = "#FA8072") +
    labs(title="Top 10 sản phẩm được mua nhiều nhất trong mùa hè", 
         x="Tên sản phẩm", y="Số lượng") +
    theme(axis.text.x = element_text(size = 16, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16, margin = margin(t = 10)),
          plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1)) 
  
  ggplot(top_sp_mua_au, aes(x=product_category_name_english, y=total_sold)) + 
    geom_bar(stat="identity", fill = "#808000") +
    labs(title="Top 10 sản phẩm được mua nhiều nhất trong mùa thu", 
         x="Tên sản phẩm", y="Số lượng") +
    theme(axis.text.x = element_text(size = 16, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16, margin = margin(t = 10)),
          plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1)) 
  
  ggplot(top_sp_mua_wi, aes(x=product_category_name_english, y=total_sold)) + 
    geom_bar(stat="identity", fill = "#AED6F1") +
    labs(title="Top 10 sản phẩm được mua nhiều nhất trong mùa đông", 
         x="Tên sản phẩm", y="Số lượng") +
    theme(axis.text.x = element_text(size = 16, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16, margin = margin(t = 10)),
          plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1))
  

#------------------
# 8. Phân tích phân khúc khách hàng

payment <- read.csv("olist_order_payments_dataset.csv")
customers <- inner_join(order_product_customer, payment, by = "order_id", relationship = "many-to-many")
customers$order_purchase_timestamp <- as.Date(customers$order_purchase_timestamp)
str(customers)

# Tính toán mô hình RFM
# Mô hình này được sử dụng để phân tích giá trị khách hàng (Customer Value),
# từ đó giúp doanh nghiệp có thể phân tích ra từng nhóm khách hàng mà mình đang có, 
# từ đó có những chiến dịch marketing hoặc chăm sóc đặc biệt.

# R (Recency): Khoảng thời gian mua hàng gần nhất
r <- customers %>%
  group_by(customer_unique_id) %>% 
  summarise(Recency = as.numeric(max(order_purchase_timestamp) - min(order_purchase_timestamp)) + 1) %>%
  ungroup()

# F (Frequency): Tần suất mua hàng
f <- customers %>%
  group_by(customer_unique_id) %>%
  summarise(Frequency = n()) %>%
  ungroup()

# M (Monetary Value): Khách hàng đã chi bao nhiêu tiền để mua hàng
m <- customers %>%
  group_by(customer_unique_id) %>%
  summarise(MonetaryValue = sum(payment_value, na.rm=TRUE)) %>%
  ungroup()

# Kết hợp R, F, M vào 1 bảng để phân tích RFM
RFM <- merge(merge(r, f, by="customer_unique_id"), m, by="customer_unique_id")

# Scale dữ liệu
RFM_scale <- scale(RFM[,2:4])
RFM_scale <- data.frame(RFM_scale)

# Do kích thước dữ liệu quá lớn nên chọn 1 mẫu dữ liệu để phân tích
set.seed(123)
RFM_scale_1 = sample_frac(RFM_scale, 0.1)
RFM_scale_1 <- apply(RFM_scale_1, 2, as.numeric)

# Tính số cụm dữ liệu - Phương pháp Elbow 
install.packages("factoextra")
library("factoextra")
install.packages("NbClust")
library("NbClust")
gc()
fviz_nbclust(RFM_scale_1, kmeans, method = "wss")

fviz_cluster(list(data = RFM_scale), kmean_model, 
             geom = "point", 
             palette = "Set1", 
             ggtheme = theme_classic())

# Phân cụm khách hàng
RFM_scale = apply(RFM_scale, 2, as.numeric)
kmean_model = kmeans(RFM_scale, centers = 4, iter.max = 100, nstart = 100)
kmean_model
RFM$cluster = as.factor(kmean_model$cluster)

# Các phân khúc khách hàng
cus_seg = aggregate(RFM[,2:4], by=list(RFM$cluster), mean)
cus_seg

###############################################################################

### KIỂM ĐỊNH ###

#--------------------
## Phân tích kiểm định tham số
# Kiểm định trung bình ANOVA về sự khác biệt thời gian giao hàng giữa các khu vực

# Đọc và hợp nhất dữ liệu
data <- read.csv("olist_orders_dataset.csv")
customers_df <- read.csv("olist_customers_dataset.csv")
data <- merge(data, customers_df, by = "customer_id")

# Đưa cột "order_delivered_customer_date" và "order_approved_at" về định dạng ngày tháng
data$order_delivered_customer_date <- as.Date(data$order_delivered_customer_date)
data$order_approved_at <- as.Date(data$order_approved_at)

# Tạo một biến mới để lưu trữ thời gian giao hàng tính bằng ngày
data$delivery_time <- as.numeric(data$order_delivered_customer_date - data$order_approved_at)

# Tạo một biến mới để lưu trữ khu vực
data$region <- substr(data$customer_state, 1, 2)

# Kiểm tra sự khác biệt về thời gian giao hàng giữa các khu vực bằng kiểm định ANOVA
model <- lm(delivery_time ~ region, data = data)
anova(model)

# Vẽ biểu đồ boxplot
ggplot(data, aes(x = region, y = delivery_time)) +
  geom_boxplot(fill = "pink", outlier.colour = "brown") +
  labs(x = "Khu vực", y = "Thời gian giao hàng (ngày)",
       title = "Sự khác biệt về thời gian giao hàng giữa các khu vực") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

#--------------------
# Kiểm định t-test

payment <- read.csv("olist_order_payments_dataset.csv")
ordpay <- merge(orders_df, payment, by = "order_id")

# Đưa cột "order_delivered_customer_date" và "order_approved_at" về định dạng ngày tháng
ordpay$order_delivered_customer_date <- as.Date(ordpay$order_delivered_customer_date)
ordpay$order_approved_at <- as.Date(ordpay$order_approved_at)

# Tạo một biến mới để lưu trữ thời gian giao hàng tính bằng ngày
ordpay$delivery_time <- as.numeric(ordpay$order_delivered_customer_date - ordpay$order_approved_at)

# Lọc ra các đơn hàng đã được thanh toán bằng thẻ tín dụng
credit_card <- subset(ordpay, payment_type == "credit_card")

# Lọc ra các đơn hàng không phải là thanh toán bằng thẻ tín dụng
other_payment <- subset(ordpay, payment_type != "credit_card")

# Kiểm tra sự khác biệt về thời gian giao hàng giữa các nhóm bằng kiểm định t test
t.test(credit_card$delivery_time, other_payment$delivery_time)

# Kết quả cho thấy có sự khác biệt về thời gian giao hàng giữa hai phương thức thanh toán
# là "credit_card" và "other_payment" (p-value < 0.05). 
# Giá trị trung bình của thời gian giao hàng khi thanh toán bằng "credit_card" là 12.113 ngày, 
# cao hơn so với khi thanh toán bằng "other_payment" là 11.698 ngày.

#--------------------
# Dự đoán dựa trên mô hình hồi quy tuyến tính và kiểm định Chi-Squared

# Nạp dữ liệu
categories <- read.csv("product_category_name_translation.csv")

# Chuẩn bị dữ liệu
# Join bảng products và translations để có tên sản phẩm đầy đủ
products <- product_df %>%
  left_join(categories, by = "product_category_name") %>%
  select(-product_category_name) %>%
  rename(product_category_name = "product_category_name_english")

# Join các bảng để có thông tin đầy đủ về đơn hàng và sản phẩm
data <- order_df %>%
  inner_join(orders_df, by = "order_id") %>%
  inner_join(products, by = "product_id") %>%
  inner_join(customers_df, by = "customer_id")

# Chọn biến đầu vào và chuyển đổi thành dạng số
data_new = data %>%
  group_by(order_id) %>%
  mutate(revenue = price + freight_value) %>%
  summarise(total_revenue = sum(revenue))

data_new_1 = data %>%
  group_by(order_id) %>%
  summarise(num_item = max(order_item_id))

data_new_2 = data %>%
  select(order_id, customer_state, product_category_name) %>%
  distinct(order_id, .keep_all = TRUE)

merge = data_new %>%
  inner_join(data_new_1, by = "order_id") %>%
  inner_join(data_new_2, by = "order_id")

# Chuyển đổi biến đầu vào thành dạng số
merge_1 = merge %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.integer)

# Kiểm tra mối tương quan giữa các biến
summary(merge_1) #=> product_category_name có giá trị NAN
merge_1 = merge_1[complete.cases(merge_1), ]
cor(merge_1)
# => Doanh thu không tương quan với bất kỳ biến nào

# Vẽ biểu đồ
install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(cor(merge_1), type = "lower", hc.order = TRUE, 
           lab = TRUE, lab_size = 3, method = "circle", 
           colors = c("#6D9EC1", "white", "#E46726")) + theme()
# => Bỏ cột order_id và product_category_name

# chia tập dữ liệu thành trainset và testset
set.seed(123)
train = sample_frac(merge_1, 0.7)
test = anti_join(merge_1, train, by = "order_id")

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(total_revenue ~ num_item + customer_state, data = train)

# Kiểm tra kết quả của mô hình
summary(model)

# Dự đoán trên tập kiểm tra
test$pred <- predict(model, newdata = test)

# Tính sai số trung bình bình phương
mse <- mean((test$pred - test$total_revenue)^2)
mse

# Kiểm định chi-squared
chisq.test(merge_1$total_revenue, merge_1$num_item)
# => bác bỏ H0, hai biến có tương quan với nhau
 
###############################################################################

### MÔ HÌNH DỰ ĐOÁN ###
## sử dụng mô hình ARIMA để dự đoán doanh số bán hàng trong những ngày tới

# Sử dụng dữ liệu doanh số bán hàng theo ngày đã tạo trước đó (biến cuối cùng là agg_data)

# Kiểm tra tính dừng của chuỗi thời gian
install.packages("tseries")
library("tseries")
install.packages("forecast")
library("forecast")
library("xts")
ts_agg_data = xts(agg_data$total_order_value, 
                  order.by = agg_data$order_purchase_timestamp)
ts.plot(ts_agg_data) # => Chuỗi không có tính dừng
adf.test(ts_agg_data)
# => p_value = 0,01 (<0,05), bác bỏ H0, chuỗi này là chuỗi dừng

# Kiểm định tính nhiễu trắng của chuỗi thời gian
Box.test(ts_agg_data, type = "Ljung-Box")
# => p_value rất bé, bác bỏ H0, chuỗi thời gian không có tính nhiễu trắng => Lấy sai phân

#Lấy sai phân
agg_data_diff = diff(log(ts_agg_data))
ts.plot(sales_ts_diff)

# Tìm mô hình ARIMA phù hợp
breakpoint = floor(nrow(agg_data_diff)*0.9) # breakpoint = 554
trainset = agg_data_diff[1:breakpoint]
auto.arima(trainset) 
# => Đề nghị sử dụng mô hình ARIMA (2,0,5)

# Xây dựng mô hình dự đoán cho tất cả các ngày trong tập test
agg_data_diff[554,]
a = 554
# Lấy giá trị date trước ngày có stt a
Actual_series = xts(0,as.Date("2018-06-28","%Y-%m-%d"))
Actual_series
# Khởi tạo dataframe cho chuỗi dự đoán
forecast_list <- list()
Actual_series <- xts(0,as.Date("2018-06-28","%Y-%m-%d"))
for (b in a:(nrow(agg_data_diff)-1)){
  train = agg_data_diff[2:b,]
  test = agg_data_diff[-row(train),]
  
  # Tạo mô hình ARIMA
  modelarima = arima(train, order = c(2,0,5), include.mean = FALSE)
  
  # Dự báo cho tập test
  arima_forecast = forecast(modelarima, h = 1, level = 95)
  
  forecast_vec <- c(STT = b+1,
                    Forecasted = arima_forecast$mean[1],
                    Upper_Forecasted = arima_forecast$upper,
                    Lower_Forecasted = arima_forecast$lower)
  
  # Thêm vector vào danh sách
  forecast_list[[b]] <- forecast_vec
  
  Actual_return = agg_data_diff[(b+1),]
  Actual_series = rbind(Actual_series, xts(Actual_return))
  rm(Actual_return)
}
forecasted_series <- do.call(rbind, forecast_list)

#Điều chỉnh độ dài của chuỗi giá trị thực tế
Actual_series = Actual_series[-1]

#Tạo ra object cho chuỗi được dự báo
forecasted_series_1 = xts(forecasted_series,index(Actual_series))

#So sánh giá trị dự báo và giá trị thực tế
plot(Actual_series,lwd=1.5,col = "#BBD38B", ylim=c(-6,6),
     main="Actual return vs Forecasted return")
lines(forecasted_series_1$Forecasted,lwd=1,col = "#17395C")
lines(forecasted_series_1$Upper_Forecasted,type = "l",pch = 22,lwd=1.5,col = "#EFB758")
lines(forecasted_series_1$Lower_Forecasted,type = "l",pch = 22,lwd=1.5,col = "#EFB758")
legend("topright", c('Actual','Forecasted',"Upper forecasted","Lower forecasted"), 
       lty = c(1,1), lwd = c(1.5,1.5),col = c('#BBD38B','#17395C','#EFB758','#EFB758'))

#Tạo bảng giá trị actual và forecasted
comparision = merge(Actual_series,forecasted_series_1$Forecasted)
comparision$Accuracy = sign(comparision$Actual_series) == sign(comparision$Forecasted)
print(comparision)

#Tính toán mức độ chính xác
Accuracy_percentage = sum(comparision$Accuracy ==1)*100/length(comparision$Accuracy)
print(Accuracy_percentage)

#Dự đoán doanh thu cho 5 ngày tiếp theo
forecast_5day = forecast(modelarima, h=5, level = 95)
forecast_5day


