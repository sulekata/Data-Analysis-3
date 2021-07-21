
# setup -------------------------------------------------------------------

library(tidyverse)
library(skimr)
library(Hmisc)
library( RColorBrewer )
library(cowplot)
source("codes/da_helper_functions.R")

# import data -------------------------------------------------------------

data <- read_csv("data/clean/cape_town_clean.csv")

# label engineering ----------------------------------------

summary(data$price)
describe(data$price)

# add ln(price) variable
data <- data %>%
  mutate(ln_price = log(price))

# remove extreme values which are way above the 95th percentile
data <- data %>%
  filter(price < 8000)

# histogram for price
ggplot( data = data, aes(price) ) +
  geom_histogram( fill = brewer.pal( 3, "Set2" )[1], alpha = 0.5 ) +
  theme_bw() +
  labs( x='\n Price', y='Absolute Frequency \n', title = 'Distribution of Price per Night') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )

# histogram for ln(price)
ggplot( data = data, aes(ln_price) ) +
  geom_histogram( fill = brewer.pal( 3, "Set2" )[2], alpha = 0.5 ) +
  theme_bw() +
  labs( x='\n Ln(Price)', y='Absolute Frequency \n', title = 'Distribution of Ln(Price per Night)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )

#### feature engineering
## check distrubutions and functional forms for OLS and OLS with LASSO
# check categorical variables ---------------------------------------------

categoricals <- c("f_property_type", "f_neighbourhood")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# check dummy variables ---------------------------------------------------

dummies <- c( "d_wifi", "d_tv", "d_refrigerator", "d_air_conditioning", "d_sound", "d_baby", "d_beach", "d_stove", "d_free_parking", "d_paid_parking", "d_superhost", "d_profile_pic", "d_identity_verified", "d_instant_bookable")

for (i in 1:length(dummies)) {
  data %>%
    group_by(get(dummies[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# check numerical variables -----------------------------------------------

### n_accommodates
data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, shape=16)+
  ylim(0,8000)+
  xlim(2,6)+
  labs(x="Number of people accommodated",y="Price")+
  geom_smooth(method="lm",  se=FALSE)

# the chart shows a linear relationship so leave as is

### n_beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# impute missing and 0 with n_accommodates assume they mean the same
data <- data %>% 
      mutate(n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds),
             n_beds = ifelse( n_beds == 0, n_accommodates, n_beds))

# check the relationship with price
ggplot( data, aes( x = n_beds, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square of n_beds
data <- data %>% mutate( n_beds2 = n_beds^2)

# regression 1: price and number of reviews
reg1<-lm(price ~ n_beds, data=data)
summary(reg1)
# regression 2: log-price and log number of reviews
reg2<-lm(price ~ n_beds + n_beds2, data=data)
summary(reg2)

# add square n_beds to the model

### n_bathrooms
data %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data, aes(n_bathrooms)) +
  geom_histogram( alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms")

# impute missing with median
data <- data %>%
  mutate( n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms))

# check the relationship with price
ggplot( data, aes( x = n_bathrooms, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add log bathrooms to df
data <- data %>% mutate( ln_bathrooms = log(n_bathrooms))

# use log bathrooms in the model

### n_number of reviews
describe(data$n_number_of_reviews)

# filter the df to check the distribution of number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews < 100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Number of reviews")

# use logs as well because the distribution is very skewed
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log Number of reviews")

# regression 3: price and number of reviews
reg3<-lm(price ~ n_number_of_reviews, data=data)
summary(reg3)
# regression 4: price and log number of reviews
reg4<-lm(price ~ ln_number_of_reviews, data=data)
summary(reg4)

# use log number of reviews in the model

### n_days_since_rv
describe(data$n_days_since_rv)

# check the relationship with price
ggplot( data, aes( x = n_days_since_rv, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_days_since_rv2=n_days_since_rv^2)

# use square in the model

### n_days_since_host
describe(data$n_days_since_host)

# check the relationship with price
ggplot( data, aes( x = n_days_since_host, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square and cube to df
data <- data %>%
  mutate(
    n_days_since_host2=n_days_since_host^2,
    n_days_since_host3=n_days_since_host^3)

# use square and cube in the model 

### n_review_scores_rating
describe(data$n_review_scores_rating)

# check the relationship with price
ggplot( data, aes( x = n_review_scores_rating, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# leave as is

### n_host_acceptance_rate
describe(data$n_host_acceptance_rate)

# check the relationship with price
ggplot( data, aes( x = n_host_acceptance_rate, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_host_acceptance_rate2=n_host_acceptance_rate^2)

### n_host_response_rate
describe(data$n_host_response_rate)

# check the relationship with price
ggplot( data, aes( x = n_host_response_rate, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_host_response_rate2=n_host_response_rate^2)

### n_host_total_listings_count
describe(data$n_host_total_listings_count)

# check the relationship with price
ggplot( data, aes( x = log(n_host_total_listings_count), y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add log to df
data <- data %>%
  mutate(
    ln_host_total_listings_count=log(n_host_total_listings_count))

### n_bedrooms
describe(data$n_bedrooms)

# check the relationship with price
ggplot( data, aes( x = n_bedrooms, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_bedrooms2=n_bedrooms^2)

# use square as well

### n_reviews_per_month
describe(data$n_reviews_per_month)

# check the relationship with price
ggplot( data, aes( x = n_reviews_per_month, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# leave as is

# change infinite values to NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

# check missing values ----------------------------------------------------

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# impute without flags where there are only a few missing
data <- data %>% 
  mutate( n_days_since_host =  ifelse(is.na(n_days_since_host), mean(n_days_since_host, na.rm = T), n_days_since_host),
          n_host_total_listings_count = ifelse(is.na(n_host_total_listings_count), median(n_host_total_listings_count, na.rm = T), n_host_total_listings_count),
          ln_bathrooms = ifelse(is.na(ln_bathrooms), 1, ln_bathrooms),
          n_bedrooms = ifelse(is.na(n_bedrooms),n_beds%/%2, n_bedrooms)) # assume that there are two beds in a bedroom 

# redo their polinomials and logs
data <- data %>% 
  mutate(n_days_since_host2 = n_days_since_host^2,
         n_days_since_host3 = n_days_since_host^3,
         ln_host_total_listings_count = log(n_host_total_listings_count+1),
         n_bedrooms2 = n_bedrooms^2)

# impute with flags where there are more missing
data <- data %>%
  mutate(
    flag_days_since_rv=ifelse(is.na(n_days_since_rv),1, 0),
    n_days_since_rv =  ifelse(is.na(n_days_since_rv), median(n_days_since_rv, na.rm = T), n_days_since_rv),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0),
    flag_host_response_rate = ifelse(is.na(n_host_response_rate), 1, 0),
    n_host_response_rate = ifelse(is.na(n_host_response_rate), median(n_host_response_rate, na.rm = T), n_host_response_rate),
    flag_host_acceptance_rate = ifelse(is.na(n_host_acceptance_rate), 1, 0),
    n_host_acceptance_rate = ifelse(is.na(n_host_acceptance_rate), median(n_host_acceptance_rate, na.rm = T), n_host_acceptance_rate)
  )

# redo their polinomials and logs
data <- data %>% mutate(
  n_days_since_rv2 = n_days_since_rv^2,
  n_host_acceptance_rate2 = n_host_acceptance_rate^2,
  n_host_response_rate2 = n_host_response_rate^2
)

# save work file
write_csv(data, "data/clean/cape_town_workfile.csv")

# check for interactions --------------------------------------------------

# check property type interactions
p1 <- price_diff_by_variables2(data, "f_property_type", "d_superhost", "Property Type", "Superhost")
p2 <- price_diff_by_variables2(data, "f_property_type", "d_profile_pic", "Property Type", "Profile Picture") # use this
p3 <- price_diff_by_variables2(data, "f_property_type", "d_identity_verified", "Property Type", "Identity Verified")
p4 <- price_diff_by_variables2(data, "f_property_type", "d_instant_bookable" , "Property Type", "Instant Bookable") # use this
p5 <- price_diff_by_variables2(data, "f_property_type", "d_wifi" , "Property Type", "Wifi") # use this
p6 <- price_diff_by_variables2(data, "f_property_type", "d_tv" , "Property Type", "Tv") # use this
p7 <- price_diff_by_variables2(data, "f_property_type", "d_refrigerator", "Property Type", "Refrigerator") # use this
p8 <- price_diff_by_variables2(data, "f_property_type", "d_air_conditioning" , "Property Type", "Air Conditioning")
p9 <- price_diff_by_variables2(data, "f_property_type", "d_sound", "Property Type", "Sound")
p10 <- price_diff_by_variables2(data, "f_property_type", "d_baby", "Property Type", "Baby Friendly") # use this
p11 <- price_diff_by_variables2(data, "f_property_type", "d_beach", "Property Type", "Beach Extras") # use this
p12 <- price_diff_by_variables2(data, "f_property_type", "d_stove", "Property Type", "Stove") # use this
p13 <- price_diff_by_variables2(data, "f_property_type", "d_free_parking", "Property Type", "Free Parking")
p14 <- price_diff_by_variables2(data, "f_property_type", "d_paid_parking", "Property Type", "Paid Parking") #use this


sum_interactions <- plot_grid(p2, p4, p5, p6, p7, p10, p11, p12, p14, nrow=3, ncol=3)
sum_interactions


amenities <- c( "d_wifi", "d_tv", "d_refrigerator", "d_air_conditioning", "d_sound", "d_baby", "d_beach", "d_stove", "d_free_parking", "d_paid_parking" )

X_for_ols <- c('f_property_type * d_profile_pic', 'f_property_type * d_instant_bookable', 'f_property_type * d_wifi', 'f_property_type * d_tv', 'f_property_type * d_refrigerator', 'f_property_type * d_baby', 'f_property_type * d_beach', 'f_property_type * d_stove', 'f_property_type * paid_parking')
X_for_lasso  <- c('f_property_type * d_profile_pic', 'f_property_type * d_identity_verified', 'f_property_type * d_superhost', 'f_property_type * d_instant_bookable', paste0("(f_property_type) * (",
                paste(amenities, collapse=" + "),")"))



