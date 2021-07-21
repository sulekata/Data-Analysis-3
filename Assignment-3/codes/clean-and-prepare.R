
# setup -------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
library(RColorBrewer)

# import raw data -------------------------------------------------------------
data <- read_csv('data/raw/hotels-vienna.csv')

# inspect variables -------------------------------------------------------

## check variables that probably have no variation
# all observations are for Austria
table(data$country)

# 418 hotels in Vienna, 10 elsewhere
table(data$city_actual)
# check those 8 observations which are not in Vienna
not_vienna <- data %>% filter(city_actual != 'Vienna')
# check the distribution of distance without those 8
describe(data[data$city_actual == 'Vienna',]$distance)
# drop these observations because they are too far from the city centre
data <- data %>% filter(city_actual == 'Vienna')

# all observations are for Vienna
table(data$city)

# center1label is city centre for all observations
table(data$center1label)

# center2label is Donauturm for all observations
table(data$center2label)

# year is 2017
table(data$year)

# month is 11
table(data$month)

# weekend is 0
table(data$weekend)

# holiday is 0
table(data$holiday)

# nnights is 1
table(data$nnights)

# drop variables which have no variation
data <- data %>% mutate(
  country = NULL,
  city = NULL,
  city_actual = NULL,
  center1label = NULL,
  center2label = NULL,
  year = NULL,
  month = NULL,
  weekend = NULL,
  holiday = NULL,
  nnights = NULL
)

# check categorical variables and convert them to factor
# there are 19 neighbourhoods
table(data$neighbourhood)
length(table(data$neighbourhood))

# there are 5 offer categories, group the 50%-75% and 75%+ categories
table(data$offer_cat)

# there are 7 accommodation types, make 3 groups: apartment, hotel, other
table(data$accommodation_type)

# keep scarce room as is
table(data$scarce_room)

# check conditional means
categoricals <- c("scarce_room", "neighbourhood", "offer_cat", "accommodation_type")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# make changes and convert to factor
data <- data %>% mutate( 
        neighbourhood = factor(neighbourhood),
        offer_cat = ifelse( offer_cat == '75%+ offer', '50%+ offer', offer_cat),
        offer_cat = ifelse( offer_cat == '50%-75% offer', '50%+ offer', offer_cat),
        offer_cat = factor(offer_cat),
        accommodation_type = ifelse( accommodation_type == 'Apartment', 'apartment', 
                                     ifelse( accommodation_type == 'Hotel', 'hotel', 'other')),
        accommodation_type = factor(accommodation_type),
        scarce_room = factor(scarce_room),
        offer = factor(offer)
        )

# numerical variables
# stars, rating are between 1 and 5
describe(data$stars)
describe(data$rating)

# distance measures do not have extreme values
# keep rating_count as is

# check target variable: price
describe(data$price)

# histogram for price
hist <- ggplot( data = data, aes(price) ) +
  geom_histogram( fill = brewer.pal( 3, "Set2" )[1], alpha = 0.5 ) +
  theme_bw() +
  labs( x='\n Price', y='Absolute Frequency \n', title = 'Distribution of Price per Night') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )
hist

# drop observations where price is above 750 euros per night (there are 2 of these)
data <- data %>% filter(price < 750)

# missing values ----------------------------------------------------------

# the ratings columns have missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# drop TripAdvisor ratings and their counts because a quarter of them is missing
data <- data %>% mutate(
  ratingta = NULL,
  ratingta_count = NULL
)

# impute missing ratings with mean
# impute missing rating counts with median
# plus add flags for these observations
data <- data %>% mutate( rating_flag = ifelse(is.na(rating), 1, 0),
                         rating = ifelse(is.na(rating), mean(data$rating, na.rm = T), rating),
                         rating_count_flag = ifelse(is.na(rating_count), 1, 0),
                         rating_count = ifelse(is.na(rating_count), median(data$rating_count, na.rm = T), rating_count))

# save workfile -----------------------------------------------------------

write_csv(data, 'data/clean/hotels-vienna_workfile.csv')

