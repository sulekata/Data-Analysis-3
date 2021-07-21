
# setup -------------------------------------------------------------------

library(tidyverse)

# import raw data ---------------------------------------------------------

gz <- gzfile('data/raw/listings.csv.gz','rt')  
listings <- read.csv( gz, header = T )

# cleaning -----------------------------------------------------

### filter variables which cannot be used for prediction in this task
df <- listings %>% select( -c( listing_url, scrape_id, description, neighborhood_overview, picture_url, host_url, host_name, host_location, host_about, host_thumbnail_url, host_picture_url, host_neighbourhood, host_listings_count, neighbourhood_group_cleansed, bathrooms, minimum_nights, minimum_minimum_nights, minimum_maximum_nights, minimum_nights_avg_ntm, maximum_nights, maximum_minimum_nights, maximum_maximum_nights, maximum_nights_avg_ntm, calendar_updated, has_availability, availability_30, availability_60, availability_90, availability_365, calendar_last_scraped, number_of_reviews_ltm, number_of_reviews_l30d, license ) )

### fix variable types
sapply( df, class )

### convert price variable to numeric
# remove $ sign
df$price <- lapply(
  df$price, 
  function(x) substring( x, 2 )
)

# remove commas
df$price <- as.numeric( gsub(",","",df$price) )

### convert bathrooms_text variable to numeric
# remove 'bath'or 'baths' from string
df$bathrooms <- unlist( lapply(
  df$bathrooms_text,
  function(x) as.numeric( strsplit( x, " ")[[1]][1] )
) )

# replace 'NA' string with NA
df$bathrooms <- ifelse( df$bathrooms == 'NA', NA, as.numeric( df$bathrooms ) )

# drop bathrooms_text variable
df <- df %>% select( -bathrooms_text )

### convert host_response_rate variable to numeric
# replace 'N/A' string with NA
df$host_response_rate <- ifelse( df$host_response_rate == 'N/A', NA, df$host_response_rate )

# remove % sign and convert to numeric
df$host_response_rate <- as.numeric( gsub( "%","",df$host_response_rate ) )

### convert host_acceptance_rate variable to numeric
df$host_acceptance_rate <- ifelse( df$host_acceptance_rate == 'N/A', NA, df$host_acceptance_rate )

# remove % sign and convert to numeric
df$host_acceptance_rate <- as.numeric( gsub( "%","",df$host_acceptance_rate ) )

### convert date columns to date
df$first_review <- as.Date( df$first_review, format="%Y-%m-%d" )

df$last_review <- as.Date( df$last_review, format="%Y-%m-%d" )

df$last_scraped <- as.Date( df$last_scraped, format="%Y-%m-%d" )

df$host_since <- as.Date( df$host_since, format="%Y-%m-%d" )

### extract amenities
# remove unnecessary signs and convert to list
df$amenities <- tolower( df$amenities )
df$amenities <- gsub("\\[","", df$amenities)
df$amenities <- gsub("\\]","", df$amenities)
df$amenities <- gsub('\\"',"",df$amenities)
df$amenities <- as.list(strsplit(df$amenities, ","))

# define levels and dummies and append to df
levs <- levels(factor(unlist(df$amenities)))
df <- cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

# function to aggregate several columns of same type/category into one generic binary column
aggregate_columns <- function(word){
  
  # subset columns which contain a specific word and save them to another dataframe, also select 'id' to use for merge later
  new_df <- df %>% select(contains(word),"id")
  
  # go row by row to see if any of the rows have a 1, if it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # save new column and id column to another dataframe, this new dataframe is used to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  # merge original dataframe and new_df_merge by 'id'
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  # remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))

  # remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <<- df %>% select(-colnames(new_df))
}

# aggregate columns for a few amenities that could be important for predicting price
aggregate_columns("wifi")
df <- df %>% rename("wifi" = col_name)

aggregate_columns("tv")
df <- df %>% rename("tv" = col_name)

aggregate_columns("refrigerator")
df <- df %>% rename("refrigerator" = col_name)

aggregate_columns("air conditioning")
df <- df %>% rename("air_conditioning" = col_name)

aggregate_columns("sound")
df <- df %>% rename("sound" = col_name)

aggregate_columns("baby")
df <- df %>% rename("baby" = col_name)

aggregate_columns("beach")
df <- df %>% rename("beach" = col_name)

aggregate_columns("stove")
df <- df %>% rename("stove" = col_name)

aggregate_columns("free parking")
df <- df %>% rename("free_parking" = col_name)

aggregate_columns("paid parking")
df <- df %>% rename("paid_parking" = col_name)

# drop the amenities column because a csv cannot store it since it is a list
df <- df %>% select( -amenities )

# drop amenities that were not used
df <- df[ -c( 41:497 )]

# filter the data frame for apartments that can accommodate 2-6 guests --------

# check room_type variable
table(df$room_type)

# keep if room_type is entire home/apartment
df <- df %>% filter( room_type == 'Entire home/apt')

# drop room_type variable
df <- df %>% select( -room_type )

# check property_type variable
table(df$property_type)

# keep if property_type suggests that the place is an apartment
df <- df %>% 
  filter( property_type %in% c('Entire apartment', 'Entire loft', 'Entire serviced apartment' ) )

# keep apartments which can host 2-6 guests
df <- df %>%
  filter( accommodates %in% c(2:6) )


# create factors ----------------------------------------------------------

# rename property_type categories to make them shorter
df <- df %>% mutate( property_type = ifelse( property_type == 'Entire apartment', 'apartment',
                                             ifelse( property_type == 'Entire loft', 'loft',
                                                     ifelse( property_type == 'Entire serviced apartment', 'serviced_apartment', "."))))
# convert property_type to factor
df <- df %>%
  mutate(f_property_type = factor(property_type))

# convert neighbourhood_cleansed to factor
df <- df %>% 
  mutate( f_neighbourhood = factor(neighbourhood_cleansed))

# create numerical variables ----------------------------------------------

# create days since first review
df <- df %>% mutate(
              n_days_since_rv = as.numeric(last_scraped - first_review) )

# create days since host registered
df <- df %>% mutate(
  n_days_since_host = as.numeric(last_scraped - host_since) )

# add new numeric columns from certain columns
numericals <- c("host_response_rate", "host_acceptance_rate", "host_total_listings_count", "accommodates", "bedrooms", "beds", "number_of_reviews", "review_scores_rating", "calculated_host_listings_count", "reviews_per_month", "bathrooms")                                 
                               
df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)

# create dummies ----------------------------------------------------------

# create dummies
df <- df %>% mutate(
  d_superhost = ifelse( host_is_superhost == 't', 1, 0),
  d_profile_pic = ifelse( host_has_profile_pic == 't', 1, 0),
  d_identity_verified = ifelse( host_identity_verified == 't', 1, 0),
  d_instant_bookable = ifelse( instant_bookable == 't', 1, 0)
)

# rename amenities dummies
dummies <- c( "wifi", "tv", "refrigerator", "air_conditioning", "sound", "baby", "beach", "stove", "free_parking", "paid_parking" )
df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


# filter needed variables -------------------------------------------------

# keep columns if they start with d_, n_, f_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed, property_type)

# check if price is missing
nrow(df %>% filter( is.na(price)))

# save the data frame -----------------------------------------------------

write_csv(df,"data/clean/cape_town_clean.csv")
