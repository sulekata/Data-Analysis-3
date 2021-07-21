
# setup -------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
library(RColorBrewer)
library(caret)
library(rpart)
library(kableExtra)

color <- c(brewer.pal( 3, "Set2" )[1], brewer.pal( 3, "Set2" )[2], brewer.pal( 3, "Set2" )[3], brewer.pal( 3, "Set2" )[5])

# import data -------------------------------------------------------------

data <- read_csv('data/clean/hotels-vienna_workfile.csv')


# inspect functional forms for OLS ----------------------------------------

num_vars <- c("rating_count", "stars", "distance", "distance_alter", "rating")
ln_num_vars <- c("ln_rating_count", "ln_distance", "ln_distance_alter") 

# create loess plots for level numerical variables
plist = sapply(num_vars, function(col) {
  ggplot(data, aes_string(x = col, y = "price")) + geom_smooth(method="loess", colour = color[1]) + geom_point() +
    labs( x = paste0("\n", col), y = "price per night \n", title = paste0("Pattern of association between price and ", col)) +
    theme( panel.grid.minor.x = element_blank(), 
           plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) +
    theme_bw()
}, simplify=FALSE)

plist[['rating']]

# add logs and squares of numerical variables
data <- data %>% mutate( ln_rating_count = log(rating_count), 
                         stars_sq = stars^2,
                         ln_distance = ifelse(distance == 0, log(0.01),log(distance)),
                         ln_distance_alter = log(distance_alter),
                         rating_sq = rating^2)

# create loess plots for log numerical variables
plist_ln = sapply(ln_num_vars, function(col) {
  ggplot(data, aes_string(x = col, y = "price")) + geom_smooth(method="loess", colour = color[1]) + geom_point() +
    labs( x = paste0("\n", col), y = "price per night \n", title = paste0("Pattern of association between price and ", col)) +
    theme( panel.grid.minor.x = element_blank(), 
           plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) +
    theme_bw()
}, simplify=FALSE)

plist_ln[['ln_distance']]


# define predictor sets -----------------------------------------------

ols_1 <- c("rating_count", "rating_count_flag", "neighbourhood", "stars", "scarce_room", "offer_cat", "distance", "accommodation_type", "rating", "rating_flag")
ols_2 <- c("ln_rating_count", "rating_count_flag", "neighbourhood", "stars", "stars_sq", "scarce_room", "offer_cat", "ln_distance", "accommodation_type", "rating", "rating_sq", "rating_flag")
ols_3 <- c("rating_count", "rating_count_flag", "neighbourhood", "stars", "scarce_room", "offer_cat", "distance", "accommodation_type", "rating", "rating_flag", "scarce_room * neighbourhood",  "accommodation_type * stars")
ols_4 <- c("ln_rating_count", "rating_count_flag", "neighbourhood", "stars", "stars_sq", "scarce_room", "offer_cat", "ln_distance", "accommodation_type", "rating", "rating_sq", "rating_flag", "scarce_room * neighbourhood", "accommodation_type * stars")
cart <- ols_1
rf <- ols_1


# modelling ---------------------------------------------------------------

# create holdout set
set.seed(890)

train_indices <- as.integer(createDataPartition(data$price, p = 0.8, list = FALSE))
df_train <- data[train_indices, ]
df_holdout <- data[-train_indices, ]

# set the number of folds for cross-validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# OLS ---------------------------------------------------------------------

# OLS 1
set.seed(890)
system.time({
  ols_model1 <- train(
    formula(paste0("price ~", paste0(ols_1, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

summary(ols_model2)

# OLS 2
set.seed(890)
system.time({
  ols_model2 <- train(
    formula(paste0("price ~", paste0(ols_2, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

# OLS 3
set.seed(890)
system.time({
  ols_model3 <- train(
    formula(paste0("price ~", paste0(ols_3, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

# OLS 4
set.seed(890)
system.time({
  ols_model4 <- train(
    formula(paste0("price ~", paste0(ols_4, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})


# CART --------------------------------------------------------------------

set.seed(890)
system.time({
  cart <- train(
    formula(paste0("price ~", paste0(cart, collapse = " + "))),
    data = df_train,
    method = "rpart",
    trControl = train_control,
    control = rpart.control(minsplit = 20),
    tuneGrid= expand.grid(cp = 0.001))
})


# Random Forest -----------------------------------------------------------

tune_grid <- expand.grid(
  .mtry = c( 3, 4, 5 ),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(890)
system.time({
  rf_model <- train(
    formula(paste0("price ~", paste0(rf, collapse = " + "))),
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

# mtry = 5, min.node.size = 5
rf_model$finalModel$mtry
rf_model$finalModel$min.node.size

# model selection ---------------------------------------------------------

final_models <-
  list("OLS 2" = ols_model2,
       "CART" = cart,
       "Random Forest" = rf_model)

results <- resamples(final_models) %>% summary()

# get average CV RMSE
final_rmse <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

# evaluate final models on holdout set
final_holdout <- map(final_models, ~{
  RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

final_combined <- cbind(final_rmse, final_holdout)

# print table
knitr::kable( final_combined, caption = "Model performance comparison", digits = 2 ) %>% kable_styling( position = "center", latex_options = 'hold_position')


# get top 5 deals ---------------------------------------------------------

# estimate all 3 models for entire original data
pred_ols_2 <- predict(ols_model2, data)
pred_cart <- predict(cart, data)
pred_rf <- predict(rf_model, data)

# add fitted values to df
data <- data %>% mutate(
  pred_ols_2 = pred_ols_2,
  pred_cart = pred_cart,
  pred_rf = pred_rf
)

# calculate differences
data <- data %>% mutate(
  diff_ols_2 = price - pred_ols_2,
  diff_cart = price - pred_cart,
  diff_rf = price - pred_rf
)

# get top 5 best deals for OLS 2
top_5_ols <- data %>% top_n( -5 , diff_ols_2 ) %>% 
  select( hotel_id, price, diff_ols_2, distance, stars, rating ) %>% arrange( diff_ols_2 )

names(top_5_ols) <- c("Hotel ID", "Price", "Residual", "Distance", "Stars", "Rating")
top_5_ols <- as.data.frame(top_5_ols)

# compare with chapter 10
# none of the IDs match

# print table
top_5_ols$`Hotel ID` = cell_spec(top_5_ols$`Hotel ID`, color = ifelse(top_5_ols$`Hotel ID` == 22156, color[1], "black"))
knitr::kable( top_5_ols, caption = "Five best deals with OLS 2", digits = 2, escape = F ) %>% kable_styling( position = "center", latex_options = 'hold_position' )

# get top 5 best deals for CART
top_5_cart <- data %>% top_n( -5 , diff_cart ) %>% 
  select( hotel_id, price, diff_cart, distance, stars, rating ) %>% arrange( diff_cart )

names(top_5_cart) <- c("Hotel ID", "Price", "Residual", "Distance", "Stars", "Rating")
top_5_cart <- as.data.frame(top_5_cart)

# compare with chapter 10
# none of the IDs match

# print table
top_5_cart$`Hotel ID` = cell_spec(top_5_cart$`Hotel ID`, color = ifelse(top_5_cart$`Hotel ID` == 22156, color[1],ifelse(top_5_cart$`Hotel ID` == 22114, color[2], "black")))
knitr::kable( top_5_cart, caption = "Five best deals with CART", digits = 2, escape = F ) %>% kable_styling( position = "center", latex_options = 'hold_position' )

# get top 5 best deals for random forest
top_5_rf <- data %>% top_n( -5 , diff_rf) %>% 
  select( hotel_id, price, diff_rf, distance, stars, rating ) %>% arrange( diff_rf )

names(top_5_rf) <- c("Hotel ID", "Price", "Residual", "Distance", "Stars", "Rating")
top_5_rf <- as.data.frame(top_5_rf)

# compare with chapter 10
# none of the IDs match

# print table
top_5_rf$`Hotel ID` = cell_spec(top_5_rf$`Hotel ID`, color = ifelse(top_5_rf$`Hotel ID` == 22114, color[2], "black"))
knitr::kable( top_5_rf, caption = "Five best deals with Random Forest", digits = 2, escape = F ) %>% kable_styling( position = "center", latex_options = 'hold_position' )

# hotels that appear in all three top 5-s
intersect(top_5_ols$`Hotel ID`, top_5_cart$`Hotel ID`)
intersect(top_5_ols$`Hotel ID`, top_5_rf$`Hotel ID`)
intersect(top_5_cart$`Hotel ID`, top_5_rf$`Hotel ID`)
