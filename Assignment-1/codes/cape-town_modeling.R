# setup -------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(rattle)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)
library(rpart)
library(pdp)

# import data -------------------------------------------------------------

data <- read_csv("data/clean/cape_town_workfile.csv")

# filter for price
data <- data %>% filter( price < 5000)

# descriptive statistics --------------------------------------------------

# boxplot of price by property type
ggplot(data = data, aes(x = f_property_type, y = price)) +
  stat_boxplot(aes(group = f_property_type), geom = "errorbar", width = 0.3, size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_property_type),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  #scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Property type",y = "Price (South-African rand)")

# preparation for modeling ------------------------------------------------

# group variables
target_var <- 'price'

basic_vars <- c('f_property_type', 'f_neighbourhood', 'n_accommodates', 'n_bedrooms', 'n_beds', 'd_instant_bookable', 'n_bathrooms')

host_vars <- c('n_days_since_host', 'n_host_response_rate', 'flag_host_response_rate','n_host_acceptance_rate', 'flag_host_acceptance_rate', 'd_superhost', 'd_profile_pic', 'd_identity_verified', 'n_host_total_listings_count')

reviews <- c(  'n_number_of_reviews', 'flag_number_of_reviews', 'n_days_since_rv', 'flag_days_since_rv', 'n_review_scores_rating', 'flag_review_scores_rating','n_reviews_per_month', 'flag_reviews_per_month')

amenities <- c("d_wifi", "d_tv", "d_refrigerator", "d_air_conditioning", "d_sound", "d_baby", "d_beach", "d_stove", "d_free_parking", "d_paid_parking")

transformed_vars <- c( 'n_beds2', 'ln_bathrooms', 'ln_number_of_reviews', 'n_days_since_rv2', 'n_days_since_host2', 'n_days_since_host3', 'n_host_acceptance_rate2', 'n_host_response_rate2', 'ln_host_total_listings_count', 'n_bedrooms2')

X_for_ols <- c('f_property_type * d_profile_pic', 'f_property_type * d_instant_bookable', 'f_property_type * d_wifi', 'f_property_type * d_tv', 'f_property_type * d_refrigerator', 'f_property_type * d_baby', 'f_property_type * d_beach', 'f_property_type * d_stove', 'f_property_type * d_paid_parking')

X_for_lasso  <- c('f_property_type * d_profile_pic', 'f_property_type * d_identity_verified', 'f_property_type * d_superhost', 'f_property_type * d_instant_bookable', paste0("(f_property_type) * (",                                                                                                                                                                              paste(amenities, collapse=" + "),")"))

# group predictors for models

predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, host_vars, reviews, amenities)
predictors_3 <- c(basic_vars[1:6], host_vars[1:9], reviews[2:8], amenities, transformed_vars)
predictors_4 <- c(predictors_3, X_for_ols)
predictors_5 <- c(predictors_3, X_for_lasso)

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

# simplest model
set.seed(8)
system.time({
  ols_model1 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs1 <-  ols_model1$finalModel$coefficients
ols_model_coeffs_df1 <- data.frame(
  "variable" = names(ols_model_coeffs1),
  "ols_coefficient" = ols_model_coeffs1
) %>%
  mutate(variable = gsub("`","",variable))

# model with transformed variables
set.seed(8)
system.time({
  ols_model2 <- train(
    formula(paste0("price ~", paste0(predictors_3, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs2 <-  ols_model2$finalModel$coefficients
ols_model_coeffs_df2 <- data.frame(
  "variable" = names(ols_model_coeffs2),
  "ols_coefficient" = ols_model_coeffs2
) %>%
  mutate(variable = gsub("`","",variable))

# model with transformed variables plus interactions
set.seed(8)
system.time({
  ols_model3 <- train(
    formula(paste0("price ~", paste0(predictors_4, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs3 <-  ols_model3$finalModel$coefficients
ols_model_coeffs_df3 <- data.frame(
  "variable" = names(ols_model_coeffs3),
  "ols_coefficient" = ols_model_coeffs3
) %>%
  mutate(variable = gsub("`","",variable))


# OLS with LASSO ----------------------------------------------------------

# transformed numeric variables, no interactions
set.seed(8)
system.time({
  lasso_model1 <- train(
    formula(paste0("price ~", paste0(predictors_4, collapse = " + "))),
    data = df_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 1, by = 0.05)),
    trControl = train_control
  )
})

print(lasso_model1$bestTune$lambda)

lasso_coeffs1 <- coef(
  lasso_model1$finalModel,
  lasso_model1$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`) 

lasso_coeffs_non_null1 <- lasso_coeffs1[!lasso_coeffs1$lasso_coefficient == 0,]

print(nrow(lasso_coeffs_non_null1))

# transformed numeric variables plus interactions
set.seed(8)
system.time({
  lasso_model2 <- train(
    formula(paste0("price ~", paste0(predictors_5, collapse = " + "))),
    data = df_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 1, by = 0.05)),
    trControl = train_control
  )
})

print(lasso_model2$bestTune$lambda)

lasso_coeffs2 <- coef(
  lasso_model2$finalModel,
  lasso_model2$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`) 

lasso_coeffs_non_null2 <- lasso_coeffs2[!lasso_coeffs1$lasso_coefficient == 0,]

print(nrow(lasso_coeffs_non_null2))

# put the coefficients in one table
regression_coeffs <- merge(ols_model_coeffs_df3, lasso_coeffs_non_null2, by = "variable", all=TRUE)
names(regression_coeffs) <- c('Variable', 'OLS 3', 'LASSO 2')
# compare OLS and LASSO performance

temp_models <-
  list("OLS 1" = ols_model1,
       "OLS 2" = ols_model2,
       "OLS 3" = ols_model3,
       "LASSO 1 (few interactions)" = lasso_model1,
       "LASSO 2 (all interactions)" = lasso_model2)

result_temp <- resamples(temp_models) %>% summary()

# get test RMSE
result_rmse <- imap(temp_models, ~{
  mean(result_temp$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

# get holdout RMSE
result_holdout <- map(temp_models, ~{
  RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

# merge the two
result_combined <- cbind(result_rmse, result_holdout )

# calculate number of variables in each model
num_coefs <-  c(
  length(ols_model1$coefnames),
  length(ols_model2$coefnames),
  length(ols_model3$coefnames),
  nrow(lasso_coeffs_non_null1),
  nrow(lasso_coeffs_non_null2))

ncoefs <- as.data.frame(num_coefs, row.names = rownames(result_combined)
) %>% rename("Number of Coefficients" = "num_coefs")

# merge the three
result_combined <- cbind(ncoefs, result_rmse, result_holdout )

# fitted vs actual values for LASSO 2
# target variable
Ylev <- df_holdout[["price"]]

# get predicted values
predictionlev_holdout_pred <- as.data.frame(predict(lasso_model2, newdata = df_holdout))

# rename column
names(predictionlev_holdout_pred) <- "fit"

# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout_pred[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev),  size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 4000, yend =5000), size=0.5, linetype=2) +
  coord_cartesian(xlim = c(0, 4000), ylim = c(0, 5000)) +
  # scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 4000), breaks=seq(0, 350, by=50)) +
  # scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (South-African rand)", x = "Predicted price  (South African rand)")
level_vs_pred


# CART --------------------------------------------------------------------

# tree with stopping parameter
set.seed(7)
system.time({
  cart1 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "rpart",
    trControl = train_control,
    control = rpart.control(minsplit = 20),
    tuneGrid= expand.grid(cp = 0.002))
})

# tree graph
fancyRpartPlot(cart1$finalModel, sub = "")

# variable importance plot
cart1_var_imp <- varImp(cart1)$importance
cart1_var_imp_df <-
  data.frame(varname = rownames(cart1_var_imp),imp = cart1_var_imp$Overall) %>%
  mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

cart1_var_imp_plot <- ggplot(cart1_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point( size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1))
cart1_var_imp_plot

# Random Forest -----------------------------------------------------------

# simpler model
tune_grid <- expand.grid(
  .mtry = c(6, 8, 10),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

set.seed(8)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

# more complex model
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(8)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

# save results
results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  ))

rf_tuning_model2 <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

# tuning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size
),
nrow=2, ncol=2,
dimnames = list(c("Model 1", "Model 2"),
                c("Min vars","Min nodes"))
)

# RMSE of models
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`)
),
nrow=2, ncol=1,
dimnames = list(c("Model 1", "Model 2"),
                c(results$metrics[2]))
)

# grouped variable importance plot
varImp(rf_model_2)$importance
rf_model_var_imp <- varImp(rf_model_2)$importance
rf_model_var_imp_df <-
  data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
rf_model_var_imp_df[rf_model_var_imp_df$varname %in% amenities,]
# grouped variable importance 2
# basic variables, host variables, review variables and amenities are grouped --> TO THE R MARKDOWN
rf_model_var_imp_df <- rf_model_var_imp_df %>% mutate(
  group2 = ifelse(varname %in% amenities, 'amenities',
                  ifelse(varname %in% basic_vars, 'basic_vars',
                         ifelse(varname %in% reviews, 'reviews', 'host_info'))))
rf_model_var_imp_grouped2 <- rf_model_var_imp_df %>%  group_by(group2) %>% summarise(group_imp_sum = sum(imp_percentage)) %>%  arrange(desc(group_imp_sum))
rf_model_var_imp_grouped2_plot <-
  ggplot(rf_model_var_imp_grouped2, aes(x=reorder(group2, group_imp_sum), y=group_imp_sum)) +
  geom_point(color="navyblue", size=1) +
  geom_segment(aes(x=group2,xend=group2,y=0,yend=group_imp_sum), color="navyblue", size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_var_imp_grouped2_plot


# Partial Dependence Plot -------------------------------------------------

pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(df_holdout, "n_accommodates"), train = df_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point( size=2) +
  geom_line( size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(2,6), breaks=seq(1,7,1))
pdp_n_acc_plot

# Model selection ---------------------------------------------------------

final_models <-
  list("OLS3" = ols_model3,
       "LASSO (all interactions)" = lasso_model2,
       "CART" = cart1,
       "Random forest" = rf_model_2)

results <- resamples(final_models) %>% summary()
results

# evaluate final models on holdout set
final_rmse <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

final_holdout <- map(final_models, ~{
  RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

final_combined <- cbind(final_rmse, final_holdout)

# Evaluate model on subsets -----------------------------------------------
data_holdout_w_prediction <- df_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = df_holdout))

######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 4, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood %in% c("Ward 115", "Ward 83", "Ward 60", "Ward 64", "Ward 71", "Ward 103")) %>%
  group_by(f_neighbourhood) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("apartment", "serviced apartment")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Neighbourhood", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)

