
# HOW MUCH YOU WILL SPENT FOR A PIZZA----

# LIBRARIES AND DATA----

# Core
library(tidyverse)
library(lubridate)
library(timetk)

# Time serires ml
library(modeltime)
library(tidymodels)

# Parallel processing
library(future)
library(doFuture)
library(tictoc)



# Read data----
order_details_tbl <- read_csv("00_data/order_details.csv")

order_tbl <- read_csv("00_data/orders.csv")

pizza_types_tbl <- read_csv("00_data/pizza_types.csv")
pizza_types_tbl

pizza_tbl <- read_csv("00_data/pizzas.csv")

pizza_order_tbl <- order_details_tbl %>% 
    left_join(order_tbl) %>% 
    left_join(pizza_tbl) %>% 
    mutate(total_price=price*quantity) %>% 
    left_join(pizza_types_tbl)
    
pizza_order_tbl %>% write_rds("00_data/pizza_order_tbl.rds")
pizza_order_tbl


pizza_order_clean_tbl <- pizza_order_tbl %>%     
    group_by(date,order_id, category,pizza_type_id) %>% 
    summarize(quantity=sum(quantity),total_price=sum(total_price),avg_spent_per_order=total_price/quantity) %>% 
    ungroup()

pizza_timeseries_data <- pizza_order_clean_tbl %>% 
    group_by(date,category,pizza_type_id) %>% 
    summarize(quantity=sum(quantity),total_sales=sum(total_price)) %>% ungroup() %>% 
    pivot_longer(total_sales)
pizza_timeseries_data %>% write_rds("00_data/pizza_timeseries_data.rds")


pizza_timeseries_data %>% 
    group_by(date,category) %>% 
    summarize(value=sum(value)) %>% ungroup() %>% 
    plot_time_series(.date_var=date,.value=value,
                     .color_var = category,
                     .facet_vars = category,
                     .smooth = FALSE)

#Summarise sales
pizza_tot_order_tbl <- pizza_timeseries_data %>% 
    #group_by(category) %>% 
    # summarise(n()) %>% 
    #ungroup() %>% 
    summarize_by_time(.date_var =date,.by = "day", value=sum(value) ) %>% 
    
    # Add mising time points
    pad_by_time(.date_var = date,
                .by = "auto",
                .pad_value = NA) %>% 
    mutate(value=ifelse(is.na(value),ts_impute_vec(value,period=1),value)) %>%
    # Perform a log transform
    mutate(value=log_interval_vec(value,limit_lower=0,offset = 1)) %>%
    mutate(value=standardize_vec(value))

# Save standard factors
limit_lower <- 0
offset      <- 1
limit_upper <- 4739.77
mean        <- -0.0685455764458231
sd          <- 0.390247405694287


# 2.0 FEATURE ENGINEERING----

horizon=30
lag_period=c(7,14,21,30)
rolling_periods <- c(7,14,21,30)

data_prepared_full_tbl <- pizza_tot_order_tbl  %>% 
    
    # Add feauture window
    bind_rows(
        future_frame(.data=.,.date_var=date,.length_out="1 month")
    ) %>% 
    
    # # Add lag
    # tk_augment_lags(value,.lags=lag_period) %>%
    # 
    # # Add rolling features
    # tk_augment_slidify(
    #     .value   = value_lag21,
    #     .f       = base::mean,
    #     .period  = rolling_periods,
    #     .align   = "center",
    #     .partial = TRUE
    #     ) %>%
    # # Rename cols
    # rename_with(.cols = contains("lag"),.fn = ~str_c("lag_",.)) %>%

    # # Pizza always have offers in specific days of the week
    tk_augment_holiday_signature(
        .date_var = date,
        .holiday_pattern = "US",
        .locale_set = "US",
        .exchange_set = "all"
     )
data_prepared_full_tbl %>% glimpse()


# ## Create Holidays feature----
# 
# holidays <- data_prepared_full_tbl %>% 
#     ungroup() %>% 
#     select(date) %>% distinct(date,.keep_all = T) %>% 
#     tk_augment_holiday_signature(
#         .date_var = date,
#         .holiday_pattern = "US",
#         .locale_set = "US",
#         .exchange_set = "all"
#         ) %>% 
#     select(date,matches("US")) %>% 
#     pivot_longer(-date) %>% 
#     select(date,holiday=value) %>% 
#     distinct(date,.keep_all = T)
# 
# holidays 

# Join holidays data with sales data
data_prepared_full_tbl <- data_prepared_full_tbl #%>% left_join(holidays, by=c("date"="date"))
data_prepared_full_tbl

# 3.0 DATA EXPLORATION----

data_prepared_full_tbl %>% 
    filter(!is.na(value)) %>%
    drop_na() %>% 
    plot_time_series_regression(date,
                     value~as.numeric(date)+
                         wday(date,label=TRUE)+
                         month(date,label=T)+.,
                     .show_summary=TRUE)

## Lag 21 and 90 are key in the function

#data_prepared_full_tbl %>% tail(horizon+1)

# 4.0 SPLIT DATA----

data_prepared_tbl <- data_prepared_full_tbl %>% filter(!is.na(value))

forecast_tbl <- data_prepared_full_tbl %>% filter(is.na(value))


# 5.0 TRAIN/VALIDATION SPLIT----


split_obj <- time_series_split(data_prepared_tbl,
                               assess = "1 months", 
                               cumulative = TRUE)
# Check if the split was successfull
split_obj %>%
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date,value)

training_tbl <- training(split_obj)
testing_tbl <- testing(split_obj)

# 6.0 RECIPE----

recipe_spec_base <- recipe(value~., data=training_tbl) %>% 
    
    # Tim seris signature
    step_timeseries_signature(date) %>% 
    
    # Remove unnecessary columns
    step_rm(matches("am.pm|hour|minute|second|iso|year|xts")) %>% 
    
    # Normalize
    step_normalize(matches("index.num")) %>% 
    
    # Encode
    step_dummy(all_nominal(),one_hot = TRUE) %>% 
    
    step_zv() #%>% 
    
    # #Interaction
    # step_interact(~matches("week2")*matches("wday.lbl")) %>%
    
    # # #Fourier
    # step_fourier(date, period = c(21,90),K=2) %>%

    # # Add spline
    # step_ns(ends_with("index.num"),deg_free = 2)
    

recipe_spec_base %>% prep() %>% juice() %>% glimpse()

recipe_spec_base_main <- recipe_spec_base %>% 
    step_rm(date)# %>% 
    # step_naomit(starts_with("lag")) %>%
    # step_normalize(contains("lag"))

recipe_spec_base_main %>% prep() %>% juice() %>% glimpse()

# 7.0 FIT MODELS----
## Linear regression model---
model_lm <- linear_reg() %>% 
    set_engine("lm") 

## ARIMA Regression----
model_fit_arima <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(value~date
        +wday(date)
        +week(date)
        +qday(date)
        +day(date)
        +quarter(date),
        data=training(split_obj))

## GLMNET model----
model_glmnet <- linear_reg(penalty = 0.01,
                           mixture=0.3) %>% 
    set_engine("glmnet")

## Prophet Model----
model_fit_prophet <- prophet_reg(
    seasonality_weekly = TRUE,
    seasonality_daily  = TRUE,
    seasonality_yearly = TRUE,
    changepoint_num = 100,
    prior_scale_holidays = TRUE

) %>% 
    set_engine("prophet") %>% 
    fit(value~.,
        data=training(split_obj))

## Prophet Boost Model Tuned----
set.seed(123)
model_fit_prophet_boost_tune <- prophet_boost(
    seasonality_weekly = F,
    seasonality_daily  = F,
    seasonality_yearly = F,
    changepoint_num = 30,
    changepoint_range = 0.8,
    
    #Xgboost params
    mtry           = tune(), # Causes error if less than 1. 
    min_n          = tune(), 
    tree_depth     = tune(), 
    learn_rate     = tune(), 
    loss_reduction = tune(), 
    trees          = tune()
    
) %>% 
    set_engine("prophet_xgboost")

set.seed(123)
resamples_kfold <- training(split_obj) %>% vfold_cv(v=10)

registerDoFuture()
n_cores <- parallel::detectCores()
plan(strategy = cluster,workers=parallel::makeCluster(n_cores))

wkflow_fit_phrophet_boost_tune <- workflow() %>% 
    add_model(model_fit_prophet_boost_tune) %>% 
    add_recipe(recipe_spec_base)

set.seed(123)
recipe_spec_prophet_boost <- tune_grid(
    object    =wkflow_fit_phrophet_boost_tune,
    resamples = resamples_kfold,
    control = control_grid(verbose=T, allow_par=T)
    )

# set.seed(123)
# grid_spec_prophet_boost_3 <- grid_latin_hypercube(
#     mtry(range = c(1, 25)),
#     min_n(range = c(3, 20)),
#     tree_depth(range = c(3, 10)),
#     learn_rate(range = c(-1.5, -0.8)),
#     loss_reduction(),
#     size = 15
# )

best_results_prophet_boost <- recipe_spec_prophet_boost  %>%
    show_best(metric = "rmse", n = 10)

wflw_fit_phrophet_boost <- wkflow_fit_phrophet_boost_tune %>%
    finalize_workflow(parameters = best_results_prophet_boost %>% 
                          slice(1)) %>%
    fit(training(split_obj))

# * End Parallel Processing ----

plan(sequential)

## TBATS----

model_fit_tbats <- seasonal_reg(
    seasonal_period_1 = 7,
    seasonal_period_2 = 30,
    seasonal_period_3 = 364/2
) %>% 
    set_engine("tbats") %>% 
    fit(value~date,data=training(split_obj))

## ETS----
model_fit_ets <- exp_smoothing(
    error = "additive",
    trend = "additive",
    season = "additive"
) %>% 
    set_engine("ets") %>% 
    fit(value~date,data=training(split_obj))

## RF----
model_rf <- rand_forest(mod="regression",
            min_n = 20,
            mtry = 3,
            trees = 300) %>% set_engine("ranger")

# 8.0 WORKFLOW----    
workflow_fit_lm <- workflow() %>%
    add_model(model_lm) %>%
    add_recipe(recipe_spec_base_main) %>%
    fit(training(split_obj))

# workflow_fit_arima <- workflow() %>%
#     add_model(model_arima) %>%
#     add_recipe(recipe_spec_base_main) %>%
#     fit(training(split_obj))

workflow_fit_lm %>%
    pull_workflow_fit() %>%
    pluck("fit") %>%
    summary()

workflow_fit_glmnet <- workflow() %>%
    add_model(model_glmnet) %>%
    add_recipe(recipe_spec_base_main) %>%
    fit(training(split_obj))

wrkflow_fit_rf <- workflow() %>% 
    add_model(model_rf) %>% 
    add_recipe(recipe_spec_base_main) %>% 
    fit(training(split_obj))

# wrkflow_fit_phrophet_boost <- workflow() %>% 
#     add_model(model_fit_prophet) %>% 
#     add_recipe(recipe_spec_base) %>% 
#     fit(training(split_obj))

# 9.0 CALIBRATION----

modeltime_tbl <- modeltime_table(
    #workflow_fit_lm,# Lineal model is off
    model_fit_arima,
    workflow_fit_glmnet,
    model_fit_prophet,
    #wkflow_fit_phrophet_boost_tune,
    wflw_fit_phrophet_boost,
    model_fit_ets,
    model_fit_tbats,
    wrkflow_fit_rf
    ) %>% 
    update_model_description(4, "Prophet Boost - Tuned") 

calibration_tbl <- modeltime_tbl %>% 
    modeltime_calibrate(new_data = testing_tbl)

# FORECAST & TESTING ACCURACY----

calibration_tbl %>% 
    modeltime_forecast(new_data = testing(split_obj),
                       actual_data = data_prepared_tbl) %>% 
    plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy() %>% arrange(rmse)

# Refit model
refit_tbl <- calibration_tbl %>% modeltime_refit(data=data_prepared_tbl)


refit_tbl %>% 
    modeltime_forecast(new_data = forecast_tbl,
                       actual_data = data_prepared_tbl) %>% 
    mutate(across(
        .value:.conf_hi,
        .fns=~standardize_inv_vec(
            x   =.,
            mean=mean,
            sd  =sd
            )
        )) %>% 
    mutate(across(
        .value:.conf_hi,
        .fns=~log_interval_inv_vec(
            x          =.,
            limit_lower=limit_lower,
            limit_upper=limit_upper,
            offset     =offset
            )
        )) %>% 
    #Inverse Transformation
    
    plot_modeltime_forecast(.conf_interval_show = F,.title = "Pizza Revenue Forecast")


# Tune  models RF and Prophet boost


