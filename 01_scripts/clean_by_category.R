

library(tidyverse)
library(timetk)
library (zoo)


order_details_tbl <- read_csv("00_data/order_details.csv")

order_tbl <- read_csv("00_data/orders.csv")

pizza_types_tbl <- read_csv("00_data/pizza_types.csv")

pizza_tbl <- read_csv("00_data/pizzas.csv")



# Clean data
pizza_order_tbl <- order_details_tbl %>% 
    # Merge data
    left_join(order_tbl) %>% 
    left_join(pizza_tbl) %>% 
    mutate(total_price=price*quantity) %>% 
    left_join(pizza_types_tbl) %>% 
    
    # Summarize by categories
    group_by(date,category) %>% 
    summarize(revenue=sum(total_price), qty=sum(quantity)) %>% 
    # Fill missing dates
    ungroup() %>% group_by(category) %>% 
    pad_by_time(.date_var = date,
                .by = "day",
                .pad_value = NA) %>% 
    # Impute Missing data using Spline
    #mutate(revenue_ln=ifelse(is.na(revenue),ts_impute_vec(revenue,period=1),revenue)) %>% 
    mutate(revenue=ifelse(is.na(revenue),na.spline(revenue),revenue)) %>% 
    mutate(revenue=ifelse(is.na(qty),na.spline(revenue) ,revenue)%>% round(0))

write_rds(pizza_order_tbl ,"00_data/pizza_category_tbl.rds")
