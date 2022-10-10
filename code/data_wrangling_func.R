#### Data Wrangling Function

data_wrangling_func <- function(data, type, final_date){
    # Filter dates and replace NAs with 0
    data <- data %>% select(Date, power) %>% mutate(power = coalesce(power, 0)) %>% filter(Date < as.Date(final_date))
    if(type == "lstm"){
        # Rescale Min Max
        min_max <- c(min(data$power), max(data$power))
        data_lstm <- data %>%
            mutate(power = (power - min_max[1])/(min_max[2] -
                                                             min_max[1]))
        return(list(data = data_lstm, min_max = min_max))
    }
    if(type=="svr"){
        return(data = data)
    }
    if(type=="lgb"){
        return(data = data)
    }
    else break
}
