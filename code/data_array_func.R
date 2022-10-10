###### Simple LSTM Array Function

data_array_func <- function(data, val, initial, assess, skip){
    # Select Appropriate Data
    data_train <- data %>%
        dplyr::select(Date, power) #%>% head((nrow(data) - 14))
    # Creating Rolling Data Splits
    data_rol <- rolling_origin(data_train,
                            initial = initial,
                            assess = assess,
                            skip = skip,
                            cumulative = FALSE)
    # Create Training Predictor Array
    x_train_array <- lapply(data_rol$splits, FUN = function(X){analysis(X)$power})
    x_train_array <- do.call("rbind", x_train_array) %>%
        array(., dim = c(length(x_train_array), initial, 1))
    # Create Training Target Array
    y_train_array <- lapply(data_rol$splits, FUN = function(X){testing(X)$power})
    y_train_array <- do.call("rbind", y_train_array) %>%
        array(., dim = c(length(y_train_array), assess, 1))


    # Select Appropriate Validation Data
    data_val <- val %>%
        dplyr::select(Date, power) #%>% head((nrow(data) - 14))
    # Creating Rolling Data Splits
    data_rol_val <- rolling_origin(data_val,
                               initial = initial,
                               assess = assess,
                               skip = skip,
                               cumulative = FALSE)
    # Create Training Predictor Array
    x_val_array <- lapply(data_rol_val$splits, FUN = function(X){analysis(X)$power})
    x_val_array <- do.call("rbind", x_val_array) %>%
        array(., dim = c(length(x_val_array), initial, 1))
    # Create Training Target Array
    y_val_array <- lapply(data_rol_val$splits, FUN = function(X){testing(X)$power})
    y_val_array <- do.call("rbind", y_val_array) %>%
        array(., dim = c(length(y_val_array), assess, 1))

    return(list(x_train_array = x_train_array,
                y_train_array = y_train_array,
                x_val_array = x_val_array,
                y_val_array = y_val_array))
}
