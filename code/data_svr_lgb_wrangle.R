#### Function for SVR data wrangling


data_svr_lgb_wrangle <- function(data_array = data_array, type){

    if(type == "svr")
    {
        # Create training list for each forcast ahead
        x_train <- data_array$x_train_array[,,1] %>% as_tibble()
        y_train <- data_array$y_train_array[,,1] %>% split(., rep(1:ncol(.), each = nrow(.)))
        train <- map(y_train, ~cbind(Y=.x, x_train))

        # Create validation list for each forecast ahead

        x_val <- data_array$x_val_array[,,1] %>% as_tibble()
        y_val <- data_array$y_val_array[,,1] %>% split(., rep(1:ncol(.), each = nrow(.)))
        validation <- map(y_val, ~cbind(Y=.x, x_val))

        return(list(train_list = train, val_list = validation))
    }
    if(type == "lightgbm"){
        # Create training list for each forcast ahead
        x_train <- data_array$x_train_array[,,1]%>% as_tibble()
        y_train <- data_array$y_train_array[,,1] %>% split(., rep(1:ncol(.), each = nrow(.)))
        train <- map(y_train, ~cbind(Y=.x, x_train))

        train_pred <- map(train, ~.x[,-1] %>% as.matrix())
        train_lgb <- map(train, ~.x[,-1] %>% as.matrix() %>% Matrix::Matrix(., sparse = TRUE) %>% lgb.Dataset(data = ., label = .x[,1]))

        # Create validation list for each forecast ahead

        x_val <- data_array$x_val_array[,,1]
        y_val <- data_array$y_val_array[,,1] %>% split(., rep(1:ncol(.), each = nrow(.)))
        validation <- map(y_val, ~cbind(Y=.x, x_val))
        validation <- map(validation, ~.x[,-1] %>% as.matrix())

        return(list(train_list = train_lgb, train_err_list = train_pred, val_list = validation))
    }

    else break
}
