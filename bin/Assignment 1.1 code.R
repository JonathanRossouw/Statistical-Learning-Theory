# Load Packages

library(tidyverse)
pacman::p_load(cowplot, ggplot2, kableExtra, rsample, glue, lubridate)

# Read in Data


data <- read.csv("data/household_power_consumption.csv", header = TRUE, sep = ';')

# Wrangle Data

data_power <- data %>% as_tibble() %>%
  mutate(date = as.character(as.Date(Date, tryFormats =c("%d/%m/%Y")))) %>%
  mutate(date = paste(date, Time)) %>%
  mutate(date_time = as_datetime(date)) %>%
  rename(power = Global_active_power) %>%
  select(date_time, power) %>%
  mutate(power = as.numeric(power))

data_power <- data_power %>% mutate(date_time = floor_date(date_time, unit="hour")) %>%
  group_by(date_time) %>%
  summarise(power = mean(power, na.rm=TRUE))

data_power <-
  data_power %>% mutate(year = year(date_time), month = month(date_time)) %>%
  group_by(year, month) %>%
  mutate(avg = mean(power, na.rm=T)) %>%
  ungroup() %>%
  mutate(power = coalesce(power, avg)) %>% select(-c(avg, year, month))

data_power %>%
  mutate(Date = as.Date(date_time)) %>%
  ggplot() +
  geom_line(aes(x = Date, y = power), alpha = 0.8, size = 0.6) +
  labs(title = "Power usage over full 47 Months", x = "", y = "Power Usage") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        plot.title = ggplot2::element_text(size = 10),
        axis.title.y = element_text(size = 9))

# Create train, validation and test sets
data_train <- data_power %>% rename(Date = date_time) %>% filter(Date < as.Date("2009-01-01")) %>% tail(-48) %>% head(-7)

data_val <- data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2009-01-01") & Date < as.Date("2010-01-01")) %>% tail(-48)

data_test <- data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2010-01-01")) %>% tail(-58)


# Load python virtual environment to run Keras and Tensorflow

pacman::p_load(reticulate)

reticulate::use_virtualenv("/Users/jonathanrossouw/Desktop/SLT/Assignment 1_ Fundamentals of Statistical Learning Theory./Assignment 1.1/.venv", require = TRUE)
reticulate::py_config()

# Load Tensorflow and Keras
library(tensorflow)
library(keras)

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

### Wrangle and Plot LSTM Data
# Wrangle data

data_lstm <- data_wrangling_func(data = data_power %>% rename(Date = date_time),
                                 type = "lstm",
                                 final_date = "2009-01-01")
data_lstm_val <- data_wrangling_func(data = data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2009-01-01")),
                                     type = "lstm",
                                     final_date = "2010-01-01")
# Set min and max for later transformations
min_max <- data_lstm$min_max
data_lstm_train <- data_lstm$data

val_min_max <- data_lstm_val$min_max
data_lstm_val <- data_lstm_val$data
# Plot Rescaled LSTM data
data_lstm_train %>%
    ggplot() +
    geom_line(aes(x = Date, y = power), alpha = 0.8, size = 0.6) +
    labs(title = "Power Usage Training after Min-Max Rescaling", x = "", y = "Power Usage") +
    theme_minimal_hgrid() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          plot.title = ggplot2::element_text(size = 10),
          axis.title.y = element_text(size = 7))

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

### Create LSTM Data Arrays
data_lstm_lists <- data_array_func(data = data_lstm_train,
                                   val = data_lstm_val,
                                   initial = 2*24, assess = 12, skip = 11)

##### Set hyperparameter grid
lstm_hyp <- expand.grid(loss = c("mse"),
                        optimizer = c("adam"),
                        epochs = c(20, 50, 100, 150),
                        lstm_layers = c(1,2),
                        lstm_units = c(20, 50),
                        return_sequences = c(TRUE, FALSE),
                        dropout_rate = c(0, 0.1)) %>%
    filter(!(return_sequences == TRUE & lstm_layers == 1)) %>%
    split(., seq(nrow(.)))


# Hyperparameter tuning wrapper

hyp_tune_func <- function(data_lists = data_lists,
                          data_actual = data_val,
                          hyperparams = hyp,
                          fit_func,
                          type,
                          min_max = val_min_max){
    # Run iteratively through hyperparameter grid
    if(type == "lstm")
    {
        hyp_fit <- map(hyperparams, fit_func, data_lists, min_max)
    }
    else{hyp_fit <- map(hyperparams, fit_func, data_lists)}

    hyp_res <- map(hyp_fit, ~data_actual %>% mutate(pred = .x) %>%
                       summarise(mean((pred - power)^2, na.rm = TRUE)) %>% .[[1]])
    # Store Results
    res <- map2_df(hyperparams, hyp_res, ~ .x %>%
                       as_tibble() %>%
                       mutate(mse = .y))
    res
}


##### LSTM Fit Function

lstm_fit_func <- function(hyperparams,
                          data_lists,
                          min_max){
    # Set seed for reproducibility
    set_random_seed(seed = 43675)
    # Create Keras Sequential Model
    lstm_model <- keras_model_sequential()
    # Add Layers
    lstm_model %>%
        layer_lstm(units = hyperparams$lstm_units, # size of the layer
                   input_shape = c(48, 1),
                   # batch size, timesteps, features
                   return_sequences = hyperparams$return_sequences,
                   stateful = FALSE) %>%
        layer_dropout(rate = hyperparams$dropout_rate)
    if(hyperparams$lstm_layers == 2 & hyperparams$return_sequences == TRUE)
    {
        lstm_model %>%
            layer_lstm(units = hyperparams$lstm_units, # size of the layer
                       input_shape = c(48, 1),
                       # batch size, timesteps, features
                       return_sequences = FALSE,
                       stateful = FALSE) %>%
            layer_dropout(rate = hyperparams$dropout_rate)
    }
    lstm_model %>%
        layer_dense(units = 12)
    # Set model parameters
    lstm_model %>%
        compile(loss = hyperparams$loss, optimizer = hyperparams$optimizer, metrics = hyperparams$loss)
    # Fit Model
    lstm_model %>%
        fit(
            x = data_lists$x_train_array,
            y = data_lists$y_train_array,
            epochs = hyperparams$epochs,
            verbose = 1,
            shuffle = FALSE)
    # Predict training data
    lstm_train_forecast <- lstm_model %>%
        predict(data_lists$x_train_array) %>% t(.) %>% c(.)
    lstm_train_forecast <- (lstm_train_forecast * (min_max$train[2] - min_max$train[1]) + min_max$train[1])
    # Predict power
    lstm_val_forecast <- lstm_model %>%
        predict(data_lists$x_val_array) %>% t(.) %>% c(.)
    lstm_val_forecast <- (lstm_val_forecast * (min_max$val[2] - min_max$val[1]) + min_max$val[1])
    # Store Results
    return(list(train = lstm_train_forecast, val = lstm_val_forecast))
}

# Perform hyperparameter tuning for LSTM

lstm_fit <- hyp_tune_func(data_lists = data_lstm_lists,
                          data_actual = data_val,
                          hyperparams = lstm_hyp,
                          fit_func = lstm_fit_func,
                          min_max = val_min_max,
                          type = "lstm")

# Best hyperparameter

best_lstm_tune <- lstm_fit %>%
    filter(mse == min(mse))

# Performance and Plotting Function

perf_plot <- function(data_lists = data_lists,
                      data_train = data_train,
                      data_val = data_val,
                      hyperparams = hyp,
                      fit_func,
                      type,
                      min_max = val_min_max,
                      train_title,
                      val_title
){
    if(type == "lstm")
    {
        hyp_fit <- fit_func(hyperparams, data_lists, min_max)
    }
    else{hyp_fit <- fit_func(hyperparams, data_lists)}

    train_err <- data_train %>% mutate(pred = hyp_fit$train) %>%
        summarise(mean((pred - power)^2, na.rm = TRUE)) %>% .[[1]]

    hyp_res <- data_val %>% mutate(pred = hyp_fit$val) %>%
        summarise(mean((pred - power)^2, na.rm = TRUE)) %>% .[[1]]
    # Store Results

    train_res <- hyperparams %>%
        as_tibble() %>%
        mutate(mse = train_err)

    res <- hyperparams %>%
        as_tibble() %>%
        mutate(mse = hyp_res)

    train_forecast <- data_train %>% mutate(pred = hyp_fit$train)

    train_plot <- train_forecast %>% ggplot() +
        geom_line(aes(x = Date, y = power), alpha = 0.8, size = 0.6) +

        geom_line(aes(x = Date, y = pred), color = "red", size = 0.8, alpha = 0.8) +
        labs(title = train_title, x = "", y = "Power Usage") +
        theme_minimal_hgrid() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
              plot.title = ggplot2::element_text(size = 10),
              axis.title.y = element_text(size = 7))

    val_forecast <- data_val %>% mutate(pred = hyp_fit$val)

    val_plot <- val_forecast %>% ggplot() +
        geom_line(aes(x = Date, y = power), alpha = 0.8, size = 0.6) +

        geom_line(aes(x = Date, y = pred), color = "red", size = 0.8, alpha = 0.8) +
        labs(title = val_title, x = "", y = "Power Usage") +
        theme_minimal_hgrid() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
              plot.title = ggplot2::element_text(size = 10),
              axis.title.y = element_text(size = 7))

    return(list(train_plot = train_plot,val_plot = val_plot, performance = res, training_error = train_res))
}

# Determine performance for best hyperparameters

lstm_perf <- perf_plot(data_lists = data_lstm_lists,
                       data_val = data_val,
                       data_train = data_train,
                       hyperparams = best_lstm_tune, #best_lstm_tune,
                       fit_func = lstm_fit_func,
                       min_max = list(train = min_max,
                                      val = val_min_max),
                       type = "lstm",
                       train_title = "LSTM Train Plot",
                       val_title = "LSTM Validation Plot")

# Plot forecasts

lstm_perf$train_plot
lstm_perf$val_plot
lstm_perf$performance
lstm_perf$training_error

# SVR wrangle data

library(liquidSVM)

data_svr_train <- data_wrangling_func(data = data_power %>% rename(Date = date_time),
                                      type = "svr",
                                      final_date = "2009-01-01")

data_svr_val <- data_wrangling_func(data = data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2009-01-01")),
                                    type = "svr",
                                    final_date = "2010-01-01")

data_svr_array <- data_array_func(data = data_svr_train,
                                  val = data_svr_val,
                                  initial = 2*24,
                                  assess = 12,
                                  skip = 11)

#### Function for SVR and GBM data wrangling


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

# Create list of training and validation sets

data_svr_lists <- data_svr_lgb_wrangle(data_svr_array, type = "svr")

# Tune SVR regressions
svr_hyp <- expand.grid(gamma = c(exp(seq(-2.5, by = 1.5, 3.5))),
                       lambda = c(exp(seq(-2.5, by = 1.5, 3.5)))) %>%
    split(., seq(nrow(.)))

svr_fit <- hyp_tune_func(data_lists = data_svr_lists,
                         data_actual = data_val,
                         hyperparams = svr_hyp,
                         fit_func = svr_fit_func, type = "svr")

# Support Vector Regression fit function

svr_fit_func <- function(hyperparams, data_lists){
    set.seed(12439078)
    SVR_fit_list <- list()
    SVR_pred_train <- list()
    SVR_pred_val <- list()
    for(i in 1:12){
        SVR_fit_list[[i]] <- svmRegression(x = data_lists$train_list[[i]][,-1],
                                           y = data_lists$train_list[[i]][,"Y"],
                                           gamma = hyperparams$gamma,
                                           lambda = hyperparams$lambda)

        SVR_pred_train[[i]] <- predict(SVR_fit_list[[i]], data_lists$train_list[[i]][,-1])
        SVR_pred_val[[i]] <- predict(SVR_fit_list[[i]], data_lists$val_list[[i]][,-1])
        print(paste(i,"DONE!!"))
    }

    SVR_train_df <- SVR_pred_train %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    SVR_val_df <- SVR_pred_val %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    print("DONE!!")
    list(train = SVR_train_df, val = SVR_val_df)
}

# Best hyperparameters

best_tune_svr <- svr_fit %>% filter(mse == min(mse))


# Plot performance
svr_perf <- perf_plot(data_lists = data_svr_lists,
                      data_val = data_val,
                      data_train = data_train,
                      hyperparams = best_tune_svr[1,],
                      fit_func = svr_fit_func,
                      type = "svr",
                      train_title = "SVR Train Plot",
                      val_title = "SVR Validation Plot")
svr_perf$train_plot
svr_perf$val_plot
svr_perf$training_error
svr_perf$performance

# GBM wrangle data

library(lightgbm)

data_lgb_train <- data_wrangling_func(data = data_power %>% rename(Date = date_time),
                                      type = "lgb",
                                      final_date = "2009-01-01")

data_lgb_val <- data_wrangling_func(data = data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2009-01-01")),
                                    type = "lgb",
                                    final_date = "2010-01-01")

data_lgb_array <- data_array_func(data = data_lgb_train,
                                  val = data_lgb_val,
                                  initial = 2*24,
                                  assess = 12,
                                  skip = 11)

data_lgb_lists <- data_svr_lgb_wrangle(data_lgb_array, type = "lightgbm")

# GBM hyperparameter tuning


lgb_hyp <- expand.grid(boosting = c("dart", "goss"),
                       learning_rate = c(0.01, 0.05, 0.1),
                       feature_fraction = c(0.6, 0.8),
                       num_leaves = c(2, 4, 8),
                       max_depth = c(2),
                       nrounds = c(50, 100, 150, 200, 250, 300, 350, 500),
                       subsample = c(0.6),
                       lambda_l1 = c(10),
                       lambda_l2 = c(10)) %>%
    split(., seq(nrow(.)))

lgb_fit <- hyp_tune_func(data_lists = data_lgb_lists,
                         data_actual = data_val,
                         hyperparams = lgb_hyp, fit_func = lgb_fit_func, type = "lightgbm")


# Function to fit lightGBM

lgb_fit_func <- function(hyperparams,
                         data_lists){
    set.seed(12439078)
    GBM_fit_list <- list()
    GBM_pred_train <- list()
    GBM_pred_val <- list()
    for(i in 1:length(data_lists$train_list)){
        GBM_fit_list[[i]] <- lightgbm(
            data = data_lists$train_list[[i]],
            obj="regression",
            num_threads = 20,
            metric = "mse",
            nrounds = hyperparams$nrounds,
            boosting = hyperparams$boosting,
            params = list(learning_rate = hyperparams$learning_rate,
                          feature_fraction = hyperparams$feature_fraction,
                          num_leaves = hyperparams$num_leaves,
                          max_depth = hyperparams$max_depth,
                          subsample = hyperparams$subsample,
                          lambda_l1 = hyperparams$lambda_l1,
                          lambda_l2 = hyperparams$lambda_l2))

        lgb.get.eval.result(GBM_fit_list[[i]],
                            data_name = "train", eval_name = "l2")

        GBM_pred_train[[i]] <- predict(GBM_fit_list[[i]], data_lists$train_err_list[[i]])
        GBM_pred_val[[i]] <- predict(GBM_fit_list[[i]], data_lists$val_list[[i]])
    }
    GBM_pred_train_df <- GBM_pred_train %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    GBM_pred_val_df <- GBM_pred_val %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    return(list(train = GBM_pred_train_df, val = GBM_pred_val_df))
}


# Best hyperparameters

best_lgb_tune <- lgb_fit %>% filter(mse == min(mse))

# Plot performance


lgb_perf <- perf_plot(data_lists = data_lgb_lists,
                      data_train = data_train,
                      data_val = data_val,
                      hyperparams = best_tune,
                      fit_func = lgb_fit_func,
                      type = "lightgbm",
                      val_title = "GBM Validation Plot",
                      train_title = "GBM Train Plot")

lgb_perf$train_plot
lgb_perf$val_plot
lgb_perf$performance %>% mutate(across(everything(), ~as.character(.x))) %>%
    pivot_longer(cols = everything())
lgb_perf$training_error %>% mutate(across(everything(), ~as.character(.x))) %>%
    pivot_longer(cols = everything())


# Testing time

# Full Training and Test Data

data_full_train <- data_power %>% rename(Date = date_time) %>% filter(Date < as.Date("2010-01-01")) %>% tail(-48) %>% head(-7)

data_test <- data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2010-01-01")) %>% tail(-48) %>% head(-10)


# LSTM Test Data
data_lstm_train <- data_wrangling_func(data = data_power %>% rename(Date = date_time),
                                       type = "lstm",
                                       final_date = "2010-01-01")
data_lstm_test <- data_wrangling_func(data = data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2010-01-01")),
                                      type = "lstm",
                                      final_date = "2010-11-27")
# Set min and max for later transformations
min_max_train <- data_lstm_train$min_max
data_lstm_train <- data_lstm_train$data

test_min_max <- data_lstm_test$min_max
data_lstm_test <- data_lstm_test$data

### Create LSTM Data Arrays
data_lstm_test_array <- data_array_func(data = data_lstm_train,
                                        val = data_lstm_test,
                                        initial = 2*24, assess = 12, skip = 11)

# SVR Test Data

data_svr_train <- data_wrangling_func(data = data_power %>% rename(Date = date_time),
                                      type = "svr",
                                      final_date = "2010-01-01")

data_svr_test <- data_wrangling_func(data = data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2010-01-01")),
                                     type = "svr",
                                     final_date = "2010-11-27")

data_svr_test_array <- data_array_func(data = data_svr_train,
                                       val = data_svr_test,
                                       initial = 2*24,
                                       assess = 12,
                                       skip = 11)

data_svr_test_lists <- data_svr_lgb_wrangle(data_svr_test_array, type = "svr")

# LightGBM

data_lgb_train <- data_wrangling_func(data = data_power %>% rename(Date = date_time),
                                      type = "lgb",
                                      final_date = "2010-01-01")

data_lgb_test <- data_wrangling_func(data = data_power %>% rename(Date = date_time) %>% filter(Date >= as.Date("2010-01-01")),
                                     type = "lgb",
                                     final_date = "2010-11-27")

data_lgb_test_array <- data_array_func(data = data_lgb_train,
                                       val = data_lgb_test,
                                       initial = 2*24,
                                       assess = 12,
                                       skip = 11)

data_lgb_test_lists <- data_svr_lgb_wrangle(data_lgb_test_array, type = "lightgbm")


# Determine LSTM performance

lstm_test_perf <- perf_plot(data_lists = data_lstm_test_lists,
                            data_val = data_test,
                            data_train = data_full_train,
                            hyperparams = best_lstm_tune,
                            fit_func = lstm_fit_func,
                            min_max = list(train = min_max_train,
                                           val = test_min_max),
                            type = "lstm",
                            train_title = "LSTM Full Training Plot",
                            val_title = "LSTM Test Plot")

lstm_test_perf$train_plot
lstm_test_perf$val_plot
lstm_test_perf$performance
lstm_test_perf$training_error

# Determine SVR performance

svr_test_perf <- perf_plot(data_lists = data_svr_test_lists,
                           data_val = data_test,
                           data_train = data_full_train,
                           hyperparams = best_tune_svr[1,],
                           fit_func = svr_fit_func,
                           type = "svr",
                           train_title = "SVR Full Training Plot",
                           val_title = "SVR Test Plot")

svr_test_perf$train_plot
svr_test_perf$val_plot
svr_test_perf$training_error
svr_test_perf$performance

# Determine lgithGBM performance

lgb_test_perf <- perf_plot(data_lists = data_lgb_test_lists,
                           data_train = data_full_train,
                           data_val = data_test,
                           hyperparams = best_tune,
                           fit_func = lgb_fit_func,
                           type = "lightgbm",
                           val_title = "GBM Test Plot",
                           train_title = "GBM Full Training Plot")

lgb_test_perf$train_plot
lgb_test_perf$val_plot
lgb_test_perf$performance %>% mutate(across(everything(), ~as.character(.x))) %>%
    pivot_longer(cols = everything())
lgb_test_perf$training_error %>% mutate(across(everything(), ~as.character(.x))) %>%
    pivot_longer(cols = everything())






