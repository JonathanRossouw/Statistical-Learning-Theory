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
