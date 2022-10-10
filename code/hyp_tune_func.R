# Hyperparameter tuning wrapper

hyp_tune_func <- function(data_lists = data_lists,
                          data_actual = data_val,
                          hyperparams = hyp,
                          fit_func,
                          type,
                          min_max = val_min_max){
    # Run iteratively through hyperparameter grid
    if(type == "LSTM")
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
