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
