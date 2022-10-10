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
