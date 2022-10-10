# Support Vector Regression fit function

svr_fit_func <- function(hyperparams, data_lists){
    set.seed(12439078)
    SVR_fit_list <- list()
    SVR_pred <- list()
    for(i in 1:12){
        SVR_fit_list[[i]] <- svmRegression(x = data_SVR_lists$train_list[[i]][,-1],
                                           y = data_SVR_lists$train_list$`1`[,"Y"],
                                           gamma = svr_hyp$`1`$gamma,
                                           lambda = svr_hyp$`1`$lambda, display = 5)

        SVR_pred[[i]] <- predict(SVR_fit_list[[i]], data_SVR_lists$val_list[[i]][,-1])
    }

    SVR_pred_df <- SVR_pred %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    SVR_pred_df
}
