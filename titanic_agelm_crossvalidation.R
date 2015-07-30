# titanic_agelm_crossvalidation.R

## data.frame, character, character -> list
## returns cv_errors and their mean given a data frame, a response
## column and an index column of integers
cv_lr <- function(data, response, cvidx){
    response_index <- which(names(data) == response)
    cvidx_index <- which(names(data) == cvidx)
    errors <- rep(NA, length(unique(data[,cvidx_index])))
    for (idx in unique(data[,cvidx])){
        train_set <- filter(data, cvidx != idx) %>% select(-cvidx_index)
        test_set <- filter(data, cvidx == idx) %>% select(-cvidx_index)
        form <- paste0(response, "~.")
        lmodel <- lm(form, data = train_set)
        pred <- predict.lm(lmodel, newdata = test_set)
        errors[idx] <- sqrt(mean((test_set[, response_index] - pred)^2))
    }
    list(errors = errors, mean = mean(errors))
}


# data.frame, integer -> vector.numeric resamples mean 
# cv-errors from for Age prediction from titanic data set using
# cv_lr function
resample_cvlr <- function(data, R){
    cv_errors <- rep(NA, R)
    for (idx in 1:R){
        data <- mutate(data, cvidx = sample(cvidx))
        cv_errors[idx] <- cv_lr(data, "Age", "cvidx")$mean
    }
    cv_errors
}