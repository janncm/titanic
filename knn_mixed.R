# knn_mixed.R

# list, list -> numeric
# compute the distance between two samples of objects. Objects can be mixed containing
# both numerical (euclidean distance) and categorical (simple matching) data as factors.
# ASSUME: numerical data has been previously normalized to values between 0 and 1
mixed_distance <- function(v1, v2){
    dist_numeric <- c()
    dist_categorical <- c()
    for (idx in 1:length(v1)){
        if (is.numeric(v1[[idx]])|is.integer(v1[[idx]])){
            dist <- (v1[[idx]] - v2[[idx]])^2
            dist_numeric <- append(dist_numeric, dist)
     
        }
        else if (is.factor(v1[[idx]])){
            dist <- (as.integer(v1[[idx]])!= as.integer(v2[[idx]]))*1
            dist_categorial <- append(dist_categorical, dist)

        }
        else {stop("argument must be one of numeric, integer or factor")}    
    }
    distance <- sum(sqrt(dist_numeric)) + sum(dist_categorical)
    distance
}



# data.frame, data.frame, vector (numeric), integer -> vector (numeric)
# performs a knn algorithm for numerical data from a data frame with both 
# categorical an numerical data types based on mixed_distance function
# ASSUME: data is either numeric, integer or factor
mixed_knn <- function(train, test, tn, k){
    dist_matrix<-matrix(Inf, nrow(train), nrow(test))
    for (row_train in 1:nrow(train)){
        for (row_test in 1:nrow(test)){
            if (row_test <= row_train){
                dist_matrix[row_train, row_test] <- mixed_distance(train[row_train, , drop = T], 
                                                                   test[row_test, , drop = T])
            if (row_test <= nrow(train) & row_train <= nrow(test)){
                dist_matrix[row_test, row_train] <- dist_matrix[row_train, row_test]
            }
        }
    }
    }
    closest_matrix <- matrix(NA, k, nrow(test))
    for (col in 1:ncol(dist_matrix)){
        closest_matrix[, col]<-tn[order(dist_matrix[,col])[1:k]]
    }
    output <- apply(closest_matrix, 2, mean)
    output
}



# integer -> vector
# performs a 5 fold cv for a knn algorithm with numerical an categorical data
# based on function mixed_knn
# ASSUME: data.frame contsins an index column of randomly distributed numbers from 1 to 5
mknn_cv <- function(train, pred_name, k, index){
    pred_index <- which(names(train) == pred_name)
    index_col <- which(names(train) == index)
    error <- rep(NA, 5)
    for (idx in 1:5){
        train_set <- train[train[, index_col] != idx, ]
        train_set <- select(train_set, -index_col)
        pred<-train[, pred_index]
        train_set <- train_set[, - pred_index]
        v_set <- train[train[, index_col] == idx, ]
        v_set <- select(v_set, -index_col)
        
        v_pred <- v_set[, pred_index]
        v_set <- v_set[, -pred_index]
        
        predictions <- mixed_knn(train_set, v_set, pred, k)
        error[idx] <-sqrt(mean((predictions - v_pred)^2))
        
    }
    cv_output <- list(resp.vs.pred = data.frame(predictions, v_pred)
                      , rss = error, mrss = mean(error))
    cv_output
}



