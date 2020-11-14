get_test_results <- function(
  model,
  test
){
  test_x = test %>% select(-Y)
  test_y = test %>% pull(Y)
  test_prediction = predict(model, new_data = test_x)
  test_results = test %>% 
    select(Y) %>% 
    bind_cols(
      test_prediction
    )
  
  if(".pred" %in% names(test_results)){
    test_results <- test_results %>% 
      rename(
        "Predicted" = ".pred",
        "Observed" = "Y"
      )
  }else if(".pred_res" %in% names(test_results)){
    test_results <- test_results %>% 
      rename(
        "Predicted" = ".pred_res",
        "Observed" = "Y"
      )
  }
  
  return(test_results)
}

get_train_results <- function(
  model,
  train
){
  train_x = train %>% select(-Y)
  train_y = train %>% pull(Y)
  train_prediction = predict(model, new_data = train_x)
  train_results = train %>% 
    select(Y) %>% 
    bind_cols(
      train_prediction
    )
  
  if(".pred" %in% names(train_results)){
    train_results <- train_results %>% 
      rename(
        "Predicted" = ".pred",
        "Observed" = "Y"
      )
  }else if(".pred_res" %in% names(train_results)){
    train_results <- train_results %>% 
      rename(
        "Predicted" = ".pred_res",
        "Observed" = "Y"
      )
  }
  
  return(train_results)
}

# Evaluate Model
evaluate_model <- function(
  model,
  train,
  test
){
  train_results = get_train_results(model, train)
  test_results = get_test_results(model, test)
  results <- train_results %>% 
    mutate(Case = "Train") %>% 
    bind_rows(
      test_results %>% 
        mutate(Case = "Test")
    )
  
  if("factor" %in% class(results$Predicted)){
    # train_matrix <- train_results %>% 
    #   mutat
    # TODO: Plot this instead as two matrices of correspondence between predicted and observed
    return()
  }else{
    limit = c(min(c(results$Predicted, results$Observed), na.rm = T), max(c(results$Predicted, results$Observed), na.rm = T))
    
    gg <- results %>% 
      ggplot(
        aes(
          x = Predicted,
          y = Observed,
          colour = Case,
          alpha = Case
        )
      ) + 
      geom_point(na.rm = T) + 
      theme_bw(12) + 
      geom_abline(slope = 1, intercept = 0) + 
      scale_colour_manual(values = c("Train" = "grey", "Test" = "blue")) + 
      scale_alpha_manual(values = c("Train" = 0.2, "Test" = 0.5)) + 
      ylim(limit) + 
      xlim(limit)
  }
  
  return(gg)
}

# General purpose preprocessing function
preprocess <- function(
  df, 
  response = NULL, 
  predictors = considered_columns, 
  drop_at_na_level = 0.99, 
  max_categorical_levels = 50,
  na.rm = T
){
  X = df
  
  if(!is.null(response)){
    # Filter out any NA in the response
    X = X[!is.na(X[[response]]), ]
  }
  
  # Drop columns with too many NAs
  ## This is specified by the "drop_at_na_level" parameter, where only columns with at least that fraction non-na will be retained
  not_too_many_na = function(x, threshold = 1 - drop_at_na_level){
    n_na = sum(is.na(x))
    n = length(x)
    return(
      (n_na/n) <= threshold
    )
  }
  X <- X %>% select(where(not_too_many_na))
  
  if(na.rm){
    # Drop NA rows
    X <- na.omit(X)
  }
  
  # Drop columns with too many (or not enough) categorical variables
  not_too_many_categorical = function(x, threshold = max_categorical_levels){
    if(!is.character(x)){
      return(TRUE)
    }else{
      n = length(unique(x))
      if(!is.null(threshold)){
        return(n <= threshold & n >= 2)
      }else{
        return(n >= 2)
      }
    }
  }
  X <- X %>% select(where(not_too_many_categorical))
  
  if(!is.null(response)){
    # Rename the response
    names(X)[names(X) == response] <- "Y"
  }else{
    # Do nothing
  }
  
  # Factorise X
  X <- X %>% 
    mutate_if(
      is.character,
      as.factor
    )
  
  if(!is.null(predictors)){
    # Drop irrelevant variables
    # cat(paste0('"', paste0(names(X), collapse = '",\n"'), '"'))
    X_ <- X[, -which(names(X) %in% predictors)]
    X <- X[, which(names(X) %in% predictors)]
  }else{
    X_ <- X[,NULL]
  }
  
  return(
    list(
      "main" = X,
      "secondary" = X_,
      "response" = response
    )
  )
}