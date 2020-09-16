library(keras)
library(tidyverse)

datalags_ = c(6, 13)

data_orig <- read_rds('../../Data/Decomposed_data.rds')

targets <-
  c("new_confirmed",
    "IMF1",
    "IMF2",
    "IMF3",
    "IMF4",
    "IMF5",
    "Residual")

# onde todas as previsões serão armazenadas
preds <- list()
preds_train <- list()

# repetindo para 7 e 14
for (datalags in datalags_) {
  # repetindo para todos os estados
  for (state_ in names(data_orig)) {
    pred <- list()
    pred_train <- list()
    #repetindo para todos os targets
    for (target in targets) {
      data_raw <- data_orig[[state_]]
      
      # retirando somente as features
      data <- data_raw %>%
        as_tibble() %>%
        select(-state,-date)
      
      # média e dp das features (somente no df de teste)
      data_means <- data %>%
        slice(1:(dim(data)[1] - datalags)) %>%
        summarise_all(mean)
      data_sds <- data %>%
        slice(1:(dim(data)[1] - datalags)) %>%
        summarise_all(sd)
      
      # normalizando as features
      for (name in names(data)) {
        data[[name]] <-
          (data[[name]] - data_means[[name]]) / data_sds[[name]]
      }
      
      # features com lag
      X <- array(
        data = lag(
          cbind(
            data[[target]]
          ),
          datalags
        )[-(1:datalags),],
        dim = c(nrow(data) - datalags, datalags, 2)
      )
      
      # targets
      y <-
        array(data = data$new_confirmed[-(1:datalags)],
              dim = c(nrow(data) - datalags, 1))
      
      X_train <- X[1:(dim(X)[1] - datalags), ,]
      X_test <- X[(dim(X)[1] - datalags):(dim(X)[1]), ,]
      
      y_train <- y[1:(dim(y)[1] - datalags),]
      y_test <- y[(dim(X)[1] - datalags):(dim(X)[1]),]
      
      batch.size <- 1
      
      model <- keras_model_sequential()
      
      model %>%
        layer_lstm(
          units = 30,
          input_shape = c(datalags, 2),
          batch_size = batch.size,
          return_sequences = TRUE,
          stateful = TRUE
        ) %>%
        layer_lstm(
          units = 20,
          return_sequences = FALSE,
          stateful = TRUE
        ) %>%
        layer_dense(units = 1)
      
      model %>%
        compile(loss = 'mae', optimizer = 'adam')
      
      model %>% fit(
        x = X_train,
        y = y_train,
        batch_size = 1,
        epochs = 100,
        verbose = 2,
        shuffle = FALSE
      )
      
      p  <-
        model %>%
        predict(X_test, batch_size = batch.size) %>% .[, 1]
      
      p_train  <-
        model %>%
        predict(X_train, batch_size = batch.size) %>% .[, 1]
      
      # "desnormalizando"
      p <- p * data_sds[[target]] + data_means[[target]]
      p_train <- p_train * data_sds[[target]] + data_means[[target]]
      
      pred[[paste(target, "_pred", sep = "")]] <- p
      pred_train[[paste(target, "_pred", sep = "")]] <- p_train
    }
    
    preds[[state_]] <-
      data_raw %>%
      as_tibble() %>%
      slice((dim(data)[1] - datalags):dim(data)[1]) %>%
      cbind(as_tibble(pred))
    
    preds_train[[state_]] <-
      data_raw %>%
      as_tibble() %>%
      slice(datalags:(dim(data)[1] - datalags-1)) %>%
      cbind(as_tibble(pred_train))
  }
  
  preds %>% write_rds(paste("preds_", (datalags+1), ".rds", sep = ""))
  preds_train %>% write_rds(paste("preds_train_", (datalags+1), ".rds", sep = ""))
  
}
