library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(plotly)
library(prophet)

nStores <- max(train$store)
nItems <- max(train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
    filter(store==s, item==i)
    storeItemTest <- test %>%
    filter(store==s, item==i)
    
    cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)
    
    prophet_model <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(sales ~ date, data = training(cv_split))
    
    cv_results <- modeltime_calibrate(prophet_model,
                                      new_data = testing(cv_split))
    
    fullfit <- cv_results %>%
      modeltime_refit(data = storeItemTrain)
    
    preds <- prophet_model_tbl %>%
      modeltime_forecast(
        new_data = storeItemTest,
        actual_data = storeItemTrain) %>%
      filter(!is.na(.model_id)) %>%
      mutate(id=storeItemTest$id) %>%
      select(id, .value) %>%
      rename(sales=.value)
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}

vroom_write(all_preds, file='/kaggle/working/submission.csv', delim=',')