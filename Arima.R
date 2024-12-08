library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(plotly)

train <- vroom('train.csv')
test <- vroom('test.csv')

train <- train %>% filter(store==2, item==24)
storeItemTrain <- train %>%
  filter(store==2, item==24)
storeItemTest <- test %>%
  filter(store==2, item==24)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_recipe <- recipe(sales~., data=train) %>%
  step_rm(item, store) %>%
  step_date(date, features=c("doy", "decimal")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_rm(date_doy)
bake(prep(arima_recipe), new_data=train)
arima_model <- arima_reg() %>%
  set_engine("auto_arima")
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
## Visualize results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )
## Refit to whole data
fullfit <- cv_results %>%
  modeltime_refit(data = train)
p2 <- fullfit %>%
  modeltime_forecast(
    new_data    = test,
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)




# Second Item
train <- vroom('train.csv')
test <- vroom('test.csv')
train <- train %>% filter(store==5, item==25)
storeItemTrain <- train %>%
  filter(store==5, item==25)
storeItemTest <- test %>%
  filter(store==5, item==25)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_recipe <- recipe(sales~., data=train) %>%
  step_rm(item, store) %>%
  step_date(date, features=c("doy", "decimal")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_rm(date_doy)
bake(prep(arima_recipe), new_data=train)
arima_model <- arima_reg() %>%
  set_engine("auto_arima")
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
## Visualize results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )
## Refit to whole data
fullfit <- cv_results %>%
  modeltime_refit(data = train)
p4 <- fullfit %>%
  modeltime_forecast(
    new_data    = test,
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)



subplot(p1,p3,p2,p4, nrows=2)







