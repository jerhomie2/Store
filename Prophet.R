library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(plotly)
library(prophet)

train <- vroom('train.csv')
test <- vroom('test.csv')

train <- train %>% filter(store==2, item==24)
test <- test %>% filter(store == 2, item == 24)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

p1 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

fullfit <- cv_results %>%
  modeltime_refit(data = train)

p2 <- fullfit %>%
  modeltime_forecast(
    new_data    = test,
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Second Item
train <- vroom('train.csv')
test <- vroom('test.csv')

train <- train %>% filter(store==5, item==25)
test <- test %>% filter(store == 5, item == 25)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

p3 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

fullfit <- cv_results %>%
  modeltime_refit(data = train)

p4 <- fullfit %>%
  modeltime_forecast(
    new_data    = test,
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

subplot(p1,p3,p2,p4, nrows=2)
