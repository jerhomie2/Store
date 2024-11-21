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

## Create a recipe for the linear model part
arima_recipe <- recipe(sales ~ date, data = train)
prep <- prep(arima_recipe)
bake <- bake(prep, new_data = train)

## Define the ARIMA Model
arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
  set_engine("auto_arima")

## Merge into a single workflow and fit to the training data
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize results
cv_plot1 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain)

## Predict for all the observations in storeItemTest
forecast1 <- fullfit %>%
  modeltime_forecast(new_data = storeItemTest,
                     actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)



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

## Create a recipe for the linear model part
arima_recipe <- recipe(sales ~ date, data = storeItemTrain)
prep <- prep(arima_recipe)
bake <- bake(prep, new_data = storeItemTrain)

## Define the ARIMA Model
arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
  set_engine("auto_arima")

## Merge into a single workflow and fit to the training data
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize results
cv_plot2 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain)

## Predict for all the observations in storeItemTest
forecast2 <- fullfit %>%
  modeltime_forecast(new_data = storeItemTest,
                     actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)

subplot(cv_plot1,cv_plot2,forecast1,forecast2, nrows=2)
