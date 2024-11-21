library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions

train <- vroom('train.csv')
test <- vroom('test.csv')

storeItem1 <- train %>%
  filter(store==2, item==2)
storeItem2 <- train %>%
  filter(store==8, item==50)


series1 <- storeItem1 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)
series2 <- storeItem2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

ACFmonth1 <- storeItem1 %>%
  pull(sales) %>%
  ggAcf(.)
ACFmonth2 <- storeItem2 %>%
  pull(sales) %>%
  ggAcf(.)

ACFyear1 <- storeItem1 %>%
  pull(sales) %>%
  ggAcf(., lag.max = 365*2)
ACFyear2 <- storeItem2 %>%
  pull(sales) %>%
  ggAcf(., lag.max = 365*2)

(series1 + ACFmonth1 + ACFyear1) / (series2 + ACFmonth2 + ACFyear2)

# Recipe
store_recipe <- recipe(sales ~ ., data = storeItem1) %>%
  step_date(date, features="dow") %>%
  step_mutate(date_dow = as.factor(date_dow)) %>%
  step_date(date, features="year") %>%
  step_date(date, features="decimal")%>%
  step_range(date_decimal, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_decimal), cosDOY=cos(date_decimal)) %>%
  step_rm(store, item)
prep <- prep(store_recipe)
bake <- bake(prep, new_data = storeItem1)


my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=15) %>%
  set_mode("regression") %>%
  set_engine("ranger")

forest_wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(my_mod)

## Grid of values to tune over
tuning_grid <- grid_regular(mtry(range = c(1,6)),
                            min_n(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(train, v = 5, repeats=1)

## Run the CV
CV_results <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape))

CV_results %>%
  show_best(n=1,
            metric = "smape")



