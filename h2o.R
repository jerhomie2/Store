library(tidymodels)
library(modeltime.h2o)
library(tidyverse)
library(timetk)
library(vroom)

train <- vroom('train.csv')
test <- vroom('test.csv')

data_tbl <- train %>%
  filter(store == 2, item == 24)

data_tbl %>% 
  group_by(store, item) %>% 
  plot_time_series(
    .date_var    = date,
    .value       = sales,
    .facet_ncol  = 10,
    .smooth      = F,
    .interactive = F
  )

splits <- time_series_split(data_tbl, assess = "3 month", cumulative = TRUE)

recipe_spec <- recipe(sales ~ ., data = training(splits)) %>%
  step_timeseries_signature(date) 

train_tbl <- training(splits) %>% bake(prep(recipe_spec), .)
test_tbl  <- testing(splits) %>% bake(prep(recipe_spec), .)

# Initialize H2O
h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

model_spec <- automl_reg(mode = 'regression') %>%
  set_engine(
    engine                     = 'h2o',
    max_runtime_secs           = 5, 
    max_runtime_secs_per_model = 3,
    max_models                 = 3,
    nfolds                     = 5,
    exclude_algos              = c("DeepLearning"),
    verbosity                  = NULL,
    seed                       = 786
  ) 

model_spec

model_fitted <- model_spec %>%
  fit(sales ~ ., data = train_tbl)

model_fitted

preds <- predict(model_fitted, test_tbl)

smape_vec(test_tbl$sales, preds$.pred)
