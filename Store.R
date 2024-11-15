library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)

train <- vroom('train.csv')
test <- vroom('test.csv')

storeItem1 <- train %>%
  filter(store==2, item==2)
storeItem2 <- train %>%
  filter(store==8, item==50)
