library(h2o)
library(data.table)
library(tidyverse)
library(checkmate)
library(caret)

source("functions/helpers.R")

train_data <- read.csv("data/training_set_values.csv", stringsAsFactors = TRUE) %>% setDT()
train_labels <- read.csv("data/training_set_labels.csv", stringsAsFactors = TRUE) %>% setDT()
test_data <- read.csv("data/test_set_values.csv", stringsAsFactors = TRUE) %>% setDT()
submission <- read.csv("data/SubmissionFormat.csv")

train_data <- change_datatypes(data = train_data)

assert_logical(train_data[,public_meeting])
assert_logical(train_data[,permit])
assert_date(train_data[,date_recorded])

train_data <- train_data[train_labels, on = "id"]

smp_size <- floor(0.7 * nrow(train_data))

set.seed(215)

train_ind <- sample(seq_len(nrow(train_data)), size = smp_size)

train <- train_data[train_ind, ]
test <- train_data[-train_ind, ]

invisible(h2o.init())

train_h <- as.h2o(train)
test_h <- as.h2o(test)

y = "status_group"
pred = setdiff(names(train), y)

# train[,y] = as.factor(train[,y])
# test[,y] = as.factor(test[,y])

aml = h2o.automl(x = pred, y = y,
                 training_frame = train_h,
                 max_models = 20,
                 seed = 1,
                 max_runtime_secs = 20
)

lb = aml@leaderboard
lb

prediction = h2o.predict(aml@leader, test_h[,-5]) %>%
  as.data.frame()

caret::confusionMatrix(test$status_group, prediction$predict)

h2o.shutdown(prompt = F)











