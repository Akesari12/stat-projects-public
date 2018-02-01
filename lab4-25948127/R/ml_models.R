library(readr)
library(tidyverse)
library(gridExtra)
library(caret)
library(nnet)
library(mlbench)
library(RCurl)
library(GGally)
library(knitr)
library(pROC)

# loading all image data. These rds files were created in load_clean_data.R 
image_1 <- readRDS("R/image_1.rds")
image_2 <- readRDS("R/image_2.rds")
image_3 <- readRDS("R/image_3.rds")

# all_image only contain data points from image 1 and 2, because we
# want to use image 1 and 2 for training only
# and saved image 3 for training
# all_image was created in load_clean_data.R as well
all_image <- readRDS( "R/all_image.rds")
## Leave out image 3 for testing
test_image <- image_3

# create group 

k <- 20
x_interval <- round(seq(min(all_image$x), max(all_image$x) + 1, 
                        by = (max(all_image$x) - min(all_image$x) + 1) / k))
y_interval <- round(seq(min(all_image$y), max(all_image$y) + 1, 
                        by = (max(all_image$y) - min(all_image$y) + 1) / k))
all_image$group <- sapply(all_image$y,function(z) (sum(z > y_interval) - 1) * k
                          ) + sapply(all_image$x,function(z) sum(z > x_interval
                                                                 ))


# Binarize the data for 0/1 classification by removing
# the "unlabeled" points and recoding "-1" to "0"
binary_image <- all_image %>%
  filter(expert_label != 0) %>%
  mutate(expert_label = if_else(expert_label == -1, 0, 1))

# create folders
group_folds <- groupKFold(binary_image$group, k = 5)
# This code creates partitions for selection via CV and ROC
# Can probably merge this in with the previous code, as it just adds arguments to caret



## Make a training control method
ctrl <- trainControl(index = group_folds,
                     method = "cv",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     number = 5)
## logistic
logistic_model <- train(as.factor(expert_alpha_label) ~ 
                          CORR + SD + NDAI 
                        + CORR * CORR + SD * SD + NDAI * NDAI,
                        data = binary_image,
                        method = "glm",
                        trControl = ctrl,
                        metric = "ROC",
                        family = binomial)


test_data <- test_image %>%
  filter(expert_label != 0)
pred_logistic <- predict(logistic_model, test_data[4:6])

mean(pred_logistic==test_data$expert_alpha_label)

saveRDS(logistic_model, "R/logistic_model.rds")


## Random Forest
qda_model <- train(as.factor(expert_alpha_label) ~ 
                     CORR + SD + NDAI,
                   data = binary_image,
                   method = "qda",
                   trControl = ctrl,
                   metric = "ROC")

pred_qda <- predict(qda_model, test_data[4:6])

mean(pred_qda==test_data$expert_alpha_label)


saveRDS(qda_model, "R/qda_model.rds")

## SVM (use a 6.5% sample)
binary_sample <- binary_image[sample(1:nrow(binary_image),10000),]

group_folds_sample <- groupKFold(binary_sample$group, k = 5)

ctrl_sample <- trainControl(index = group_folds_sample,
                            method = "cv",
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            number = 5,
                            savePredictions = TRUE)

#grid = expand.grid(sigma = c(1,1.5,2),
#                   C = c(0.1, 0.5, 1, 2, 3, 4))

svm_model <- train(expert_alpha_label ~ 
                     CORR + SD + NDAI,
                   data = binary_sample,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneGrid = grid,
                   trControl = ctrl_sample,
                   metric = "ROC")


pred_svm <- predict(svm_model, test_data[4:6])
pred_svm_prob <- predict(svm_model, test_data[4:6],type = 'prob')

mean(pred_svm==test_data$expert_alpha_label)



saveRDS(svm_model, "R/svm_model.rds")
