# Density Relationships between agle radiances and expert label
df_ang_expertlab_plot <- ggplot(data = all_image, 
                                aes(x = rad_ang_df, 
                                    fill = expert_alpha_label))+
  geom_density()  

cf_ang_expertlab_plot <- ggplot(data = all_image, 
                                aes(x = rad_ang_cf, 
                                    fill = expert_alpha_label)) +
  geom_density()  

bf_ang_expertlab_plot <- ggplot(data = all_image, 
                                aes(x = rad_ang_bf, 
                                    fill = expert_alpha_label)) +
  geom_density()  

af_ang_expertlab_plot <- ggplot(data = all_image, 
                                aes(x = rad_ang_af, 
                                    fill = expert_alpha_label)) +
  geom_density()  

grid.arrange(df_ang_expertlab_plot, cf_ang_expertlab_plot,
             bf_ang_expertlab_plot, af_ang_expertlab_plot,
             ncol = 2)
####

# Correlation Matrix
# Calculate correlation matrix
correlationMatrix <- cor(image_1[,4:11])
# Summarize the correlation matrix
print(correlationMatrix)
# Find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# Print indexes of highly correlated attributes
print(highlyCorrelated)

####

# Initial Model Fitting with Caret
# Binarize the data for 0/1 classification by removing
# the "unlabeled" points and recoding "-1" to "0"
binary_image_1 <- image_1 %>%
  filter(expert_label != 0) %>%
  mutate(expert_label = if_else(expert_label == -1, 0, 1))

# Logit model
logit_model <- glm(expert_label ~ CORR + SD + NDAI + rad_ang_df,
                   data = binary_image_1, family = "binomial")

# Ridge regression with Caret
ridge_model <- train(expert_label ~ CORR + SD + NDAI + rad_ang_df,
                     binary_image_1,
                     method = "ridge")

# CART with caret
cart_model <- train(expert_label ~ CORR + SD + NDAI + rad_ang_df,
                    binary_image_1,
                    method = "rpart")

# GLM Net with caret
glmnet_model <- train(expert_label ~ CORR + SD + NDAI + rad_ang_df,
                      binary_image_1,
                      method = "glmnet")

# Random Forest with caret
# Takes a while to run
rf_model <- train(expert_label ~ CORR + SD + NDAI + rad_ang_df,
                  binary_image_1,
                  method = "ranger")


#####

# Models

## CART
cart_model <- train(factor(expert_alpha_label) ~ 
                      CORR + SD + NDAI + rad_ang_df,
                    training,
                    method = "rpart",
                    trControl = ctrl,
                    metric = "ROC",
                    control = rpart.control(minsplit = 5))

## GLM Net
glmnet_model <- train(expert_alpha_label ~ 
                        CORR + SD + NDAI + rad_ang_df,
                      training,
                      method = "glmnet",
                      trControl = ctrl,
                      metric = "ROC")

## Random Forest
rf_model <- train(expert_alpha_label ~ 
                    CORR + SD + NDAI + rad_ang_df,
                  training,
                  method = "ranger",
                  trControl = ctrl,
                  metric = "ROC")

## SVM (use a 5% sample)
sample_training <- training %>%
  sample_frac(.01)

trainX <- sample_training %>%
  select(4:7)

svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))

svm_model <- train(expert_alpha_label ~ 
                     CORR + SD + NDAI + rad_ang_df,
                   sample_training,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneLength = 9,
                   trControl = ctrl,
                   metric = "ROC")

save(svm_model, file = "R/svm_model.rda")
saveRDS(svm_model, "R/svm_model.rds")


```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Predictions for test set taken from svmClasses
svm_test_predictions <- as.data.frame(svmClasses)
# Add test predictions to the testing data with true values
testing <- testing %>%
  bind_cols(svm_test_predictions)

# Plot the cloud coverage for the true values
true_plot <- ggplot(data = testing, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot the predicted cloud coverage
predict_plot <- ggplot(data = testing, aes(x = x, y = y)) +
  geom_point(aes(colour = svmClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Grid arrange the two plots side-by-side
grid.arrange(true_plot, predict_plot, ncol = 2)

# Subset only to misclassified points
misclassified_regions <- testing %>%
  filter(expert_alpha_label != svmClasses)

# Plot of the true values with only the misclassified obs.
misclass_true_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot of the predicted values with only misclassified obs.
misclass_predict_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = svmClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

grid.arrange(misclass_true_plot, misclass_predict_plot, ncol = 2)

# Histogram of the NDAI distribution for true values
true_NDAI_histogram <- ggplot(data = testing) +
  geom_histogram(aes(x = NDAI, fill = expert_alpha_label)) 

# Histogram of the NDAI distribution for the predicted values
predict_NDAI_histogram <- ggplot(data = testing) +
  geom_histogram(aes(x = NDAI, fill = svmClasses))

grid.arrange(true_NDAI_histogram, predict_NDAI_histogram)
```
# Plots of Predicted vs. True

# Plot of the true values with only the misclassified obs.
misclass_true_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot of the predicted values with only misclassified obs.
misclass_predict_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = svmClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

grid.arrange(misclass_true_plot, misclass_predict_plot, ncol = 2)

```{r, eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
rfClasses <- predict(rf_model, 
                     newdata = testing)

# Predictions for test set taken from rfClasses
rf_test_predictions <- as.data.frame(rfClasses)
# Add test predictions to the testing data with true values
testing <- testing %>%
  bind_cols(rf_test_predictions)

# Plot the cloud coverage for the true values
true_plot <- ggplot(data = testing, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot the predicted cloud coverage
predict_plot <- ggplot(data = testing, aes(x = x, y = y)) +
  geom_point(aes(colour = rfClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Grid arrange the two plots side-by-side
grid.arrange(true_plot, predict_plot, ncol = 2)

# Subset only to misclassified points
misclassified_regions <- testing %>%
  filter(expert_alpha_label != rfClasses)

# Plot of the true values with only the misclassified obs.
misclass_true_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot of the predicted values with only misclassified obs.
misclass_predict_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = rfClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

grid.arrange(misclass_true_plot, misclass_predict_plot, ncol = 2)

# Histogram of the NDAI distribution for true values
true_NDAI_histogram <- ggplot(data = testing) +
  geom_histogram(aes(x = NDAI, fill = expert_alpha_label)) 

# Histogram of the NDAI distribution for the predicted values
predict_NDAI_histogram <- ggplot(data = testing) +
  geom_histogram(aes(x = NDAI, fill = rfClasses))

grid.arrange(true_NDAI_histogram, predict_NDAI_histogram)
```

```{r, eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
rfClasses <- predict(rf_model, 
                     newdata = testing)

# Predictions for test set taken from rfClasses
rf_test_predictions <- as.data.frame(rfClasses)

head(rfClasses)
# Add test predictions to the testing data with true values
testing <- testing %>%
  bind_cols(rf_test_predictions)

# Plot the cloud coverage for the true values
true_plot <- ggplot(data = testing, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot the predicted cloud coverage
predict_plot <- ggplot(data = testing, aes(x = x, y = y)) +
  geom_point(aes(colour = rfClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Grid arrange the two plots side-by-side
grid.arrange(true_plot, predict_plot, ncol = 2)

# Subset only to misclassified points
misclassified_regions <- testing %>%
  filter(expert_alpha_label != rfClasses)

# Plot of the true values with only the misclassified obs.
misclass_true_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = expert_alpha_label)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

# Plot of the predicted values with only misclassified obs.
misclass_predict_plot <- ggplot(data = misclassified_regions, aes(x = x, y = y)) +
  geom_point(aes(colour = rfClasses)) +
  ggtitle("Plot of Cloud Cover for Image 1") + 
  xlab("X-Coordinate") +
  ylab("Y-Coordinate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = "right") +
  scale_y_reverse()

grid.arrange(misclass_true_plot, misclass_predict_plot, ncol = 2)

# Histogram of the NDAI distribution for true values
true_NDAI_histogram <- ggplot(data = testing) +
  geom_histogram(aes(x = NDAI, fill = expert_alpha_label)) 

# Histogram of the NDAI distribution for the predicted values
predict_NDAI_histogram <- ggplot(data = testing) +
  geom_histogram(aes(x = NDAI, fill = rfClasses))

grid.arrange(true_NDAI_histogram, predict_NDAI_histogram)
```