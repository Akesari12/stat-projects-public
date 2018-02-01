# all_image only contain data points from image 1 and 2, because we
# want to use image 1 and 2 for training only
# and saved image 3 for training
# all_image was created in load_clean_data.R
all_image <- readRDS( "R/all_image.rds")

# Cross-Validation Learning Vector Quantization
## We used 1% data for feature selection

set.seed(12345678)
all_data_subset <- all_image %>%
  sample_frac(.1, replace = FALSE) %>%
  select(NDAI, SD, CORR, rad_ang_df, rad_ang_an, 
         expert_label, expert_alpha_label) %>%
  filter(expert_label != 0)

all_data_subset <- as.data.frame(all_data_subset)

# Create the control method
control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3)

# Train the model
model <- train(as.factor(expert_label) ~ 
                 CORR + SD + NDAI + rad_ang_df + rad_ang_an, 
               data = all_data_subset, 
               method = "lvq", 
               trControl = control)

# Estimate Variable Importance
importance <- varImp(model, scale = FALSE)

#saveRDS(importance, "R/cvlvq.rds")


png("R/feature_selection.png", width=7, height=3, units="in", res=300)
plot(importance, main = "LVQ Variable Importance")
dev.off()
