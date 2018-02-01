# Load Libraries
#install.packages("foba")
#install.packages("lars")
#install.packages("ranger")
library(tidyverse)
library(caret)
library(rgl)
#library(HDCI)
#library(corrplot)
#library(superheat)
library(pROC)
library(glmnet)
#library(foba)
library(lars)
library(ranger)
library(doParallel)
library(caretEnsemble)

## Load Data
load("~/stat215a/lab_final/data/fMRIdata.RData")
#fit_stim <- read_csv("C:/Users/Anike/Desktop/fit_stim.csv")
#real_wav <- read_csv("C:/Users/Anike/Desktop/real_wav.csv")


# Data Cleaning/Processing

# Add column names to feature dataframe
fit_feat <- as.data.frame(fit_feat)
val_feat <- as.data.frame(val_feat)
loc_dat <- as.data.frame(loc_dat)
resp_dat <- as.data.frame(resp_dat)

# Rename columns to take form "feature_#"
fit_feat <- fit_feat %>%
  rename_all(
    funs(
      stringr::str_to_lower(.) %>%
        stringr::str_replace_all("^v", 'feature.')
    )
  )

val_feat <- val_feat %>%
  rename_all(
    funs(
      stringr::str_to_lower(.) %>%
        stringr::str_replace_all("^v", 'feature.')
    )
  )

#real_wav <- real_wav %>%
#  rename_all(
#    funs(
#      stringr::str_to_lower(.) %>%
#        stringr::str_replace_all("^v", "feature.")
#    )
#  )

# Add voxel IDs

loc_dat <- loc_dat %>%
  mutate(voxel_id = paste0('voxel.', row_number())) %>%
  rename(X = V1, Y = V2, Z = V3)

resp_dat <- resp_dat %>%
  rename_all(
    funs(
      stringr::str_to_lower(.) %>%
        stringr::str_replace_all("^v", "voxel.")
    )
  )

# Create a Master Dataset

master_data <- resp_dat %>%
  bind_cols(fit_feat)

# Exploratory Data Analysis

# Partititon the Data

# Create partitions

inTrain <- createDataPartition(y = master_data$voxel.1,
                               p = .6,
                               list = FALSE)

## Training

training <- master_data[inTrain,]

## Validation and Test Sets
temp <- master_data[-inTrain,]
inTemp <- createDataPartition(y = temp$voxel.1,
                              p = .5,
                              list = FALSE)

validation <- temp[inTemp, ]
testing <- temp[-inTemp, ]

# Fit ML Models
train.2 <- training %>%
  select(voxel.1, 21:10941)

zero_var_cols <- nearZeroVar(train.2)
train.2 <- train.2 %>%
  select(-zero_var_cols)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

rctrl1 <- trainControl(method = "boot", 
                       savePredictions = "final",
                       number = 2, 
                       allowParallel = TRUE,
                       index = createResample(train.2$voxel.1, 2),
                       returnData = FALSE)

model_list <- caretList(
  voxel.1 ~ .,
  data = train.2,
  trControl = rctrl1,
  methodList = c("lasso", "ridge", "ranger")
)

ensemble_model <- caretEnsemble(
  model_list,
  trControl = trainControl(
    number = 2)
)

stopCluster(cl)

saveRDS(ensemble_model, "ensemble_model.rds")