library(readr)
library(tidyverse)

image_1 <- read.table("data/image1.txt") 
image_2 <- read.table("data/image2.txt")
image_3 <- read.table("data/image3.txt")

# Rename columns and add Categorical Labels for Expert Labels
image_1 <- image_1 %>%
  rename(y = V1, 
         x = V2, 
         expert_label = V3, 
         NDAI = V4, 
         SD = V5, 
         CORR = V6, 
         rad_ang_df = V7, 
         rad_ang_cf = V8, 
         rad_ang_bf = V9, 
         rad_ang_af = V10, 
         rad_ang_an = V11) %>%
  mutate(expert_alpha_label = 
           if_else(expert_label == 1, "cloud",
                   if_else(expert_label == -1, "clear",
                           "unknown")))

image_2 <- image_2 %>%
  rename(y = V1, 
         x = V2, 
         expert_label = V3, 
         NDAI = V4, 
         SD = V5, 
         CORR = V6, 
         rad_ang_df = V7, 
         rad_ang_cf = V8, 
         rad_ang_bf = V9, 
         rad_ang_af = V10, 
         rad_ang_an = V11) %>%
  mutate(expert_alpha_label = 
           if_else(expert_label == 1, "cloud",
                   if_else(expert_label == -1, "clear", "unknown")))

image_3 <- image_3 %>%
  rename(y = V1, 
         x = V2, 
         expert_label = V3, 
         NDAI = V4, 
         SD = V5, 
         CORR = V6, 
         rad_ang_df = V7, 
         rad_ang_cf = V8, 
         rad_ang_bf = V9, 
         rad_ang_af = V10, 
         rad_ang_an = V11) %>%
  mutate(expert_alpha_label = 
           if_else(expert_label == 1, "cloud",
                   if_else(expert_label == -1, "clear", "unknown")))

all_image <- image_1 %>%
  bind_rows(image_2, .id = "image_source")

saveRDS(image_1, "R/image_1.rds")
saveRDS(image_2, "R/image_2.rds")
saveRDS(image_3, "R/image_3.rds")
saveRDS(all_image, "R/all_image.rds")