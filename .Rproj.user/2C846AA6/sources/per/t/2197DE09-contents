library(randomForest)
library(dplyr)
library(stringr)

# Datasets
mental <- read.csv("datasets/Mental_Health_Lifestyle_Dataset.csv")
sleep <- read.csv("datasets/Sleep_health_and_lifestyle_dataset.csv")

# Data Information
head(mental)
head(sleep)

str(mental)
str(sleep)

summary(mental)
summary(sleep)

# Data Preprocessing
# Calibrate of mental stress to sleep dataset scale
mental$Stress.Level <- recode(mental$Stress.Level,
                              "Low" = 3.5,
                              "Moderate" = 5.5,
                              "High" = 7.5)

# Calibrate exercise level
mental$Exercise.Level <- recode(mental$Exercise.Level,
                                "Low" = 40,
                                "Moderate" = 60,
                                "High" = 80)


# Rename Identical Columns

mental <- mental %>%
  rename(
    Stress = Stress.Level,
    Sleep = Sleep.Hours,
    Activity = Exercise.Level,
    Work_Hours = Work.Hours.per.Week,
    Screen_Time = Screen.Time.per.Day..Hours.,
    Social_Score = Social.Interaction.Score,
    Happiness = Happiness.Score
  )

sleep <- sleep %>%
  rename(
    Stress = Stress.Level,
    Sleep = Sleep.Duration,
    Activity = Physical.Activity.Level,
    Sleep_Quality = Quality.of.Sleep,
    Steps = Daily.Steps,
    HR = Heart.Rate
  )

sleep$BP_Sys <- as.numeric(str_extract(sleep$Blood.Pressure, "^[0-9]+"))
sleep$BP_Dia <- as.numeric(str_extract(sleep$Blood.Pressure, "[0-9]+$"))

sleep$BMI <- factor(sleep$BMI.Category,
                    levels = c("Normal","Overweight","Obese"),
                    ordered = TRUE)

sleep$BMI <- as.numeric(sleep$BMI)

sleep$BMI[is.na(sleep$BMI)] <- median(sleep$BMI, na.rm = TRUE)

# Select required variables for target variable
phys_data <- sleep %>%
  select(BMI, HR, BP_Sys, BP_Dia, Activity, Sleep_Quality, Age)

colSums(is.na(phys_data))

# PCA Training
phys_scaled <- scale(phys_data)
pca_model <- prcomp(phys_scaled)
sleep$Physical_Risk <- pca_model$x[,1]
summary(pca_model)

pca_model$rotation

set.seed(42)

train_index <- sample(1:nrow(sleep), 0.8 * nrow(sleep))

train_data <- sleep[train_index, ]
test_data  <- sleep[-train_index, ]

sleep_model <- sleep %>%
  select(Physical_Risk, Stress, Sleep, Activity, Age)

sleep_model_scaled <- as.data.frame(scale(sleep_model))

# model_linear <- lm(Physical_Risk ~ Stress + Sleep + I(Sleep^2) +
#                     Activity + Age +
#                     Stress:Activity,
#                   data = sleep_model_scaled)

# summary(model_linear)
train_linear <- lm(Physical_Risk ~ Stress + Sleep + Activity + Age,
                   data = train_data)

pred_linear <- predict(train_linear, test_data)

linear_r2 <- cor(pred_linear, test_data$Physical_Risk)^2
linear_r2

train_rf <- randomForest(Physical_Risk ~ Stress + Sleep + Activity + Age,
                         data = train_data,
                         ntree = 500)

pred_rf <- predict(train_rf, test_data)

rf_r2 <- cor(pred_rf, test_data$Physical_Risk)^2
rf_r2

# rf_model <- randomForest(Physical_Risk ~ Stress + Sleep + Activity + Age,
#                         data = sleep,
#                         ntree = 500)

# rf_model

# importance(rf_model)
# varImpPlot(rf_model)

# summary(model_linear)$r.squared
# pred_rf <- predict(rf_model, sleep)

# cor(pred_rf, sleep$Physical_Risk)^2

mental$Predicted_Risk_LR <- predict(train_linear, mental)
mental$Predicted_Risk_RF <- predict(train_rf, mental)

summary(mental$Predicted_Risk_LR)
summary(mental$Predicted_Risk_RF)

hist(mental$Predicted_Risk_LR)
hist(mental$Predicted_Risk_RF)

cor(mental$Predicted_Risk_LR,
    mental$Predicted_Risk_RF)

tapply(mental$Predicted_Risk_RF,
       mental$Mental.Health.Condition,
       mean)

anova_result <- aov(Predicted_Risk_RF ~ Mental.Health.Condition, data = mental)
summary(anova_result)