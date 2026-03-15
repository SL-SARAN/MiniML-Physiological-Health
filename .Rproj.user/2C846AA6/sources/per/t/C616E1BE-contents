
phys_vars <- c(
  "LBXGLU",
  "LBXTC",
  "LBXTR",
  "LBDLDL",
  "BMXBMI",
  "BPXSY1",
  "BPXDI1"
)

risk_data <- master %>%
  select(all_of(phys_vars))

summary(risk_data)


master$risk_glucose <- ifelse(master$LBXGLU >= 126, 1, 0)

master$risk_bp <- ifelse(master$BPXSY1 >= 130 | master$BPXDI1 >= 80, 1, 0)

master$risk_bmi <- ifelse(master$BMXBMI >= 30, 1, 0)

master$risk_trig <- ifelse(master$LBXTR >= 150, 1, 0)

master$risk_chol <- ifelse(master$LBXTC >= 240, 1, 0)

master$cardio_risk <- ifelse(
  master$risk_glucose +
    master$risk_bp +
    master$risk_bmi +
    master$risk_trig +
    master$risk_chol >= 2,
  1, 0
)

table(master$cardio_risk)
prop.table(table(master$cardio_risk))

phq_items <- c("DPQ010","DPQ020","DPQ030","DPQ040",
               "DPQ050","DPQ060","DPQ070","DPQ080","DPQ090")

for(col in phq_items){
  master[[col]][master[[col]] %in% c(7,9)] <- NA
}

master$phq9 <- rowSums(
  master[, c("DPQ010","DPQ020","DPQ030","DPQ040",
             "DPQ050","DPQ060","DPQ070","DPQ080","DPQ090")],
  na.rm = TRUE
)

summary(master$phq9)

master$SLD012[master$SLD012 %in% c(77,99)] <- NA
summary(master$SLD012)

model_data <- master %>%
  select(cardio_risk, phq9, SLD012, RIDAGEYR, RIAGENDR)

model_data <- na.omit(model_data)

dim(model_data)

table(model_data$cardio_risk)
prop.table(table(model_data$cardio_risk))

set.seed(42)

train_index <- sample(1:nrow(model_data), 0.8 * nrow(model_data))

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

dim(train_data)
dim(test_data)

# Logistic Model
model_log <- glm(cardio_risk ~ phq9 + SLD012 + RIDAGEYR + RIAGENDR,
                 data = train_data,
                 family = binomial)

summary(model_log)

# Model accuracy for logistic model
pred_prob <- predict(model_log, test_data, type = "response")

pred_class <- ifelse(pred_prob > 0.5, 1, 0)

table(pred_class, test_data$cardio_risk)

mean(pred_class == test_data$cardio_risk)


# RandomForest Model

rf_model <- randomForest(
  as.factor(cardio_risk) ~ phq9 + SLD012 + RIDAGEYR + RIAGENDR,
  data = train_data,
  ntree = 500,
  mtry = 2,
  importance = TRUE
)

rf_model

rf_pred <- predict(rf_model, test_data)

table(rf_pred, test_data$cardio_risk)

mean(rf_pred == test_data$cardio_risk)
