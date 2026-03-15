library(haven)
library(dplyr)
library(randomForest)

# ================= LOAD DATA =================
demo  <- read_xpt("datasets/nhanes/DEMO_J.XPT")
dpq   <- read_xpt("datasets/nhanes/DPQ_J.XPT")
slq   <- read_xpt("datasets/nhanes/SLQ_J.XPT")
bmx   <- read_xpt("datasets/nhanes/BMX_J.XPT")
bpx   <- read_xpt("datasets/nhanes/BPX_J.XPT")
glu   <- read_xpt("datasets/nhanes/GLU_J.XPT")
chol  <- read_xpt("datasets/nhanes/TCHOL_J.XPT")
trig  <- read_xpt("datasets/nhanes/TRIGLY_J.XPT")

# ================= MERGE DATA =================
mental <- dpq %>% inner_join(slq, by = "SEQN")
phys   <- bmx %>% inner_join(bpx, by = "SEQN")

bio <- glu %>%
  inner_join(trig, by = "SEQN") %>%
  inner_join(chol, by = "SEQN")

bio_demo   <- bio %>% inner_join(demo, by = "SEQN")
mental_bio <- mental %>% inner_join(bio_demo, by = "SEQN")
master     <- mental_bio %>% inner_join(phys, by = "SEQN")

# ================= MENTAL HEALTH FEATURE =================
phq_items <- c("DPQ010","DPQ020","DPQ030","DPQ040",
               "DPQ050","DPQ060","DPQ070","DPQ080","DPQ090")

for(col in phq_items){
  master[[col]][master[[col]] %in% c(7,9)] <- NA
}

master$phq9 <- rowSums(master[, phq_items], na.rm = TRUE)

master$SLD012[master$SLD012 %in% c(77,99)] <- NA

# ================= PHYSIOLOGICAL FEATURE ENGINEERING =================
master$BP_sys_avg <- rowMeans(master[,c("BPXSY1","BPXSY2","BPXSY3","BPXSY4")], na.rm=TRUE)
master$BP_dia_avg <- rowMeans(master[,c("BPXDI1","BPXDI2","BPXDI3","BPXDI4")], na.rm=TRUE)

master$chol_trig_ratio <- master$LBXTC / master$LBXTR
master$waist_hip_ratio <- master$BMXWAIST / master$BMXHIP

# ================= PCA PHYSICAL HEALTH SCORE =================
risk_data <- master %>%
  select(LBXGLU, LBXTC, LBXTR, LBDLDL, BMXBMI, BP_sys_avg, BP_dia_avg)

risk_data <- na.omit(risk_data)

for(col in names(risk_data)){
  upper <- quantile(risk_data[[col]], 0.99)
  lower <- quantile(risk_data[[col]], 0.01)
  risk_data[[col]][risk_data[[col]] > upper] <- upper
  risk_data[[col]][risk_data[[col]] < lower] <- lower
}

risk_scaled <- scale(risk_data)

pca_model <- prcomp(risk_scaled)

var_prop <- summary(pca_model)$importance[2,]

risk_score <- pca_model$x[,1]*var_prop[1] +
  pca_model$x[,2]*var_prop[2] +
  pca_model$x[,3]*var_prop[3]

master$risk_score <- NA
master$risk_score[complete.cases(master[,c("LBXGLU","LBXTC","LBXTR","LBDLDL","BMXBMI","BP_sys_avg","BP_dia_avg")])] <- risk_score

master$risk_score_scaled <- as.numeric(scale(master$risk_score))

# ================= FINAL ML DATASET =================
reg_data <- master %>%
  select(
    risk_score_scaled,
    phq9,
    SLD012,
    RIDAGEYR,
    RIAGENDR,
    BP_sys_avg,
    BP_dia_avg,
    BMXWAIST,
    waist_hip_ratio,
    chol_trig_ratio,
    INDFMPIR,
    RIDRETH3
  )

reg_data <- na.omit(reg_data)

# ================= FEATURE ENGINEERING =================
reg_data$phq9_sq   <- reg_data$phq9^2
reg_data$age_sq    <- reg_data$RIDAGEYR^2
reg_data$sleep_dev <- abs(reg_data$SLD012 - 7)

reg_data$phq9_age  <- reg_data$phq9 * reg_data$RIDAGEYR
reg_data$sleep_age <- reg_data$SLD012 * reg_data$RIDAGEYR

# ================= TRAIN TEST SPLIT =================
set.seed(42)

train_idx <- sample(1:nrow(reg_data), 0.8 * nrow(reg_data))

train <- reg_data[train_idx, ]
test  <- reg_data[-train_idx, ]

# ================= RANDOM FOREST MODEL =================
rf_model <- randomForest(
  risk_score_scaled ~ .,
  data = train,
  ntree = 1000,
  mtry = 4,
  importance = TRUE
)

# ================= EVALUATION =================
pred_test <- predict(rf_model, test)

r2 <- cor(pred_test, test$risk_score_scaled)^2
r2

# Testing the model for overfitting/bias
importance(rf_model)
varImpPlot(rf_model)


library(caret)
# Cross Validation
train_control <- trainControl(method="cv", number=5)

rf_cv <- train(
  risk_score_scaled ~ .,
  data = reg_data,
  method = "rf",
  trControl = train_control,
  ntree = 500
)

rf_cv

varImp(rf_cv)
plot(varImp(rf_cv))

importance(rf_model)
varImpPlot(rf_model)