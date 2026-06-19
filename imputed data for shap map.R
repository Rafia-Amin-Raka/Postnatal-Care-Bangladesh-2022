library(FSelectorRcpp)
library(caret)
library(randomForest)
library(FactoMineR)
library(factoextra)
library(VIM)
library(missRanger)
library(tidyverse)
library(haven)    # For reading DHS datasets (SPSS, Stata, SAS)
library(dplyr)    # For data manipulation
library(survey)   # For survey-weighted analysis
library(ggplot2)  # For visualization


# Read DHS dataset SPSS .sav file
NRdata <- read_sav(file.choose())

# View the structure of the dataset
#glimpse( NRdata)
dim( NRdata)

colnames( NRdata)


#..................................
# #Identify Key Variables for Survey Design
#V001 = dhs cluster
#v002 = household number
# v003 = respondand line number
# V021 = Cluster variable: Primary sampling unit (PSU)
# V023 = Stratification variable
# V005 = Sampling weights
#CASEID

# M80 = Pregnancy outcome for this section 
# P19 = Months since pregnancy outcome 

# m62 = Respondent's health checked before discharge
# m63 = How long after delivery respondent’s health check took place before discharg
# m64 = Who checked respondent health before discharge
# m66 = Respondent's health checked after discharge/delivery at home
# m67 = How long after discharge/delivery at home respondent health check took place
# m68 = Who checked respondent health after discharge/delivery at home

# V012 = Respondents current age
# V024 =	Region (Division)
# V025 =	Type of residence (urban/rural)
# V106 =	Education level (Highest education level)
# V130 =	Religion
# V501 =	Current marital status
# V190 =	Wealth index
# 
# PORD = Pregnancy order number () 
# M10 =	Wanted pregnency when become pregnant?
# V169A =	Owns a mobile telephone
# M15 =	Place of delivery
# M14 =	Number of ANC Visits
# V525 =	Age at first sex
#v467A = distance to health facili
# 
# V701 =	Husband/partner's education level
# V714 =	Working/Employment Status of the respondents, currently working or not
# V704 =	Husband/partner's occupation
# V705 =	Husband/partner's occupation grouped
# V481 =  Covered by any health insurance


#V157 = Frequency of reading newspaper or magazine (women)
#V158 = Frequency of listening to radio (women)
#V159 = Frequency of watching television (wom



pnc_data <- NRdata %>%
  select(V001, V002, V003, V021, V023, V005, M80, P19, M62, M63, M64, M66, M68, M67,
         V157, V158, V159, V012, V024, V025, V106, V130, V501,
         V190, PORD, M10,M14, M15, V701, V714, V705, V467D, V481, V743A,
         CASEID) %>%                          # include case ID
  filter(M80 %in% c(1, 3), P19 < 24) # %>%    # live births & stillbirths in 2 years
#  arrange(CASEID, P19) %>%                   # sort by woman ID, then timing
#  group_by(CASEID) %>%                              # keep only the most recent birth (smallest P19)
#  ungroup()

# Check unweighted N
dim(pnc_data)
colSums(is.na(pnc_data)) 
# Check weighted N
sum(pnc_data$V005 / 1000000)

table(pnc_data$V501)


#-----------------------outcome variable-------
classify_pnc <- function(M62, M64, M63, M66, M68, M67) {
  
  #Checked before discharge at facility by skilled provider
  if (!is.na(M62) && M62 == 1 &&
      !is.na(M64) && M64 %in% 10:29) {
    if (!is.na(M63) &&
        (M63 %in% 100:171 ||
         M63 %in% 200:202 ||
         M63 %in% c(198, 199))) {
      return("1") #yes
    } else {
      return("0") #No
    }
  }
  #Checked after discharge/home delivery by skilled provider
  if (!is.na(M66) && M66 == 1 &&
      !is.na(M68) && M68 %in% 10:29) {
    if (!is.na(M67) &&
        (M67 %in% 100:171 ||
         M67 %in% 200:202 ||
         M67 %in% c(198, 199))) {
      return("1") #yes
    } else {
      return("0") #No
    }
  }
  
  return("0") #No
}

# Apply classification
pnc_data$PNC_care <- mapply(classify_pnc,
                            pnc_data$M62, pnc_data$M64, pnc_data$M63,
                            pnc_data$M66, pnc_data$M68, pnc_data$M67)
# Check distribution before proceeding
cat("── PNC Classification Check ──\n")
freq_table    <- table(pnc_data$PNC_care, useNA = "ifany")
percent_table <- round(prop.table(freq_table) * 100, 1)
print(freq_table)
print(percent_table)

# Weighted PNC proportion — should be ~55.2%
pnc_data %>%
  mutate(wt = as.numeric(V005) / 1e6) %>%
  group_by(PNC_care) %>%
  summarise(n_weighted = sum(wt), .groups = "drop") %>%
  mutate(pct = round(n_weighted / sum(n_weighted) * 100, 1)) %>%
  print()

#========================

pnc_data <- pnc_data %>%
  mutate(
    Age = case_when(
      V012 >= 15 &  V012 <= 19 ~ "0", #15-19
      V012 >= 20 &  V012 <= 34 ~ "1", #20-34
      V012 >= 35 &  V012 <= 49 ~ "2", #35-49
    )
  )

# Frequency with percentage
pnc_data %>%
  count(Age) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

#------marital status
table(pnc_data$V501)
pnc_data <- pnc_data %>%
  mutate(
    Marital_status = case_when(
      V501 == 1 ~ "1", #Married,
      V501 %in% c(3, 4, 5) ~ "2", #Widowed/Divorced/Separated
    )
  )

pnc_data %>%
  count( Marital_status) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

#--birth order

pnc_data <- pnc_data %>%
  mutate(
    Birth_order = case_when(
      PORD == 1 ~ "1",
      PORD %in% 2:3 ~ "2", #2-3
      PORD %in% 4:5 ~ "3", #4-5
      PORD >= 6     ~ "4", #6+
      
    )
  )


pnc_data %>%
  count( Birth_order) %>%
  mutate(Percent = round(100 * n / sum(n), 1))


#----place of delivery
pnc_data <- pnc_data %>%
  mutate(
    Place_of_delivery = case_when(
      M15 %in% c(20:49, 50, 96) ~ "1", #Health facility
      M15 %in% c(10, 11) ~ "2", #Home
    )
  )


pnc_data %>%
  count(Place_of_delivery) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

#----------husband occupation
pnc_data <- pnc_data %>%
  mutate(
    Husbands_occupation = case_when(
      V705 %in% c(1:9) ~ "1", #Employed
      V705 %in% c(0,98) ~ "2", #Unemployed 
    )
  )

pnc_data$V705
table(pnc_data$V701)
pnc_data %>%
  count(  Husbands_occupation) %>%
  mutate(Percent = round(100 * n / sum(n), 1))




#Recode media exposure variables
pnc_data <- pnc_data %>%
  mutate(
    radio_exposure = ifelse(V158 %in% c(2, 3), 1, 0),  # 1 = Daily or weekly exposure
    tv_exposure = ifelse(V159 %in% c(2, 3), 1, 0),      # 1 = Daily or weekly exposure
    newspaper_exposure = ifelse(V157 %in% c(2, 3), 1, 0) # 1 = Daily or weekly exposure
  )

# Create a combined media exposure variable
pnc_data <- pnc_data %>%
  mutate(
    Media_exposure = ifelse(radio_exposure == 1 | tv_exposure == 1 | newspaper_exposure == 1, 1, 0)
  )

# Check distribution of the new variable
table(pnc_data$Media_exposure)
pnc_data %>%
  count(Media_exposure) %>%
  mutate(Percent = round(100 * n / sum(n), 1))


##------------------------ Decision making on helthcare seeking 

# Recode the variable into 3 categories
pnc_data<- pnc_data %>%
  mutate(Decision_in_healthcare = case_when(
    V743A %in% c(1, 2, 3) ~ "1",   #Woman involved Categories 1, 2, 3
    V743A == 4 ~ "2",               #Partner alone Category 4
    V743A %in% c(5, 6) ~ "3",              #Others Categories 5, 6
    TRUE ~ NA_character_  # Assign NA for missing values
  ))
# Frequency with percentage
pnc_data %>%
  count(Decision_in_healthcare) %>%
  mutate(Percent = round(100 * n / sum(n), 1))
pnc_data$V743A #HAS MISSING VALUE



#anc_visit
pnc_data <- pnc_data %>%
  mutate(
    ANC_visit = case_when(
      M14 %in% c(0,98) ~ "0", #No visit
      M14 %in% 1:4 ~ "1", #4 visit
      M14 >= 5 ~ "2" #More than 4 visit
    )
  )

pnc_data %>%
  count(ANC_visit) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

dim(pnc_data)
colSums(is.na(pnc_data))



pnc_data <- pnc_data %>%
  rename(
    Region = V024 ,
    Residence = V025,
    Mothers_Education = V106,
    Religion = V130,
    Wealth_index = V190,
    Wanted_pregnancy = M10,
    Husbands_education = V701,
    Maternal_working = V714,
    Health_insurance = V481 
  )

#saveRDS(pnc_data,"C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/model 3/data/pnc_data.rds")
#write.csv(pnc_data,"C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/model 3/data/pnc_data.csv",row.names = FALSE)



# Load required packages
library(VIM)
library(Metrics)
library(dplyr)

# Your dataset (replace with your own data object if needed)
#df <- pnc_data

# # Choose a variable that has no missing values
# # (for simulation; we’ll artificially remove some values)
# test_var <- "Wealth_index"  # choose a fully observed variable
# 
# # Check if chosen variable has NAs
# sum(is.na(df[[test_var]]))
# 
# # Define range of k values to test
# k_values <- seq(3, 50, by = 1)
# 
# # Number of times to repeat masking to stabilize results
# n_repeats <- 5
# 
# # Storage for RMSE results
# rmse_summary <- numeric(length(k_values))
# 
# # ========================
# # Loop over k values
# # ========================
# for (i in seq_along(k_values)) {
#   rmse_runs <- c()
#   
#   for (r in 1:n_repeats) {
#     set.seed(100 + r)  # different random seeds for robustness
#     
#     # Make a temporary copy of the data
#     df_temp <- df
#     
#     # Randomly mask 100 values (or adjust as needed)
#     available_idx <- which(!is.na(df_temp[[test_var]]))
#     missing_idx <- sample(available_idx, 100)
#     
#     true_values <- df_temp[[test_var]][missing_idx]
#     df_temp[[test_var]][missing_idx] <- NA
#     
#     # Perform KNN imputation with the current k
#     imp <- kNN(df_temp, k = k_values[i], imp_var = FALSE)
#     
#     # Extract imputed values
#     imputed_values <- imp[[test_var]][missing_idx]
#     
#     # Calculate RMSE for this run
#     rmse_runs <- c(rmse_runs, rmse(true_values, imputed_values))
#   }
#   
#   # Average RMSE for this k
#   rmse_summary[i] <- mean(rmse_runs)
# }
# 
# # ========================
# # Summarize and visualize
# # ========================
# 
# # Create results table
# result_table <- data.frame(k = k_values, mean_RMSE = rmse_summary)
# print(result_table)
# 
# # Find best k
# best_k <- result_table$k[which.min(result_table$mean_RMSE)]
# cat("✅ Optimal k value based on minimum average RMSE:", best_k, "\n")
# # Plot
# plot(result_table$k, result_table$mean_RMSE, type = "b", pch = 19, col = "blue",
#      main = "Optimal k Selection for KNN Imputation",
#      xlab = "Number of Neighbors (k)",
#      ylab = "Average RMSE (over repeated masking)")
# abline(v = best_k, col = "red", lty = 2, lwd = 2)
# text(best_k, min(result_table$mean_RMSE), labels = paste("Best k =", best_k),
#      pos = 4, col = "red", cex = 0.9)
# 
# 
# 
# 
# 
# 

















# Load necessary packages
#install.packages("VIM")  # if not installed
library(VIM)
library(dplyr)

# Ensure your columns are factors
pnc_data <- pnc_data %>%
  mutate(
    Husbands_education = as.factor(as.character(Husbands_education)),
    Husbands_occupation = as.factor(as.character(Husbands_occupation)),
    Decision_in_healthcare = as.factor(as.character(Decision_in_healthcare))
  )

# Columns to impute
target_cols <- c("Husbands_education", "Husbands_occupation","Decision_in_healthcare")

# KNN imputation (k = 18/30)
pnc_data <- kNN(pnc_data,variable = target_cols,k = 30, imp_var = FALSE)

# Check if missing values are imputed
pnc_data %>%
  summarise(across(all_of(target_cols), ~ sum(is.na(.))))

# Optional: view distribution after imputation
pnc_data %>%
  count(Husbands_education) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

pnc_data %>%
  count(Husbands_occupation) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

pnc_data %>%
  count(Decision_in_healthcare) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

colnames(pnc_data)
colSums(is.na(pnc_data))

table(pnc_data$Husbands_occupation)




# # Function to summarize counts for plotting
# summarize_counts <- function(df, var, label){
#   df %>%
#     count(!!sym(var)) %>%
#     mutate(Data = label)
# }
# 
# # Get counts
# edu_counts_orig <- summarize_counts(pnc_data_clas, "Husband_education", "Original")
# edu_counts_imp  <- summarize_counts(pnc_imp_data, "Husband_education", "Imputed")
# edu_counts <- bind_rows(edu_counts_orig, edu_counts_imp)
# 
# occ_counts_orig <- summarize_counts(pnc_data_class, "Husband_occupation", "Original")
# occ_counts_imp  <- summarize_counts(pnc_imp_data, "Husband_occupation", "Imputed")
# occ_counts <- bind_rows(occ_counts_orig, occ_counts_imp)
# 
# decision_counts_orig <- summarize_counts(pnc_data_class, "decision_in_healthcare", "Original")
# decision_counts_imp  <- summarize_counts(pnc_imp_data, "decision_in_healthcare", "Imputed")
# decision_counts <- bind_rows(decision_counts_orig, decision_counts_imp)
# 
# # Plot with geom_point for Husband_education
# ggplot(edu_counts, aes(x = Husband_education, y = n, color = Data)) +
#   geom_point(size = 4, position = position_dodge(width = 0.5)) +
#   theme_minimal() +
#   labs(title = "Husband Education: Original vs Imputed",
#        x = "Husband Education",
#        y = "Count") +
#   scale_color_manual(values = c("Original" = "steelblue", "Imputed" = "orange"))
# 
# # Plot with geom_point for Husband_occupation
# ggplot(occ_counts, aes(x = Husband_occupation, y = n, color = Data)) +
#   geom_point(size = 3, position = position_dodge(width = 0.5)) +
#   theme_minimal() +
#   labs(title = "Husband Occupation: Original vs Imputed",
#        x = "Husband Occupation",
#        y = "Count") +
#   scale_color_manual(values = c("Original" = "steelblue", "Imputed" = "orange")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Plot with geom_point for Husband_occupation
# ggplot(decision_counts, aes(x = decision_in_healthcare, y = n, color = Data)) +
#   geom_point(size = 3, position = position_dodge(width = 0.5)) +
#   theme_minimal() +
#   labs(title = "decision_in_healthcare: Original vs Imputed",
#        x = "decision_in_healthcare",
#        y = "Count") +
#   scale_color_manual(values = c("Original" = "steelblue", "Imputed" = "orange")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# pnc_imp_data %>%
#   count(  Husband_occupation) %>%
#   mutate(Percent = round(100 * n / sum(n), 1))
# 
# 
# pnc_imp_data %>%
#   count(  decision_in_healthcare) %>%
#   mutate(Percent = round(100 * n / sum(n), 1))
# 
# 
# pnc_imp_data %>%
#   count(  Husband_education) %>%
#   mutate(Percent = round(100 * n / sum(n), 1))
# 


pnc_data_imp_clean <- pnc_data %>%
  select(
    V001, # dhs cluster
    V001, #dhs cluster
    V002, #household number
    V003, #espondand line number
    V021, # Cluster variable: Primary sampling unit (PSU)
    V023, # Stratification variable
    V005, # Sampling weights
    CASEID,
     M80,# Pregnancy outcome for this section 
     P19, # Months since pregnancy outcome 
    M62, # Respondent's health checked before discharge
    M63, # How long after delivery respondent’s health check took place before discharg
    M64, # Who checked respondent health before discharge
    M66, # Respondent's health checked after discharge/delivery at home
    M67, # How long after discharge/delivery at home respondent health check took place
    M68, # Who checked respondent health after discharge/delivery at home
    
    PNC_care, Age, Region,Residence, Mothers_Education,Religion,Marital_status,Wealth_index,
    Birth_order,Wanted_pregnancy,Place_of_delivery,Husbands_education, Maternal_working,
    Husbands_occupation, Health_insurance, Media_exposure,Decision_in_healthcare, ANC_visit
  )

colnames(pnc_data_imp_clean)
dim(pnc_data_imp_clean)
colSums(is.na(pnc_data_imp_clean))


write_sav(pnc_data_imp_clean, path ="C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_imp_clean.sav")
write.csv(pnc_data_imp_clean, "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_imp_clean.csv", row.names = FALSE)
saveRDS(pnc_data_imp_clean,"C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_imp_clean.rds")








df <- readRDS("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_imp_clean.rds")

head(pnc_data_imp_clean)

dim(df)
colnames(df)
library(sf)
library(dplyr)

# ── Step 1: Load GPS cluster file ─────────────────────────────────────────
gps_data <- st_read("C:/Users/Raka/OneDrive/Thesis/data/GPS DATA/BDGE81FL/BDGE81FL.shp")
bgd_adm2 <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm2.rds")
bgd_adm1 <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm1.rds")

# ── Step 2: Check GPS data structure ──────────────────────────────────────
print(names(gps_data))       # find cluster ID column name
print(head(gps_data))
st_crs(gps_data)             # check CRS

# ── Step 3: Make sure CRS matches ─────────────────────────────────────────
bgd_adm2 <- st_transform(bgd_adm2, st_crs(gps_data))

# ── Step 4: Spatial join — assign each cluster to a district ──────────────
cluster_district <- st_join(
  gps_data,
  bgd_adm2 %>% select(NAME_1, NAME_2),  # division + district name
  join = st_within
)

# Check result
print(head(cluster_district %>% select(DHSCLUST, NAME_1, NAME_2)))
print(sum(is.na(cluster_district$NAME_2)))  # how many unmatched

# ── Step 5: Extract cluster→district lookup table ─────────────────────────
cluster_lookup <- cluster_district %>%
  st_drop_geometry() %>%
  select(DHSCLUST, Division = NAME_1, District = NAME_2) %>%
  distinct()

print(head(cluster_lookup))

# ── Step 6: Merge district onto your df via V001 (cluster ID) ─────────────
df <- df %>%
  left_join(cluster_lookup, by = c("V001" = "DHSCLUST"))

# Check
cat("Districts matched:", sum(!is.na(df$District)), "\n")
cat("Unmatched:", sum(is.na(df$District)), "\n")
print(head(df %>% select(V001, Division, District, Region, PNC_care)))


table(df$District)   # should show all 64 districts
table(df$Division)   # should show 8 divisions


library(xgboost)
library(caret)
library(pROC)
library(dplyr)
library(SHAPforxgboost)
library(sf)
library(ggplot2)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 1 — Prepare data
# ══════════════════════════════════════════════════════════════════════════════
selected_features <- c("Age", "Region", "Residence", "Mothers_Education",
                       "Wealth_index", "Wanted_pregnancy", "Religion",
                       "Place_of_delivery", "Husbands_education",
                       "Husbands_occupation")
outcome <- "PNC_care"

data_model <- df %>%
  select(all_of(c(selected_features, outcome, "District", "Division"))) %>%
  drop_na() %>%
  mutate(across(all_of(selected_features), as.numeric),
         PNC_care = as.integer(as.character(PNC_care)))

X        <- as.matrix(data_model[selected_features])
y        <- data_model[[outcome]]
district <- data_model$District
division <- data_model$Division

cat("Total rows:", nrow(data_model), "\n")
cat("PNC Yes:", sum(y == 1), "| PNC No:", sum(y == 0), "\n")
cat("NA in X:", sum(is.na(X)), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2 — Train/test split 80/20
# ══════════════════════════════════════════════════════════════════════════════
set.seed(123)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)

X_train   <- X[train_idx, ]
X_test    <- X[-train_idx, ]
y_train   <- y[train_idx]
y_test    <- y[-train_idx]
dist_test <- district[-train_idx]
div_test  <- division[-train_idx]

y_train_fac <- factor(y_train, levels = c(0,1), labels = c("No","Yes"))
y_test_fac  <- factor(y_test,  levels = c(0,1), labels = c("No","Yes"))

cat("Train:", nrow(X_train), "| Test:", nrow(X_test), "\n")
cat("Train PNC Yes:", sum(y_train==1), "| No:", sum(y_train==0), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 3 — Hyperparameter tuning (RandomizedSearchCV equivalent)
# ══════════════════════════════════════════════════════════════════════════════
set.seed(123)
param_grid_xgb <- expand.grid(
  nrounds          = c(100, 300, 500),
  max_depth        = c(3, 5, 7),
  eta              = c(0.01, 0.05, 0.1, 0.2),
  subsample        = c(0.7, 0.8, 1.0),
  colsample_bytree = c(0.7, 0.8, 1.0),
  gamma            = c(0, 0.1, 0.3),
  min_child_weight = 1
)

set.seed(123)
param_sample <- param_grid_xgb[sample(nrow(param_grid_xgb), 25), ]
rownames(param_sample) <- NULL

# ══════════════════════════════════════════════════════════════════════════════
# STEP 4 — Direct xgboost (avoids caret ROC NA issue)
# ══════════════════════════════════════════════════════════════════════════════
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# Manual 5-fold CV over 25 param combinations
set.seed(123)
folds <- createFolds(y_train, k = 5, list = TRUE)

best_auc    <- 0
best_params <- NULL
best_nrounds <- NULL

cat("Running randomized search over 25 parameter combinations...\n")

for (i in 1:nrow(param_sample)) {
  
  params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    max_depth        = param_sample$max_depth[i],
    eta              = param_sample$eta[i],
    subsample        = param_sample$subsample[i],
    colsample_bytree = param_sample$colsample_bytree[i],
    gamma            = param_sample$gamma[i],
    min_child_weight = 1,
    seed             = 123
  )
  
  cv_result <- xgb.cv(
    params   = params,
    data     = dtrain,
    nrounds  = param_sample$nrounds[i],
    folds    = folds,
    metrics  = "auc",
    verbose  = FALSE
  )
  
  mean_auc <- max(cv_result$evaluation_log$test_auc_mean)
  
  if (mean_auc > best_auc) {
    best_auc     <- mean_auc
    best_params  <- params
    best_nrounds <- param_sample$nrounds[i]
  }
}

cat("Best CV AUC:", round(best_auc, 4), "\n")
cat("Best nrounds:", best_nrounds, "\n")
cat("Best Params:\n"); print(best_params)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 5 — Train final model on full training set
# ══════════════════════════════════════════════════════════════════════════════
best_xgb <- xgb.train(
  params  = best_params,
  data    = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 6 — Test set evaluation
# ══════════════════════════════════════════════════════════════════════════════
y_prob_xgb <- predict(best_xgb, dtest)
y_pred_xgb <- factor(ifelse(y_prob_xgb >= 0.5, "Yes", "No"),
                     levels = c("No", "Yes"))

cm      <- confusionMatrix(y_pred_xgb, y_test_fac, positive = "Yes")
roc_obj <- roc(y_test, y_prob_xgb, quiet = TRUE)

cat("\n===== XGBoost Test Set Evaluation =====\n")
cat("Accuracy   :", round(cm$overall["Accuracy"],       4), "\n")
cat("Sensitivity:", round(cm$byClass["Sensitivity"],    4), "\n")
cat("Specificity:", round(cm$byClass["Specificity"],    4), "\n")
cat("Precision  :", round(cm$byClass["Pos Pred Value"], 4), "\n")
cat("F1 Score   :", round(cm$byClass["F1"],             4), "\n")
cat("AUC        :", round(auc(roc_obj),                 4), "\n")
print(cm$table)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 7 — 10-Fold CV
# ══════════════════════════════════════════════════════════════════════════════
set.seed(123)
folds_10 <- createFolds(y_train, k = 10, list = TRUE)

cv10_result <- xgb.cv(
  params   = best_params,
  data     = dtrain,
  nrounds  = best_nrounds,
  folds    = folds_10,
  metrics  = c("auc", "logloss"),
  verbose  = FALSE
)

fold_auc <- cv10_result$evaluation_log$test_auc_mean
cat("\n===== 10-Fold CV Results =====\n")
cat("Mean AUC :", round(mean(fold_auc), 4), "\n")
cat("SD AUC   :", round(sd(fold_auc),   4), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 8 — SHAP values
# ══════════════════════════════════════════════════════════════════════════════
shap_vals <- shap.values( xgb_model = best_xgb, X_train   = X_test)

shap_df <- as.data.frame(shap_vals$shap_score)
colnames(shap_df) <- selected_features
shap_df$District  <- dist_test
shap_df$Division  <- div_test

# ── Division-wise mean |SHAP| ─────────────────────────────────────────────
div_shap <- shap_df %>%
  group_by(Division) %>%
  summarise(across(all_of(selected_features),
                   ~ mean(abs(.)),
                   .names = "{.col}"),
            .groups = "drop")

div_shap$Top_Feature <- selected_features[
  apply(div_shap[selected_features], 1, which.max)]

cat("\n===== Division-wise Mean |SHAP| =====\n")
print(div_shap)
write.csv(div_shap, "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/division_wise_shap_xgb.csv", row.names = FALSE)

# ── District-wise mean |SHAP| ─────────────────────────────────────────────
dist_shap <- shap_df %>%
  group_by(Division, District) %>%
  summarise(across(all_of(selected_features),
                   ~ mean(abs(.)),
                   .names = "{.col}"),
            .groups = "drop")

dist_shap$Top_Feature <- selected_features[
  apply(dist_shap[selected_features], 1, which.max)]

cat("\n===== District-wise Mean |SHAP| =====")


write.csv(dist_shap, "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/dist_wise_shap_value.csv")





# ── 0. Packages ───────────────────────────────────────────────────────────────
library(tidyverse)
library(haven)
library(sf)
library(srvyr)          # weighted survey stats
library(scico)          # perceptually-uniform scientific palettes
library(ggtext)         # rich-text in ggplot labels
library(shadowtext)     # readable text labels on maps
library(patchwork)      # combine plots
library(scales)
library(ggspatial)  # north arrow / scale bar
library(survey)
library(ggplot2)
library(dplyr)
library(viridis)
library(geodata)
library(tidyr)

# ── Features and clean labels ─────────────────────────────────────────────
features <- c( "Place_of_delivery", "Wealth_index","Mothers_Education","Husbands_education", 
               "Wanted_pregnancy", "Husbands_occupation","Residence", "Age","Religion")

nice_labels <- c("Place of Delivery","Wealth Index","Mother's Education", "Husband's Education",
                 "Wanted Pregnancy", "Husband's Occupation","Residence", "Age","Religion")


# ── Plot function for one feature ─────────────────────────────────────────
plot_shap <- function(feat, lab) {
  ggplot(div_shap) +
    geom_sf(aes_string(fill = feat), color = "white", linewidth = 0.4) +
    scale_fill_viridis_c(
      option    = "viridis",
      direction = 1,
      name      = "Mean |SHAP|",
      guide     = guide_colorbar(
        title.position = "top",
        title.hjust    = 0.5,
        barwidth       = unit(2.5, "cm"),
        barheight      = unit(0.25, "cm"),
        frame.colour   = NA,
        ticks.colour   = "#6b4f3a"
      )
    ) +
    coord_sf(
      xlim   = c(88.0, 92.7),
      ylim   = c(20.7, 26.7),
      expand = FALSE
    ) +
    labs(x = NULL, y = NULL, title = lab) +
    theme_bw(base_size = 7) +
    theme(
      plot.title       = element_text(face = "bold", size = 7,
                                      hjust = 0.5, colour = "#3b2f2f"),
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid.major = element_line(colour = "white", linewidth = 0.2,
                                      linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "#6b4f3a", fill = NA,
                                      linewidth = 0.8),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", color = NA),
      legend.position   = c(0.33, 0.08),
      legend.direction  = "horizontal",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title      = element_text(face = "bold", size = 5,
                                       colour = "#3b2f2f"),
      legend.text       = element_text(size = 4.5, colour = "#3b2f2f"),
      plot.margin       = margin(3, 3, 3, 3)
    )
}

# ── Build all maps ────────────────────────────────────────────────────────
map_list <- mapply(plot_shap, features, nice_labels, SIMPLIFY = FALSE)

# ── Combine with patchwork ────────────────────────────────────────────────
panel <- wrap_plots(map_list, ncol = 3) 
print(panel)


















