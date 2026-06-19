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
# View variablta)

# To see the variables labels 
#print_labels( NRdata$)


#..................................
# #Identify Key Variables for Survey Design
#V001 = dhs cluster
#v002 = household number
# v003 = respondand line number
# V021 = Cluster variable: Primary sampling unit (PSU)
# V023 = Stratification variable
# V005 = Sampling weights

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
#v467d = distance to health facili
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
colSums(is.na(pnc_data)) # 3 VARIABLE HAS MISSING VALUE HUS_EDU (V701) , hUS_OCCU(V705) , DEC_IN_HEALTHCARE (V743A)
pnc_data <- pnc_data %>%
  drop_na(V701, V705, V743A)
dim(pnc_data)

# Check weighted N
sum(pnc_data$V005 / 1000000)

table(pnc_data$V501)


###-----------------------outcome variable-------
classify_pnc <- function(M62, M64, M63, M66, M68, M67) {
  
  # Block 1: Checked before discharge at facility by skilled provider
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
  # Block 2: Checked after discharge/home delivery by skilled provider
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
      TRUE ~ "2" #Unemployed 
    )
  )

pnc_data$V705
pnc_data %>%
  count(  Husbands_occupation) %>%
  mutate(Percent = round(100 * n / sum(n), 1))




#---------------------- Recode media exposure variables
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

pnc_data_raw_clean <- pnc_data %>%
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
    
    PNC_care, Age, Region,Residence, Mothers_Education,Religion,Wealth_index,
    Birth_order,Wanted_pregnancy,Place_of_delivery,Husbands_education, Maternal_working,
    Husbands_occupation, Health_insurance, Media_exposure,Decision_in_healthcare, ANC_visit
  )

colnames(pnc_data_raw_clean)
dim(pnc_data_raw_clean)
colSums(is.na(pnc_data_raw_clean))


write_sav(pnc_data_raw_clean, path ="C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_raw_clean.sav")
write.csv(pnc_data_raw_clean, "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_raw_clean.csv", row.names = FALSE)
saveRDS(pnc_data_raw_clean,"C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_raw_clean.rds")




