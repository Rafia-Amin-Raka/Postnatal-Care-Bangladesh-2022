# =============================================================================
# DHS Standard Descriptive Table — PNC Care
# Shows: Unweighted n | Weighted % | PNC Yes % | PNC No % | p-value
# =============================================================================

library(tidyverse)
library(survey)
library(gtsummary)
library(flextable)
library(officer)
library(ggplot2)
library(dplyr)
library(haven)
library(tibble)

#Loaddata 
df <- read_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/map_pnc_data.csv")
#df <- read_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_raw_clean.csv")   
table(df$PNC_care)
colnames(df)
head(df)
#weight variable 
df <- df %>%
  mutate(weight = V005 / 1000000,
         PNC_care = factor(PNC_care, levels = c(0, 1), labels = c("No", "Yes"))  )
table(df$Maternal_working)
table(df$ANC_visit)


# # Label variables cleanly ────────────────────────────────────────────
# df <- df %>%
#   mutate(
#     Age = factor(Age, levels = sort(unique(Age))),   # or recode to groups below
#     
#     # Age groups (adjust breaks to match your data)
#     Age_group = cut(as.numeric(as.character(Age)),
#                     breaks = c(14, 19, 24, 29, 34, 39, 49),
#                     labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-49"),
#                     right  = TRUE),
#     
#     Region = factor(Region, levels = 1:8,
#                     labels = c("Barishal", "Chattogram", "Dhaka", "Khulna",
#                                "Mymensingh", "Rajshahi", "Rangpur", "Sylhet")),
#     
#     Residence = factor(Residence, levels = c(1, 2),
#                        labels = c("Urban", "Rural")),
#     
#     Mothers_Education = factor(Mothers_Education,
#                                levels = 0:3,
#                                labels = c("No Education", "Primary",
#                                           "Secondary", "Higher")),
#     
#     Religion = factor(Religion, levels = c(1, 2, 3, 4),
#                       labels = c("Islam", "Hinduism", "Christianity", "Other")),
#     
#     Wealth_index = factor(Wealth_index, levels = 1:5,
#                           labels = c("Poorest", "Poorer", "Middle",
#                                      "Richer", "Richest")),
#     
#     Birth_order = factor(Birth_order,
#                          levels = c(1, 2, 3, 4),
#                          labels = c("1st", "2nd", "3rd", "4th+")),
#     
#     Wanted_pregnancy = factor(Wanted_pregnancy, levels = c(1, 2, 3),
#                               labels = c("Wanted", "Mistimed", "Unwanted")),
#     
#     Place_of_delivery = factor(Place_of_delivery, levels = c(0, 1),
#                                labels = c("Home", "Health Facility")),
#     
#     Husbands_education = factor(Husbands_education,
#                                 levels = 0:3,
#                                 labels = c("No Education", "Primary",
#                                            "Secondary", "Higher")),
#     Husbands_occupation = factor(Husbands_occupation),
#     ANC_visit = factor(ANC_visit, levels = c(0, 1, 2),
#                        labels = c("No ANC", "1-3 visits", "4+ visits"))
#   )


df <- df %>%mutate(
    Maternal_working = factor(Maternal_working, levels = c(0, 1),labels = c("Not Working", "Working")),
    Health_insurance = factor(Health_insurance, levels = c(0, 1),labels = c("No", "Yes")),
   Media_exposure = factor(Media_exposure, levels = c(0, 1),labels = c("No", "Yes"))
  )
    
table(df$Health_insurance)
table(df$Media_exposure)

#survey design
svydesign_obj <- svydesign(
  ids     = ~V001,       # cluster/PSU
  strata  = ~V023,       # stratum
  weights = ~weight,
  data    = df,
  nest    = TRUE)
# 
# #Variables to include in table 
# table_vars <- c("Age", "Region", "Residence", "Mothers_Education",
#   "Religion", "Wealth_index", "Birth_order", "Wanted_pregnancy", "Marital_status",
#   "Place_of_delivery", "Husbands_education", "Maternal_working",
#   "Husbands_occupation", "Health_insurance", "Media_exposure",
#   "ANC_visit", "Decision_in_healthcare")
# 
# #table
# table1 <- svydesign_obj %>%
#   tbl_svysummary(
#     include    = all_of(c(table_vars, "PNC_care")),
#     by         = PNC_care,
#     statistic  = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
#     digits     = list(all_categorical() ~ c(0, 1)),
#     type       = list(Health_insurance ~ "categorical",   # force both levels
#                       Media_exposure   ~ "categorical",
#                       Maternal_working ~ "categorical"),
#     label      = list(
#       Age                    ~ "Age (years)",
#       Region                 ~ "Division",
#       Residence              ~ "Residence",
#       Mothers_Education      ~ "Mother's Education",
#       Religion               ~ "Religion",
#       Wealth_index           ~ "Wealth Index",
#       Marital_status         ~ "Marital Status",
#       Birth_order            ~ "Birth Order",
#       Wanted_pregnancy       ~ "Wanted Pregnancy",
#       Place_of_delivery      ~ "Place of Delivery",
#       Husbands_education     ~ "Husband's Education",
#       Maternal_working       ~ "Mother's Working Status",
#       Husbands_occupation    ~ "Husband's Occupation",
#       Health_insurance       ~ "Health Insurance",
#       Media_exposure         ~ "Media Exposure",
#       Decision_in_healthcare ~ "Decision in Healthcare",
#       ANC_visit              ~ "ANC Visits"
#     ),
#     missing = "no"
#   ) %>%
#   add_overall(
#     statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
#   ) %>%
#   modify_header(
#     label  ~ "**Characteristics**",
#     stat_1 ~ "**PNC No**\nn (%)",
#     stat_2 ~ "**PNC Yes**\nn (%)",
#     stat_0 ~ "**Total**\nn (%)"
#   ) %>%
#   modify_spanning_header(
#     c(stat_1, stat_2) ~ "**PNC Care**"
#   ) %>%
#   bold_labels() %>%
#   italicize_levels()
# 
# table1
# #Export to Word 
# table1 %>%
#   as_flex_table() %>%
#   set_table_properties(width = 1, layout = "autofit") %>%
#   font(fontname = "Times New Roman", part = "all") %>%
#   fontsize(size = 11, part = "all") %>%
#   save_as_docx(path = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/PNC_descriptive_table_weighted_raw.docx")
# cat("Saved → PNC_descriptive_table.docx\n")
# #Export to CSV
# table1 %>%as_tibble() %>%
#   write_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/PNC_descriptive_table_weighted_raw.csv")
# cat("Saved → PNC_descriptive_table.csv\n")





#pnc distribution pie chart--------------------
#Weighted PNC care percentages
pnc_wtd <- svymean(~factor(PNC_care), svydesign_obj, na.rm = TRUE)
pnc_df  <- data.frame(Category   = c("Did Not Recieve PNC", "Recieved PNC"),
  Percentage = as.numeric(pnc_wtd) * 100) %>%
  mutate(Label = paste0(Category, "\n", round(Percentage, 1), "%"))
print(pnc_df)
dim(df)

#Pie chart 
ggplot(
  pnc_df, aes(x = "", y = Percentage, fill = Category)) +
  geom_col(width = 1, color = "white", linewidth = 0.8) +
  coord_polar(theta = "y", start = -pi/2) +
  geom_text(aes(label = Label),position = position_stack(vjust = 0.5),size = 7,fontface = "bold",color= "white") +
  scale_fill_manual(values = c("Did Not Recieve PNC"= "#3e4989","Recieved PNC" = "#35b779")) +
  theme_void(base_size = 8) +
  theme(plot.title= element_text(face = "bold", size = 11,hjust = 0.5, colour = "#3b2f2f"),
    plot.subtitle = element_text(size = 9, hjust = 0.5,colour = "#6b4f3a"),
    legend.position = "none",plot.margin= margin(10, 10, 10, 10)
  )

ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/PNC_pie_chart.tiff",
       dpi= 300,width  = 12,height = 12,units  = "cm",bg= "white")













# describtive table with ci
cat_vars <- c(
  "PNC_care",
  "Age",
  "Region",
  "Residence",
  "Mothers_Education",
  "Religion",
  "Wealth_index",
  "Birth_order",
  "Wanted_pregnancy",
  "Place_of_delivery",
  "Husbands_education",
  "Maternal_working",
  "Husbands_occupation",
  "Health_insurance",
  "Media_exposure",
  "ANC_visit",
  "Decision_in_healthcare"
)

df[cat_vars] <- lapply(df[cat_vars], as_factor)

table_vars <- c(
  "Age", "Region", "Residence", "Mothers_Education",
  "Religion", "Wealth_index", "Birth_order",
  "Wanted_pregnancy", 
  "Place_of_delivery", "Husbands_education",
  "Maternal_working", "Husbands_occupation",
  "Health_insurance", "Media_exposure",
  "ANC_visit", "Decision_in_healthcare")

# calculate weighted prevalence + CI by category 
calc_ci_table <- function(varname) {
  formula_by <- as.formula( paste0("~", varname))
  result <- svyby(~I(PNC_care == "Yes"),formula_by,svydesign_obj,svyciprop,
    vartype = "ci",method = "logit",na.rm = TRUE)
  est <- round(result[[2]] * 100, 1)
  low <- round(result$ci_l * 100, 1)
  up  <- round(result$ci_u * 100, 1)
  ci_text <- paste0(est,"% (",low,"–",up,")")
  data.frame(variable = varname,label = as.character(result[[1]]),CI = ci_text)
}

ci_all <- bind_rows(lapply(table_vars, calc_ci_table))

# Main table
table1 <- svydesign_obj %>%
  tbl_svysummary(
    include = all_of(c(table_vars, "PNC_care")),
    by = PNC_care,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    type = list(Health_insurance ~ "categorical",Media_exposure   ~ "categorical",
      Maternal_working ~ "categorical"),
    label = list(
      Age                    ~ "Age (years)",
      Region                 ~ "Division",
      Residence              ~ "Residence",
      Mothers_Education      ~ "Mother's Education",
      Religion               ~ "Religion",
      Wealth_index           ~ "Wealth Index",

      Birth_order            ~ "Birth Order",
      Wanted_pregnancy       ~ "Wanted Pregnancy",
      Place_of_delivery      ~ "Place of Delivery",
      Husbands_education     ~ "Husband's Education",
      Maternal_working       ~ "Mother's Working Status",
      Husbands_occupation    ~ "Husband's Occupation",
      Health_insurance       ~ "Health Insurance",
      Media_exposure         ~ "Media Exposure",
      Decision_in_healthcare ~ "Decision in Healthcare",
      ANC_visit              ~ "ANC Visits"),
    
    missing = "no"
  ) %>%
  
  add_overall(statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")) %>%
  add_p(test = list(all_categorical() ~ "svy.chisq.test"),pvalue_fun = ~style_pvalue(.x, digits = 3))

table1$table_body <- table1$table_body %>%left_join(ci_all,  by = c("variable", "label"))
table1$table_styling$header <- bind_rows(
  table1$table_styling$header,
  tibble(column = "CI",hide = FALSE,align = "center",interpret_label = "md",
    label = "**Weighted PNC Utilization\n(95% CI)**"))

table1 <- table1 %>%
  modify_header(label~ "**Characteristics**",  stat_1  ~ "**PNC No**\nn (%)",
    stat_2  ~ "**PNC Yes**\nn (%)",
    stat_0  ~ "**Total**\nn (%)",
    CI      ~ "**Weighted PNC Utilization\n(95% CI)**",
    p.value ~ "**P-value**") %>%
  
  modify_spanning_header(c(stat_1, stat_2) ~ "**PNC Care**") %>%
  bold_labels() %>%
  italicize_levels()

table1

#Export to Word 
table1 %>%as_flex_table() %>%
  set_table_properties(width = 1,layout = "autofit") %>%
  font(fontname = "Times New Roman",part = "all") %>%
  fontsize(size = 11,part = "all") %>%
  save_as_docx(path = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/PNC_descriptive_table_weighted_CI.docx")
cat("Saved → PNC_descriptive_table_weighted_CI.docx\n")

#Export CSV
table1 %>%  as_tibble() %>%
  write_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/PNC_descriptive_table_weighted_CI.csv")
cat("Saved → PNC_descriptive_table_weighted_CI.csv\n")















#=================imputed data weighted==================
df <- read_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/pnc_data_imp_clean.csv")  
table(df$PNC_care)
# weight variable 
df <- df %>%mutate(weight = V005 / 1000000,PNC_care = factor(PNC_care, levels = c(0, 1), labels = c("No", "Yes")))

#survey design 
svydesign_obj <- svydesign(
  ids     = ~V001,       # cluster/PSU
  strata  = ~V023,       # stratum
  weights = ~weight,
  data    = df,
  nest    = TRUE)

table_vars <- c("Age", "Region", "Residence", "Mothers_Education",
  "Religion", "Wealth_index", "Birth_order", "Wanted_pregnancy","Marital_status",
  "Place_of_delivery", "Husbands_education", "Maternal_working",
  "Husbands_occupation", "Health_insurance", "Media_exposure","Decision_in_healthcare", "ANC_visit")


table2 <- svydesign_obj %>%
  tbl_svysummary(
    include    = all_of(c(table_vars, "PNC_care")),
    by         = PNC_care,
    statistic  = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
    digits     = list(all_categorical() ~ c(0, 1)),
    type       = list(Health_insurance ~ "categorical",   # force both levels
                      Media_exposure   ~ "categorical",
                      Maternal_working ~ "categorical"),
    label      = list(
      Age                    ~ "Age (years)",
      Region                 ~ "Division",
      Residence              ~ "Residence",
      Mothers_Education      ~ "Mother's Education",
      Religion               ~ "Religion",
      Wealth_index           ~ "Wealth Index",
      Marital_status         ~ "Marital Status",
      Birth_order            ~ "Birth Order",
      Wanted_pregnancy       ~ "Wanted Pregnancy",
      Place_of_delivery      ~ "Place of Delivery",
      Husbands_education     ~ "Husband's Education",
      Maternal_working       ~ "Mother's Working Status",
      Husbands_occupation    ~ "Husband's Occupation",
      Health_insurance       ~ "Health Insurance",
      Media_exposure         ~ "Media Exposure",
      Decision_in_healthcare ~ "Decision in Healthcare",
      ANC_visit              ~ "ANC Visits"
    ),
    missing = "no"
  ) %>%
  add_overall(statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")) %>%
  modify_header(label  ~ "**Characteristics**",stat_1 ~ "**PNC No**\nn (%)",
    stat_2 ~ "**PNC Yes**\nn (%)",stat_0 ~ "**Total**\nn (%)") %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**PNC Care**") %>%
  bold_labels() %>%
  italicize_levels()
table2

#Export to Word 
table2 %>%
  as_flex_table() %>%set_table_properties(width = 1, layout = "autofit") %>%
  font(fontname = "Times New Roman", part = "all") %>%fontsize(size = 11, part = "all") %>%
  save_as_docx(path = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/PNC_descriptive_table_weighted_imp.docx")
cat("Saved → PNC_descriptive_table.docx\n")

#Export to CSV
table2 %>%
  as_tibble() %>%write_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/PNC_descriptive_table_weighted_imp.csv")
cat("Saved → PNC_descriptive_table.csv\n")






#================imp unweighted===

table3 <- df %>%
  select(all_of(c(table_vars, "PNC_care"))) %>%
  tbl_summary(
    by        = PNC_care,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits    = list(all_categorical() ~ c(0, 1)),
    type      = list(Health_insurance       ~ "categorical",
                     Media_exposure         ~ "categorical",
                     Maternal_working       ~ "categorical"),
    label     = list(
      Age                    ~ "Age (years)",
      Region                 ~ "Division",
      Residence              ~ "Residence",
      Mothers_Education      ~ "Mother's Education",
      Religion               ~ "Religion",
      Wealth_index           ~ "Wealth Index",
      Marital_status         ~ "Marital Status",
      Birth_order            ~ "Birth Order",
      Wanted_pregnancy       ~ "Wanted Pregnancy",
      Place_of_delivery      ~ "Place of Delivery",
      Husbands_education     ~ "Husband's Education",
      Maternal_working       ~ "Mother's Working Status",
      Husbands_occupation    ~ "Husband's Occupation",
      Health_insurance       ~ "Health Insurance",
      Media_exposure         ~ "Media Exposure",
      Decision_in_healthcare ~ "Decision in Healthcare",
      ANC_visit              ~ "ANC Visits"
    ),
    missing = "no"
  ) %>%
  add_overall(statistic = list(all_categorical() ~ "{n} ({p}%)")) %>%
  modify_header(label  ~ "**Characteristics**",stat_1 ~ "**PNC No**\nn (%)",
    stat_2 ~ "**PNC Yes**\nn (%)",stat_0 ~ "**Total**\nn (%)") %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**PNC Care**") %>%
  bold_labels() %>%
  italicize_levels()

table3

#Export to Word
table3 %>%
  as_flex_table() %>%set_table_properties(width = 1, layout = "autofit") %>%
  font(fontname = "Times New Roman", part = "all") %>%fontsize(size = 11, part = "all") %>%
  save_as_docx(path = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/PNC_descriptive_table_unweighted_imp.docx")
cat("Saved → PNC_descriptive_table.docx\n")

#Export to CSV
table3 %>%
  as_tibble() %>%write_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/PNC_descriptive_table_unweighted_imp.csv")
cat("Saved → PNC_descriptive_table.csv\n")





















