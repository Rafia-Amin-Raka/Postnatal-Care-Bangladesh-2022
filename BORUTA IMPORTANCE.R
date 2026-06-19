library(haven)
library(dplyr)
library(Boruta)
library(ggplot2)

# ── 1. Load & prepare data ────────────────────────────────────────────────────
pnc_grp_data <- read.csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/map_data.csv")   # uncomment if loading from SPSS
# If already loaded as df in R, rename:
# pnc_grp_data <- df

# ── 2. Select relevant columns ────────────────────────────────────────────────
cols <- c('Age', 'Region', 'Residence', 'Mothers_Education', 'Religion',
           'Wealth_index', 'Birth_order', 'Wanted_pregnancy',
          'Place_of_delivery', 'Husbands_education', 'Maternal_working',
          'Husbands_occupation', 'Health_insurance', 'Media_exposure',
          'Decision_in_healthcare', 'ANC_visit', 'PNC_care')

pnc_grp_data <- pnc_grp_data[, cols]

# ── 3. Convert to factors ─────────────────────────────────────────────────────
pnc_grp_data <- pnc_grp_data %>%
  mutate(
    Age                  = as.factor(Age),
    Region               = as.factor(Region),
    Residence            = as.factor(Residence),
    Mothers_Education    = as.factor(Mothers_Education),
    Religion             = as.factor(Religion),
    Wealth_index         = as.factor(Wealth_index),
    Birth_order          = as.factor(Birth_order),
    Wanted_pregnancy     = as.factor(Wanted_pregnancy),
    Place_of_delivery    = as.factor(Place_of_delivery),
    Husbands_education   = as.factor(Husbands_education),
    Maternal_working     = as.factor(Maternal_working),
    Husbands_occupation  = as.factor(Husbands_occupation),
    Health_insurance     = as.factor(Health_insurance),
    Media_exposure       = as.factor(Media_exposure),
    Decision_in_healthcare = as.factor(Decision_in_healthcare),
    ANC_visit            = as.factor(ANC_visit),
    PNC_care             = as.factor(PNC_care)   # outcome as factor
  )

boruta_output <- Boruta(PNC_care ~ ., data=na.omit(pnc_grp_data), doTrace=2) 
boruta_output
Significant_vars = getSelectedAttributes(boruta_output, withTentative = TRUE)
Significant_vars
roughFixMod = TentativeRoughFix(boruta_output)
boruta_signif = getSelectedAttributes(roughFixMod)
boruta_signif
# Variable Importance Scores
imps = attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ],10)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)
par(mar = c(12, 4, 4, 2) + 0.1)
plot(boruta_output, cex.axis=.7, las=2,ylim = c(0,200 ), xlab="", main="Variable Importance")  # plot variable importance
getSelectedAttributes(boruta_output)

# Extract importance values
imp_vals <- boruta_output$ImpHistory

# Apply log transformation to compress higher values
log_imp_vals <- log1p(imp_vals)  # log1p(x) = log(1 + x), avoids log(0)

# Replace original values with transformed ones
boruta_output$ImpHistory <- log_imp_vals

# Plot with transformed values
par(mar = c(12, 5, 5, 2) + 0.1)
plot(boruta_output, 
     cex.axis = 0.7, 
     las = 2, 
     xlab = "", 
     ylim = c(-1.5, 5.5))



# Save as high-resolution journal-style TIFF
tiff("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/boruta_importance.tiff", 
     width       = 3000, 
     height      = 2200, 
     res         = 300, 
     compression = "lzw",     # lossless compression — standard for journals
     units       = "px")

par(
  mar      = c(13, 5.5, 4.5, 2),
  family   = "serif",
  font     = 2,
  font.lab = 2,
  font.axis = 2,
  font.main = 2,
  cex.main = 1.3,
  cex.axis = 1,
  cex.lab  = 1.1,
  mgp      = c(3.2, 0.7, 0),
  tcl      = -0.3,
  bg       = "white"
)

plot(boruta_output, las      = 2,
     xlab     = "",
     ylim     = c(-1.5, 5.5),)

dev.off()




# ── 4. Run Boruta ─────────────────────────────────────────────────────────────
set.seed(42)
boruta_output <- Boruta(
  PNC_care ~ .,
  data     = na.omit(pnc_grp_data),
  doTrace  = 2,
  maxRuns  = 100
)

print(boruta_output)

# ── 5. Tentative Rough Fix ────────────────────────────────────────────────────
roughFixMod   <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
cat("\nSelected variables:\n")
print(boruta_signif)

# ── 6. Importance table ───────────────────────────────────────────────────────
imps <- attStats(roughFixMod)
imps_filtered <- imps[imps$decision != "Rejected", c("meanImp", "decision")]
imps_sorted   <- imps_filtered[order(-imps_filtered$meanImp), ]

cat("\nTop variables by Mean Importance:\n")
print(imps_sorted)