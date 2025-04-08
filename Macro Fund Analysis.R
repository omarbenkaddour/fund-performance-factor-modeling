############################################################
# SECTION 1: LOAD PACKAGES
############################################################
# Install if needed:
# install.packages("tidyverse")
# install.packages("glmnet")
# install.packages("broom")

library(tidyverse)
library(lubridate)
library(glmnet)
library(broom)
library(dbplyr)
library(ggplot2)
library(car)

############################################################
# SECTION 2: IMPORT DATA
############################################################

fama_french <- read_csv("fama_french_factors.csv") %>%
  mutate(
    Date = ymd(Date),
    Date = floor_date(Date, unit = "month")
  )

momentum <- read_csv("momentum_factors.csv") %>%
  mutate(
    Date = ymd(Date),
    Date = floor_date(Date, unit = "month")
  )

aqr_extra <- read_csv("aqr_extra_factors.csv") %>%
  mutate(
    Date = ymd(Date),
    Date = floor_date(Date, unit = "month")
  )

vanguard <- read_csv("vanguard_funds.csv") %>%
  mutate(
    Date = ymd(Date),
    Date = floor_date(Date, unit = "month")
  )

macro_fund <- read_csv("macro_fund_and_aum.csv") %>%
  mutate(
    Date = ymd(Date),
    Date = floor_date(Date, unit = "month")
  )

rf_data <- read_csv("risk_free.csv") %>%
  mutate(
    Date = ymd(Date),
    Date = floor_date(Date, unit = "month")
  )     

start_date <- ymd("2014-05-01")
end_date <- ymd("2025-01-01")

fama_french <- fama_french %>% filter(Date >= start_date & Date <= end_date)
momentum    <- momentum    %>% filter(Date >= start_date & Date <= end_date)
aqr_extra   <- aqr_extra   %>% filter(Date >= start_date & Date <= end_date)
vanguard    <- vanguard    %>% filter(Date >= start_date & Date <= end_date)
macro_fund  <- macro_fund  %>% filter(Date >= start_date & Date <= end_date)
rf_data     <- rf_data     %>% filter(Date >= start_date & Date <= end_date)

convert_to_numeric <- function(df) {
  df %>%
    mutate(across(!Date, ~ as.numeric(.)))
}

fama_french <- convert_to_numeric(fama_french)
momentum    <- convert_to_numeric(momentum)
aqr_extra   <- convert_to_numeric(aqr_extra)
vanguard    <- convert_to_numeric(vanguard)
macro_fund  <- convert_to_numeric(macro_fund)
rf_data     <- convert_to_numeric(rf_data)

#Data Frame for Factors Regressions
df_merged <- fama_french %>%
  inner_join(momentum, by = "Date") %>%
  inner_join(aqr_extra, by = "Date") %>%
  inner_join(macro_fund, by = "Date") %>%
  inner_join(rf_data, by = "Date")


#Create fund excess return
df_merged$Fund_Excess_Return <- df_merged$Ri - df_merged$Rf

#Drop non predictors 
df_merged$AUM <- null 
df_merged$Rf <- null 
df_merged$Ri <- null 

############################################################
# SECTION 4: CAPM REGRESSION
############################################################
# Model: (Ri - RF) = alpha + beta * (Rm - RF) + error

capm_fit <- lm( Fund_Excess_Return ~ Rm..Rf, data = df_merged)
summary_capm <- summary(capm_fit)
cat("=== CAPM Regression Results ===\n")
print(summary_capm)


############################################################
# SECTION 5: FAMA-FRENCH 3-FACTOR REGRESSION
############################################################
# Model: (Ri - RF) = alpha + b1*MKT_minus_RF + b2*SMB + b3*HML

ff_fit <- lm(Fund_Excess_Return ~ Rm..Rf + SMB + HML, data = df_merged)
summary_ff <- summary(ff_fit)
cat("\n=== Fama-French 3-Factor Regression Results ===\n")
print(summary_ff)


############################################################
# SECTION 6: CARHART 4-FACTOR REGRESSION
############################################################
# Model: (Ri - RF) = alpha + b1*MKT_minus_RF + b2*SMB + b3*HML + b4*Mom

carhart_fit <- lm(Fund_Excess_Return ~ MKT_minus_RF + SMB + HML + Mom, data = df_merged)
summary_carhart <- summary(carhart_fit)
cat("\n=== Carhart 4-Factor Regression Results ===\n")
print(summary_carhart)


######################################################
# SECTION 7: ELASCTIC NET REGRESSION ON VANGUARD FUNDS 
######################################################

df_el_vanguard <- vanguard
df_el_vanguard$Fund_Excess_Return <- df_merged$Fund_Excess_Return

vanguard_cols <- unique(grep("^V[A-Z]{4}$", names(df_el_vanguard), value = TRUE))

for (col in vanguard_cols) {
  excess_col <- paste0(col, "_excess")
  df_el_vanguard[[excess_col]] <- df_el_vanguard[[col]] - rf_data$Rf 
}

# X matrix and y vector
vanguard_excess_cols <- paste0(vanguard_cols, "_excess")
X_vanguard <- as.matrix(df_el_vanguard[, vanguard_excess_cols])
y_el_vanguard <- df_el_vanguard[["Fund_Excess_Return"]]


# Run Elasctic Net regression 
#We decide to move forward with alpha = 0.4 as it achieves the least vif and 
#highest R^2
cvfit_vanguard <- cv.glmnet(
  x = X_vanguard,
  y = y_el_vanguard,
  alpha = 0.4,            
)


best_lambda_vanguard <- cvfit_vanguard$lambda.min
coef_vanguard <- coef(cvfit_vanguard, s = "lambda.min")
print(coef_vanguard)

nz_idx <- which(abs(coef_vanguard[-1]) > 1e-8)
best_predictors_vanguard <- vanguard_excess_cols[nz_idx]

#Fit lm based on Elastic Net selection 
X_selected <- df_el_vanguard[complete_rows, best_predictors_vanguard]

lm_vanguard <- lm(y_el_vanguard ~ ., data = as.data.frame(X_selected))


cat("\n=== OLS Refit Summary (Selected Factors) ===\n")
print(summary(lm_vanguard))    

print(vif(lm_vanguard))
                  
############################################################
# SECTION 8: ELASTIC NET ON ALL MARKET PREMIA FACTORS
############################################################

y_factors <- df_merged[["Fund_Excess_Return"]]

predictor_cols <- setdiff(names(df_merged), c("Date", "MacroFund_excess"))
X_factors <- as.matrix(df_merged[, predictor_cols])

#Experiment with alpha for best fit + lowest multicolinearity 
cvfit_factors <- cv.glmnet(
  x = X_factors,
  y = y_factors,
  alpha = 0.3,
)

best_lambda_factors <- cvfit_factors$lambda.min
cat("\n=== EN on Market Premia Factors ===\n")
cat("Best lambda:", best_lambda_factors, "\n")

coef_factors <- coef(cvfit_factors, s = "lambda.min")
print(coef_factors)

nz_idx_factors <- which(abs(coef_factors[-1]) > 1e-8)
selected_factors <- predictor_cols[nz_idx_factors]

cat("\nSelected Factors (non-zero coef):\n")
print(selected_factors)

X_selected_factors <- df_merged[, selected_factors]
lm_factors <- lm(y_factors ~ ., data = as.data.frame(X_selected_factors))


cat("\n=== OLS Refit Summary (Selected Factors) ===\n")
print(summary(lm_factors))

print(vif(lm_factors))


##################################################################
# SECTION 9: EXTRACT COEFFICIENTS, COMPUTE EXPECTED RETURNS, ALPHA
##################################################################

coefs <- coef(lm_factors)
intercept <- coefs["(Intercept)"]
betas <- coefs[-1]

factor_names <- names(betas)
X_selected_factors <- X_selected_factors[, factor_names]

expected_returns <- intercept + rowSums(sweep(X_selected_factors, 2, betas, `*`))
alphas <- y_factors - expected_returns


#######################################################
# SECTION 10: ALPHA VS AUM, OPTIMAL AUM, VALUE ADDED 
#######################################################
df_alpha_aum <- data.frame(
  Alpha = alphas,
  AUM = macro_fund$AUM
)

lm_alpha_aum <- lm(Alpha ~ AUM, data = df_alpha_aum)

cat("\n=== Regression of Alpha on AUM ===\n")
print(summary(lm_alpha_aum))

# Following Berk and Green (2004)

a <- coef(lm_alpha_aum)["(Intercept)"]
b <- coef(lm_alpha_aum)["AUM"]

AUM_current <- tail(df_alpha_aum$AUM, 1)       
alpha_current <- tail(df_alpha_aum$Alpha, 1)

if (b <= 0) {
  q_star     <- a / (2 * b)            # optimal AUM
  alpha_opt  <- a / 2                  # alpha at optimal AUM
  value_opt  <- (a^2) / (4 * b)        # optimal value added 
  excess_AUM <- AUM_current - q_star    

  cat("Optimal AUM (q*):      ", round(q_star, 2), "\n")
  cat("Optimal Alpha:         ", round(alpha_opt, 6), "\n")
  cat("Max Value Added:       ", round(value_opt, 6), "\n")
  cat("Excess AUM (vs. q*):   ", round(excess_AUM, 2), "\n")
}

current_value_added <- alpha_current* AUM_current 
cat("Current Value Added:   ", round(current_value_added, 6), "\n")


############################################################
# SECTION 13: 2007-2009 CRISIS DATA & EXPECTED RETURN
############################################################

crisis_factors <- read_csv("factor_data_2007_2009.csv") %>%
  select(Date, all_of(factor_names)) %>%
  mutate(across(-Date, as.numeric))


crisis_X <- crisis_factors[, factor_names]
crisis_X <- crisis_X[, names(betas)]

crisis_expected_returns <- intercept + rowSums(sweep(crisis_X, 2, betas, `*`))

crisis_plot_df <- crisis_plot_df %>%
  mutate(Cumulative_Return = cumsum(Expected_Return))

ggplot(crisis_plot_df, aes(x = Date, y = Cumulative_Return)) +
  geom_line(color = "darkred") +
  labs(title = "Cumulative Expected Excess Return (2008–2009)",
       x = "Date", y = "Cumulative Return") +
  theme_minimal()
