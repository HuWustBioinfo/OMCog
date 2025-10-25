#1.Baseline Characteristics Table---------------------------------------------------------
library(tableone)
myvars <- c("Age","Age_quartile","Gender","Residence","Education_level3","Smoking","Drinking",
            "Exercise","Hypertension","Hyperlipidemia","Diabetes","Cerebrovascular",
            "ABSI100","ZGABSI100","WWI","CI","RFM","BRI","WHTR","WC","BMI","WHR","CI",
            "Global","Memory","Language","Attention","Executive","Year","MMSE_score") # All variables
catvars <- c("Age_quartile","Gender","Residence","Education_level3","Smoking","Drinking",
             "Exercise","Hypertension","Hyperlipidemia","Diabetes","Cerebrovascular")# Factor variables
tab2 <- CreateTableOne(vars = myvars, data =data, factorVars = catvars, strata = "CI")
tab2 <- CreateTableOne(vars = myvars, data =data, factorVars = catvars)
print(tab2)
# Convert the TableOne object to a data frame
tab2_df <- as.data.frame(print(tab2, quote = FALSE, noSpaces = TRUE))
# Add row names as a new column in the data frame
tab2_df <- cbind(Variable = rownames(tab2_df), tab2_df)
print(tab2_df)

# 2. Logistic regression---------------------------------------------------------
library(dplyr)
library(magrittr)
library(broom)
logit_model <- glm(Outcome ~ Exposure + Covariates, data = data, family = binomial())
tidy(logit_model1, exponentiate = TRUE, conf.int = TRUE)

# 3. Linear regression---------------------------------------------------------
library(dplyr)
library(magrittr)
library(broom)
linear_model <- lm(Outcome ~ Exposure + Covariates ,data = data)
tidy(linear_model, conf.int = TRUE)

# 4. Cox proportional hazards regression---------------------------------------------------------
library(survival)
library(broom)
# Base model
surv_object <- Surv(time = data$year, event = data$status != 0)
cox_model <- coxph(Surv(time, status) ~ Exposure + Covariates ,data = data)
# Test proportional hazards assumption
cox.zph(cox_model)
# Tidy output with HR (hazard ratio) and 95% CI
tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# 5. Linear mixed-effects model (LME)---------------------------------------------------------
library(nlme)
library(broom.mixed)
library(dplyr)
# Random intercept and slope of Year by participant (ID)
lme_model <- lme(
  fixed   = Outcome ~ Exposure * Year + Covariates,
  random  = ~ Year | ID,
  data    = data,
  method  = "REML",
  control = lmeControl(opt = "optim", maxIter = 1000, msMaxIter = 1000))
# Tidy fixed effects with 95% Wald CIs
tidy(lme_model, effects = "fixed", conf.int = TRUE) %>%
  select(term, estimate, std.error, p.value, conf.low, conf.high)

# 6. RCS---------------------------------------------------------
# ---- Core template: RCS plot for a continuous exposure (logistic model) ----
#devtools::install_github("KunHuo/plotRCS")
library(plotRCS)
options(datadist = NULL)
# 1) Data (replace with yours)
# df: your data.frame
# outcome: binary outcome (0/1 or factor with two levels)
# exposure: continuous exposure to spline
# covars: character vector of covariate names
df <- your_data
outcome   <- "outcome_var"         # e.g., "CI"
exposure  <- "exposure_var"        # e.g., "BMI"
covars    <- c("covar1","covar2","covar3")  # e.g., "sex","age","edu"
# 2) Basic cleaning (keep only needed vars & drop NA)
keep_vars <- c(outcome, exposure, covars)
dat <- na.omit(df[ , keep_vars])
# 3) Ensure outcome is binary (0/1 or 2-level factor)
# If it's numeric 0/1 already, you can skip this.
if (!is.factor(dat[[outcome]])) {
  # assume numeric 0/1; if not, convert accordingly
  dat[[outcome]] <- factor(dat[[outcome]], levels = c(0,1))
}
# 4) Draw RCS plot (logistic regression by default)
#    Key options:
#    - knots: number or explicit numeric vector (e.g., c(5,35,65,95) percentiles)
#    - ref: reference value for the exposure (often median)
#    - family: "binomial" (logistic), "cox" (Cox PH), "gaussian" (linear)
p <- rcsplot(
  data       = dat,
  outcome    = outcome,
  exposure   = exposure,
  covariates = covars,
  knots      = 4,           # or: knots = c(5,35,65,95)  # percentiles
  ref        = "median",    # or a numeric value, e.g., ref = 100
  family     = "binomial",  # "binomial" | "cox" | "gaussian"
  showCI     = TRUE,        # show 95% CI
  xlab       = exposure,
  ylab       = "Odds ratio (95% CI)")
print(p)


# 7. Partial correlation (core template) ---------------------------------------------------------
# install.packages(c("dplyr","tibble","tidyr","ppcor","readr","openxlsx","pheatmap"))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble)
  library(ppcor); library(pheatmap)})
# 1) Data & column config (REPLACE with yours)
df <- your_data_frame                      # e.g., readr::read_csv("data.csv")
vars_cont  <- c("X1","X2","X3","X4")       # continuous variables to correlate (e.g., ABSI, WWI, BMI, ...)
covars     <- c("sex","residence","edu","smoking","alcohol","exercise","cerebrovascular")  # adjusters
# 2) Clean: keep needed columns & drop NA
keep_cols <- unique(c(vars_cont, covars))
dat <- na.omit(df[, keep_cols, drop = FALSE])
# Ensure covariates have proper types (factors/numerics as needed)
# Example:
# dat$sex        <- factor(dat$sex)
# dat$residence  <- factor(dat$residence)
# dat$edu        <- as.numeric(dat$edu)
# 3) Helper: partial correlation of one target vs others, adjusted for covars
pcor_with_target <- function(dat, target, vars, covars, method = "spearman") {
  # design matrix for covariates (one-hot encoded, drop intercept)
  Z <- model.matrix(as.formula(paste("~", paste(covars, collapse = "+"))), data = dat)[, -1, drop = FALSE]
  others <- setdiff(vars, target)
  res <- lapply(others, function(v) {
    pc <- ppcor::pcor.test(dat[[target]], dat[[v]], Z, method = method)
    tibble::tibble(
      var1  = target,
      var2  = v,
      r     = unname(pc$estimate),
      p     = pc$p.value
    )
  }) %>% bind_rows()
  res <- res %>%
    mutate(p_adj = p.adjust(p, method = "BH"),
           sig   = case_when(
             p_adj < 0.001 ~ "***",
             p_adj < 0.01  ~ "**",
             p_adj < 0.05  ~ "*",
             TRUE          ~ ""
           ))
  res
}
# 4) Example: pick one target and compute partial correlations
target <- "X1"  # e.g., "ABSI"
res_target <- pcor_with_target(dat, target, vars_cont, covars, method = "spearman")
print(res_target, n = nrow(res_target))
# 5) (Optional) Batch over multiple targets
targets <- c("X1","X2")  # any subset of vars_cont
res_all <- lapply(targets, function(tg) pcor_with_target(dat, tg, vars_cont, covars)) %>%
  bind_rows()
# 6) (Optional) Heatmap for one target (vector of r)
# reshape to wide (one row = target, columns = others)
mat <- res_target |>
  select(var2, r) |>
  tibble::column_to_rownames("var2") |>
  as.matrix()
pheatmap(mat, cluster_rows = TRUE, cluster_cols = FALSE, main = paste0("Partial r: ", target))
# 7) (Optional) Save results
# openxlsx::write.xlsx(res_all, "partial_correlation_results.xlsx")
