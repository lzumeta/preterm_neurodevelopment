## modelling.R

## load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(MASS)
library(MuMIn)
library(corrplot)
library(sjPlot)
library(car)

## set paths
data_path <- "Analysis/Data/"
mod_path  <- "Analysis/Models/"
if (!dir.exists(mod_path)) dir.create(mod_path, recursive = TRUE)

## source functions
source("Analysis/functions.R", echo = T)


# |- Cognition ------------------------------------------------------------

# |-- Load data -----------------------------------------------------------
datalongCog <- readRDS(paste0(data_path, "datalongCog.rds"))

# |-- Model selection -----------------------------------------------------
# |--- First step ---------------------------------------------------------
vars <- univ_numeric(datalongCog, "1st year")
vars <- c(vars, univ_numeric(datalongCog, "2nd year"))
vars <- c(vars, univ_numeric(datalongCog, "3rd year")) |> 
  unique()
vars <- c(vars, univ_nonumeric(datalongCog, "1st year"))
vars <- c(vars, univ_nonumeric(datalongCog, "2nd year"))
vars <- c(vars, univ_nonumeric(datalongCog, "3rd year")) |> 
  unique() 

# |--- Second step --------------------------------------------------------
formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
full_model_lmm <- lmer(as.formula(formula_full_lmm),
                       data = datalongCog)
summary(full_model_lmm)

formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
full_model_lm <- lm(as.formula(formula_full_lm), 
                    data = na.omit(datalongCog))
model_selected_lm <- stepAIC(full_model_lm,
                            direction = c("backward"),
                            trace = 1)
summary(model_selected_lm)
formula_selected <- as.character(model_selected_lm$call$formula)[3]

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", formula_selected, " + (1 | Código)"),
                           data = datalongCog)
coef_summary <- summary(model_selected_lmm)$coefficients
id_vars <- which(coef_summary[, "Pr(>|t|)"] < 0.05)

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", paste0(names(id_vars)[-c(1:3)], collapse = " + "), " + (1 | Código)"),
                           data = datalongCog)
summary(model_selected_lmm)
final_lmm_cog <- model_selected_lmm



## Goodness of fit
final_lmm_cog2 <- nlme::lme(as.formula(paste0("value ~ ano + ", formula_selected)), random =~ 1| Código, data = na.omit(datalongCog))
qqnorm(final_lmm_cog2, abline = c(0, 1))
plot(residuals(final_lmm_cog2))
plot(final_lmm_cog2)
shapiro.test(residuals(final_lmm_cog2))
lattice::qqmath(ranef(final_lmm_cog, postVar = TRUE), strip = FALSE)$Código
vif(final_lmm_cog)

# |-- SAVE ----------------------------------------------------------------
saveRDS(final_lmm_cog, paste0(mod_path, "final_lmm_cog.rds"), compress = T)




##FULLL
vars <- c("Sexo_bebé", "Tipo_emb", "Tipo_parto", "Dif_hijos",
          "Abortos_previos", "Pais_origen_M_new", "Pais_origen_P_new", "Income_M_new",
          "Income_P_new", "Num_hijos_M_new", "Num_hijos_P_new")
vars <- c(vars, datalongCog |> dplyr::select(where(is.numeric)) |> names())

formula_full_lm <- paste0("value ~ ano +", paste0(vars, collapse = " + ")) 
full_model_lm <- lm(as.formula(formula_full_lm), 
                    data = na.omit(datalongCog))
model_selected_lm <- stepAIC(full_model_lm,
                             direction = c("backward"),
                             trace = 1)

library(olsrr)
ols_step_backward_p(full_model_lm, p_val = 0.05)


# |- Receptive language ---------------------------------------------------
# |-- Load data -----------------------------------------------------------
datalongLengR <- readRDS(paste0(data_path, "datalongLengR.rds"))

# |-- Model selection -----------------------------------------------------
# |--- First step ---------------------------------------------------------
vars <- univ_numeric(datalongLengR, "1st year")
vars <- c(vars, univ_numeric(datalongLengR, "2nd year"))
vars <- c(vars, univ_numeric(datalongLengR, "3rd year")) |> 
  unique()
vars <- c(vars, univ_nonumeric(datalongLengR, "1st year"))
vars <- c(vars, univ_nonumeric(datalongLengR, "2nd year"))
vars <- c(vars, univ_nonumeric(datalongLengR, "3rd year")) |> 
  unique() 

# |--- Second step --------------------------------------------------------
formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
full_model_lmm <- lmer(as.formula(formula_full_lmm),
                       data = datalongLengR)
summary(full_model_lmm)

formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
full_model_lm <- lm(as.formula(formula_full_lm), 
                    data = na.omit(datalongLengR))
model_selected_lm <- stepAIC(full_model_lm,
                             direction = c("backward"),
                             trace = 1)
summary(model_selected_lm)
formula_selected <- as.character(model_selected_lm$call$formula)[3]

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", formula_selected, " + (1 | Código)"),
                           data = datalongLengR)
coef_summary <- summary(model_selected_lmm)$coefficients
id_vars <- which(coef_summary[, "Pr(>|t|)"] < 0.05)
names(id_vars)[7] <- "Pais_origen_M_new"

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", paste0(names(id_vars)[-c(1:2)], collapse = " + "), " + (1 | Código)"),
                           data = datalongLengR)
summary(model_selected_lmm)
final_lmm_lengr <- model_selected_lmm


## Goodness of fit
final_lmm_lengr2 <- nlme::lme(as.formula(paste0("value ~ ano + ", formula_selected)), random =~ 1| Código, data = na.omit(datalongLengR))
qqnorm(final_lmm_lengr2, abline = c(0, 1))
plot(residuals(final_lmm_lengr2))
plot(final_lmm_lengr2)
shapiro.test(residuals(final_lmm_lengr2))
lattice::qqmath(ranef(final_lmm_lengr, postVar = TRUE), strip = FALSE)$Código
vif(final_lmm_lengr)

# |-- SAVE ----------------------------------------------------------------
saveRDS(final_lmm_lengr, paste0(mod_path, "final_lmm_lengr.rds"), compress = T)



# |- Expressive language --------------------------------------------------
# |-- Load data -----------------------------------------------------------
datalongLengE <- readRDS(paste0(data_path, "datalongLengE.rds"))

# |-- Model selection -----------------------------------------------------
# |--- First step ---------------------------------------------------------
vars <- univ_numeric(datalongLengE, "1st year")
vars <- c(vars, univ_numeric(datalongLengE, "2nd year"))
vars <- c(vars, univ_numeric(datalongLengE, "3rd year")) |> 
  unique()
vars <- c(vars, univ_nonumeric(datalongLengE, "1st year"))
vars <- c(vars, univ_nonumeric(datalongLengE, "2nd year"))
vars <- c(vars, univ_nonumeric(datalongLengE, "3rd year")) |> 
  unique() 

# |--- Second step --------------------------------------------------------
formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
full_model_lmm <- lmer(as.formula(formula_full_lmm),
                       data = datalongLengE)
summary(full_model_lmm)

formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
full_model_lm <- lm(as.formula(formula_full_lm), 
                    data = na.omit(datalongLengE))
model_selected_lm <- stepAIC(full_model_lm,
                             direction = c("backward"),
                             trace = 1)
summary(model_selected_lm)
formula_selected <- as.character(model_selected_lm$call$formula)[3]

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", formula_selected, " + (1 | Código)"),
                           data = datalongLengE)
coef_summary <- summary(model_selected_lmm)$coefficients
id_vars <- which(coef_summary[, "Pr(>|t|)"] < 0.05)
names(id_vars)[5] <- "Pais_origen_M_new"

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", paste0(names(id_vars)[-1], collapse = " + "), " + (1 | Código)"),
                           data = datalongLengE)
summary(model_selected_lmm)
final_lmm_lenge <- model_selected_lmm



## Goodness of fit
final_lmm_lenge2 <- nlme::lme(as.formula(paste0("value ~ ano + ", formula_selected)), random =~ 1| Código, data = na.omit(datalongLengE))
qqnorm(final_lmm_lenge2, abline = c(0, 1))
plot(residuals(final_lmm_lenge2))
plot(final_lmm_lenge2)
shapiro.test(residuals(final_lmm_lenge2))
lattice::qqmath(ranef(final_lmm_lenge, postVar = TRUE), strip = FALSE)$Código
vif(final_lmm_lenge)

# |-- SAVE ----------------------------------------------------------------
saveRDS(final_lmm_lenge, paste0(mod_path, "final_lmm_lenge.rds"), compress = T)




# |- Fine motor -----------------------------------------------------------

# |-- Load data -----------------------------------------------------------
datalongMotF <- readRDS(paste0(data_path, "datalongMotF.rds"))

# |-- Model selection -----------------------------------------------------
# |--- First step ---------------------------------------------------------
vars <- univ_numeric(datalongMotF, "1st year")
vars <- c(vars, univ_numeric(datalongMotF, "2nd year"))
vars <- c(vars, univ_numeric(datalongMotF, "3rd year")) |> 
  unique()
vars <- c(vars, univ_nonumeric(datalongMotF, "1st year"))
vars <- c(vars, univ_nonumeric(datalongMotF, "2nd year"))
vars <- c(vars, univ_nonumeric(datalongMotF, "3rd year")) |> 
  unique() 

# |--- Second step --------------------------------------------------------
formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
full_model_lmm <- lmer(as.formula(formula_full_lmm),
                       data = datalongMotF)
summary(full_model_lmm)

formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
full_model_lm <- lm(as.formula(formula_full_lm), 
                    data = na.omit(datalongMotF))
model_selected_lm <- stepAIC(full_model_lm,
                             direction = c("backward"),
                             trace = 1)
summary(model_selected_lm)
formula_selected <- as.character(model_selected_lm$call$formula)[3]

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", formula_selected, " + (1 | Código)"),
                           data = datalongMotF)
coef_summary <- summary(model_selected_lmm)$coefficients
id_vars <- which(coef_summary[, "Pr(>|t|)"] < 0.05)
names(id_vars)[3] <- "Sexo_bebé"

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", paste0(names(id_vars)[-c(1)], collapse = " + "), " + (1 | Código)"),
                           data = datalongMotF)
summary(model_selected_lmm)
final_lmm_motf <- model_selected_lmm



## Goodness of fit
final_lmm_motf2 <- nlme::lme(as.formula(paste0("value ~ ano + ", formula_selected)), random =~ 1| Código, data = na.omit(datalongMotF))
qqnorm(final_lmm_motf2, abline = c(0, 1))
plot(residuals(final_lmm_motf2))
plot(final_lmm_motf2)
shapiro.test(residuals(final_lmm_motf2))
lattice::qqmath(ranef(final_lmm_motf, postVar = TRUE), strip = FALSE)$Código
vif(final_lmm_motf)

# |-- SAVE ----------------------------------------------------------------
saveRDS(final_lmm_motf, paste0(mod_path, "final_lmm_motf.rds"), compress = T)



# |- Gross motor ----------------------------------------------------------

# |-- Load data -----------------------------------------------------------
datalongMotG <- readRDS(paste0(data_path, "datalongMotG.rds"))

# |-- Model selection -----------------------------------------------------
# |--- First step ---------------------------------------------------------
vars <- univ_numeric(datalongMotG, "1st year")
vars <- c(vars, univ_numeric(datalongMotG, "2nd year"))
vars <- c(vars, univ_numeric(datalongMotG, "3rd year")) |> 
  unique()
vars <- c(vars, univ_nonumeric(datalongMotG, "1st year"))
vars <- c(vars, univ_nonumeric(datalongMotG, "2nd year"))
vars <- c(vars, univ_nonumeric(datalongMotG, "3rd year")) |> 
  unique() 

# |--- Second step --------------------------------------------------------
formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
full_model_lmm <- lmer(as.formula(formula_full_lmm),
                       data = datalongMotG)
summary(full_model_lmm)

formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
full_model_lm <- lm(as.formula(formula_full_lm), 
                    data = na.omit(datalongMotG))
model_selected_lm <- stepAIC(full_model_lm,
                             direction = c("backward"),
                             trace = 1)
summary(model_selected_lm)
formula_selected <- as.character(model_selected_lm$call$formula)[3]

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", formula_selected, " + (1 | Código)"),
                           data = datalongMotG)
coef_summary <- summary(model_selected_lmm)$coefficients
id_vars <- which(coef_summary[, "Pr(>|t|)"] < 0.05)

model_selected_lmm <- lmer(formula = paste0("value ~ ano +", paste0(names(id_vars)[-c(1:3)], collapse = " + "), " + (1 | Código)"),
                           data = datalongMotG)
summary(model_selected_lmm)
final_lmm_motg <- model_selected_lmm



## Goodness of fit
final_lmm_motg2 <- nlme::lme(as.formula(paste0("value ~ ano + ", formula_selected)), random =~ 1| Código, data = na.omit(datalongMotG))
qqnorm(final_lmm_motg2, abline = c(0, 1))
plot(residuals(final_lmm_motg2))
plot(final_lmm_motg2)
shapiro.test(residuals(final_lmm_motg2))
lattice::qqmath(ranef(final_lmm_motg, postVar = TRUE), strip = FALSE)$Código
vif(final_lmm_motg)

# |-- SAVE ----------------------------------------------------------------
saveRDS(final_lmm_motg, paste0(mod_path, "final_lmm_motg.rds"), compress = T)
