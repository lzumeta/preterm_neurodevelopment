## sensitivity_analysis.R

## source functions and set the paths
source("Analysis/functions.R", echo = T)
data_path <- "Analysis/Data/"
res_path <- "Results/Sens_analyses/"
if (!dir.exists(res_path)) dir.create(res_path, recursive = TRUE)




# Cognition ---------------------------------------------------------------
datalongCog <- readRDS(paste0(data_path, "datalongCog.rds"))
res_path2  <- paste0(res_path, "Cognition/")
if (!dir.exists(res_path2)) dir.create(res_path2, recursive = T)

set.seed(1900)
res <- NULL
for (i in 1:500){
  cat("\n\niteration = ", i, "\n")
  cat("--------------------------------------\n\n\n")
  
  select_ids <- sample(1:342, size = 342, replace = T) ## bootstrapping 342 observations with replacement
  
  data_b <- datalongCog[select_ids, ]
  
  # |-- Model selection -----------------------------------------------------
  # |--- First step ---------------------------------------------------------
  vars <- univ_numeric(data_b, "1st year")
  vars <- c(vars, univ_numeric(data_b, "2nd year"))
  vars <- c(vars, univ_numeric(data_b, "3rd year")) |> 
    unique()
  vars <- c(vars, univ_nonumeric(data_b, "1st year"))
  vars <- c(vars, univ_nonumeric(data_b, "2nd year"))
  vars <- c(vars, univ_nonumeric(data_b, "3rd year")) |> 
    unique() 
  
  # |--- Second step --------------------------------------------------------
  formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
  full_model_lmm <- lmer(as.formula(formula_full_lmm),
                         data = data_b)
  summary(full_model_lmm)
  
  formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
  full_model_lm <- lm(as.formula(formula_full_lm), 
                      data = na.omit(data_b))
  model_selected_lm <- stepAIC(full_model_lm,
                               direction = c("backward"),
                               trace = 1)
  res <- c(res, attr(model_selected_lm$coefficients, "names"))
}

sort(table(res))
saveRDS(res, paste0(res_path2, "res_sens_cog.rds"))


# Receptive language ------------------------------------------------------
datalongLengR <- readRDS(paste0(data_path, "datalongLengR.rds"))
res_path2  <- paste0(res_path, "ReceptiveLanguage/")
if (!dir.exists(res_path2)) dir.create(res_path2, recursive = T)

set.seed(1900)
res <- NULL
for (i in 1:500){
  cat("\n\niteration = ", i, "\n")
  cat("--------------------------------------\n\n\n")
  
  select_ids <- sample(1:342, size = 342, replace = T) ## bootstrapping 342 observations with replacement
  
  data_b <- datalongLengR[select_ids, ]
  
  # |-- Model selection -----------------------------------------------------
  # |--- First step ---------------------------------------------------------
  vars <- univ_numeric(data_b, "1st year")
  vars <- c(vars, univ_numeric(data_b, "2nd year"))
  vars <- c(vars, univ_numeric(data_b, "3rd year")) |> 
    unique()
  vars <- c(vars, univ_nonumeric(data_b, "1st year"))
  vars <- c(vars, univ_nonumeric(data_b, "2nd year"))
  vars <- c(vars, univ_nonumeric(data_b, "3rd year")) |> 
    unique() 
  
  # |--- Second step --------------------------------------------------------
  formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
  full_model_lmm <- lmer(as.formula(formula_full_lmm),
                         data = data_b)
  summary(full_model_lmm)
  
  formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
  full_model_lm <- lm(as.formula(formula_full_lm), 
                      data = na.omit(data_b))
  model_selected_lm <- stepAIC(full_model_lm,
                               direction = c("backward"),
                               trace = 1)
  res <- c(res, attr(model_selected_lm$coefficients, "names"))
}

sort(table(res))
saveRDS(res, paste0(res_path2, "res_sens_lengr.rds"))


# Expressive language -----------------------------------------------------
datalongLengE <- readRDS(paste0(data_path, "datalongLengE.rds"))
res_path2  <- paste0(res_path, "ExpressiveLanguage/")
if (!dir.exists(res_path2)) dir.create(res_path2, recursive = T)

set.seed(1900)
res <- NULL
for (i in 1:500){
  cat("\n\niteration = ", i, "\n")
  cat("--------------------------------------\n\n\n")
  
  select_ids <- sample(1:342, size = 342, replace = T) ## bootstrapping 342 observations with replacement
  
  data_b <- datalongLengE[select_ids, ]
  
  # |-- Model selection -----------------------------------------------------
  # |--- First step ---------------------------------------------------------
  vars <- univ_numeric(data_b, "1st year")
  vars <- c(vars, univ_numeric(data_b, "2nd year"))
  vars <- c(vars, univ_numeric(data_b, "3rd year")) |> 
    unique()
  vars <- c(vars, univ_nonumeric(data_b, "1st year"))
  vars <- c(vars, univ_nonumeric(data_b, "2nd year"))
  vars <- c(vars, univ_nonumeric(data_b, "3rd year")) |> 
    unique() 
  
  # |--- Second step --------------------------------------------------------
  formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
  full_model_lmm <- lmer(as.formula(formula_full_lmm),
                         data = data_b)
  summary(full_model_lmm)
  
  formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
  full_model_lm <- lm(as.formula(formula_full_lm), 
                      data = na.omit(data_b))
  model_selected_lm <- stepAIC(full_model_lm,
                               direction = c("backward"),
                               trace = 1)
  res <- c(res, attr(model_selected_lm$coefficients, "names"))
}

sort(table(res))
saveRDS(res, paste0(res_path2, "res_sens_lenge.rds"))


# Fine motor --------------------------------------------------------------
datalongMotF <- readRDS(paste0(data_path, "datalongMotF.rds"))
res_path2  <- paste0(res_path, "FineMotor/")
if (!dir.exists(res_path2)) dir.create(res_path2, recursive = T)

set.seed(1900)
res <- NULL
for (i in 1:500){
  cat("\n\niteration = ", i, "\n")
  cat("--------------------------------------\n\n\n")
  
  select_ids <- sample(1:342, size = 342, replace = T) ## bootstrapping 342 observations with replacement
  
  data_b <- datalongMotF[select_ids, ]
  
  # |-- Model selection -----------------------------------------------------
  # |--- First step ---------------------------------------------------------
  vars <- univ_numeric(data_b, "1st year")
  vars <- c(vars, univ_numeric(data_b, "2nd year"))
  vars <- c(vars, univ_numeric(data_b, "3rd year")) |> 
    unique()
  vars <- c(vars, univ_nonumeric(data_b, "1st year"))
  vars <- c(vars, univ_nonumeric(data_b, "2nd year"))
  vars <- c(vars, univ_nonumeric(data_b, "3rd year")) |> 
    unique() 
  
  # |--- Second step --------------------------------------------------------
  formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
  full_model_lmm <- lmer(as.formula(formula_full_lmm),
                         data = data_b)
  summary(full_model_lmm)
  
  formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
  full_model_lm <- lm(as.formula(formula_full_lm), 
                      data = na.omit(data_b))
  model_selected_lm <- stepAIC(full_model_lm,
                               direction = c("backward"),
                               trace = 1)
  res <- c(res, attr(model_selected_lm$coefficients, "names"))
}

sort(table(res))
saveRDS(res, paste0(res_path2, "res_sens_motf.rds"))


# Gross motor -------------------------------------------------------------
datalongMotG <- readRDS(paste0(data_path, "datalongMotG.rds"))
res_path2  <- paste0(res_path, "GrossMotor/")
if (!dir.exists(res_path2)) dir.create(res_path2, recursive = T)

set.seed(1900)
res <- NULL
for (i in 1:500){
  cat("\n\niteration = ", i, "\n")
  cat("--------------------------------------\n\n\n")
  
  select_ids <- sample(1:342, size = 342, replace = T) ## bootstrapping 342 observations with replacement
  
  data_b <- datalongMotG[select_ids, ]
  
  # |-- Model selection -----------------------------------------------------
  # |--- First step ---------------------------------------------------------
  vars <- univ_numeric(data_b, "1st year")
  vars <- c(vars, univ_numeric(data_b, "2nd year"))
  vars <- c(vars, univ_numeric(data_b, "3rd year")) |> 
    unique()
  vars <- c(vars, univ_nonumeric(data_b, "1st year"))
  vars <- c(vars, univ_nonumeric(data_b, "2nd year"))
  vars <- c(vars, univ_nonumeric(data_b, "3rd year")) |> 
    unique() 
  
  # |--- Second step --------------------------------------------------------
  formula_full_lmm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + "), " + (1 | Código)") 
  full_model_lmm <- lmer(as.formula(formula_full_lmm),
                         data = data_b)
  summary(full_model_lmm)
  
  formula_full_lm <- paste0("value ~ ano +", paste0(vars[-1], collapse = " + ")) 
  full_model_lm <- lm(as.formula(formula_full_lm), 
                      data = na.omit(data_b))
  model_selected_lm <- stepAIC(full_model_lm,
                               direction = c("backward"),
                               trace = 1)
  res <- c(res, attr(model_selected_lm$coefficients, "names"))
}

sort(table(res))
saveRDS(res, paste0(res_path2, "res_sens_motg.rds"))


