## Figures.R

## load libraries
library(tidyverse)
library(summata)
library(lme4)

## source functions
source("Analysis/functions.R", echo = TRUE)
source("Analysis/forestPlot_v2/autoforest_v2.R", echo = T)
source("Analysis/forestPlot_v2/lmforest_v2.R", echo = T)
source("Analysis/forestPlot_v2/number_utils.R")
source("Analysis/forestPlot_v2/forest_utils.R")


## set paths
data_path <- "Analysis/Data/"
mod_path  <- "Analysis/Models_05/" 
fig_path  <- "Results/Figures_05/"
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = TRUE)

# Cognition ---------------------------------------------------------------
## Data
datalongCog <- readRDS(paste0(data_path, "datalongCog.rds"))

sapply(datalongCog, function(x) attr(x, "label"))
attr(datalongCog$ano, "label") <- "Year"
attr(datalongCog$SCL_P_FOB_T0, "label") <- "Partner's phobic anxiety"
attr(datalongCog$PBQ_M_T_T0, "label")   <- "Mother's postnatal bonding"

## Model
final_lmm_cog <- readRDS(paste0(mod_path, "final_lmm_cog.rds"))

## random effect label: sigma^2, tau^2, ICC, Observations analyzed
sigma2 <- round(summary(final_lmm_cog)$sigma^2, 2)
tau00 <- round(as.numeric(VarCorr(final_lmm_cog)$Código), 2)
icc   <- round(tau00/(sigma2 + tau00), 2)
Nid   <- final_lmm_cog@Gp[[2]]
random_effect_label <- c(sigma2, tau00, icc, Nid)



autoforest_v2(x = final_lmm_cog,
              data = datalongCog,
              indent_groups = TRUE,
              title = "Cognition",
              show_n = F)
ggsave(filename = paste0(fig_path, "Figure1.pdf"),
       width = 12, height = 7,
       device = cairo_pdf)


# Receptive language ------------------------------------------------------
## Data
datalongLengR <- readRDS(paste0(data_path, "datalongLengR.rds"))

attr(datalongLengR$ano, "label") <- "Year"
attr(datalongLengR$Pais_origen_M_new, "label") <- "Mother's country of origin"
attr(datalongLengR$APG_5min, "label") <- "5-minute Apgar"
attr(datalongLengR$SCL_M_INT_T0, "label") <- "Mother's interpersonal sensitivity"
attr(datalongLengR$SCL_P_FOB_T0, "label") <- "Partner's phobic anxiety"
attr(datalongLengR$PBQ_M_T_T0, "label")   <- "Mother's postnatal bonding"

## Model
final_lmm_lengr <- readRDS(paste0(mod_path, "final_lmm_lengr.rds"))

## random effect label: sigma^2, tau^2, ICC, Observations analyzed
sigma2 <- round(summary(final_lmm_lengr)$sigma^2, 2)
tau00 <- round(as.numeric(VarCorr(final_lmm_lengr)$Código), 2)
icc   <- round(tau00/(sigma2 + tau00), 2)
Nid   <- final_lmm_lengr@Gp[[2]]
random_effect_label <- c(sigma2, tau00, icc, Nid)



autoforest_v2(x = final_lmm_lengr,
              data = datalongLengR,
              indent_groups = TRUE,
              title = "Receptive Language",
              show_n = F)
ggsave(filename = paste0(fig_path, "Figure2.pdf"),
       width = 12, height = 7,
       device = cairo_pdf)


# Expressive language -----------------------------------------------------

## Data
datalongLengE <- readRDS(paste0(data_path, "datalongLengE.rds"))

attr(datalongLengE$ano, "label") <- "Year"
attr(datalongLengE$Pais_origen_M_new, "label") <- "Mother's country of origin"
attr(datalongLengE$SCL_P_FOB_T0, "label") <- "Partner's phobic anxiety"
attr(datalongLengE$PBQ_M_T_T0, "label")   <- "Mother's postnatal bonding"
attr(datalongLengE$SCL_M_INT_T0, "label") <- "Mother's interpersonal sensitivity"

## Model
final_lmm_lenge <- readRDS(paste0(mod_path, "final_lmm_lenge.rds"))

## random effect label: sigma^2, tau^2, ICC, Observations analyzed
sigma2 <- round(summary(final_lmm_lenge)$sigma^2, 2)
tau00 <- round(as.numeric(VarCorr(final_lmm_lenge)$Código), 2)
icc   <- round(tau00/(sigma2 + tau00), 2)
Nid   <- final_lmm_lenge@Gp[[2]]
random_effect_label <- c(sigma2, tau00, icc, Nid)



autoforest_v2(x = final_lmm_lenge,
              data = datalongLengE,
              indent_groups = TRUE,
              title = "Expressive Language",
              show_n = F)
ggsave(filename = paste0(fig_path, "Figure3.pdf"),
       width = 12, height = 7,
       device = cairo_pdf)


# Fine motor --------------------------------------------------------------
## Data 
datalongMotF <- readRDS(paste0(data_path, "datalongMotF.rds"))

attr(datalongMotF$ano, "label") <- "Year"
attr(datalongMotF$Sexo_bebé, "label") <- "Baby's sex"
attr(datalongMotF$SCL_M_PAR_T0, "label") <- "Mother's paranoid ideation"

## Model
final_lmm_motf <- readRDS(paste0(mod_path, "final_lmm_motf.rds"))

## random effect label: sigma^2, tau^2, ICC, Observations analyzed
sigma2 <- round(summary(final_lmm_motf)$sigma^2, 2)
tau00 <- round(as.numeric(VarCorr(final_lmm_motf)$Código), 2)
icc   <- round(tau00/(sigma2 + tau00), 2)
Nid   <- final_lmm_motf@Gp[[2]]
random_effect_label <- c(sigma2, tau00, icc, Nid)



autoforest_v2(x = final_lmm_motf,
              data = datalongMotF,
              indent_groups = TRUE,
              title = "Fine Motor",
              show_n = F)
ggsave(filename = paste0(fig_path, "Figure4.pdf"),
       width = 12, height = 7,
       device = cairo_pdf)


# Gross motor -------------------------------------------------------------
## Data
datalongMotG <- readRDS(paste0(data_path, "datalongMotG.rds"))

attr(datalongMotG$ano, "label") <- "Year"
attr(datalongMotG$SCL_M_PAR_T0, "label") <- "Mother's paranoid ideation"
attr(datalongMotG$Días_ingr_bebe, "label") <- "Days in the hospital"

## Model
final_lmm_motg <- readRDS(paste0(mod_path, "final_lmm_motg.rds"))

## random effect label: sigma^2, tau^2, ICC, Observations analyzed
sigma2 <- round(summary(final_lmm_motg)$sigma^2, 2)
tau00 <- round(as.numeric(VarCorr(final_lmm_motg)$Código), 2)
icc   <- round(tau00/(sigma2 + tau00), 2)
Nid   <- final_lmm_motg@Gp[[2]]
random_effect_label <- c(sigma2, tau00, icc, Nid)

autoforest_v2(x = final_lmm_motg,
              data = datalongMotG,
              indent_groups = TRUE,
              title = "Gross Motor",
              show_n = F)
ggsave(filename = paste0(fig_path, "Figure5.pdf"),
       width = 12, height = 7,
       device = cairo_pdf)





