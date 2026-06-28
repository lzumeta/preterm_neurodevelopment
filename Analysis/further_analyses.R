## further_analyses.R

## load libraries
library(tidyverse)
library(e1071)
library(knitr)
library(flextable)
library(compareGroups)
set_theme(theme_bw())

## set paths
fig_path <- "Results/Figures/"
tab_path <- "Results/Tables/"
data_path <- "Analysis/Data/"
if (!dir.exists(tab_path)) dir.create(tab_path)

## load data
data_nd <- readRDS("Analysis/Data/data_nd_cleaned.rds")


# Attrition rates ---------------------------------------------------------
## The outcome variables (Bayley-III scale)

## Cognition
datalongCog <- readRDS(paste0(data_path, "datalongCog.rds"))
datalongCog |> 
  group_by(ano) |> 
  summarise(complete_n = sum(!is.na(value)),
            attrition_rate = round(complete_n*100/114, 1)) |> 
  ungroup() |> 
  pivot_longer(names_to = "variable", values_to = "value", cols = c("complete_n", "attrition_rate")) |> 
  pivot_wider(names_from = "ano", values_from = c("value")) |> 
  flextable() |> 
  save_as_docx(path = paste0(tab_path, "TableSY_cog.docx"))

## receptive language
datalongLengR <- readRDS(paste0(data_path, "datalongLengR.rds"))
datalongLengR |> 
  group_by(ano) |> 
  summarise(complete_n = sum(!is.na(value)),
            attrition_rate = round(complete_n*100/114, 1)) |> 
  ungroup() |> 
  pivot_longer(names_to = "variable", values_to = "value", cols = c("complete_n", "attrition_rate")) |> 
  pivot_wider(names_from = "ano", values_from = c("value")) |> 
  flextable() |> 
  save_as_docx(path = paste0(tab_path, "TableSY_lengr.docx"))

## expressive language
datalongLengE <- readRDS(paste0(data_path, "datalongLengE.rds"))
datalongLengE |> 
  group_by(ano) |> 
  summarise(complete_n = sum(!is.na(value)),
            attrition_rate = round(complete_n*100/114, 1)) |> 
  ungroup() |> 
  pivot_longer(names_to = "variable", values_to = "value", cols = c("complete_n", "attrition_rate")) |> 
  pivot_wider(names_from = "ano", values_from = c("value")) |> 
  flextable() |> 
  save_as_docx(path = paste0(tab_path, "TableSY_lenge.docx"))

## fine motor
datalongMotF <- readRDS(paste0(data_path, "datalongMotF.rds"))
datalongMotF |> 
  group_by(ano) |> 
  summarise(complete_n = sum(!is.na(value)),
            attrition_rate = round(complete_n*100/114, 1)) |> 
  ungroup() |> 
  pivot_longer(names_to = "variable", values_to = "value", cols = c("complete_n", "attrition_rate")) |> 
  pivot_wider(names_from = "ano", values_from = c("value")) |> 
  flextable() |> 
  save_as_docx(path = paste0(tab_path, "TableSY_motf.docx"))

## gross motor
datalongMotG <- readRDS(paste0(data_path, "datalongMotG.rds"))
datalongMotG |> 
  group_by(ano) |> 
  summarise(complete_n = sum(!is.na(value)),
            attrition_rate = round(complete_n*100/114, 1)) |> 
  ungroup() |> 
  pivot_longer(names_to = "variable", values_to = "value", cols = c("complete_n", "attrition_rate")) |> 
  pivot_wider(names_from = "ano", values_from = c("value")) |> 
  flextable() |> 
  save_as_docx(path = paste0(tab_path, "TableSY_motg.docx"))


# Individual trajectories of each outcome variable ------------------------


bind_rows(datalongCog,
          datalongLengR,
          datalongLengE,
          datalongMotF,
          datalongMotG, .id = "variable") |> 
  mutate(variable = factor(
    fct_recode(variable,
               "Cognition" = "1",
               "Receptive language" = "2",
               "Expressive language" = "3",
               "Fine motor" = "4",
               "Gross motor" = "5"
    ),
    levels = c("Cognition", "Receptive language", "Expressive language", "Fine motor", "Gross motor"))
  )|> 
  ggplot(aes(x = as.numeric(ano), y = value)) +
  geom_line(aes(group = Código), col = "grey40") +
  geom_smooth(size = 1, method = "lm", fill = "dodgerblue") +
  facet_wrap(~variable) +
  scale_x_continuous(breaks = 1:3,
                     labels = c("1<span style='font-size:8pt'>st year</span>", 
                                "2<span style='font-size:8pt'>nd year</span>",
                                "3<span style='font-size:8pt'>rd year</span>")) +
  labs(x = "Follow-up time", y = "", title = "Bayley-III scale scores") +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = ggtext::element_markdown(size = rel(1.2), hjust = 0.7),
        axis.text.y = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.25)), 
        strip.text = element_text(size = rel(1.1)),
        plot.title = element_text(size = rel(1.3)),
        panel.spacing = unit(1.7, "lines"))
ggsave(paste0(fig_path, "figSX.jpg"), width = 13, height = 6.4, dpi = 300)
ggsave(paste0(fig_path, "figSX.pdf"), width = 13, height = 6.4)



# Comparison of complete info vs a lost to follow-up ----------------------
complete_ids <- datalongMotF |> 
  group_by(Código) |> 
  summarise(n = sum(!is.na(value))) |> 
  ungroup() |> 
  filter(n == 3) |> 
  pull(Código)

data_nd <- data_nd |> 
  mutate(complete = ifelse(Código %in% complete_ids, "Yes", "No"))

## Sexo_bebé, Peso_Bebé_Nac, SG_Nac, PC_Nac, Long_Nac, APG_1min, APG_5min,
## Días_ingr_bebe, Edad_M, Edad_P, Pais_origen_M_new, Pais_origen_P_new, 
## Income_M_new, Income_P_new
tab1 <- compareGroups(as.formula(paste0("complete ~ ", paste0(names(data_nd)[c(5:12, 14:15, 99:102)], collapse = " + "))),
                      data = data_nd)
export2word(createTable(tab1), file = normalizePath(paste0(tab_path, "TableSXX1.docx")))
## EPDS_M_T0, ..., SCL_P_GSI_T0
tab2 <- compareGroups(as.formula(paste0("complete ~ ", paste0(names(data_nd)[36:65], collapse = " + "))),
                      data = data_nd)
export2word(createTable(tab2),  file = normalizePath(paste0(tab_path, "TableSXX2.docx")))

# Distribution of SCL-90-R dimensions -------------------------------------

data_nd |> 
  select(starts_with("SCL")) |> 
  pivot_longer(everything()) |> 
  group_by(name) |> 
  summarise(
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    N = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    skewness = e1071::skewness(value, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(across(where(is.numeric), function(x) round(x, 2))) |> 
  flextable() |> 
  save_as_docx(path = paste0(tab_path, "TableSX.docx"))


data_nd |> 
  select(starts_with("SCL")) |> 
  pivot_longer(everything()) |> 
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  labs(x = "", y = "Value", title = "Distribution of SCL-related variables") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(paste0(fig_path, "FigureSXX.pdf"), width = 12, height = 8)
ggsave(paste0(fig_path, "FigureSXX.jpg"), dpi = 300, width = 12, height = 8)



# Sensitivity_analyses ----------------------------------------------------
mod_path <- "Analysis/Models/"
final_lmm_cog   <- readRDS(paste0(mod_path, "final_lmm_cog.rds"))
final_lmm_lengr <- readRDS(paste0(mod_path, "final_lmm_lengr.rds"))
final_lmm_lenge <- readRDS(paste0(mod_path, "final_lmm_lenge.rds"))
final_lmm_motf  <- readRDS(paste0(mod_path, "final_lmm_motf.rds"))
final_lmm_motg  <- readRDS(paste0(mod_path, "final_lmm_motg.rds"))

vif(final_lmm_cog)
vif(final_lmm_lengr)
vif(final_lmm_lenge)
vif(final_lmm_motf)
vif(final_lmm_motg)

# source("Analysis/Sensitivity_analysis.R")

## evaluate the results
res_path <- "Results/Sens_analyses/"
## Cognition
res_sens_cog <- readRDS(paste0(res_path, "Cognition/res_sens_cog.rds"))

df_res_cog <- data.frame(SCL_P_FOB_T0 = c(
                           table(res_sens_cog)["SCL_P_FOB_T0"]/500*100
                         ),
                         PBQ_M_T_T0 = c(
                           table(res_sens_cog)["PBQ_M_T_T0"]/500*100
                         ))

## Receptive Language
res_sens_lengr <- readRDS(paste0(res_path, "ReceptiveLanguage/res_sens_lengr.rds"))

df_res_lengr <- data.frame(PBQ_M_T_T0 = c(
                             table(res_sens_lengr)["PBQ_M_T_T0"]/500*100
                           ),
                           SCL_M_INT_T0 = c(
                             table(res_sens_lengr)["SCL_M_INT_T0"]/500*100
                           ),
                           APG_5min = c(
                             table(res_sens_lengr)["APG_5min"]/500*100
                           ),
                           Pais_origen_M_new = c(
                             table(res_sens_lengr)["Pais_origen_M_newNon Spanish"]/500*100
                           ),
                           SCL_P_FOB_T0 = c(
                             table(res_sens_lengr)["SCL_P_FOB_T0"]/500*100
                           )
)

## Expressive Language
res_sens_lenge <- readRDS(paste0(res_path, "ExpressiveLanguage/res_sens_lenge.rds"))

df_res_lenge <- data.frame(PBQ_M_T_T0 = c(
                             table(res_sens_lenge)["PBQ_M_T_T0"]/500*100
                           ),
                           SCL_P_FOB_T0 = c(
                             table(res_sens_lenge)["SCL_P_FOB_T0"]/500*100
                           ),
                           SCL_M_INT_T0 = c(
                             table(res_sens_lenge)["SCL_M_INT_T0"]/500*100
                           ),
                           Pais_origen_M_new = c(
                             table(res_sens_lenge)["Pais_origen_M_newNon Spanish"]/500*100
                           )
)


## Fine Motor
res_sens_motf <- readRDS(paste0(res_path, "FineMotor/res_sens_motf.rds"))

df_res_motf <- data.frame(SCL_M_PAR_T0 = c(
                            table(res_sens_motf)["SCL_M_PAR_T0"]/500*100
                          ),
                          Sexo_bebé = c(
                            table(res_sens_motf)["Sexo_bebéFemale"]/500*100
                          ))


## Gross Motor
res_sens_motg <- readRDS(paste0(res_path, "GrossMotor/res_sens_motg.rds"))

df_res_motg <- data.frame(Días_ingr_bebe = c(
                            table(res_sens_motg)["Días_ingr_bebe"]/500*100
                          )
)



df_res_cog
df_res_lengr
df_res_lenge
df_res_motf
df_res_motg


