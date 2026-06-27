## prepare_data.R

## For each outcome variable prepare the data sets in appropriate (long) format

## load packages
library(tidyverse)

## set path
data_path <- "Analysis/Data/"
if (!dir.exists(data_path)) dir.create(data_path, recursive = T)

## load cleaned data
data_nd <- readRDS(paste0(data_path, "data_nd_cleaned.rds"))



# Cognition ---------------------------------------------------------------
end_var <- "cog_pes"
datalongCog <- data_nd |>   ### aldagai gehiago sartu 
  pivot_longer(names_to = "variable", values_to = "value",
               cols = c(paste0("Bay_1r_", end_var), paste0("Bay_2o_", end_var), 
                        paste0("Bay_3r_", end_var))) |>  ## pivot_longer edo gather
  mutate(ano = factor(case_when(variable == paste0("Bay_1r_", end_var) ~ "1st year",
                                variable == paste0("Bay_2o_", end_var) ~ "2nd year",
                                variable == paste0("Bay_3r_", end_var) ~ "3rd year")),
         Código = factor(Código)) |> 
  dplyr::select(Código, -variable, value, ano, Sexo_bebé:Días_ingr_bebe,
                Edad_M:Abortos_previos, EPDS_M_T_T0:SCL_P_GSI_T0,
                Pais_origen_M_new:Num_hijos_P_new)
## save
saveRDS(object = datalongCog,
        file = paste0(data_path, "datalongCog.rds"), compress = TRUE)


# Receptive language ------------------------------------------------------
end_var <- "LengR_pes"
datalongLengR <- data_nd |>   ### aldagai gehiago sartu 
  pivot_longer(names_to = "variable", values_to = "value",
               cols = c(paste0("Bay_1r_", end_var), paste0("Bay_2o_", end_var), 
                        paste0("Bay_3r_", end_var))) |>  ## pivot_longer edo gather
  mutate(ano = factor(case_when(variable == paste0("Bay_1r_", end_var) ~ "1st year",
                                variable == paste0("Bay_2o_", end_var) ~ "2nd year",
                                variable == paste0("Bay_3r_", end_var) ~ "3rd year")),
         Código = factor(Código),
         Pais_origen_M_new = fct_recode(Pais_origen_M_new, "Spanish" = "España-Euskal Herria", "Non Spanish" = "No España")) |> 
  dplyr::select(Código, -variable, value, ano, Sexo_bebé:Días_ingr_bebe,
                Edad_M:Abortos_previos, EPDS_M_T_T0:SCL_P_GSI_T0,
                Pais_origen_M_new:Num_hijos_P_new)
## save
saveRDS(datalongLengR,
        paste0(data_path, "datalongLengR.rds"), compress = TRUE)

# Expressive language -----------------------------------------------------
end_var <- "LengE_pes"
datalongLengE <- data_nd |>   ### aldagai gehiago sartu 
  pivot_longer(names_to = "variable", values_to = "value",
               cols = c(paste0("Bay_1r_", end_var), paste0("Bay_2o_", end_var), 
                        paste0("Bay_3r_", end_var))) |>  ## pivot_longer edo gather
  mutate(ano = factor(case_when(variable == paste0("Bay_1r_", end_var) ~ "1st year",
                                variable == paste0("Bay_2o_", end_var) ~ "2nd year",
                                variable == paste0("Bay_3r_", end_var) ~ "3rd year")),
         Código = factor(Código),
         Pais_origen_M_new = fct_recode(Pais_origen_M_new, "Spanish" = "España-Euskal Herria", "Non Spanish" = "No España")) |> 
  dplyr::select(Código, -variable, value, ano, Sexo_bebé:Días_ingr_bebe,
                Edad_M:Abortos_previos, EPDS_M_T_T0:SCL_P_GSI_T0,
                Pais_origen_M_new:Num_hijos_P_new)
## save
saveRDS(datalongLengE,
        paste0(data_path, "datalongLengE.rds"), compress = TRUE)

# Fine motor --------------------------------------------------------------
end_var <- "MotF_pes"
datalongMotF <- data_nd |>   ### aldagai gehiago sartu 
  pivot_longer(names_to = "variable", values_to = "value",
               cols = c(paste0("Bay_1r_", end_var), paste0("Bay_2o_", end_var), 
                        paste0("Bay_3r_", end_var))) |>  ## pivot_longer edo gather
  mutate(ano = factor(case_when(variable == paste0("Bay_1r_", end_var) ~ "1st year",
                                variable == paste0("Bay_2o_", end_var) ~ "2nd year",
                                variable == paste0("Bay_3r_", end_var) ~ "3rd year")),
         Código = factor(Código),
         Sexo_bebé = fct_recode(Sexo_bebé, "Male" = "Hombre", "Female" = "Mujer")) |> 
  dplyr::select(Código, -variable, value, ano, Sexo_bebé:Días_ingr_bebe,
                Edad_M:Abortos_previos, EPDS_M_T_T0:SCL_P_GSI_T0,
                Pais_origen_M_new:Num_hijos_P_new) 
## save
saveRDS(datalongMotF,
        paste0(data_path, "datalongMotF.rds"), compress = TRUE)

# Gross motor -------------------------------------------------------------
end_var <- "MotG_pes"
datalongMotG <- data_nd |>   ### aldagai gehiago sartu 
  pivot_longer(names_to = "variable", values_to = "value",
               cols = c(paste0("Bay_1r_", end_var), paste0("Bay_2o_", end_var), 
                        paste0("Bay_3r_", end_var))) |>  ## pivot_longer edo gather
  mutate(ano = factor(case_when(variable == paste0("Bay_1r_", end_var) ~ "1st year",
                                variable == paste0("Bay_2o_", end_var) ~ "2nd year",
                                variable == paste0("Bay_3r_", end_var) ~ "3rd year")),
         Código = factor(Código)) |> 
  dplyr::select(Código, -variable, value, ano, Sexo_bebé:Días_ingr_bebe,
                Edad_M:Abortos_previos, EPDS_M_T_T0:SCL_P_GSI_T0,
                Pais_origen_M_new:Num_hijos_P_new)
## save
saveRDS(datalongMotG,
        paste0(data_path, "datalongMotG.rds"), compress = TRUE)



