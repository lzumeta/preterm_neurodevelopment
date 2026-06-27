## load libraries
library(haven)
library(tidyverse)

## set paths
data_path <- "Analysis/Data/"
if (!dir.exists(data_path)) dir.create(data_path, recursive = T)

## import the data
data_nd <- read_sav("Raw_data/RAW_DATA_NO_PUBLIC")


data_nd <- data_nd |> 
  mutate(Sexo_bebé = factor(Sexo_bebé, levels = attr(data_nd$Sexo_bebé, "labels"), labels = names(attr(data_nd$Sexo_bebé, "labels"))),
         Pais_origen_M = factor(Pais_origen_M, levels = attr(data_nd$Pais_origen_M, "labels"), labels = names(attr(data_nd$Pais_origen_M, "labels"))),
         Pais_origen_P = factor(Pais_origen_P, levels = attr(data_nd$Pais_origen_P, "labels"), labels = names(attr(data_nd$Pais_origen_P, "labels"))),
         Education_M = factor(Education_M, levels = attr(data_nd$Education_M, "labels"), labels = names(attr(data_nd$Education_M, "labels"))),
         Education_P = factor(Education_P, levels = attr(data_nd$Education_P, "labels"), labels = names(attr(data_nd$Education_P, "labels"))),
         Income_M = factor(Income_M, levels = attr(data_nd$Income_M, "labels"), labels = names(attr(data_nd$Income_M, "labels"))),
         Income_P = factor(Income_P, levels = attr(data_nd$Income_P, "labels"), labels = names(attr(data_nd$Income_P, "labels"))),
         Marital_M = factor(Marital_M, levels = attr(data_nd$Marital_M, "labels"), labels = names(attr(data_nd$Marital_M, "labels"))),
         Marital_P = factor(Marital_P, levels = attr(data_nd$Marital_P, "labels"), labels = names(attr(data_nd$Marital_P, "labels"))),
         Cohab_M = factor(Cohab_M, levels = attr(data_nd$Cohab_M, "labels"), labels = names(attr(data_nd$Cohab_M, "labels"))),
         Cohab_P = factor(Cohab_P, levels = attr(data_nd$Cohab_P, "labels"), labels = names(attr(data_nd$Cohab_P, "labels"))),
         Parity_M = factor(Parity_M, levels = attr(data_nd$Parity_M, "labels"), labels = names(attr(data_nd$Parity_M, "labels"))),
         Parity_P = factor(Parity_P, levels = attr(data_nd$Parity_P, "labels"), labels = names(attr(data_nd$Parity_P, "labels"))),
         Num_hijos_M = factor(Num_hijos_M, levels = attr(data_nd$Num_hijos_M, "labels"), labels = names(attr(data_nd$Num_hijos_M, "labels"))),
         Num_hijos_P = factor(Num_hijos_P, levels = attr(data_nd$Num_hijos_P, "labels"), labels = names(attr(data_nd$Num_hijos_P, "labels"))),
         Tipo_emb = factor(Tipo_emb, levels = attr(data_nd$Tipo_emb, "labels"), labels = names(attr(data_nd$Tipo_emb, "labels"))),
         Tipo_parto = factor(Tipo_parto, levels = attr(data_nd$Tipo_parto, "labels"), labels = names(attr(data_nd$Tipo_parto, "labels"))),
         Parto_gemelar = factor(Parto_gemelar, levels = attr(data_nd$Parto_gemelar, "labels"), labels = names(attr(data_nd$Parto_gemelar, "labels"))),
         Dif_hijos = factor(Dif_hijos, levels = attr(data_nd$Dif_hijos, "labels"), labels = names(attr(data_nd$Dif_hijos, "labels"))),
         Abortos_previos = factor(Abortos_previos, levels = attr(data_nd$Abortos_previos, "labels"), labels = names(attr(data_nd$Abortos_previos, "labels")))
  ) |> droplevels() |> 
  mutate(Pais_origen_M_new = case_when(Pais_origen_M != "España-Euskal Herria" ~ "No España",
                                       .default = Pais_origen_M),
         Pais_origen_P_new = case_when(Pais_origen_P != "España-Euskal Herria" ~ "No España",
                                       .default = Pais_origen_P),
         Income_M_new = factor(case_when(Income_M %in% c("Entre 30.000 y 39.999 euros", "Entre 40.000 y 49.999 euros", "Más de 50.000 euros") ~ "Más de 30.000 euros",
                                         Income_M %in% c("Menos de 5.000 euros", "Entre 5.000 y 9.999 euros") ~ "Menos de 10.000 euros",
                                         .default = Income_M),
                               levels = c("Menos de 10.000 euros", "Entre 10.000 y 14.999 euros", "Entre 15.000 y 19.999 euros", "Entre 20.000 y 24.999 euros", "Entre 25.000 y 29.999 euros", "Más de 30.000 euros")),
         Income_P_new = factor(
           case_when(Income_P %in% c("Entre 30.000 y 39.999 euros", "Entre 40.000 y 49.999 euros", "Más de 50.000 euros") ~ "Más de 30.000 euros",
                     Income_P %in% c("Menos de 5.000 euros", "Entre 5.000 y 9.999 euros") ~ "Menos de 10.000 euros",
                     .default = Income_P),
           levels = c("Menos de 10.000 euros", "Entre 10.000 y 14.999 euros", "Entre 15.000 y 19.999 euros", "Entre 20.000 y 24.999 euros", "Entre 25.000 y 29.999 euros", "Más de 30.000 euros")),
         Num_hijos_M_new = factor(case_when(Num_hijos_M %in% c("Tres hijo/as", "Más de tres hijo/as") ~ "Tres hijos/as o más",
                                            .default = Num_hijos_M),
                                  levels = c("Un hijo/a", "Dos hijos/as", "Tres hijos/as o más")),
         Num_hijos_P_new = factor(case_when(Num_hijos_P %in% c("Tres hijo/as", "Más de tres hijo/as") ~ "Tres hijos/as o más",
                                            .default = Num_hijos_P),
                                  levels = c("Un hijo/a", "Dos hijos/as", "Tres hijos/as o más"))
  )

saveRDS(data_nd, file = paste0(data_path, "data_nd_cleaned.rds"), compress = TRUE)
