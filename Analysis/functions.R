## functions.R


univ_numeric <- function(data, year) {
  d <- data |> dplyr::select(ano, where(is.numeric))
  CorM <- cor(d |> dplyr::filter(ano == year) |> dplyr::select(-ano), use = "complete.obs")
  CorMp <- cor.mtest(d |> dplyr::filter(ano == year) |> dplyr::select(-ano))$p

  # which(CorM1p[1,] < 0.05)
  vars <- colnames(CorMp)[which(CorMp[1,] < 0.05)]
  # CorM1[1, which(CorM1p[1,] < 0.05)]
  return(vars)
}



univ_nonumeric <- function(data, year) {
  d <- data |> dplyr::select(value, !where(is.numeric))
  ## Cognitivo 1er año 
  dd <- d |> dplyr::filter(ano == year) |> dplyr::select(-Código, -ano, 
                                                         -Pais_origen_M, -Pais_origen_P, 
                                                         -Income_M, -Income_P, 
                                                         -Education_M, -Education_P,
                                                         -Num_hijos_M, -Num_hijos_P,
                                                         -Num_hijos_M_new, -Num_hijos_P_new,
                                                         -Marital_M, -Marital_P,
                                                         -Cohab_M, -Cohab_P,
                                                         -Parity_M, -Parity_P, -Parto_gemelar)
  vars <- sapply(names(dd |> dplyr::select(-value)), function(v) {
    pp <- try({dd |> 
        group_by(.data[[v]]) |> 
        shapiro_test(value)}, 
        silent = TRUE)
    if (inherits(pp, "try-error")) {
      return(NA)
    } else {
      if (any(pp$p < 0.05)) {
        kruskal_test(data = dd, formula(paste0("value ~ ", v)))$p < 0.05
      } else {
        anova_test(data = dd, formula(paste0("value ~ ", v)))$p < 0.05
      }
    }
  }) |> 
    which() |> 
    names()
  
  return(vars)
}
