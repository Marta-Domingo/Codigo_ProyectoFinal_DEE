# =======================================================
# CARGA DE LIBRERÍAS Y DATOS
# =======================================================

library(readxl)
library(mapSpain)
library(dplyr)
library(stringr)
library(knitr)
library(ggplot2)
library(cowplot)
library(spdep)
library(glue)

# Carga de datos desde archivos Excel
esperanza <- read_xls("Esperanza_de_vida_al_nacer.xls")
gasto <- read_xls("Gasto_sanitario_publico_gestionado_por_las_comunidades_autonomas_por_habitante_protegido.xls")
medicos <- read_xls("Personal_medico_atencion_primaria_por_1000_personas_asignadas.xls")
tiempo <- read_xls("Tiempo_medio_de_espera_dias_para_una_intervencion_quirugica_no_urgente.xls")

ccaa <- esp_get_ccaa()


# =======================================================
# LIMPIEZA Y TRANSFORMACIÓN DE DATOS
# =======================================================

# Eliminación de filas innecesarias y conversión de datos numéricos

limpiar_df <- function(df, n_inicio = 3, n_final = 0, nuevo_nombre_columna = "valor") {
  df %>%
    slice((n_inicio + 1):(n() - n_final)) %>%
    rename(ccaa = 1, !!nuevo_nombre_columna := 2) %>%
    mutate(!!nuevo_nombre_columna := as.numeric(.data[[nuevo_nombre_columna]]))
}

esperanza <- limpiar_df(esperanza, n_inicio = 3, n_final = 0, nuevo_nombre_columna = "valor_esperanza")
gasto <- limpiar_df(gasto, n_inicio = 3, n_final = 3, nuevo_nombre_columna = "valor_gasto")
medicos <- limpiar_df(medicos, n_inicio = 3, n_final = 3, nuevo_nombre_columna = "valor_medicos")
tiempo <- limpiar_df(tiempo, n_inicio = 3, n_final = 3, nuevo_nombre_columna = "valor_tiempo")



# =======================================================
# UNIFICACIÓN DE DATOS CON MAPA ESPAÑA
# =======================================================

# Función para limpiar nombres de CCAA
limpiar_ccaa <- function(nombre) {
  nombre_limpio <- str_remove(nombre, " \\(.*\\)")
  nombre_corregido <- recode(nombre_limpio,
                                    "Navarra, C. Foral de" = "Navarra, Comunidad Foral de",
                                    "La Rioja" = "Rioja, La")
  return(nombre_corregido)
}

# Aplicación de limpieza de nombres a cada conjunto de datos
esperanza <- esperanza %>% mutate(ccaa = limpiar_ccaa(ccaa))
gasto <- gasto %>% mutate(ccaa = limpiar_ccaa(ccaa))
medicos <- medicos %>% mutate(ccaa = limpiar_ccaa(ccaa))
tiempo <- tiempo %>% mutate(ccaa = limpiar_ccaa(ccaa))


# =======================================================
# ANÁLISIS Y VISUALIZACIÓN DE DATOS
# =======================================================

# Función para resumir variables principales
resumen_variable <- function(df, nombre_variable) {
  df_top <- df[order(-df[[2]]), ][1:3, ]
  df_bottom <- df[order(df[[2]]), ][1:3, ]
  
  data.frame(
    Variable = nombre_variable,
    `Top 1` = paste0(df_top$ccaa[1], " (", round(df_top[[2]][1], 1), ")"),
    `Top 2` = paste0(df_top$ccaa[2], " (", round(df_top[[2]][2], 1), ")"),
    `Top 3` = paste0(df_top$ccaa[3], " (", round(df_top[[2]][3], 1), ")"),
    `Bottom 1` = paste0(df_bottom$ccaa[1], " (", round(df_bottom[[2]][1], 1), ")"),
    `Bottom 2` = paste0(df_bottom$ccaa[2], " (", round(df_bottom[[2]][2], 1), ")"),
    `Bottom 3` = paste0(df_bottom$ccaa[3], " (", round(df_bottom[[2]][3], 1), ")")
  )
}

# Tabla resumen
tabla_resumen <- rbind(
  resumen_variable(esperanza, "Esperanza de vida"),
  resumen_variable(gasto, "Gasto sanitario"),
  resumen_variable(medicos, "Médicos por 1000 hab."),
  resumen_variable(tiempo, "Tiempo de espera"))

kable(tabla_resumen, caption = "Resumen de valores más altos y más bajos por variable")

# =======================================================
# MAPAS BIVARIADOS
# =======================================================

ccaa_bivariado <- ccaa %>%
  left_join(gasto, by = c("ine.ccaa.name" = "ccaa")) %>%
  left_join(medicos, by = c("ine.ccaa.name" = "ccaa")) %>%
  left_join(esperanza, by = c("ine.ccaa.name" = "ccaa")) %>%
  left_join(tiempo, by = c("ine.ccaa.name" = "ccaa"))

biv_colores_legibles <- c( 
  "Bajo / Bajo" = "#e8e8e8",
  "Bajo / Medio" = "#ace4e4",
  "Bajo / Alto" = "#5ac8c8",
  "Medio / Bajo" = "#dfb0d6",
  "Medio / Medio" = "#a5add3",
  "Medio / Alto" = "#5698b9",
  "Alto / Bajo" = "#be64ac",
  "Alto / Medio" = "#8c62aa",
  "Alto / Alto" = "#3b4994")

clasificar_bivariado <- function(df, var1, var2) {
  df %>%
    mutate(
      cat1 = cut(.data[[var1]],
                 breaks = quantile(.data[[var1]],
                                   probs = seq(0, 1, length.out = 4),
                                   na.rm = TRUE),
                 labels = c("Bajo", "Medio", "Alto"),
                 include.lowest = TRUE),
      cat2 = cut(.data[[var2]],
                 breaks = quantile(.data[[var2]],
                                   probs = seq(0, 1, length.out = 4),
                                   na.rm = TRUE),
                 labels = c("Bajo", "Medio", "Alto"),
                 include.lowest = TRUE),
      clase = paste(cat1, "/", cat2))
  }

extraer_leyenda <- function(nombre1, nombre2) {
  dummy <- tibble(
    x = 1:9,
    clase = factor(names(biv_colores_legibles),
                   levels = names(biv_colores_legibles)))
  p <- ggplot(dummy, aes(x, fill = clase)) +
    geom_bar() +
    scale_fill_manual(
      values = biv_colores_legibles,
      name = paste0(nombre1, " / ", nombre2)) +
    theme(legend.position = "right")
  get_legend(p, return_all = FALSE)
  }

crear_mapa_bivariado <- function(df, var1, var2, nombre1, nombre2) {
  
  df_clasificado <- clasificar_bivariado(df, var1, var2)
  
  mapa <- ggplot(df_clasificado) +
    geom_sf(aes(fill = clase), color = "white") +
    scale_fill_manual(values = biv_colores_legibles, guide = "none") +
    labs(title = paste(nombre1, "+", nombre2)) +
    theme_minimal() +
    theme(plot.margin = margin(10, 10, 10, 10))
  
  leyenda <- extraer_leyenda(nombre1, nombre2)
  
  plot_grid(
    mapa,
    leyenda,
    rel_widths = c(1, 0.2),
    ncol = 2)
  }

crear_mapa_bivariado(ccaa_bivariado, "valor_gasto", "valor_esperanza", "Gasto sanitario", "Esperanza de vida")
crear_mapa_bivariado(ccaa_bivariado, "valor_gasto", "valor_tiempo", "Gasto sanitario", "Tiempo de espera")
crear_mapa_bivariado(ccaa_bivariado, "valor_medicos", "valor_esperanza", "Médicos", "Esperanza de vida")
crear_mapa_bivariado(ccaa_bivariado, "valor_medicos", "valor_tiempo", "Médicos", "Tiempo de espera")


# =======================================================
# ÍNDICE MORAN
# =======================================================

indice_moran <- function(sf_objeto, variables, nombres_legibles) {
  resultados <- lapply(seq_along(variables), function(i) {
    var <- variables[i]
    nombre <- nombres_legibles[i]
    
    datos_validos <- !is.na(sf_objeto[[var]])
    sf_filtrado <- sf_objeto[datos_validos, ]
    
    vecinos <- poly2nb(sf_filtrado, queen = TRUE)
    pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
    
    moran_res <- moran.test(sf_filtrado[[var]], pesos, zero.policy = TRUE)
    
    tibble(
      Variable = nombre,
      `Moran I` = round(moran_res$estimate[["Moran I statistic"]], 3),
      `p-valor` = signif(moran_res$p.value, 3)
    )
  })
  
  bind_rows(resultados) %>%
    kable(caption = "Índice de Moran para variables seleccionadas")
}

vars <- c("valor_esperanza", "valor_tiempo", "valor_medicos", "valor_gasto")
nombres <- c("Esperanza de vida", "Tiempo medio de espera", "Personal médico", "Gasto sanitario")

indice_moran(ccaa_bivariado, vars, nombres)


# =======================================================
# MAPAS LISA
# =======================================================

crear_lisa_bivariado <- function(sf_objeto, var1, var2, 
                                 nombre1 = var1, nombre2 = var2, 
                                 alpha = 0.05) {
  
  # Filtrar regiones con datos completos
  datos_validos <- !is.na(sf_objeto[[var1]]) & !is.na(sf_objeto[[var2]])
  sf_filtrado <- sf_objeto[datos_validos, ]
  
  if (nrow(sf_filtrado) < 3) {
    stop("No hay suficientes datos completos para calcular LISA.")
  }
  
  # Crear vecinos y pesos
  vecinos <- poly2nb(sf_filtrado, queen = TRUE)
  pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
  
  # Lag espacial de la segunda variable
  y_lag <- lag.listw(pesos, sf_filtrado[[var2]], zero.policy = TRUE)
  
  # Calcular LISA sobre la primera variable
  lisa_result <- localmoran(sf_filtrado[[var1]], pesos, zero.policy = TRUE)
  
  # Detectar columna de p-valor automáticamente
  p_col <- grep("^Pr\\(", colnames(lisa_result), value = TRUE)
  p_vals <- lisa_result[, p_col]
  
  # Clasificación de clusters espaciales
  media_x <- mean(sf_filtrado[[var1]], na.rm = TRUE)
  media_y_lag <- mean(y_lag, na.rm = TRUE)
  
  cluster <- case_when(
    sf_filtrado[[var1]] > media_x & y_lag > media_y_lag & p_vals < alpha ~ "High-High",
    sf_filtrado[[var1]] < media_x & y_lag < media_y_lag & p_vals < alpha ~ "Low-Low",
    sf_filtrado[[var1]] > media_x & y_lag < media_y_lag & p_vals < alpha ~ "High-Low",
    sf_filtrado[[var1]] < media_x & y_lag > media_y_lag & p_vals < alpha ~ "Low-High",
    TRUE ~ "No significativo"
  )
  
  sf_filtrado$lisa_cluster <- factor(cluster, 
                                     levels = c("High-High", "Low-Low", "High-Low", "Low-High", "No significativo"))
  
  colores_lisa <- c(
    "High-High" = "#d7191c",
    "Low-Low" = "#2c7bb6",
    "High-Low" = "#fdae61",
    "Low-High" = "#abd9e9",
    "No significativo" = "grey80"
  )
  
  ggplot(sf_filtrado) +
    geom_sf(aes(fill = lisa_cluster), color = "white") +
    scale_fill_manual(values = colores_lisa, name = "Cluster LISA") +
    labs(
      title = glue("LISA bivariado: {nombre1} vs entorno de {nombre2}"),
      caption = glue("Significancia: p < {alpha}")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right"
    )
}

crear_lisa_bivariado(ccaa_bivariado, "valor_gasto", "valor_esperanza", "Gasto sanitario", "Esperanza de vida")
crear_lisa_bivariado(ccaa_bivariado, "valor_gasto", "valor_tiempo", "Gasto sanitario", "Tiempo medio de espera")
crear_lisa_bivariado(ccaa_bivariado, "valor_medicos", "valor_esperanza", "Personal médico", "Esperanza de vida")
crear_lisa_bivariado(ccaa_bivariado, "valor_medicos", "valor_tiempo", "Personal médico", "Tiempo medio de espera")

