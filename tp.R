# Desactivo notación científica
options(scipen=999)

# Limpio el entorno
rm(list = ls())

# Cargo las librerías necesarias
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("httr")) {
  install.packages("httr")
  library(httr)
}
if (!require("janitor")) {
  install.packages("janitor")
  library(janitor)
}
if (!require("rstudioapi")) {
  install.packages("rstudioapi")
  library(rstudioapi)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("gt")) {
  install.packages("gt")
  library(gt)
}
if (!require("leaflet")){
   install.packages("leaflet");
library(leaflet)
}
if (!require("leafsync")) {
  install.packages("leafsync"); 
  library(leafsync)
}
if (!require("sf")) {
  install.packages("sf"); 
  library(sf)
}
if (!require("scales")) {
  install.packages("scales");
  library(scales)
}
if (!require("htmltools")) {
  install.packages("htmltools");
  library(htmltools)
} 

# Cargo las rutas de los archivos
snic_departamentos_mes_sexo <- "snic-departamentos-mes-sexo.csv"
caba_proyeccion_poblacion_2025 <- "caba_proyeccion_poblacion_2025.xls"
comunas_geojson <- "comunas.geojson"

caba_homicidios_dolosos_2014_2023 <- "caba_homicidios_dolosos_2014_2023.csv"
caba_homicidios_dolosos_2023 <- "caba_homicidios_dolosos_2023.csv"

# Configuro el entorno de trabajo
path <- ""
script_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "")
setwd(script_path)
getwd()

# ###########################################################################
# 1.- Informe de homicidios dolosos en CABA (2014-2023)
# ###########################################################################

# Cargo el archivo CSV
df_snic <- read_csv2(snic_departamentos_mes_sexo)

# Verificaciones generales
head(df_snic)                       # Verifico las primeras filas del df
str(df_snic)                        # Verifico la estructura del df
colnames(df_snic)                   # Verifico los nombres de las columnas

# Filtro los datos para quedarme solo con homicidios dolosos en CABA entre 2014 y 2023
df_snic_filtrado <- df_snic |>
  filter(anio >= 2014 & anio < 2024, codigo_delito_snic_id == 1 & provincia_id == '02')

# Verifico si hay valores NA en las columnas del df filtrado
colSums(is.na(df_snic_filtrado))

# Agrupo por año y calculo los totales y porcentajes por sexo
snic_caba_agrupado <- df_snic_filtrado |>
  summarise(cantidad_victimas_masc = sum(cantidad_victimas_masc),
            cantidad_victimas_fem = sum(cantidad_victimas_fem),
            cantidad_victimas_sd = sum(cantidad_victimas_sd),
            cantidad_victimas = sum(cantidad_victimas),
            porcentaje_masc = round(cantidad_victimas_masc / cantidad_victimas * 100, 2),
            porcentaje_fem = round(cantidad_victimas_fem / cantidad_victimas * 100, 2), 
            porcentaje_sd = round(cantidad_victimas_sd / cantidad_victimas * 100, 2),
            .by = anio) 

# Leo el Excel de proyecciones de población
proyeccion_poblacion_caba <- read_excel(caba_proyeccion_poblacion_2025, range = "A3:Q19", col_names = TRUE)

# Transformo la tabla de proyecciones a formato largo y filtro la fila total
poblacion_total_x_año <- proyeccion_poblacion_caba |>
  pivot_longer(
    cols = -Comuna,
    names_to = "anio",
    values_to = "poblacion") |>
  mutate(anio = as.numeric(anio)) |>
  filter(Comuna == "Total")

# Uno las proyecciones con los datos de homicidios
snic_caba_agrupado_poblacion <- snic_caba_agrupado |>
  left_join(poblacion_total_x_año, by = "anio")

# Calculo la tasa de homicidios por cada 100.000 habitantes
snic_caba_agrupado_poblacion <- snic_caba_agrupado_poblacion |>
  mutate(tasa_victimas_100k = round(100000 * cantidad_victimas / poblacion, 2))

# Elimino la columna Comuna que ya no necesito
snic_caba_agrupado_poblacion$Comuna <- NULL

# Imprimo el resultado para verificar que esté todo bien
print(snic_caba_agrupado_poblacion)

# Reordeno las columnas del dataframe para mejor presentación
snic_caba_agrupado_poblacion <- snic_caba_agrupado_poblacion |>
  select(anio, cantidad_victimas_masc, cantidad_victimas_fem, cantidad_victimas_sd, 
         cantidad_victimas, porcentaje_masc, porcentaje_fem, porcentaje_sd, 
         tasa_victimas_100k, poblacion) |>
  arrange(anio)

# Escribo el resultado a un archivo CSV
write.csv(snic_caba_agrupado_poblacion, caba_homicidios_dolosos_2014_2023, row.names = FALSE)

# Escalo la tasa para representarla en el gráfico
df <- snic_caba_agrupado_poblacion %>%
  mutate(tasa_escalada = tasa_victimas_100k * 35)

# Aplico interpolación spline para suavizar la curva
spline_data <- as.data.frame(spline(x = df$anio, y = df$tasa_escalada, n = 300))

# Construyo el gráfico con barras y curva suavizada
grafico <- plot_ly() %>%
  add_bars(
    data = df,
    x = ~factor(anio),
    y = ~cantidad_victimas,
    name = "Víctimas",
    marker = list(color = '#bed5b4'),
    text = ~cantidad_victimas,
    textposition = 'middle',
    insidetextanchor = 'middle',
    textfont = list(color = '#62685f', size = 13, family = 'Open Sans', bold = TRUE),
    hoverinfo = 'text',
    hovertext = ~paste0("Año: ", anio, "<br>Víctimas: ", cantidad_victimas)
  ) %>%
  add_trace(
    x = spline_data$x,
    y = spline_data$y,
    type = 'scatter',
    mode = 'lines',
    name = "Tasa",
    line = list(color = '#78a659', width = 2),
    hoverinfo = "skip"
  ) %>%
  add_trace(
    data = df,
    x = ~factor(anio),
    y = ~tasa_escalada,
    type = 'scatter',
    mode = 'markers+text',
    text = ~round(tasa_victimas_100k, 1),
    textposition = 'top',
    textfont = list(color = '#4C8C2B', size = 12, family = 'Open Sans'),
    marker = list(color = '#78a659', size = 6),
    name = "Tasa",
    hoverinfo = 'text',
    hovertext = ~paste0("Año: ", anio, "<br>Tasa: ", round(tasa_victimas_100k, 1))
  ) %>%
  layout(
    title = list(
      text = "Víctimas de homicidios dolosos por año. Valores absolutos y tasas cada<br>100.000 habitantes. Ciudad Autónoma de Buenos Aires. Años 2014-2023",
      x = 0.5,
      font = list(size = 16)
    ),
    xaxis = list(
      title = "",
      tickvals = df$anio,
      ticktext = df$anio,
      showline = FALSE, 
      showgrid = FALSE
    ),
    yaxis = list(
      title = "",
      showticklabels = FALSE,
      showgrid = FALSE
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      showticklabels = FALSE,
      showgrid = FALSE
    ),
    legend = list(
      orientation = "h",
      x = 0.85, y = 1.05,
      font = list(size = 12)
    ),
    annotations = list(
      list(
        x = 0.5, y = -0.25,
        xref = "paper", yref = "paper",
        text = "Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC - SAT),<br>Ministerio de Seguridad de la Nación e INDEC",
        showarrow = FALSE,
        font = list(size = 11),
        xanchor = "center",
        align = "left"
      )
    ),
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    margin = list(b = 140)
  )

grafico

# Preparo el data frame para mostrarlo con gt
df <- snic_caba_agrupado_poblacion %>%
  mutate(
    cantidad_victimas_sd = ifelse(cantidad_victimas_sd == 0, "-", as.character(cantidad_victimas_sd)),
    porcentaje_sd = ifelse(porcentaje_sd == 0, "-", as.character(porcentaje_sd))
  ) %>%
  select(
    anio,
    cantidad_victimas_masc,
    porcentaje_masc,
    cantidad_victimas_fem,
    porcentaje_fem,
    cantidad_victimas_sd,
    porcentaje_sd,
    cantidad_victimas
  )

# Armo la tabla con gt
tabla_homicidios <- df %>%
  gt() %>%
  tab_header(
    title = "Víctimas de homicidios dolosos por sexo. Valores absolutos y participación.",
    subtitle = "Ciudad Autónoma de Buenos Aires. Años 2014–2023"
  ) %>%
  cols_label(
    anio = "Años",
    cantidad_victimas_masc = "Cantidad",
    porcentaje_masc = "Porcentaje",
    cantidad_victimas_fem = "Cantidad",
    porcentaje_fem = "Porcentaje",
    cantidad_victimas_sd = "Cantidad",
    porcentaje_sd = "Porcentaje",
    cantidad_victimas = "Total"
  ) %>%
  tab_spanner(label = "Masculino", columns = c("cantidad_victimas_masc", "porcentaje_masc")) %>%
  tab_spanner(label = "Femenino", columns = c("cantidad_victimas_fem", "porcentaje_fem")) %>%
  tab_spanner(label = "No consta", columns = c("cantidad_victimas_sd", "porcentaje_sd")) %>%
  fmt_percent(columns = c("porcentaje_masc", "porcentaje_fem"), decimals = 1, scale_values = FALSE, dec_mark = ",") %>%
  fmt_number(columns = c("cantidad_victimas_masc", "cantidad_victimas_fem", "cantidad_victimas"), decimals = 0) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) %>%
  tab_options(
    table.border.top.width = 0,
    heading.border.bottom.width = 0,
    column_labels.border.bottom.width = px(1.5),
    column_labels.border.bottom.color = "#333333",
    table_body.hlines.width = px(0),
    table.border.bottom.width = px(0),
    source_notes.border.bottom.width = px(0),
    source_notes.padding = px(8),
    table.font.size = px(13),
    data_row.padding = px(4),
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "#333333", weight = px(1.5)),
    locations = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "#333333", weight = px(1.5)),
    locations = cells_body(columns = everything(), rows = nrow(df))
  ) %>%
  tab_style(style = cell_text(weight = "normal"), locations = cells_column_spanners()) %>%
  tab_style(style = cell_text(v_align = "middle"), locations = cells_column_labels(columns = "anio")) %>%
  tab_source_note(source_note = html("&emsp;Referencias: (-) Cero absoluto.<br>&emsp;Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC - SAT), Ministerio de Seguridad de la <br>&emsp;Nación e INDEC."))

tabla_homicidios

# ###########################################################################
# 2.- Mapa de víctimas de homicidios dolosos por comuna - CABA (2023)
# ###########################################################################

# Cargo el archivo de SNIC y filtro los datos de 2023 para CABA y homicidios dolosos
df_homicidios_2023 <- read_csv2(snic_departamentos_mes_sexo) |>
  filter(anio == 2023, codigo_delito_snic_id == 1, provincia_id == '02')

# Verifico si hay valores NA en el dataset
colSums(is.na(df_homicidios_2023))

# Agrupo los homicidios por comuna y calculo totales
df_filtrado <- df_homicidios_2023 %>%
  filter(provincia_id == '02',
         codigo_delito_snic_id == 1,
         anio == 2023,
         grepl("^Comuna", departamento_nombre)) %>%
  mutate(comuna = as.numeric(gsub("Comuna ", "", departamento_nombre))) %>%
  group_by(comuna) %>%
  summarise(
    total_homicidios = sum(cantidad_hechos, na.rm = TRUE),
    total_victimas = sum(cantidad_victimas, na.rm = TRUE)
  )

# Vuelvo a leer el Excel y ajusto nombres
proyeccion_poblacion_caba <- read_excel(caba_proyeccion_poblacion_2025, range = "A3:Q19", col_names = TRUE)
colnames(proyeccion_poblacion_caba)[1] <- "comuna"

# Filtro solo las comunas y me quedo con la población 2023
poblacion_comunas <- proyeccion_poblacion_caba %>%
  filter(comuna != "Total") %>%
  mutate(comuna = as.numeric(comuna)) %>%
  select(comuna, `2023`) %>%
  rename(poblacion = `2023`)

# Uno con la población y calculo tasas
df_final <- df_filtrado %>%
  left_join(poblacion_comunas, by = "comuna") %>%
  mutate(
    tasa_victimas_100k = round(100000 * total_victimas / poblacion, 2)
  ) %>%
  select(comuna, total_homicidios, total_victimas, poblacion, tasa_victimas_100k)

# Verifico la estructura final
str(df_final)

# Exporto el CSV
write_csv(df_final, "caba_homicidios_dolosos_2023.csv")

# Cargo el GeoJSON de comunas
comunas_geo <- st_read("comunas.geojson", quiet = TRUE)
comunas_geo$comuna <- as.numeric(comunas_geo$COMUNA)

# Filtro los homicidios dolosos en CABA para 2023 y agrupo por comuna
snic_2023 <- df_snic %>%
  filter(anio == 2023, provincia_id == "02", codigo_delito_snic_id == 1) %>%
  filter(grepl("^Comuna", departamento_nombre)) %>%
  mutate(comuna = as.numeric(gsub("Comuna ", "", departamento_nombre))) %>%
  group_by(comuna) %>%
  summarise(victimas = sum(cantidad_victimas, na.rm = TRUE))

# Tomo la proyección de población para 2023 por comuna
poblacion_comunas <- proyeccion_poblacion_caba %>%
  filter(comuna != "Total") %>%
  mutate(comuna = as.numeric(comuna)) %>%
  select(comuna, `2023`) %>%
  rename(poblacion = `2023`)

# Uno homicidios con población y calculo tasa cada 100.000
df_final <- snic_2023 %>%
  left_join(poblacion_comunas, by = "comuna") %>%
  mutate(tasa_victimas_100k = round(100000 * victimas / poblacion, 1))

# Uno la info de homicidios y tasas con el GeoJSON
comunas_final <- comunas_geo %>%
  left_join(df_final, by = "comuna")

# Primero valido las geometrías (por si hay polígonos corruptos o degenerados)
comunas_final_valid <- st_make_valid(comunas_final)

# Calculo los centroides sobre geometrías válidas
centroides <- st_centroid(comunas_final_valid)

# Paleta para víctimas absolutas (en verde)
pal_abs <- colorNumeric("Greens", domain = comunas_final$victimas, na.color = "gray90")

# Mapa de valores absolutos con etiquetas en el centro
mapa_abs <- leaflet(comunas_final) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_abs(victimas),
    fillOpacity = 0.85,
    weight = 1,
    color = "white",
    label = ~paste0("Comuna ", comuna, ": ", victimas, " víctimas"),
    highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
  ) %>%
  addLabelOnlyMarkers(
    data = centroides,
    label = ~as.character(victimas),
    labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                style = list("font-weight" = "bold", "font-size" = "13px"))
  ) %>%
  addLegend("bottomright", pal = pal_abs, values = ~victimas,
            title = "Total víctimas", opacity = 1)

# Paleta para tasas
pal_tasa <- colorNumeric("Greens", domain = comunas_final$tasa_victimas_100k, na.color = "gray90")

# Mapa de tasas por 100.000 habitantes
mapa_tasa <- leaflet(comunas_final) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_tasa(tasa_victimas_100k),
    fillOpacity = 0.85,
    weight = 1,
    color = "white",
    label = ~paste0("Comuna ", comuna, ": ", tasa_victimas_100k, " cada 100.000 hab."),
    highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
  ) %>%
  addLabelOnlyMarkers(
    data = centroides,
    label = ~as.character(tasa_victimas_100k),
    labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                style = list("font-weight" = "bold", "font-size" = "13px"))
  ) %>%
  addLegend("bottomright", pal = pal_tasa, values = ~tasa_victimas_100k,
            title = "Víctimas por 100k", opacity = 1)

# Sincronizo ambos mapas
mapas_sync <- sync(mapa_abs, mapa_tasa)

# Creo título y fuente con htmltools
titulo <- tags$h3(
  style = "text-align: center; margin-bottom: 20px; font-family: 'Open Sans', font-weight: bold;",
  "Víctimas de homicidios dolosos por comuna. Valores absolutos y tasas cada 100.000 habitantes.",
  tags$br(),
  tags$b("Ciudad Autónoma de Buenos Aires. Año 2023.")
)

fuente <- tags$p(
  style = "text-align: center; font-size: 12px; margin-top: 20px;",
  "Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC-SAT), Ministerio de Seguridad de la Nación e INDEC."
)

estilo_fuente <- tags$style(HTML("
  body, .leaflet-container, .leaflet-label, .leaflet-popup-content {
    font-family: 'Arial', sans-serif;
  }
"))

# Muestro el resultado
htmltools::browsable(
  tagList(
    estilo_fuente,
    titulo,
    mapas_sync,
    fuente
  )
)
