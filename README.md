# 📊 Informe de Homicidios Dolosos en CABA (2014–2023)

Trabajo Práctico individual realizado para la **Maestría en Ciencia de Datos (MCD)** –  
*Algoritmos y Estructuras de Datos (2025)*, **Universidad Austral**.

Este repositorio contiene un **script en R totalmente reproducible** que procesa datos oficiales del Sistema Nacional de Información Criminal (SNIC) y del INDEC para analizar la evolución de los homicidios dolosos en la Ciudad Autónoma de Buenos Aires (CABA), incorporando visualizaciones estáticas e interactivas de nivel profesional.

---

## 🎯 Objetivos del proyecto

El trabajo aborda dos ejes principales:

### 1️⃣ Análisis temporal (2014–2023)
- Cálculo de **víctimas totales y por sexo**.
- Estimación de **tasas de homicidios por cada 100.000 habitantes** utilizando proyecciones oficiales de población.
- Generación de:
  - 📈 **Gráfico combinado** (barras + curva suavizada) de víctimas y tasas.
  - 📋 **Tabla analítica** con distribución por sexo y participación porcentual.

### 2️⃣ Análisis espacial (CABA – 2023)
- Cálculo de víctimas y tasas por **comuna**.
- Generación de:
  - 🗺️ **Mapa interactivo de valores absolutos**.
  - 🗺️ **Mapa interactivo de tasas cada 100.000 habitantes**.
  - Sincronización de mapas para análisis comparativo.

---

## 📂 Estructura del repositorio

```text
.
├── tp.R                                # Script principal (ejecutable end-to-end)
├── snic-departamentos-mes-sexo.csv     # Dataset SNIC (no incluido en la entrega)
├── caba_proyeccion_poblacion_2025.xls  # Proyecciones de población INDEC
├── comunas.geojson                     # Geometrías de comunas CABA
├── caba_homicidios_dolosos_2014_2023.csv  # Output generado
├── caba_homicidios_dolosos_2023.csv       # Output generado
└── README.md
