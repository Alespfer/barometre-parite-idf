<!-- Language Navigation -->
<div align="right">
  <a href="./README.md">English</a> | <a href="./README_fr.md">Français</a> | <b><a href="./README_es.md">Español</a></b>
</div>

# Dashboard de Igualdad de Género - Región de París

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
![Language](https://img.shields.io/badge/Idioma-R-blue)
![Framework](https://img.shields.io/badge/Framework-Shiny-hotpink)
[![Status](https://img.shields.io/badge/Estado-Activo-success)](https://alespfer.shinyapps.io/barometre-parite-idf/)

Una aplicación web interactiva desarrollada con **R** y **Shiny** para analizar y visualizar datos sobre la igualdad profesional de género en la región de **Île-de-France** (París y sus alrededores). Este dashboard utiliza datos públicos del "Índice Egapro" francés para proporcionar análisis territoriales y sectoriales a responsables de políticas públicas, agencias de desarrollo económico e investigadores.

**[➡️ Ver la aplicación desplegada](https://alespfer.shinyapps.io/barometre-parite-idf/)**

![Vista principal del dashboard](img/main-dashboard-view.png)

## Tabla de Contenidos

- [Sobre el Proyecto](#sobre-el-proyecto)
- [Características Principales](#características-principales)
- [Proceso de Datos y Automatización](#proceso-de-datos-y-automatización)
- [Tecnologías Utilizadas](#tecnologías-utilizadas)
- [Cómo Empezar](#cómo-empezar)
  - [Prerrequisitos](#prerrequisitos)
  - [Instalación](#instalación)
- [Uso](#uso)
- [Licencia](#licencia)
- [Contacto](#contacto)

## Sobre el Proyecto

Desde 2018, las empresas francesas con más de 50 empleados deben calcular y publicar su **Índice de Igualdad de Género "Egapro"**. Este índice, con una puntuación de 100, es una herramienta clave para medir y reducir las desigualdades de género en el ámbito laboral. Se basa en cinco indicadores:
- Brecha salarial de género (40 puntos)
- Diferencia en los aumentos salariales individuales (20-35 puntos)
- Diferencia en las tasas de promoción (15 puntos)
- Aumentos salariales para empleadas que regresan de la baja por maternidad (15 puntos)
- Paridad entre los 10 empleados mejor pagados (10 puntos)

Aunque estos datos son públicos, su análisis suele limitarse al nivel nacional. Este proyecto fue desarrollado para ofrecer una **perspectiva territorial** detallada dentro de la región de Île-de-France, permitiendo análisis a nivel de departamento, intermunicipalidad (EPCI) y zona de empleo (Zone d'Emploi).

## Características Principales

El dashboard se organiza en varios módulos analíticos:

*   🗺️ **Mapa y Territorios:** Un mapa interactivo para visualizar las puntuaciones medias del Egapro en diferentes territorios administrativos y económicos. Los usuarios pueden filtrar por año, tamaño de empresa y sector de actividad, y buscar empresas específicas por su número SIREN.
*   📊 **Análisis Sectorial:** Un gráfico de "lollipops" que destaca los sectores empresariales con mejor y peor rendimiento. Este módulo es interactivo: hacer clic en un sector filtra el mapa principal para una exploración más profunda.
*   🔍 **Análisis de Indicadores:** Un módulo de desglose para analizar el rendimiento en cada uno de los cinco indicadores individuales que componen la puntuación global del Egapro.
*   📈 **Análisis Sociodemográfico:** Una herramienta exploratoria para visualizar posibles correlaciones entre el rendimiento de las empresas y el contexto socioeconómico de su zona de empleo (p. ej., tasa de actividad femenina, proporción de mujeres en puestos directivos).
*   📉 **Tendencias Históricas:** Un módulo de análisis de series temporales para seguir y comparar la evolución de las puntuaciones del Egapro a lo largo de varios años para los territorios seleccionados.

## Proceso de Datos y Automatización

Para garantizar que los datos estén siempre actualizados y sean fiables, el proyecto cuenta con un proceso de tratamiento de datos totalmente automatizado mediante **GitHub Actions**.



1.  **Extracción de Datos:** Un flujo de trabajo programado (`.github/workflows/data-pipeline.yml`) se ejecuta mensualmente. Obtiene los datos más recientes de varias API públicas:
    *   **Índice Egapro:** de `data.gouv.fr`
    *   **Base de datos SIRENE (info de empresas):** de `Opendatasoft`
    *   **Datos del Censo (sociodemográficos):** del INSEE (archivos locales)
2.  **Transformación de Datos:** El script `run_pipeline.R` limpia, estandariza, enriquece y fusiona estos conjuntos de datos en una tabla maestra final. Pasos clave incluyen la geolocalización de las sedes de las empresas, el mapeo de códigos NAF a sectores de actividad y el cálculo de indicadores sociodemográficos.
3.  **Carga para Shiny:** Los datos procesados se guardan como archivos `.RDS` optimizados en el directorio `data_shiny/`. La aplicación Shiny lee estos archivos directamente, asegurando tiempos de carga rápidos y una alta reactividad.
4.  **Despliegue Continuo:** Un segundo flujo de trabajo de GitHub Actions (`.github/workflows/deploy-shinyapp.yml`) vuelve a desplegar automáticamente la aplicación en `shinyapps.io` cada vez que se realizan cambios en la rama `main`, incluidas las actualizaciones automáticas de datos.

## Tecnologías Utilizadas

Este proyecto se basa en un moderno ecosistema de R para el Tidyverse y el análisis de datos espaciales:

*   **Núcleo:** [R](https://www.r-project.org/), [Shiny](https://shiny.posit.co/)
*   **UI/UX:** [{bslib}](https://rstudio.github.io/bslib/) para temas de Bootstrap 5, [{plotly}](https://plotly.com/r/) para gráficos interactivos
*   **Manipulación de Datos:** [{dplyr}](https://dplyr.tidyverse.org/), [{tidyr}](https://tidyr.tidyverse.org/)
*   **Análisis Espacial y Mapas:** [{sf}](https://r-spatial.github.io/sf/), [{leaflet}](https://rstudio.github.io/leaflet/)
*   **Reproducibilidad:** [{renv}](https://rstudio.github.io/renv/) para la gestión de dependencias

## Cómo Empezar

Para ejecutar este proyecto localmente, sigue estos pasos.

### Prerrequisitos

*   R (versión 4.2 o superior)
*   Se recomienda RStudio para una mejor experiencia.

### Instalación

1.  Clona el repositorio:
    ```bash
    git clone https://github.com/Alespfer/barometre-parite-idf.git
    ```
2.  Navega al directorio del proyecto:
    ```bash
    cd barometre-parite-idf
    ```
3.  Abre el archivo `egapro.Rproj` en RStudio.
4.  El paquete `{renv}` restaurará automáticamente las dependencias del proyecto desde el archivo `renv.lock`. Si se te solicita, escribe `renv::restore()` en la consola y confirma. Esto instalará todos los paquetes necesarios en una librería específica del proyecto.
5.  Si necesitas ejecutar el proceso de datos tú mismo, tendrás que descargar los archivos del censo del INSEE especificados en `methodologie_preparation_donnees.Rmd` y colocarlos en el directorio `data/raw/`. De lo contrario, los datos preprocesados ya están disponibles en `data_shiny/`.

## Uso

Una vez instaladas las dependencias, puedes ejecutar la aplicación abriendo el archivo `app.R` y haciendo clic en "Run App" en RStudio, o ejecutando el siguiente comando en la consola de R:

```R
shiny::runApp('app.R')
```

## Licencia

Este proyecto se distribuye bajo la Licencia MIT. Consulta el archivo `LICENSE` para más información.

## Contacto

Alberto Esperon - [LinkedIn](https://www.linkedin.com/in/alberto-espfer) - [Perfil de GitHub](https://github.com/Alespfer)

Enlace del Proyecto: [https://github.com/Alespfer/barometre-parite-idf](https://github.com/Alespfer/barometre-parite-idf)
