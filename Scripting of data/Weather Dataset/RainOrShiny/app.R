# Package
packages = c(
  'rsconnect',
  'tinytex',
  'plotly',
  'RColorBrewer',
  'classInt',
  'ggthemes',
  'tidyverse',
  'pivottabler',
  'dplyr',
  'shiny',
  'shinythemes',
  'lubridate',
  'sf',
  'tmap',
  'shinyWidgets',
  'leaflet',
  'ggmosaic',
  'htmltools',
  'raster',
  'rgdal',
  'rgeos',
  'remotes',
  'ggrepel',
  'scales',
  'd3Tree',
  'data.table'
)
for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

