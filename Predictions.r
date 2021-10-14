library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(dplyr)

studentData <- st_read("studentData.geojson", crs = 'ESRI:102254' )
