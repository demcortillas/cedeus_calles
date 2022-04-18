options(scipen=999)
library(opentripplanner)
library(sf)
library(gtfsio)
library(httr)
library(jsonlite)
sf::sf_use_s2(FALSE)

# No terminado porque se encontraron los shp's previos de Bryan. Los cuales se cruzan y trabajan en el siguiente script (6)

##################
#### SANTIAGO ####
##################

# Zonas y manzanas

SANTIAGO        <- st_read("outputs/ISMT/SANTIAGO/SANTIAGO_ISMT.shp")
SANTIAGO_point <- SANTIAGO %>% st_transform(4326) %>% subset(PROVINCIA == 131 | COMUNA == 13201) %>% st_centroid()
SANTIAGO_poly  <- SANTIAGO %>% st_transform(4326) %>% subset(PROVINCIA == 131 | COMUNA == 13201)

MANZ_SANTIAGO_point <- st_read("inputs/geodata/MANZANA/Manzana_nivel.shp") %>% subset(PROVINCIA == 'SANTIAGO' | COMUNA == 'PUENTE ALTO') %>% st_centroid() %>% st_transform(4326)
MANZ_SANTIAGO_point$PERSONAS_M[MANZ_SANTIAGO_point$PERSONAS_M == 'Indeterminado'] <- 0
MANZ_SANTIAGO_point$PERSONAS_M <- as.numeric(MANZ_SANTIAGO_point$PERSONAS_M)

# Creacion de grafo

path_data  <- file.path(getwd(), "inputs/OTP")
path_otp   <- otp_dl_jar(path_data, cache = FALSE)

log1       <- otp_build_graph(otp = path_otp, dir = path_data,memory = 10240) 
otp_setup(otp = path_otp, dir = path_data)
otpcon     <- otp_connect()

# Paraderos y estaciones

path  <- 'inputs/OTP/graphs/default/GTFS.zip'
stops <- import_gtfs(path)
stops <- as.data.frame(stops$stops)
stops <- st_as_sf(stops, coords = c('stop_lon','stop_lat'), crs = 4326, agr = "constant")

Metro_station <- st_read("inputs/geodata/METRO/Estaciones_metro.shp") %>% st_transform(4326)

# Por alguna razonque desconozco, las funciones de R fallan creando la isocrona, por lo que se le pide directamente a la API

URL1 <- 'http://localhost:8080/otp/routers/default/isochrone?fromPlace=-33.496170356043066%2C-70.70526123046875&date=2022/12/7&time=08:00:00&mode=WALK&cutoffSec=600'
resp <- GET(URL1)
resp <- content(resp, "text", encoding = "UTF-8")




