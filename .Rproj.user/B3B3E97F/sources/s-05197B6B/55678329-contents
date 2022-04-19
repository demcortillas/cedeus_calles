library(sf)
library(sp)
library(httr)
library(jsonlite)
library(tidyverse)
library(xlsx)
library(dplyr)

# Nota: se hizo en 2 partes: la primera sin San bBernardo simplemente porque se olvido en el analisis. La segunda parte s solo san bernardo y luego se unen.

##################
#### SANTIAGO ####
##################

SANTIAGO <- st_read("outputs/ISMT/SANTIAGO/SANTIAGO_ISMT.shp") %>% st_transform(4019) %>% subset(PROVINCIA == 131 | COMUNA == 13201 | COMUNA == 13401)

MANZ_SANTIAGO_point <- st_read("inputs/geodata/MANZANA/Manzana_nivel.shp") %>% subset(PROVINCIA == 'SANTIAGO' | COMUNA == 'PUENTE ALTO') %>% st_centroid() %>% st_transform(4019)
MANZ_SANTIAGO_point$PERSONAS_M[MANZ_SANTIAGO_point$PERSONAS_M == 'Indeterminado'] <- 0
MANZ_SANTIAGO_point$PERSONAS_M <- as.numeric(MANZ_SANTIAGO_point$PERSONAS_M)
Lat            <- st_coordinates(st_centroid(st_transform(MANZ_SANTIAGO_point,4019)))[,1]
Lon            <- st_coordinates(st_centroid(st_transform(MANZ_SANTIAGO_point,4019)))[,2]
MANZ_SANTIAGO_point$Lat   <- Lat
MANZ_SANTIAGO_point$Lon   <- Lon

MANZ_SANTIAGO_point2 <- st_read("inputs/geodata/MANZANA/Manzana_nivel.shp") %>% subset(COMUNA == 'SAN BERNARDO') %>% st_centroid() %>% st_transform(4019)
MANZ_SANTIAGO_point2$PERSONAS_M[MANZ_SANTIAGO_point2$PERSONAS_M == 'Indeterminado'] <- 0
MANZ_SANTIAGO_point2$PERSONAS_M <- as.numeric(MANZ_SANTIAGO_point2$PERSONAS_M)
Lat            <- st_coordinates(st_centroid(st_transform(MANZ_SANTIAGO_point2,4019)))[,1]
Lon            <- st_coordinates(st_centroid(st_transform(MANZ_SANTIAGO_point2,4019)))[,2]
MANZ_SANTIAGO_point2$Lat   <- Lat
MANZ_SANTIAGO_point2$Lon   <- Lon
# Para 5 minutos caminando

errores0 <- NULL
score0   <- read.csv('outputs/WALKSCORE/score0.csv')$score0
i<-1

conteo <- 1
while(i < dim(MANZ_SANTIAGO_point)[1]){
  print(i)
  lat <- MANZ_SANTIAGO_point$Lat[i]
  lon <- MANZ_SANTIAGO_point$Lon[i]
  URL <- paste0("http://walkmyplace.cedeus.cl/call_wps.php?wps=pedestrian&start_point=",lon,",",lat,"&walking_time_period=5&walking_speed=2.40&distance_decay_function=false&demographic=Adulto%20Mayor%20-%20Conce%20(%3E60)&escenario=1")
  succes <- try(
    exp = {
      resp <- GET(URL)
      text <- fromJSON(content(resp, "text"))
      score <- text$walkshed$properties$score
    }
  )
  if (is.null(score)){
    print(paste('Es nulo ',as.character(i)))
    errores <- c(errores0,i)
    i<-i+1
  }else{
    if('try-error' %in% class(succes)){
      print(paste('Error en ',as.character(i)))
      Sys.sleep(10)
    }else{
      print(score)
      score0[i] <- score
      write.csv(data.frame(ID=1:length(score0),score0),'outputs/WALKSCORE/1/score0.csv')
      Sys.sleep(0.5)
      i <- i+1
    }
  }
  conteo <- conteo + 1
  if(conteo%%100 == 0){Sys.sleep(10)}
}

# Para 10 minutos caminando

errores1 <- NULL
score1   <- read.csv('outputs/WALKSCORE/score1.csv')$score1
i<-1

while(i < dim(MANZ_SANTIAGO_point)[1]){
  print(i)
  lat <- MANZ_SANTIAGO_point$Lat[i]
  lon <- MANZ_SANTIAGO_point$Lon[i]
  URL <- paste0("http://walkmyplace.cedeus.cl/call_wps.php?wps=pedestrian&start_point=",lon,",",lat,"&walking_time_period=10&walking_speed=2.40&distance_decay_function=false&demographic=Adulto%20Mayor%20-%20Conce%20(%3E60)&escenario=1")
  succes <- try(
    exp = {
      resp <- GET(URL)
      text <- fromJSON(content(resp, "text"))
      score <- text$walkshed$properties$score
    }
  )
  if (is.null(score)){
    print(paste('Es nulo ',as.character(i)))
    errores <- c(errores1,i)
    i<-i+1
  }else{
    if('try-error' %in% class(succes)){
      print(paste('Error en ',as.character(i)))
      Sys.sleep(60)
    }else{
      print(score)
      score1[i] <- score
      write.csv(data.frame(ID=1:length(score1),score1),'outputs/WALKSCORE/1/score1.csv')
      Sys.sleep(1)
      i <- i+1
    }
  }
}



#### AGREGANDO LAS DE SAN BERNARDO ####

errores0 <- NULL
score0   <- rep(NA,dim(MANZ_SANTIAGO_point2)[1])
i<-1
conteo<-1
while(i < dim(MANZ_SANTIAGO_point2)[1]){
  print(i)
  lat <- MANZ_SANTIAGO_point2$Lat[i]
  lon <- MANZ_SANTIAGO_point2$Lon[i]
  URL <- paste0("http://walkmyplace.cedeus.cl/call_wps.php?wps=pedestrian&start_point=",lon,",",lat,"&walking_time_period=5&walking_speed=2.40&distance_decay_function=false&demographic=Adulto%20Mayor%20-%20Conce%20(%3E60)&escenario=1")
  succes <- try(
    exp = {
      resp <- GET(URL)
      text <- fromJSON(content(resp, "text"))
      score <- text$walkshed$properties$score
    }
  )
  if (is.null(score)){
    print(paste('Es nulo ',as.character(i)))
    errores <- c(errores0,i)
    i<-i+1
  }else{
    if('try-error' %in% class(succes)){
      print(paste('Error en ',as.character(i)))
      Sys.sleep(10)
    }else{
      print(score)
      score0[i] <- score
      write.csv(data.frame(ID=1:length(score0),score0),'outputs/WALKSCORE/2/score0.csv')
      Sys.sleep(0.5)
      i <- i+1
    }
  }
  conteo <- conteo + 1
  if(conteo%%100 == 0){Sys.sleep(10)}
}


errores1 <- NULL
score1   <- rep(NA,dim(MANZ_SANTIAGO_point2)[1])
i<-1

while(i < dim(MANZ_SANTIAGO_point2)[1]){
  print(i)
  lat <- MANZ_SANTIAGO_point2$Lat[i]
  lon <- MANZ_SANTIAGO_point2$Lon[i]
  URL <- paste0("http://walkmyplace.cedeus.cl/call_wps.php?wps=pedestrian&start_point=",lon,",",lat,"&walking_time_period=10&walking_speed=2.40&distance_decay_function=false&demographic=Adulto%20Mayor%20-%20Conce%20(%3E60)&escenario=1")
  succes <- try(
    exp = {
      resp <- GET(URL)
      text <- fromJSON(content(resp, "text"))
      score <- text$walkshed$properties$score
    }
  )
  if (is.null(score)){
    print(paste('Es nulo ',as.character(i)))
    errores <- c(errores1,i)
    i<-i+1
  }else{
    if('try-error' %in% class(succes)){
      print(paste('Error en ',as.character(i)))
      Sys.sleep(60)
    }else{
      print(score)
      score1[i] <- score
      write.csv(data.frame(ID=1:length(score1),score1),'outputs/WALKSCORE/2/score1.csv')
      Sys.sleep(1)
      i <- i+1
    }
  }
}

############### DETERMINAR EL PROMEDIO

# TODO MENOS SAN BERNARDO
score0 <- read.csv('outputs/WALKSCORE/1/score0.csv')$score0
score1 <- read.csv('outputs/WALKSCORE/1/score1.csv')$score1
MANZ_SANTIAGO_point$score0 <- score0
MANZ_SANTIAGO_point$score1 <- score1

X          <- select(as.data.frame(MANZ_SANTIAGO_point),score0,score1,MANZENT)
geocodigo  <- subset(SANTIAGO,COMUNA != 13401)$GEOCODIGO
walkscore0 <- rep(NA,length(geocodigo))
walkscore1 <- rep(NA,length(geocodigo))
for(i in 1:length(geocodigo)){
   walkscore0[i] <- subset(X, substr(MANZENT,1,11) == geocodigo[i])$score0 %>% mean(na.rm=TRUE)
   walkscore1[i] <- subset(X, substr(MANZENT,1,11) == geocodigo[i])$score1 %>% mean(na.rm=TRUE)
}

a <- data.frame(geocodigo,walkscore0,walkscore1)


# SAN BERNARDO
score0 <- read.csv('outputs/WALKSCORE/2/score0.csv')$score0
score1 <- read.csv('outputs/WALKSCORE/2/score1.csv')$score1
MANZ_SANTIAGO_point2$score0 <- score0
MANZ_SANTIAGO_point2$score1 <- score1

X          <- select(as.data.frame(MANZ_SANTIAGO_point2),score0,score1,MANZENT)
geocodigo  <- subset(SANTIAGO,COMUNA == 13401)$GEOCODIGO
walkscore0 <- rep(NA,length(geocodigo))
walkscore1 <- rep(NA,length(geocodigo))
for(i in 1:length(geocodigo)){
  walkscore0[i] <- subset(X, substr(MANZENT,1,11) == geocodigo[i])$score0 %>% mean(na.rm=TRUE)
  walkscore1[i] <- subset(X, substr(MANZENT,1,11) == geocodigo[i])$score1 %>% mean(na.rm=TRUE)
}

b <- data.frame(geocodigo,walkscore0,walkscore1)
c <- rbind(a,b)
SANTIAGO <- merge(SANTIAGO,c,by.x="GEOCODIGO",by.y="geocodigo",all.x=TRUE)
st_write(obj = SANTIAGO  , dsn="outputs/WALKSCORE", layer="SANTIAGO_WS.shp"  , driver="ESRI Shapefile",delete_layer = TRUE)
