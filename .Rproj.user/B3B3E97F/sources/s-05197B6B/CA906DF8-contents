library(sp)
library(sf)

##################
#### SANTIAGO ####
##################

SANTIAGO                <- st_read("outputs/CLUSTER/SANTIAGO_CLUSTER.shp")
ISMT_STGO               <- as.data.frame(st_read("inputs/geodata/ISMT/ISMT_SANTIAGO/ISMT_17_02.shp"))
ISMT_STGO               <- ISMT_STGO[c('GEOCODIGO','ISMT_PN_17')]
ISMT_STGO$GEOCODIGO     <- as.character(ISMT_STGO$GEOCODIGO)
SANTIAGO                <- sp::merge(SANTIAGO,ISMT_STGO,by.x="GEOCODIGO",by.y="GEOCODIGO")
colnames(SANTIAGO)[which(colnames(as.data.frame(SANTIAGO))=="ISMT_PN_17")] <- "ISMT"
st_write(obj = SANTIAGO  , dsn="outputs/ISMT/SANTIAGO", layer="SANTIAGO_ISMT.shp"  , driver="ESRI Shapefile",delete_layer = TRUE)

####################
#### CONCEPCION ####
####################

CONCEPCION              <- st_read("outputs/CLUSTER/CONCEPCION_CLUSTER.shp")
ISMT_CCP                <- as.data.frame(st_read("inputs/geodata/ISMT/ISMT_CONCEPCION/ISMT_CCP.shp"))
ISMT_CCP                <- ISMT_CCP[c('GEOCODIGO','ismtpn')]
ISMT_CCP$GEOCODIGO      <- as.character(ISMT_CCP$GEOCODIGO)
CONCEPCION              <- sp::merge(CONCEPCION,ISMT_CCP,by.x="GEOCODIGO",by.y="GEOCODIGO")
colnames(CONCEPCION)[which(colnames(as.data.frame(CONCEPCION))=='ismtpn')] <- "ISMT"
st_write(obj = CONCEPCION  , dsn="outputs/ISMT/CONCEPCION", layer="CONCEPCION_ISMT.shp"  , driver="ESRI Shapefile",delete_layer = TRUE)

####################
#### VALPARAISO ####
####################

VALPARAISO              <- st_read("outputs/CLUSTER/VALPARAISO_CLUSTER.shp")
ISMT_VALPO              <- as.data.frame(st_read("inputs/geodata/ISMT/ISMT_VALPO/ISMT_VALPO.shp"))
ISMT_VALPO              <- ISMT_VALPO[c('geocode','ISMT_mzn')]
ISMT_VALPO$geocode      <- as.character(ISMT_VALPO$geocode)
VALPARAISO              <- sp::merge(VALPARAISO,ISMT_VALPO,by.x="GEOCODIGO",by.y="geocode")
colnames(VALPARAISO)[which(colnames(as.data.frame(VALPARAISO)) == 'ISMT_mzn')] <- "ISMT"
colnames(VALPARAISO)[which(colnames(as.data.frame(VALPARAISO)) == 'geocode')] <- "GEOCODIGO"
st_write(obj = VALPARAISO  , dsn="outputs/ISMT/VALPARAISO", layer="VALPARAISO_ISMT.shp"  , driver="ESRI Shapefile",delete_layer = TRUE)

###################
#### LA SERENA ####
###################

LASERENA                <- st_read("outputs/CLUSTER/LASERENA_CLUSTER.shp")
ISMT_LASERENA           <- as.data.frame(st_read("inputs/geodata/ISMT/ISMT_COQ/ISMT_COQ.shp"))
ISMT_LASERENA           <- ISMT_LASERENA[c('GEOCODIGO','ISMT')]
ISMT_LASERENA$GEOCODIGO <- as.character(ISMT_LASERENA$GEOCODIGO)
LASERENA                <- sp::merge(LASERENA,ISMT_LASERENA,by.x="GEOCODIGO",by.y="GEOCODIGO")
st_write(obj = LASERENA  , dsn="outputs/ISMT/LA SERENA", layer="LA_SERENA_ISMT.shp"  , driver="ESRI Shapefile",delete_layer = TRUE)
