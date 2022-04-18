library(sf)
library(rgdal)
library(factoextra)
library(ggplot2)
library(cowplot)
library(openxlsx)

database    <- st_read(dsn="outputs/GEODATABASE/adultomayor.shp")

##################
#### SANTIAGO ####
##################

SANTIAGO     <- subset(database,REGION=='13')
data.scaled  <- scale(x = as.data.frame(SANTIAGO)[c("PORC_ADM","PJHSEM","TIM","EMP","HAC","PIMVZ")],center=TRUE,scale=TRUE)
res.dist     <- dist(x = data.scaled, method = "euclidean")
res.hc       <- hclust(d=res.dist, method = "ward.D")
fviz_dend(x = res.hc, k = 8, k_colors = "jco",show_labels = FALSE)

SANTIAGO$cluster2 <-  factor(cutree(res.hc, k=2))
SANTIAGO$cluster3 <-  factor(cutree(res.hc, k=3))
SANTIAGO$cluster4 <-  factor(cutree(res.hc, k=4))
SANTIAGO$cluster5 <-  factor(cutree(res.hc, k=5))
SANTIAGO$cluster6 <-  factor(cutree(res.hc, k=6))
SANTIAGO$cluster7 <-  factor(cutree(res.hc, k=7))
SANTIAGO$cluster8 <-  factor(cutree(res.hc, k=8))

# ------ CLUSTER k = 8, {4,5,7}(a) {Grupo avejentado},{1,2,3,6,8}(b) {Grupo menos avejentado} -------- (I)  #

k8I <- rep(NA,dim(SANTIAGO)[1])
for(i in 1:dim(SANTIAGO)[1]){
  if(SANTIAGO$cluster8[i] %in% c(4,5,7)){
    k8I[i] <- "a"
  }else{
    k8I[i] <- "b"
  }
}

SANTIAGO$k8I      <-  k8I

# ------ CLUSTER k = 8, {5}(a) {Avejentado y acomodado}, {4,7}(b) {avejentado y más empobrecido}, otros (c)  -------- (II)  #

k8II <- rep(NA,dim(SANTIAGO)[1])
for(i in 1:dim(SANTIAGO)[1]){
  if(SANTIAGO$cluster8[i] %in% c(5)){
    k8II[i] <- "a"
  }else{
    if(SANTIAGO$cluster8[i] %in% c(4,7)){
      k8II[i] <- "b"
    }else{
      k8II[i] <- "c"
    }
  }
}

SANTIAGO$k8II      <-  k8II


### Donde estan los mas avejentados en k8II ???

quantile(subset(SANTIAGO,k8II=='a')$PORC_ADM)
quantile(subset(SANTIAGO,k8II=='b')$PORC_ADM)
quantile(subset(SANTIAGO,k8II=='c')$PORC_ADM)


k8III <- rep('c',dim(SANTIAGO)[1])
for(i in 1:dim(SANTIAGO)[1]){
  
  # Clasificacion para 'a'
  
  if(SANTIAGO$k8II[i] == 'a' & SANTIAGO$PORC_ADM[i] <= 16){k8III[i] <- 'a1'}
  if(SANTIAGO$k8II[i] == 'a' & SANTIAGO$PORC_ADM[i] > 16 & SANTIAGO$PORC_ADM[i] <= 20){k8III[i] <- 'a2'}
  if(SANTIAGO$k8II[i] == 'a' & SANTIAGO$PORC_ADM[i] > 20){k8III[i] <- 'a3'}
  
  # Clasificacion para 'b'
  
  if(SANTIAGO$k8II[i] == 'b' & SANTIAGO$PORC_ADM[i] <= 12.5){k8III[i] <- 'b1'}
  if(SANTIAGO$k8II[i] == 'b' & SANTIAGO$PORC_ADM[i] > 12.5 & SANTIAGO$PORC_ADM[i] <= 16.15){k8III[i] <- 'b2'}
  if(SANTIAGO$k8II[i] == 'b' & SANTIAGO$PORC_ADM[i] > 16.15){k8III[i] <- 'b3'}
}

SANTIAGO$k8III      <-  k8III


st_write(obj=SANTIAGO, dsn="outputs/CLUSTER", layer="SANTIAGO_CLUSTER.shp", driver="ESRI Shapefile",delete_layer = TRUE)




####################
#### CONCEPCION ####
####################

CONCEPCION   <- subset(database,REGION=='8')
data.scaled  <- scale(x = as.data.frame(CONCEPCION)[c("PORC_ADM","PJHSEM","TIM","EMP","HAC","PIMVZ")],center=TRUE,scale=TRUE)
res.dist     <- dist(x = data.scaled, method = "euclidean")
res.hc       <- hclust(d=res.dist, method = "ward.D")
plot(res.hc)

CONCEPCION$cluster2 <-  factor(cutree(res.hc, k=2))
CONCEPCION$cluster3 <-  factor(cutree(res.hc, k=3))
CONCEPCION$cluster4 <-  factor(cutree(res.hc, k=4))
CONCEPCION$cluster5 <-  factor(cutree(res.hc, k=5))
CONCEPCION$cluster6 <-  factor(cutree(res.hc, k=6))

st_write(obj=CONCEPCION, dsn="outputs/CLUSTER", layer="CONCEPCION_CLUSTER.shp", driver="ESRI Shapefile",delete_layer = TRUE)

####################
#### VALPARAISO ####
####################

VALPARAISO   <- subset(database,REGION=='5')
data.scaled  <- scale(x = as.data.frame(VALPARAISO)[c("PORC_ADM","PJHSEM","TIM","EMP","HAC","PIMVZ")],center=TRUE,scale=TRUE)
res.dist     <- dist(x = data.scaled, method = "euclidean")
res.hc       <- hclust(d=res.dist, method = "ward.D")
plot(res.hc)

VALPARAISO$cluster2 <-  factor(cutree(res.hc, k=2))
VALPARAISO$cluster3 <-  factor(cutree(res.hc, k=3))
VALPARAISO$cluster4 <-  factor(cutree(res.hc, k=4))
VALPARAISO$cluster5 <-  factor(cutree(res.hc, k=5))
VALPARAISO$cluster6 <-  factor(cutree(res.hc, k=6))

st_write(obj=VALPARAISO, dsn="outputs/CLUSTER", layer="VALPARAISO_CLUSTER.shp", driver="ESRI Shapefile",delete_layer = TRUE)

###################
#### LA SERENA ####
###################

LASERENA     <- subset(database,REGION=='4')
data.scaled  <- scale(x = as.data.frame(LASERENA)[c("PORC_ADM","PJHSEM","TIM","EMP","HAC","PIMVZ")],center=TRUE,scale=TRUE)
res.dist     <- dist(x = data.scaled, method = "euclidean")
res.hc       <- hclust(d=res.dist, method = "ward.D")
plot(res.hc)

LASERENA$cluster2 <-  factor(cutree(res.hc, k=2))
LASERENA$cluster3 <-  factor(cutree(res.hc, k=3))
LASERENA$cluster4 <-  factor(cutree(res.hc, k=4))
LASERENA$cluster5 <-  factor(cutree(res.hc, k=5))
LASERENA$cluster6 <-  factor(cutree(res.hc, k=6))

st_write(obj=LASERENA, dsn="outputs/CLUSTER", layer="LASERENA_CLUSTER.shp", driver="ESRI Shapefile",delete_layer = TRUE)
