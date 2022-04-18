options(scipen=999)
library(DBI)
library(dbplyr)
library(dplyr)
library(DescTools)
library(RSQLite)
library(Rfast)

COM_AMC  <- c(8101,8102,8103,8105,8106,8107,8108,8109,8110,8111,8112)
COM_AMV  <- c(5101,5109,5801,5804,5103)
COM_AMLS <- c(4101,4102)

con <- dbConnect(RSQLite::SQLite(), "inputs/data/MVE.db")

DB1 <- tbl(con, "PERSONAS") %>% select(REGION,PROVINCIA,COMUNA,DC,AREA,ZC_LOC,ID_ZONA_LOC,ID_PER,ID_ZL_PER,NVIV,NHOGAR,P07,P08,P09,P10,P11,P12,P15,P15A,P17,ESCOLARIDAD) %>% 
                                collect() %>% 
                                as.data.frame() %>% 
                                subset(COMUNA %in% COM_AMC | COMUNA %in% COM_AMV | COMUNA %in% COM_AMLS |PROVINCIA == 131 | COMUNA == 13201 | COMUNA == 13401) %>% 
                                arrange(ID_ZL_PER)

DB2 <- tbl(con,"VIVIENDAS") %>% collect() %>%
                                as.data.frame() %>%
                                subset(COMUNA %in% COM_AMC | COMUNA %in% COM_AMV | COMUNA %in% COM_AMLS |PROVINCIA == 131 | COMUNA == 13201 | COMUNA == 13401) %>%
                                arrange(ID_ZL_VIV)

DB3 <- tbl(con,"HOGARES") %>% collect() %>% 
                              as.data.frame() %>% 
                              subset(COMUNA %in% COM_AMC | COMUNA %in% COM_AMV | COMUNA %in% COM_AMLS |PROVINCIA == 131 | COMUNA == 13201 | COMUNA == 13401) %>% 
                              arrange(ID_ZL_HOG)

GEOCOD    <- unique(DB1$ID_ZL_PER)

codhog <- function(x,ID_ZL_PER,NVIV,NHOGAR){
  m<-dim(x)[1]
  out<-rep(NA,m)
  for(i in 1:m){
    if(nchar(NVIV[i]) == 1){
      nviv <- paste0('000',NVIV[i])
    }else{
      if(nchar(NVIV[i]) == 2){
        nviv <- paste0('00',NVIV[i])
      }else{
        if(nchar(NVIV[i]) == 3){
          nviv <- paste0('0',NVIV[i])
        }
      }
    }
    
    if(nchar(NHOGAR[i]) == 1){
      nhog <- paste0('0',NHOGAR[i])
    }
    out[i] <- paste0(ID_ZL_PER[i],NVIV[i],NHOGAR[i])
  }
  return(out)
}

A1        <- select(DB1,ID_ZL_PER,NVIV,NHOGAR,P07,P08,P09,P10,P11,P12,P15,P15A,P17,ESCOLARIDAD)
A1$CODHOG <- codhog(A1,A1$ID_ZL_PER,A1$NVIV,A1$NHOGAR) %>% as.numeric()

A2        <- select(DB2,ID_ZL_VIV,ID_VIV,P01,P02,P03A,P03B,P03C,P04,P05,CANT_HOG,CANT_PER)

A3        <- select(DB3,ID_ZL_HOG,NVIV,NHOGAR,TIPO_HOGAR)
A3$CODHOG <- codhog(A3,A3$ID_ZL_HOG,A3$NVIV,A3$NHOGAR)  %>% as.numeric()

A2[which(A2$P04 == 0),]$P04                    <- 1
A2[which(A2$P04  == 98 | A2$P04  == 99),]$P04  <- NA
A2[which(A2$P03A == 98 | A2$P03A == 99),]$P03A <- NA
A2[which(A2$P03B == 98 | A2$P03B == 99),]$P03B <- NA
A2[which(A2$P03C == 98 | A2$P03C == 99),]$P03C <- NA
A2[which(A2$CANT_PER == 98),]$CANT_PER         <- NA
A2[which(A2$CANT_HOG == 98),]$CANT_HOG         <- NA

dbDisconnect(con)
rm(list=c('DB1','DB2','DB3'))

#######################################
#### Porcentaje de adultos mayores ####
#######################################

ADM    <- rep(0,length(GEOCOD))
TOT1   <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B        <- subset(A1,ID_ZL_PER == GEOCOD[i])
  count_ad <- 0
  tot1     <- 0
  for(j in 1:dim(B)[1]){
    if(B$P09[j] >= 65){count_ad <- count_ad + 1}
    tot1   <- tot1 + 1
  }
  ADM[i]   <- count_ad
  TOT1[i]  <- tot1
}

###################################################################
#### Porcentaje jefes de hogar con ensenanza media incompleta #####
###################################################################

PJHSEM <- rep(0,length(GEOCOD))
TOT2   <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B         <- subset(A1,ID_ZL_PER == GEOCOD[i] & P07 == 1)
  count_esc <- 0
  tot2      <- 1 # parte en 1 para que a posterior no se indetermine
  j <- 1
  while(j < dim(B)[1]){
    if(B$P15[j] < 7){count_esc <- count_esc + 1}
    tot2 <- tot2 + 1
    j <- j+1
  }
  PJHSEM[i] <- count_esc
  TOT2[i]   <- tot2
}

###############################
##### TASA DE INMIGRACION ##### 
###############################

# Definida como la poblacion que llego a la zona desde otra comuna (ojo: no de otra zona) o pais

EEC        <- rep(0,length(GEOCOD))
EOCOP      <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B        <- subset(A1,ID_ZL_PER == GEOCOD[i])
  
  eec      <- 0 # en esta comuna
  eocop    <- 0 # otra comuna o país
  for(j in 1:dim(B)[1]){
    if(B$P11[j] == 2){eec <- eec + 1}
    if(B$P11[j] %in% c(3,4,5,6,7)){eocop <- eocop + 1}
  }
  EEC[i]   <- eec
  EOCOP[i] <- eocop
}

#######################################
#### Porcentaje de empleo por zona ####
#######################################

EMP   <- rep(0,length(GEOCOD)) # (1 o 3) y edad > 15
MAY15 <- rep(0,length(GEOCOD)) # edad > 15
TOT3  <- rep(0,length(GEOCOD)) # total
for(i in 1:length(GEOCOD)){
  B        <- subset(A1,ID_ZL_PER == GEOCOD[i])
  emp      <- 0
  may15    <- 0
  tot3     <- 0
  for(j in 1:dim(B)[1]){
    if((B$P17[j] == 1 | B$P17[j] == 3) & (B$P09[j]>15)){
      emp <- emp + 1
    }
    if(B$P09[j] > 15){
      may15 <- may15 + 1
    }
    tot3 <- tot3 +1
  }
  EMP[i]   <- emp
  MAY15[i] <- may15
  TOT3[i]  <- tot3
}

######################
#### Hacinamiento ####
######################

HAC_CERO    <- rep(0,length(GEOCOD))
HAC_MEDIO   <- rep(0,length(GEOCOD))
HAC_ALTO    <- rep(0,length(GEOCOD))
HAC_CRITICO <- rep(0,length(GEOCOD))
TOT_VIV1    <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B           <- subset(A2,ID_ZL_VIV == GEOCOD[i] & P01 < 8 & P02 ==1) # Vivienda no colectiva y con moradores presentes
  B           <- subset(B, !is.na(B$P04) &!is.na(B$P05))
  sin_hac     <- 0
  hac_medio   <- 0
  hac_critico <- 0
  hac_alto    <- 0
  tot_valido  <- 1 # se parte en 1 para no indeterminar a posterior
  j <- 1
  while(j < dim(B)[1]){
    tot_valido <- tot_valido + 1
    if(B$CANT_PER[j]/B$P04[j] <2.5){sin_hac <- sin_hac + 1}
    if(B$CANT_PER[j]/B$P04[j] >2.5 & B$CANT_PER[j]/B$P04[j]< 3.5){hac_medio <- hac_medio + 1}
    if(B$CANT_PER[j]/B$P04[j] >3.5 & B$CANT_PER[j]/B$P04[j]<   5){hac_alto <- hac_alto + 1}
    if(B$CANT_PER[j]/B$P04[j] >  5){hac_critico <- hac_critico + 1}
    j <- j +1
  }
  HAC_CERO[i]    <- sin_hac
  HAC_MEDIO[i]   <- hac_medio
  HAC_ALTO[i]    <- hac_alto
  HAC_CRITICO[i] <- hac_critico
  TOT_VIV1[i]    <- tot_valido
}

######################
#### Allegamiento ####
######################

# Cantidad de hogares por vivienda

VIV_ALLE <- rep(0,length(GEOCOD))
TOT_VIV2 <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B        <- subset(A2,ID_ZL_VIV == GEOCOD[i] & P01[i] < 8 & P02 == 1) # Vivienda no colectiva y con moradores presentes
  B        <- subset(B, !is.na(B$CANT_HOG))
  viv_alle <- 0
  tot_viv2 <- 1 # se parte en 1 para no indeterminar a posterior
  j <- 1
  while(j < dim(B)[1]){
    if(B$CANT_HOG[j]>1){viv_alle <- viv_alle + 1}
    tot_viv2 <- tot_viv2 + 1
    j <- j +1
  }
  VIV_ALLE[i] <- viv_alle
  TOT_VIV2[i] <- tot_viv2
}

#####################################
#### Materialidad de la vivienda ####
#####################################

# 0: Aceptable; 1: Recuperable; 2: Irrecuperable

B <- subset(A2,P01 < 8 & P02 == 1)

MURO  <- rep(0,dim(B)[1])
TECHO <- rep(0,dim(B)[1])
PISO  <- rep(0,dim(B)[1])

for(i in 1:dim(B)[1]){
  # Respecto al muro
  if(B$P03A[i] %in% c(1,2,3)){MURO[i]  <- 0}
  if(B$P03A[i] %in% c(4,5)){MURO[i]    <- 1}
  if(B$P03A[i] %in% c(6)){MURO[i]      <- 2}
  # Respecto al techo
  if(B$P03B[i] %in% c(1,2,3)){TECHO[i] <- 0}
  if(B$P03B[i] %in% c(4,5)){TECHO[i]   <- 1}
  if(B$P03B[i] %in% c(6,7)){TECHO[i]   <- 2}
  # Respecto a piso
  if(B$P03C[i] %in% c(1)){PISO[i]      <- 0}
  if(B$P03C[i] %in% c(2,3)){PISO[i]    <- 1}
  if(B$P03C[i] %in% c(4,5)){PISO[i]    <- 2}
}

IMV <- rep(NA,dim(B)[1])
for(i in 1:dim(B)[1]){
  if(MURO[i]== 0 &  TECHO[i] == 0 & PISO[i] == 0){IMV[i]  <- 0}
  if(MURO[i]== 1 & (TECHO[i] == 0 | PISO[i] == 0)){IMV[i] <- 1}
  if(MURO[i]== 2 |  TECHO[i] == 2 | PISO[i] == 2){IMV[i]  <- 2}
  
}

B$IMV    <- IMV
PIMVZ    <- rep(0,length(GEOCOD)) # Porcentaje Indice Materialidad de la Vivienda Zonal
TOT_VIV3 <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  C        <- subset(B,ID_ZL_VIV == GEOCOD[i])
  pimvz    <- 0
  tot_viv3 <- 1 #para no indeterminar
  j <- 1
  while(j < dim(C)[1]){
    if(C$IMV[j] %in% c(1,2)){pimvz <- pimvz + 1}
    tot_viv3 <- tot_viv3 + 1
    j <- j+1
  }
  PIMVZ[i]    <- pimvz
  TOT_VIV3[i] <- tot_viv3
}

#########################
#### CARACTERIZACION ####
#########################

# Porcentaje de adultos mayores que trabaja y porcentaje que está jubilado

PAMQTRA <- rep(0,length(GEOCOD))
PAMJUBI <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B        <- subset(A1,ID_ZL_PER == GEOCOD[i])
  count_ad <- 0
  trab_ad  <- 0
  jubi_ad  <- 0
  j <- 1
  while(j < dim(B)[1]){
    if(B$P09[j] >= 65){count_ad <- count_ad + 1}
    if(B$P09[j] >= 65 & (B$P17[j] == 1 | B$P17[j] == 3)){trab_ad <- trab_ad + 1}
    if(B$P09[j] >= 65 & (B$P17[j] == 7)){jubi_ad <- jubi_ad + 1}
    j <- j + 1
  }
  PAMQTRA[i] <- trab_ad/count_ad # Porcentaje de adultos mayores que trabaja
  PAMJUBI[i] <- jubi_ad/count_ad # Porcentaje de adultos mayores que jubilado
}

# Numero de casas y departamentos

NCASA <- rep(0,length(GEOCOD))
NDEPA <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  A <- subset(A2,ID_ZL_VIV==GEOCOD[i])
  j <- 1
  Ncasa <- 0
  Ndepa <- 0
  while(j < dim(A)[1]){
    if(A$P01[j]==1){Ncasa <- Ncasa + 1}
    if(A$P01[j]==2){Ndepa <- Ndepa + 1}
    j<-j+1
  }
  NCASA[i] <- Ncasa
  NDEPA[i] <- Ndepa
}

################################
##### Composición del hogar ####
################################

UNI     <- rep(0,length(GEOCOD))
MONO    <- rep(0,length(GEOCOD))
BI_SHIJ <- rep(0,length(GEOCOD))
BI_CHIJ <- rep(0,length(GEOCOD))
COMPUES <- rep(0,length(GEOCOD))
EXTENSO <- rep(0,length(GEOCOD))
SIN_NUC <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  B       <- subset(A3,ID_ZL_HOG == GEOCOD[i])
  uni     <- 0
  mono    <- 0
  bi_shij <- 0
  bi_chij <- 0
  compues <- 0
  extenso <- 0
  sin_nuc <- 0
  j <- 1
  while(j < dim(B)[1]){
    if(B$TIPO_HOGAR[j] == 1){uni     <- uni +1}
    if(B$TIPO_HOGAR[j] == 2){mono    <- mono    + 1}
    if(B$TIPO_HOGAR[j] == 3){bi_shij <- bi_shij + 1}
    if(B$TIPO_HOGAR[j] == 4){bi_chij <- bi_chij + 1}
    if(B$TIPO_HOGAR[j] == 5){compues <- compues + 1}
    if(B$TIPO_HOGAR[j] == 6){extenso <- extenso + 1}
    if(B$TIPO_HOGAR[j] == 7){sin_nuc <- sin_nuc + 1}
    j   <- j + 1
  }
  UNI[i]     <- uni
  MONO[i]    <- mono
  BI_SHIJ[i] <- bi_shij
  BI_CHIJ[i] <- bi_chij
  COMPUES[i] <- compues
  EXTENSO[i] <- extenso
  SIN_NUC[i] <- sin_nuc
}

#######################################################
#### Porcentaje de adultos mayores que viven solos ####
#######################################################

A11<-A1[c("ID_ZL_PER","NVIV","NHOGAR","CODHOG","P09")]
AMQVS    <- rep(0,length(GEOCOD))
COUNT_AD <- rep(0,length(GEOCOD))
for(i in 1:length(GEOCOD)){
  porc <- round((i/length(GEOCOD))*100,2)
  print(porc)
  B        <- subset(A11,ID_ZL_PER == GEOCOD[i])
  C        <- subset(A3,ID_ZL_HOG  == GEOCOD[i])
  count_ad <- 0
  amqvs    <- 0
  j<-1
  while(j < dim(B)[1]){
    if(B$P09[j] >= 65){
      count_ad <- count_ad + 1
      a <- C$TIPO_HOGAR[binary_search(C$CODHOG,B$CODHOG[j],index=TRUE)]
      if(!is.na(a)){
        if(a == 2){
          amqvs <- amqvs + 1 
        }
      }
    }
    j<-j+1
  }
  AMQVS[i] <- amqvs
  COUNT_AD[i] <- count_ad
}

#################################
#### FORMACION BASE DE TABLA ####
#################################

datacenso <- data.frame(GEOCOD, PORC_ADM = (ADM/TOT1)*100,
                        PJHSEM   = (PJHSEM/TOT2)*100, 
                        TIM      = (EOCOP/EEC)*100, 
                        EMP      = (EMP/MAY15)*100, 
                        HAC      = ((HAC_CRITICO+HAC_ALTO+HAC_MEDIO)/TOT_VIV1)*100,
                        ALLE     = (VIV_ALLE/TOT_VIV2)*100,
                        PIMVZ    = (PIMVZ/TOT_VIV3)*100,
                        CAMAY    = TOT1,
                        NCASA    = NCASA,
                        NDEPA    = NDEPA,
                        PAMQTRA  = PAMQTRA*100,
                        PAMJUBI  = PAMJUBI*100,
                        UNI      = UNI,
                        MONO     = MONO,
                        BI_SHIJ  = BI_SHIJ,
                        BI_CHIJ  = BI_CHIJ,
                        COMPUES  = COMPUES,
                        EXTENSO  = EXTENSO,
                        SIN_NUC  = SIN_NUC,
                        PAMQVS   = (AMQVS/COUNT_AD)*100,
                        AMQVS    = AMQVS,
                        COUNT_AD = COUNT_AD)
datacenso[datacenso$TIM == Inf,] <- 0
#########################
#### Espacializacion ####
#########################

library(sf)
library(rgdal)

COM_AMC   <- c(8101,8102,8103,8105,8106,8107,8108,8109,8110,8111,8112)
COM_AMV   <- c(5101,5109,5801,5804,5103)
COM_AMLS  <- c(4101,4102)

RMET <- st_read(dsn="inputs/geodata/R13/ZONA_C17.shp")
RBIO <- st_read(dsn="inputs/geodata/R08/ZONA_C17.shp")
RCOQ <- st_read(dsn="inputs/geodata/R04/ZONA_C17.shp")
RVAL <- st_read(dsn="inputs/geodata/R05/ZONA_C17.shp")

RMET <- st_transform(RMET,4989) #SIRGAS2000 es el datum original
RBIO <- st_transform(RBIO,4989) #SIRGAS2000 es el datum original
RCOQ <- st_transform(RCOQ,4989) #SIRGAS2000 es el datum original
RVAL <- st_transform(RVAL,4989) #SIRGAS2000 es el datum original

GEODATABASE <- rbind(RMET,RBIO,RCOQ,RVAL) #Juntar los pol?gonos con iguales atributos
GEODATABASE <- st_transform(GEODATABASE,32719) # lo transformamos a WGS 84 / UTM zone 19S
GEODATABASE <- subset(GEODATABASE,COMUNA %in% COM_AMC | COMUNA %in% COM_AMV | COMUNA %in% COM_AMLS |PROVINCIA == 131 | COMUNA == 13201 | COMUNA == 13401)
GEODATABASE <- merge(GEODATABASE,datacenso, by.x="GEOCODIGO",by.y="GEOCOD")

st_write(obj=GEODATABASE, dsn="outputs/GEODATABASE", layer="adultomayor.shp", driver="ESRI Shapefile",delete_layer = TRUE)
