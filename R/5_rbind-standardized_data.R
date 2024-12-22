rm(list = ls()); gc(reset = T)
# LIBRARY -----------------------------------------------------------------------------------------------------------------
require(lubridate); require(data.table)
require(ggplot2); require(cowplot)
require(ggmap); require(rnaturalearth)
require(tidyverse); require(dplyr)
require(r4fish); require(readxl)
library(modeest); require(fenix)

# SOURCE ----------------------------------------------------------------------------
USB <- "I:/"
dir_cin <- file.path(USB, "database/cin/")
dir_raw <- file.path(USB, "database/rawdat/")

outFile   <- file.path(dir_cin, "DATA_JUVENILES_1972_2024.csv")

fileone  <- file.path(dir_cin, "STANDARIZED_DATA_1972_2014.csv")
filetwo  <- file.path(dir_cin, "STANDARIZED_DATA_TOTAL_2005_2024.csv")
filethree <- file.path(dir_cin, "STANDARIZED_IMARSIS_2016-2019.csv")
filefour  <- file.path(dir_cin, "STANDARIZED_IMARSIS_2018-2021.csv")

dataone   <- read.csv(fileone,    encoding = "latin1")
datatwo   <- read.csv(filetwo,    encoding = "latin1")
datathree <- read.csv(filethree,  encoding = "latin1")
datafour  <- read.csv(filefour,   encoding = "latin1")
areaISO   <- read_xlsx(file.path(dir_raw, "/areaISOEXCEL.xlsx"))

baseone   <- as.data.frame(dataone)
basetwo   <- as.data.frame(datatwo)
basethree <- as.data.frame(datathree)
basefour  <- as.data.frame(datafour)

names(baseone) == names(basetwo)
names(baseone) == names(basethree)
names(baseone) == names(basefour)

marks0 <- (seq(5, 20, .5))
marks <- paste0("X", marks0)

data <- rbind(baseone, basetwo, basethree, basefour)

data$DIA <- as.Date(data$DIA)
data$MES <- month(data$DIA)
data$YEAR <- year(data$DIA)

tt <- as.data.frame.table(table(data$YEAR))
plot(as.numeric(as.character(tt$Var1)), tt$Freq, type = "o", xlab = "Año", ylab = "Frecuencia", main = "Frecuencia de muestreo", col = "blue")

#PUERTOS SUMADOS --------
data$PUERTO <- toupper(trimws(data$PUERTO))
data$PUERTO <- gsub("CARQUIN", "HUACHO", data$PUERTO) 
data$PUERTO <- gsub("CPLP", "PISCO", data$PUERTO)
data$PUERTO <- gsub("CALETA LAGUNILLAS", "PISCO", data$PUERTO)
data$PUERTO <- gsub("PACASMAYO", "CHICAMA", data$PUERTO)
data$PUERTO <- gsub("CHALA", "ATICO", data$PUERTO)
data$PUERTO <- gsub("BAYOVAR", "PARACHIQUE", data$PUERTO)
data$PUERTO <- gsub("COISHCO", "CHIMBOTE", data$PUERTO)
data$PUERTO <- gsub("PUERTO RICO", "PARACHIQUE", data$PUERTO)
data$PUERTO <- gsub("ACAPULCO", "PAITA", data$PUERTO)
data$PUERTO <- gsub("HUANCHACO", "CHICAMA", data$PUERTO)
data$PUERTO <- gsub("LOS CHIMU", "SAMANCO", data$PUERTO)
data$PUERTO <- gsub("MORRO SAMA", "ILO", data$PUERTO)
data$PUERTO <- gsub("VILA VILA", "ILO", data$PUERTO)
data$PUERTO <- gsub("SAN JOSE", "PIMENTEL", data$PUERTO)
data$PUERTO <- gsub("SANTA", "CHIMBOTE", data$PUERTO)
data$PUERTO <- gsub("TALARA", "PAITA", data$PUERTO)
data$PUERTO <- gsub("YACILA", "PAITA", data$PUERTO)
data$PUERTO <- gsub("MANCORA", "PAITA", data$PUERTO)
data$PUERTO <- gsub("ANCON", "CHANCAY", data$PUERTO)
data$PUERTO[data$PUERTO == "PLANCHADA"] <- "LA PLANCHADA"
data$PUERTO[data$PUERTO == "QUILCA"] <- "MOLLENDO"
data$PUERTO[data$PUERTO == "ILO(GRAU)"] <- "ILO"
data$PUERTO[data$PUERTO == "SAMANCOS"] <- "SAMANCO"
data$PUERTO[data$PUERTO == "LAGUNILLAS"] <- "PISCO"
data$PUERTO[data$PUERTO == "LAGUNA GRANDE"] <- "PISCO"

table(data$PUERTO)[order(table(data$PUERTO), decreasing = T)]

#LIMPIEZA PARA IDONEIDAD DEL MUESTREO-------------------------------------------
nobs   <- nrow(data)
id.spt <- is.na(data$AREA) & is.na(data$LONG) & is.na(data$LAT)
data   <- data[-id.spt, ]
data$LAT[data$LAT == 0]   <- NA
data$LONG[data$LONG == 0] <- NA
data$CB[data$CB <= 0]     <- NA

data$AREA    <- as.numeric(data$AREA)
areaISO$AREA <- as.numeric(areaISO$AREA)
idx <- which(areaISO$AREA %in% data$AREA)
data$LONG <- ifelse(is.na(data$LONG), areaISO$LONG[idx], data$LONG)
data$LAT <- ifelse(is.na(data$LAT), areaISO$LAT[idx], data$LAT)

data$dc <- estima_dc2(lon = data$LONG, lat = data$LAT, polygon = PERU_SP)
data <- data[data$dc <= 160, ]
nobs2  <- nrow(data)

tt <- as.data.frame.table(table(data$YEAR))
plot(as.numeric(as.character(tt$Var1)), tt$Freq, type = "o", xlab = "Año", ylab = "Frecuencia", main = "Frecuencia de muestreo", col = "blue")

data$nAnc <- rowSums(data[, as.character(marks)], na.rm = T)
data$nAnc <- round(data$nAnc, 0)
data <- data[data$nAnc >= 120, ]
nobs3 <- nrow(data)
tt <- as.data.frame.table(table(data$YEAR))
lines(as.numeric(as.character(tt$Var1)), tt$Freq, type = "o", xlab = "Año", ylab = "Frecuencia", main = "Frecuencia de muestreo", col = "red")

data$CAPTURA..t. <- as.numeric(data$CAPTURA..t.)
data <- data[which(!is.na(data$CAPTURA..t.) | data$CAPTURA..t. != 0), ]
nobs4 <- nrow(data)

nobs; (nobs2/nobs)*100; (nobs3/nobs)*100; (nobs4/nobs)*100

moda <- as.matrix.data.frame(data[, as.character(marks)])
dimnames(moda) <- NULL
data$moda <- marks0[apply(moda, 1, function(x) which.max(as.vector(x)) )]

#JUVENILES--------------------------------------------------------------# captura de juv
a = 0.0036; b = 3.2380

peso <- a*(marks0^b)
freqsim_num <- data[, as.character(marks)]
freqsim_pes <- sweep(freqsim_num, 2, peso, "*")
peso_m      <- rowSums(freqsim_pes, na.rm = T)
fx          <- (data$CAPTURA..t.)*1e6/peso_m
freqsim_num_pon <- sweep(freqsim_num, 1, fx, "*")
data$CAPTURA..n. <- rowSums(freqsim_num_pon, na.rm = TRUE)  
data$CAPTURA..n. <- round(data$CAPTURA..n., 1)  
data$juv_n <- rowSums(freqsim_num_pon[, marks0 < 12], na.rm = TRUE)  
data$juv_n <- round(data$juv_n, 2)
temp_peso <- freqsim_num_pon[, marks0 < 12]  
temp_peso <- sweep(temp_peso, 2, peso[marks0 < 12], "*")
data$juv_p  <- rowSums(temp_peso, na.rm = TRUE)/1E6
data$juv_p <- round(data$juv_p, 2)

tmp <- data[, as.character(marks)]
tmp[which(tmp == 0, arr.ind = T)] <- NA
data[, as.character(marks)] <- tmp

ar  <- area_isoparalitoral(dist_costa = data$dc, 
                            latitude = data$LAT)

data$AREA     <- ar[,3]
data <- data[!duplicated(data), ]
data$Nid <- 1:nrow(data)

# duplicados
data$key <- paste(data$DIA, data$CAPTURA..n., data$nAnc,
                   data$moda,  data$juv_p, sep = "-")

key <- names(table(data$key)[table(data$key) > 1])

id_ommit <- NULL
for (t in 1:length(key)) {
  print(t)
  tmp <- data[data$key == key[t],]
  idna <- which.min(colSums(apply(tmp, 1, is.na)))
  id_ommit <- c(id_ommit, tmp$Nid[-idna])
}


data  <- data[-id_ommit, ]
nobs5 <- nrow(data)
names(data)

data <- data[, 1:52]
names(data)[4]  <- "CAPTURA_P"
names(data)[48] <- "MUESTREO_N"
names(data)[50] <- "CAPTURA_N"
names(data)[10] <- "TIPO_FLOTA"

names(data) <- toupper(names(data))
data        <- data[c(1:13,46, 45, 47:52,14:44)]
data        <- data[order(data$YEAR, data$MES, data$PUERTO), ]
names(data)

data$YEAR_SB <- data$YEAR
data$YEAR_SB[data$MONTH %in% 1:3] <- data$YEAR[data$MONTH %in% 1:3] - 1

data$SEMESTRE <- 1
data$SEMESTRE[data$MES %in% c(1:3,10:12)] <- 2
data$SEMB <- paste0("Sem. ",data$YEAR_SB,"-",data$SEMESTRE)

write.csv(data, outFile, row.names = F)
(outFile)
