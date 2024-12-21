# Maria Diaz Ortiz 
rm(list = ls()); gc(reset = T)

# Library -----------------------------------------------------------------
library(devtools)
library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)

# Data -------------------------------------------------------------------
file <- "rawdat/Seguimiento Imarsis 2016-2019.xlsx"
outFile    <- "cin/STANDARIZED_IMARSIS_2016-2019.csv"

data <- read_xlsx(path = file, sheet = 1, col_types = NULL)
names(data) <- toupper(names(data))
base <- as.data.frame(data)

marks_anc <- seq(5.0, 20, .5)

##1. latitud --------
base[,"LONGITUD"]             <- -1*abs(base[, "LONGITUD"])
colnames(base)[colnames(base) == "LONGITUD"] <- "LONG"
##2. longitud --------
base[,"LATITUD"]       <- -1*abs(base[, "LATITUD"])
colnames(base)[colnames(base) == "LATITUD"] <- "LAT"
##3. Especie -------
base[,"ESPECIE"]      <- "Anchoveta, anchoveta peruana, peladilla"
##4. Captura -------
base[,"CAPTURA"]      <- as.numeric(data$CAPTURA)
colnames(base)[colnames(base) == "CAPTURA"] <- "CAPTURA(t)"
base$`CAPTURA(t)`  <- base$`CAPTURA(t)`
##5. Region --------
base$PUERTO <- toupper(base$PUERTO)
base[!is.na(base$PUERTO) & base$PUERTO == "CHIMBOTE", "REGION" ] <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "CHICAMA", "REGION" ] <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "MALABRIGO", "REGION" ] <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "SAMANCO", "REGION" ] <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "SUPE" , "REGION"] <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "VEGUETA", "REGION" ] <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "CHANCAY", "REGION" ] <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "HUACHO", "REGION" ] <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "PISCO", "REGION" ] <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "CALLAO", "REGION" ] <- "CALLAO"
base[!is.na(base$PUERTO) & base$PUERTO == "TAMBO DE MORA", "REGION" ] <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "PARACHIQUE", "REGION" ] <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "SALAVERRY", "REGION" ] <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "PAITA", "REGION" ] <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "ATICO", "REGION" ] <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "PLANCHADA", "REGION" ] <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "ILO", "REGION" ] <- "MOQUEGUA"
base[!is.na(base$PUERTO) & base$PUERTO == "MOLLENDO", "REGION" ] <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "PACASMAYO" , "REGION"] <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "BAYOVAR", "REGION" ] <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "COISHCO", "REGION" ] <- "ANCASH"

##6. Fabrica ------
base[,"FABRICA"]      <- as.character(NA)
##7 Embarcacion ----
colnames(base)[colnames(base) == "EMBARCACIONES"] <- "EMBARCACION"
base[,"EMBARCACION"] <- toupper(base$EMBARCACION)
##8 Matricula ----
base$MATRICULA <- gsub("[^a-zA-Z0-9-]", "", base$MATRICULA)
base$MATRICULA <- gsub("([a-zA-Z]+)([0-9]+)", "\\1-\\2", base$MATRICULA)

##9 Capacidad de bodega ----
##10 Tipo de flota ----
colnames(base)[colnames(base) =="TIPO"]  <- "TIPO DE FLOTA"
base$`TIPO DE FLOTA`[base$`TIPO DE FLOTA` == "ind mad"] <- "MADERA"
base$`TIPO DE FLOTA`[base$`TIPO DE FLOTA` == "Ind Mad"] <- "MADERA"
base$`TIPO DE FLOTA`[base$`TIPO DE FLOTA` == "Art"] <- "ARTESANAL"
base$`TIPO DE FLOTA`[base$`TIPO DE FLOTA` == "Ind"]   <- "ACERO NAVAL"
##11 Puerto ----
base$PUERTO[base$PUERTO == "PACASMAYO" ] <- "CHICAMA"
base$PUERTO[base$PUERTO == "BAYOVAR" ] <- "PARACHIQUE"
##12 Area Isoparalitoral ----
##13 Fecha ----
base[,"DIA"]             <- paste(base$DIA, base$MES, base$YEAR, sep = "/")
base$DIA <- as.Date(base$DIA)
#Selección de columnas
colnames(base) <- trimws(colnames(base))
base$'20' <- NA
base2 <-  base[, c("LONG", "LAT", "ESPECIE", 'CAPTURA(t)', "REGION", 
             "FABRICA", "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
             "PUERTO", "AREA", "DIA", '5', '5.5', '6', '6.5',  '7', '7.5', '8', '8.5', '9',
             '9.5','10', '10.5', '11', '11.5', '12', '12.5', '13', '13.5','14', '14.5', '15',
            '15.5', '16', '16.5', '17', '17.5', '18', '18.5', '19', '19.5', '20')]

base2$'19' <- as.numeric(base2$'19')
base2$'19.5' <- as.numeric(base2$'19.5')
base2$'20' <- as.numeric(base2$'20')

write.csv(base2, outFile, row.names = F)

