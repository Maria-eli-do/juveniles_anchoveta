# Maria Diaz Ortiz 
rm(list = ls()); gc(reset = T)

# Library -----------------------------------------------------------------
library(readxl); library(stringr); library(lubridate)
library(tidyverse); library(dplyr); library(tidyr)

# Data -------------------------------------------------------------------
USB <- "I:/"
dir_raw <- file.path(USB, "database/rawdat/")
dir_cin <- file.path(USB, "database/cin/")

cinFile <- file.path(dir_raw, "Seguimiento Imarsis 2018-2021.xlsx")
outFile  <- file.path(dir_cin, "STANDARIZED_IMARSIS_2018-2021.csv")

data <- read_xlsx(path = cinFile, sheet = 1, col_types = NULL)
names(data) <- toupper(names(data))
base <- as.data.frame(data)
marks_anc <- seq(5.0, 20, .5)

##1. latitud --------
base[,"LONG"]             <- -1*abs(base[, "LONGITUD"])

##2. longitud --------
base[,"LAT"]       <- -1*abs(base[, "LATITUD"])

##3. Especie -------
base[,"ESPECIE"]      <- "Anchoveta, anchoveta peruana, peladilla"

##4. Captura -------
base[,"CAPTURA (t)"]      <- as.numeric(abs(data$'CAPTURA ESPECIE'))

##5. Region --------
base$PUERTO <- toupper(trimws(base$PUERTO))
base$REGION <- as.character(NA)

base$REGION[ base$PUERTO == "CHIMBOTE"]   <- "ANCASH"
base$REGION[ base$PUERTO == "CHICAMA"]    <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "MALABRIGO"]  <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "SAMANCO"]    <- "ANCASH"
base$REGION[ base$PUERTO == "SUPE" ]      <- "LIMA"
base$REGION[ base$PUERTO == "VEGUETA"]    <- "LIMA"
base$REGION[ base$PUERTO == "CHANCAY"]    <- "LIMA"
base$REGION[ base$PUERTO == "HUACHO"]     <- "LIMA"
base$REGION[ base$PUERTO == "CARQUIN"]    <- "LIMA"
base$REGION[ base$PUERTO == "PACASMAYO"]  <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "SAN ANDRES"] <- "PISCO"
base$REGION[ base$PUERTO == "LAS DELICIAS"] <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "PISCO"]     <- "ICA"
base$REGION[ base$PUERTO == "CALLAO"]    <- "CALLAO"
base$REGION[ base$PUERTO == "TAMBO DE MORA"] <- "ICA"
base$REGION[ base$PUERTO == "EL CHACO"]  <- "ICA"
base$REGION[ base$PUERTO == "PARACHIQUE"] <- "PIURA"
base$REGION[ base$PUERTO == "SALAVERRY"] <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "PAITA"]     <- "PIURA"
base$REGION[ base$PUERTO == "ATICO"]     <- "AREQUIPA"
base$REGION[ base$PUERTO == "PLANCHADA"] <- "AREQUIPA"
base$REGION[ base$PUERTO == "ILO"]       <- "MOQUEGUA"
base$REGION[ base$PUERTO == "MOLLENDO"]  <- "AREQUIPA"
base$REGION[ base$PUERTO == "PACASMAYO"] <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "BAYOVAR"]   <- "PIURA"
base$REGION[ base$PUERTO == "COISHCO"]   <- "ANCASH"

##6. Fabrica ------
base[,"FABRICA"]      <- as.character(NA)

##7 Embarcacion ----
base[,"EMBARCACION"] <- toupper(trimws(base$EMBARCACIÓN))

##8 Matricula ----
base[,"MATRICULA"] <- gsub("[^a-zA-Z0-9-]", "", base$MATRICULA)
base[,"MATRICULA"]   <- toupper(trimws(base[,"MATRICULA"]))

##9 Capacidad de bodega ----
base[,"CB"]          <- as.numeric(NA)

##10 Tipo de flota ----
base$FLOTA[base$FLOTA == "INDUSTRIAL DE MADERA"]   <- "MADERA"
base$FLOTA[base$FLOTA == "MENOR ESCALA(CERCO)"]    <- "ARTESANAL"
base$FLOTA[base$FLOTA == "INDUSTRIAL DE FIERRO"]   <- "ACERO NAVAL"
base[, "TIPO DE FLOTA"] <- base$FLOTA
unique(base$FLOTA)

##11 Puerto ----
base$PUERTO[base$PUERTO == "PACASMAYO" ]    <- "CHICAMA"
base$PUERTO[base$PUERTO == "MALABRIGO" ]    <- "CHICAMA"
base$PUERTO[base$PUERTO == "BAYOVAR" ]      <- "PARACHIQUE"
base$PUERTO[base$PUERTO == "CARQUIN" ]      <- "HUACHO"
base$PUERTO[base$PUERTO == "SAN ANDRES" ]   <- "PISCO"
base$PUERTO[base$PUERTO == "EL CHACO" ]     <- "PISCO"
base$PUERTO[base$PUERTO == "LAS DELICIAS" ] <- "SALAVERRY"
unique(base$PUERTO)

##12 Area Isoparalitoral ----
base[,"AREA"] <- as.numeric(base$AREA)

##13 Fecha ----
base[,"DIA"]  <- as.Date(base$FECHA, format = "%Y-%m-%d")

#Selección de columnas

names(base[, 20:50]); df <- base[, 20:50]; 
names(df) <- seq(5, 20, .5)

base2 <-  base[, c("LONG", "LAT", "ESPECIE", 'CAPTURA (t)', "REGION", 
                   "FABRICA", "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
                   "PUERTO", "AREA", "DIA")]

base2 <- cbind(base2, df)

write.csv(base2, outFile, row.names = F)

