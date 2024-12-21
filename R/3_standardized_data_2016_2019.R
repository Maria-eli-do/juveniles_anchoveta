# Maria Diaz Ortiz 
rm(list = ls()); gc(reset = T)

# Library -----------------------------------------------------------------
library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)

# Data -------------------------------------------------------------------
USB <- "I:/"
dir_raw <- file.path(USB, "database/rawdat/")
dir_cin <- file.path(USB, "database/cin/")

cinFile <- file.path(dir_raw, "Seguimiento Imarsis 2016-2019.xlsx")
outFile  <- file.path(dir_cin, "/STANDARIZED_IMARSIS_2016-2019.csv")

base <- read_xlsx(path = cinFile, sheet = 1, col_types = NULL)
names(base) <- toupper(names(base))
base <- as.data.frame(base)

marks_anc <- seq(5.0, 20, .5)

##1. latitud --------
base[,"LONG"]      <- -1*abs(base[, "LONGITUD"])

##2. longitud --------
base[,"LAT"]       <- -1*abs(base[, "LATITUD"])

##3. Especie -------
base[,"ESPECIE"]   <- "Anchoveta, anchoveta peruana, peladilla"

##4. Captura -------
base[,"CAPTURA (t)"]      <- as.numeric(base$CAPTURA)

##5. Region --------
base$PUERTO <- toupper(base$PUERTO)
base$REGION <- NA

base$REGION[ base$PUERTO == "CHIMBOTE"]   <- "ANCASH"
base$REGION[ base$PUERTO == "CHICAMA"]    <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "MALABRIGO"]  <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "SAMANCO"]    <- "ANCASH"
base$REGION[ base$PUERTO == "SUPE"]       <- "LIMA"
base$REGION[ base$PUERTO == "VEGUETA"]    <- "LIMA"
base$REGION[ base$PUERTO == "CHANCAY"]    <- "LIMA"
base$REGION[ base$PUERTO == "HUACHO"]     <- "LIMA"
base$REGION[ base$PUERTO == "PISCO"]      <- "ICA"
base$REGION[ base$PUERTO == "CALLAO"]     <- "CALLAO"
base$REGION[ base$PUERTO == "TAMBO DE MORA"] <- "ICA"
base$REGION[ base$PUERTO == "PARACHIQUE"] <- "PIURA"
base$REGION[ base$PUERTO == "SALAVERRY"]  <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "PAITA"]      <- "PIURA"
base$REGION[ base$PUERTO == "ATICO"]      <- "AREQUIPA"
base$REGION[ base$PUERTO == "PLANCHADA"]  <- "AREQUIPA"
base$REGION[ base$PUERTO == "ILO"]        <- "MOQUEGUA"
base$REGION[ base$PUERTO == "MOLLENDO"]   <- "AREQUIPA"
base$REGION[ base$PUERTO == "PACASMAYO"]  <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "BAYOVAR"]    <- "PIURA"
base$REGION[ base$PUERTO == "COISHCO"]    <- "ANCASH"

unique(base$REGION)
base$PUERTO[is.na(base$REGION)]

##6. Fabrica ------
base[,"FABRICA"]     <- as.character(NA)

##7 Embarcacion ----
base[,"EMBARCACION"] <- toupper(trimws(base$EMBARCACIONES))

##8 Matricula ----
base[,"MATRICULA"]   <- gsub("[^a-zA-Z0-9-]", "", base$MATRICULA)
base[,"MATRICULA"]   <- toupper(trimws(base[,"MATRICULA"]))

##9 Capacidad de bodega ----
base[,"CB"]          <- as.numeric(NA)

##10 Tipo de flota ----
base$TIPO[base$TIPO == "ind mad"] <- "MADERA"
base$TIPO[base$TIPO == "Ind Mad"] <- "MADERA"
base$TIPO[base$TIPO == "Art"] <- "ARTESANAL"
base$TIPO[base$TIPO == "Ind"]   <- "ACERO NAVAL"
base[, "TIPO DE FLOTA"] <- base$TIPO
unique(base$TIPO)

##11 Puerto ----
base$PUERTO[base$PUERTO == "PACASMAYO" ] <- "CHICAMA"
base$PUERTO[base$PUERTO == "BAYOVAR" ] <- "PARACHIQUE"
base$PUERTO <- toupper(trimws(base$PUERTO))
unique(base$PUERTO)

##12 Area Isoparalitoral ----
base[,"AREA"] <- as.numeric(base$AREA)

##13 Fecha ----
base[,"DIA"]  <- as.Date(paste(base$DIA, base$MES, base$YEAR, sep = "-"), 
                         format = "%d-%m-%Y")

#Selección de columnas

names(base[, 18:47]); df <- base[, 18:47]; 
df$"20" <- NA; names(df) <- seq(5, 20, .5)

base2 <-  base[, c("LONG", "LAT", "ESPECIE", 'CAPTURA (t)', "REGION", 
                   "FABRICA", "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
                   "PUERTO", "AREA", "DIA")]
base2 <- cbind(base2, df)

base2$'19' <- as.numeric(base2$'19')
base2$'19.5' <- as.numeric(base2$'19.5')
base2$'20' <- as.numeric(base2$'20')

write.csv(base2, outFile, row.names = F)

