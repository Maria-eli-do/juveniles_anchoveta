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
library(readr)

# Data -------------------------------------------------------------------

file1 <- "rawdat/1972-1989.xlsx"
file2 <- "rawdat/1990-1999.xlsx"
file3 <- "rawdat/2000.xlsx"
file4 <- "rawdat/2001-2002.xlsx"
file5 <- "rawdat/2003-2004.xlsx"
file6 <- "rawdat/2005-2006.xlsx"
file7 <- "rawdat/2007-2009.xlsx"
file8 <- "rawdat/2010-2014.xlsx"

data1 <- read_xlsx(path = file1, sheet = 1, col_types = NULL)
data2 <- read_xlsx(path = file2, sheet = 1, col_types = NULL)
data3 <- read_xlsx(path = file3, sheet = 1, col_types = NULL)
data4 <- read_xlsx(path = file4, sheet = 1, col_types = NULL)
data5 <- read_xlsx(path = file5, sheet = 1, col_types = NULL)
data6 <- read_xlsx(path = file6, sheet = 1, col_types = NULL)
data7 <- read_xlsx(path = file7, sheet = 1, col_types = NULL)
data8 <- read_xlsx(path = file8, sheet = 1, col_types = NULL)

data1$DIA  <- as.Date(data1$DIA,format = "%d-%m-%y")
data2$DIA  <- as.Date(data2$DIA,format = "%d-%m-%y")
data3$DIA  <- as.Date(data3$DIA,format = "%d/%m/%Y")
data4$DIA  <- as.Date(data4$DIA,format = "%d/%m/%Y")
data5$DIA  <- as.Date(data5$DIA,format = "%d/%m/%Y")
data6$DIA  <- as.Date(data6$DIA,format = "%d/%m/%Y")
data7$DIA  <- as.Date(data7$DIA,format = "%d/%m/%Y")
data8$DIA  <- as.Date(data8$DIA,format = "%d/%m/%Y")

data <- rbind(data1, data2, data3, data4, data5, data6, data7, data8)
names(data)

sp <- unique(data$ESPECIE_NOM_COMUN)
dataAnc <- data[data$ESPECIE_NOM_COMUN == sp[5], ]
head(dataAnc)

write.csv(dataAnc, "rawdat/RAW_DATA_1972_2014.csv", row.names = F)

# DataAnc ------------------------------------------------
cinFile <- "rawdat/RAW_DATA_1972_2014.csv"
outFile <- "cin/STANDARIZED_DATA_1972_2014.csv"

data <- read_csv(cinFile)
names(data)  <- toupper(names(data))
base <- as.data.frame(data)


##1. latitud --------
base[, "LAT"] <- base$LATITUD_INICIAL
base$LAT <- as.numeric(base$LAT)
##2. longitud --------
base[, "LONG"] <- base$LONGITUD_INICIAL
base$LONG <- as.numeric(base$LONG)
##3. Especie -------
base[, "ESPECIE"] <- "Anchoveta, anchoveta peruana, peladilla"
##4. Captura -------
base[, "CAPTURA (t)"]     <- base$ESPECIE_CAPTURA
base$`CAPTURA (t)` <- base$`CAPTURA (t)`/1000

##5. Region  --------
tmp <- base[,c("LUGAR_NOM", "REGION_NOM")]
tmp[!duplicated(tmp),]

base[, "REGION"]  <- NA
base[base$LUGAR_NOM == "TAMBO DE MORA", "REGION" ] <- "ICA"
base[base$LUGAR_NOM == "PISCO", "REGION" ] <- "ICA"
base[base$LUGAR_NOM == "CHIMBOTE", "REGION" ] <- "ANCASH"
base[base$LUGAR_NOM == "ILO", "REGION" ] <- "MOQUEGUA"
base[base$LUGAR_NOM == "CALLAO", "REGION" ] <- "CALLAO"
base[base$LUGAR_NOM == "HUACHO", "REGION" ] <- "LIMA"
base[base$LUGAR_NOM == "MOLLENDO", "REGION" ] <- "AREQUIPA"
base[base$LUGAR_NOM == "CHANCAY", "REGION" ] <- "LIMA"
base[base$LUGAR_NOM == "ATICO", "REGION" ] <- "AREQUIPA"
base[base$LUGAR_NOM == "VEGUETA", "REGION" ] <- "LIMA"
base[base$LUGAR_NOM == "LA PLANCHADA", "REGION" ] <- "AREQUIPA"
base[base$LUGAR_NOM == "PAITA", "REGION" ] <- "PIURA"
base[base$LUGAR_NOM == "SUPE" , "REGION"] <- "LIMA"
base[base$LUGAR_NOM == "HUARMEY", "REGION" ] <- "ANCASH" 
base[base$LUGAR_NOM == "MALABRIGO/CHICAMA", "REGION" ] <- "LA LIBERTAD"
base[base$LUGAR_NOM == "BAYOVAR", "REGION" ] <- "PIURA"
base[base$LUGAR_NOM == "CASMA", "REGION" ] <- "ANCASH"
base[base$LUGAR_NOM == "SALAVERRY", "REGION" ] <- "LA LIBERTAD"
base[base$LUGAR_NOM == "PARACHIQUE", "REGION" ] <- "PIURA"
base[base$LUGAR_NOM == "SAMANCO", "REGION" ] <- "ANCASH"
base[base$LUGAR_NOM == "PUCUSANA", "REGION" ] <- "LIMA"
base[base$LUGAR_NOM == "MATARANI/MOLLENDO", "REGION" ] <- "AREQUIPA"
base[base$LUGAR_NOM == "VILA VILA", "REGION"] <- "TACNA"
base[base$LUGAR_NOM == "SAN JOSE", "REGION" ] <- "LAMBAYEQUE"
base[base$LUGAR_NOM == "PIMENTEL" , "REGION"] <- "LAMBAYEQUE"
base[base$LUGAR_NOM == "HUANCHACO", "REGION" ] <- "LA LIBERTAD"
base[base$LUGAR_NOM == "COISHCO", "REGION" ] <- "ANCASH" 
base[base$LUGAR_NOM == "MANCORA", "REGION" ] <- "PIURA"
base[base$LUGAR_NOM == "MORRO SAMA(GRAU)", "REGION" ] <- "TACNA"
base[base$LUGAR_NOM == "TALARA", "REGION" ] <- "PIURA"
base[base$LUGAR_NOM == "QUILCA", "REGION" ] <- "AREQUIPA"
base[base$LUGAR_NOM == "ANCON", "REGION" ] <- "LIMA"
base[base$LUGAR_NOM == "YACILA", "REGION" ] <- "PIURA"
base[base$LUGAR_NOM == "SANTA", "REGION" ] <- "ANCASH"
base[base$LUGAR_NOM == "CHALA", "REGION" ] <- "AREQUIPA"
base[base$LUGAR_NOM == "LOS CHIMUS", "REGION" ] <- "ANCASH"
base[base$LUGAR_NOM == "ACAPULCO", "REGION" ] <- "TUMBES"

unique(base$LUGAR_NOM)
unique(base$REGION)
base$LUGAR_NOM[is.na(base$REGION)]

##6. Fabrica ------
base[,"FABRICA"]      <- as.character(NA)
##7 Embarcacion ----
base[,"EMBARCACION"]  <- base$EMBARCACION_NOM
##8 Matricula ----
base[,"MATRICULA"]    <- base$EMBARCACION_MAT
base$MATRICULA <- gsub("[^a-zA-Z0-9-]", "", base$MATRICULA)
base$MATRICULA <- gsub("([a-zA-Z]+)([0-9]+)", "\\1-\\2", base$MATRICULA)

##9 Capacidad de bodega ----
base[,"CB"]           <- base$EMBARCACION_BOD
##10 Tipo de flota ----
base[!is.na(base$FLOTA_NOM) & base$FLOTA_NOM == "INDUSTRIAL DE MADERA", "FLOTA_NOM"] <- "MADERA"
base[!is.na(base$FLOTA_NOM) & base$FLOTA_NOM == "INDUSTRIAL DE FIERRO", "FLOTA_NOM"]     <- "ACERO NAVAL"
base[!is.na(base$FLOTA_NOM) & base$FLOTA_NOM == "INDUSTRIAL DE ARRASTRE", "FLOTA_NOM"]     <- "ACERO NAVAL"
base[!is.na(base$FLOTA_NOM) & base$FLOTA_NOM == "ARTESANAL", "FLOTA_NOM"]     <- "ARTESANAL"
base[!is.na(base$FLOTA_NOM) & base$FLOTA_NOM == "PALANGRERA", "FLOTA_NOM"]     <- "ARTESANAL"
base[!is.na(base$FLOTA_NOM) & base$FLOTA_NOM == "SIN DATO", "FLOTA_NOM"]     <- NA
base[,"TIPO DE FLOTA"]               <- base[, "FLOTA_NOM"]
##11 Puerto ----
base[ ,"PUERTO"]          <- base$LUGAR_NOM
base$PUERTO <- gsub("MALABRIGO/CHICAMA", "CHICAMA", base$PUERTO) 
base$PUERTO <- gsub("MATARANI/MOLLENDO", "MOLLENDO", base$PUERTO) 

##12 Area Isoparalitoral ----
base[,"AREA"]            <- base$AREA_PESCA_COD
base$AREA <- as.numeric(base$AREA)

##13 Fecha ----
base[,"DIA"]            <- as.Date(base$FECHA, format = "%d-%m-%Y")
##14 Marcas anchoveta -----
base[,"LONGITUD"]       <- as.numeric(base$LONGITUD)

base2 <- base %>%
  group_by(LONG, LAT, ESPECIE, `CAPTURA (t)`, REGION, FABRICA, EMBARCACION, MATRICULA, 
           CB, `TIPO DE FLOTA`, PUERTO, AREA, DIA, LONGITUD) %>%
  summarise(FRECUENCIA_SIMPLE = sum(FRECUENCIA_SIMPLE, na.rm = TRUE)) %>%
  pivot_wider(
    names_from = LONGITUD,
    values_from = FRECUENCIA_SIMPLE,
    names_sort = TRUE
  ) %>%
  ungroup()

head(base2)
str(base2)
#Guardar data --------------------------------
write.csv(base2, outFile, row.names = F)



      