# Maria Diaz Ortiz
rm(list = ls()); gc(reset = T)

# Library -----------------------------------------------------------------
library(devtools)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
# library(skimr)

# Data -------------------------------------------------------------------
cinFile    <- "rawdat/FRECUENCIA ANCHOVETA 2010 2023_Ilo.xlsx"
outFile    <- "cin/STANDARIZED_DATA_ILO.csv"


base        <- read_xlsx(path = cinFile, sheet = 1, skip = 2, col_types = NULL)
base        <- as.data.frame(base)
names(base) <- toupper(names(base))

marks_anc <- seq(5.0, 20, .5)
#marks_otr <- paste("LS", seq(5, 70, 1), sep = "_")

##1. latitud --------
coordenadas <-  gsub("[^0-9 ]", "", base$`LAT (SEXAG)`)
coordenadas <-  gsub("[^[:alnum:]-]", "", coordenadas)
grados <- substr(start = 1, stop = 2, coordenadas)
min  <-  substr(start = 3, stop = nchar(coordenadas), coordenadas)
min <- ifelse(nchar(min) == 1, paste0("0", min), min)
lat <- as.numeric(grados) + (as.numeric(min)/60)
lat <- -1*abs(lat)
base$`LAT (CENT)` <- as.numeric(base$`LAT (CENT)`)
lat[which(is.na(lat))] <- base$`LAT (CENT)`[which(is.na(lat))]
base[, "LAT"] <- lat

##2. longitud --------
coordenadas <-  gsub("[^0-9 ]", "", base$`LONG (SEXAG)`)
coordenadas <-  gsub("[^[:alnum:]-]", "", coordenadas)
grados <- substr(start = 1, stop = 2, coordenadas)
min  <-  substr(start = 3, stop = nchar(coordenadas), coordenadas)
min <- ifelse(nchar(min) == 1, paste0("0", min), min)
lon <- as.numeric(grados) + (as.numeric(min)/60)
base$`LONG (CENT)` <- as.numeric(base$`LONG (CENT)`)
lon[which(is.na(lon))] <- base$`LONG (CENT)`[which(is.na(lon))]
lon <- -1*abs(lon)
base[, "LONG"] <- lon

##3. Especie -------
base[,"ESPECIE"]      <- "Anchoveta, anchoveta peruana, peladilla"
##4. Captura -------
base[,"CAPTURA(t)"]   <- as.numeric(base$`CAPTURA (T)`)
##5. Region --------
base[,"REGION"]       <- "Moquegua"
##6. Fabrica ------
base[,"FABRICA"]      <- as.character(NA)
##7 Embarcacion ----
base[,"EMBARCACION"]  <- toupper(base[, "EMBARCACIÓN"])
##8 Matricula ----
base[,"MATRICULA"]    <- gsub("[^[:alnum:]-]", "", base$MATRICULA)
base$MATRICULA <- gsub("([0-9])([A-Za-z])", "\\1-\\2", base$MATRICULA)
base$MATRICULA <- gsub("--", "-", base$MATRICULA)
base$MATRICULA <- gsub("([A-Za-z]+)([0-9]+)(-[A-Za-z]+)", "\\1-\\2\\3", base$MATRICULA)

##9 Capacidad de bodega ----
base[,"CB"]           <- as.numeric(NA)
##10 Tipo de flota ----
base[base$TIPO == "ART", "TIPO"]     <- "ARTESANAL"
base[base$TIPO == "IND MAD", "TIPO"] <- "MADERA"
base[base$TIPO == "IND", "TIPO"]     <- "ACERO NAVAL"
base[,"TIPO DE FLOTA"]               <- base[, "TIPO"]
##11 Puerto ----

base[ ,"PUERTO"]          <- base[ ,"PUERTO"]
##12 Area Isoparalitoral ----
base[,"AREA"]            <- base[,"AREA"]

##13 Fecha ----
meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", 
           "JULIO", "AGOSTO", "SETIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
base$MES <- match(base$MES, meses)
dia  <- paste(base$DIA, base$MES, base$AÑO,sep = "-")
base[,"DIA"]            <- as.Date(dia, format = "%d-%m-%Y")

##14 Marcas anchoveta -----
id_b <- !(marks_anc %in% names(base))
df <- as.data.frame.matrix(matrix(as.numeric(NA), nrow = nrow(base),
                                    ncol = sum(id_b)))
dimnames(df)[[2]] <- as.character(marks_anc[id_b])

base <- cbind(base, df)

##15 Otras ----
#base[,"Otras"]  <- as.character(NA)

##16 Marcas otras -----
# pf <- as.data.frame.matrix(matrix(as.numeric(NA), nrow = nrow(base),
#                                   ncol = length(marks_otr)))
# names(pf) <- marks_otr
# base <- cbind(base, pf)

# Selección de columnas

names2 <-  c("LONG", "LAT", "ESPECIE", "CAPTURA(t)", "REGION", 
             "FABRICA", "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
             "PUERTO", "AREA", "DIA", as.character(marks_anc))
base2 <- base[, names2]

# t(t(table(year(base2$DIA), base2$`TIPO DE FLOTA`)))

write.csv(base2, outFile, row.names = F)

