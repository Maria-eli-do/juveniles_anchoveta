# Maria Diaz Ortiz
rm(list = ls()); gc(reset = T)
# Library -----------------------------------------------------------------
library(devtools)
library(readxl)
library(stringr)
library(lubridate)
# library(skimr)

# Data -------------------------------------------------------------------
cinFile    <- "rawdat/Data anchoveta Arequipa 2010-2023 (1).xlsx"
outFile    <- "cin/STANDARIZED_DATA_AREQUIPA_2010_2014.csv"

base_cap  <- read_xlsx(path = cinFile, sheet = 1,skip = 8)
base_biom <- read_xlsx(path = cinFile, sheet = 3)

base_cap <- as.data.frame(base_cap)
base_biom <- as.data.frame(base_biom)

names(base_cap)  <- toupper(names(base_cap))
names(base_biom) <- toupper(names(base_biom))

marks_anc <- seq(5.0, 20, .5)
#marks_otr <- paste("LS", seq(5, 70, 1), sep = "_")

##1. latitud --------
base_biom[,"LONG"]             <- -1*abs(base_biom[, "LONG"])
##2. longitud --------
base_biom[,"LAT"]             <- -1*abs(base_biom[, "LAT"])
##3. Especie -------
base_biom[,"ESPECIE"]      <- "Anchoveta, anchoveta peruana, peladilla"
##4. Captura -------
base_biom[,"CAPTURA(t)"]      <- as.numeric(base_biom$`CAPTURA (T)`)
##5. Region --------
base_biom[,"REGION"]          <- "AREQUIPA"
##6. Fabrica ------
base_cap$MATRIC        <- gsub("[^[:alnum:]-]", "", base_cap$MATRIC)
base_biom$MATRICULA    <- gsub("[^[:alnum:]-]", "", base_biom$MATRICULA)
base_cap$MES     <- ifelse(nchar(base_cap$MES) == 1, paste0("0", base_cap$MES), 
                           base_cap$MES) 
base_cap$DIA     <- ifelse(nchar(base_cap$DIA) == 1, paste0("0", base_cap$DIA), 
                           base_cap$DIA) 
base_cap$KEY     <- paste(base_cap$AÑO, base_cap$MES, base_cap$DIA, 
                          base_cap$MATRIC, sep = "-")
base_biom$FECHA  <- gsub(pattern = " UTC", replacement = "", base_biom$FECHA)
base_biom$KEY    <- paste(base_biom$FECHA, base_biom$MATRICULA, sep = "-")
base_cap$KEY     <- trimws(base_cap$KEY)
base_biom$KEY    <- trimws(base_biom$KEY)
base_biom <- merge(base_biom, base_cap[, c("KEY", "FABRICA", "CBOD(M3)")], by = "KEY", 
                   all.x = TRUE)
base_biom[,"FABRICA"]  <- base_biom[,"FABRICA"]
##7 Embarcacion ----
base_biom[,"EMBARCACION"]  <- toupper(base_biom[, "EMBARCACION"])
##8 Matricula ----
base_biom[,"MATRICULA"] <- gsub("[^[:alnum:]-]", "", base_biom$MATRICULA)
base_biom$MATRICULA <- gsub("([0-9])([A-Za-z])", "\\1-\\2", base_biom$MATRICULA)


##9 Capacidad de bodega ----
base_biom[,"CB"] <- base_biom[,"CBOD(M3)"]
##10 Tipo de flota ----
base_biom$`TIPO DE FLOTA`[base_biom$`TIPO DE FLOTA` == "IND MAD"] <- "MADERA"
base_biom$`TIPO DE FLOTA`[base_biom$`TIPO DE FLOTA` == "IND MADERA"] <- "MADERA"
base_biom$`TIPO DE FLOTA`[base_biom$`TIPO DE FLOTA` == "IND. MAD."] <- "MADERA"
base_biom$`TIPO DE FLOTA`[base_biom$`TIPO DE FLOTA` == "MAD"] <- "MADERA"
base_biom$`TIPO DE FLOTA`[base_biom$`TIPO DE FLOTA` == "IND"]   <- "ACERO NAVAL"

##11 Puerto ----
base_biom[,"PUERTO"]          <- base_biom[ ,"PUERTO"]
##12 Area Isoparalitoral ----
base_biom[,"AREA"]            <- base_biom[,"AREA"]
##13 Fecha ----
base_biom[,"DIA"]             <- as.Date(base_biom[, "FECHA"], format ="%Y-%m-%d" )
##14 Marcas anchoveta -----

##15 Otras ----
#base_biom[,"Otras"]  <- as.character(NA)
##16 Marcas otras -----
# pf <- as.data.frame.matrix(matrix(as.numeric(NA), nrow = nrow(base_biom),
#                                   ncol = length(marks_otr)))
# names(pf) <- marks_otr
# 
# base <- cbind(base_biom, pf)

# Selección de columnas
names2 <-  c("LONG", "LAT", "ESPECIE", "CAPTURA(t)", "REGION", 
             "FABRICA", "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
             "PUERTO", "AREA", "DIA", as.character(marks_anc))
base2 <- base_biom[, names2]
str(base2)


write.csv(base2, outFile, row.names = F)



