# Maria Diaz Ortiz
rm(list = ls()); gc(reset = T)
# Library -----------------------------------------------------------------
library(devtools)
library(readxl)
library(stringr)
library(lubridate)
# library(skimr)

# Data -------------------------------------------------------------------
cinFile    <- "rawdat/DATA_TOTAL_IMARPE_2005 AL 2024.xlsx"
outFile    <- "cin/STANDARIZED_DATA_TOTAL_2005_2024.csv"

data <- read_xlsx(path = cinFile, sheet = 1, col_types = NULL)
base <- as.data.frame(data)
names(base)  <- toupper(names(base))

##1. latitud --------
base[, "LAT"] <- base$LATITUD
##2. longitud --------
base[, "LONG"] <- base$LONGITUD
##3. Especie -------
base[, "ESPECIE"] <- "Anchoveta, anchoveta peruana, peladilla"
##4. Captura -------
base[, "CAPTURA (t)"]     <- base$CAPTURA
##5. Region  --------

base[!is.na(base$PUERTO) & base$PUERTO == "Chicama", "REGION"]        <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "Chimbote", "REGION" ]      <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "Samanco", "REGION" ]       <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "Supe" , "REGION"]          <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "Vegueta", "REGION" ]       <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "Huacho", "REGION" ]        <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "Chancay", "REGION" ]       <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "Pisco", "REGION" ]         <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "Callao", "REGION" ]        <- "CALLAO"
base[!is.na(base$PUERTO) & base$PUERTO == "Tambo de Mora", "REGION" ] <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "Paita", "REGION" ]         <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "Parachique", "REGION" ]    <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "Ilo", "REGION" ]           <- "MOQUEGUA"
base[!is.na(base$PUERTO) & base$PUERTO == "Salaverry", "REGION" ]     <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "Atico", "REGION" ]         <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "Mollendo", "REGION" ]      <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "Planchada", "REGION" ]     <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "CPLP", "REGION" ]          <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "Caleta Lagunillas", "REGION" ] <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "CASMA", "REGION" ]         <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "Pacasmayo", "REGION" ]     <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "caleta Lagunillas", "REGION" ] <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "Huarmey", "REGION" ]      <- "ANCASH" 
base[!is.na(base$PUERTO) & base$PUERTO == "Quilca", "REGION" ]       <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "Chala", "REGION" ]        <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "Bayovar", "REGION" ]      <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "Coishco", "REGION" ]      <- "ANCASH" 
base[!is.na(base$PUERTO) & base$PUERTO == "Matarani", "REGION" ]     <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "Puerto Rico", "REGION" ]  <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "ILO", "REGION" ]          <- "MOQUEGUA"
base[!is.na(base$PUERTO) & base$PUERTO == "Máncora", "REGION" ]      <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "T. Mora", "REGION" ]      <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "Malabrigo", "REGION" ]    <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "SUPE" , "REGION"]         <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "CALLAO", "REGION" ]       <- "CALLAO"
base[!is.na(base$PUERTO) & base$PUERTO == "CHIMBOTE", "REGION" ]     <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "VEGUETA", "REGION" ]      <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "HUACHO", "REGION" ]       <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "CHANCAY", "REGION" ]      <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "PISCO", "REGION" ]        <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "TAMBO DE MORA", "REGION" ] <- "ICA"
base[!is.na(base$PUERTO) & base$PUERTO == "CALLAO", "REGION" ]       <- "CALLAO"
base[!is.na(base$PUERTO) & base$PUERTO == "CHIMBOTE", "REGION" ]     <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "COISHCO", "REGION" ]      <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "MOLLENDO", "REGION" ]     <- "AREQUIPA"
base[!is.na(base$PUERTO) & base$PUERTO == "MALABRIGO", "REGION" ]    <- "LA LIBERTAD"
base[!is.na(base$PUERTO) & base$PUERTO == "BAYOVAR", "REGION" ]      <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "CARQUIN", "REGION" ]      <- "LIMA"
base[!is.na(base$PUERTO) & base$PUERTO == "parachique", "REGION" ]   <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "PAITA", "REGION" ]        <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "PARACHIQUE", "REGION" ]   <- "PIURA"
base[!is.na(base$PUERTO) & base$PUERTO == "Casma", "REGION" ]   <- "ANCASH"
base[!is.na(base$PUERTO) & base$PUERTO == "Carquin", "REGION" ]   <- "LIMA"

unique(base$PUERTO)
unique(base$REGION)
unique(base$PUERTO[is.na(base$REGION)])

##6. Fabrica ------
base[,"FABRICA"]      <- as.character(NA)
##7 Embarcacion ----
base[,"EMBARCACION"]  <- base$EMBARCACIONES
##8 Matricula ----
base[,"MATRICULA"]    <- base$MATRICULA
base$MATRICULA <- gsub("[^a-zA-Z0-9-]", "", base$MATRICULA)
base$MATRICULA <- gsub("([a-zA-Z]+)([0-9]+)", "\\1-\\2", base$MATRICULA)
##9 Capacidad de bodega ----
base[,"CB"]           <- as.character(NA)
##10 Tipo de flota ----
base[!is.na(base$TIPO) & base$TIPO == "Ind", "TIPO"]              <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "Ind Mad", "TIPO"]          <- "MADERA"
base[!is.na(base$TIPO) & base$TIPO == "Art", "TIPO"]              <- "ARTESANAL"
base[!is.na(base$TIPO) & base$TIPO == "menor escala", "TIPO"]     <- "ARTESANAL"
base[!is.na(base$TIPO) & base$TIPO == "Art.", "TIPO"]             <- "ARTESANAL"
base[!is.na(base$TIPO) & base$TIPO == "Ind.", "TIPO"]             <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "Art/Med Esc.", "TIPO"]     <- "ARTESANAL"
base[!is.na(base$TIPO) & base$TIPO == "Artesanal", "TIPO"]        <- "ARTESANAL"
base[!is.na(base$TIPO) & base$TIPO == "Ind. Mad", "TIPO"]         <- "MADERA"
base[!is.na(base$TIPO) & base$TIPO == "ARTESANAL", "TIPO"]        <- "ARTESANAL"
base[!is.na(base$TIPO) & base$TIPO == "INDUSTRIAL DE ACERO", "TIPO"] <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "INDUSTRIAL DE MADER", "TIPO"] <- "MADERA"
base[!is.na(base$TIPO) & base$TIPO == "Industrial", "TIPO"]          <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "IND", "TIPO"]                 <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "IND MAD", "TIPO"]             <- "MADERA"
base[!is.na(base$TIPO) & base$TIPO == "ACERO NAVAL", "TIPO"]         <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "MADERA", "TIPO"]              <- "MADERA"
base[!is.na(base$TIPO) & base$TIPO == "Rsw", "TIPO"]              <- "ACERO NAVAL"
base[!is.na(base$TIPO) & base$TIPO == "RSW", "TIPO"]              <- "ACERO NAVAL"
base[,"TIPO DE FLOTA"]               <- base[, "TIPO"]

unique(base$TIPO)
unique(base$TIPO[is.na(base$`TIPO DE FLOTA`)])

##11 Puerto ----
base[ ,"PUERTO"]          <- toupper(base$PUERTO)
base$PUERTO <- gsub("T. MORA", "TAMBO DE MORA", base$PUERTO)
base$PUERTO <- gsub("MÁNCORA", "MANCORA", base$PUERTO)
base$PUERTO <- gsub("PLANCHADA", "LA PLANCHADA", base$PUERTO)
base$PUERTO <- gsub("MALABRIGO", "CHICAMA", base$PUERTO)
base$PUERTO <- gsub("MATARANI", "MOLLENDO", base$PUERTO)
unique(base$PUERTO)

##12 Area Isoparalitoral ----
base[,"AREA"]            <- base$AREA
unique(base$AREA)


##13 Fecha ----
base$DIA <- as.Date(paste(base$DIA, base$MES, base$YEAR, sep = "-"), format = "%d-%m-%Y")

##14 Marcas anchoveta -----
names(base[, 18:48])
df <- base[, 18:48]
names(df) <- seq(5, 20, .5)


base2 <- base[, c("LONG", "LAT", "ESPECIE", "CAPTURA (t)", "REGION", "FABRICA", 
                 "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
                 "PUERTO", "AREA", "DIA")]
base2 <- cbind(base2, df)
names(base2)
base2$'19' <- as.numeric(base2$'19')
base2$'19.5' <- as.numeric(base2$'19.5')
base2$'20' <- as.numeric(base2$'20')

write.csv(base2, outFile, row.names = F)
