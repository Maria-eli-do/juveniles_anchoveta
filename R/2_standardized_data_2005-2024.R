# Maria Diaz Ortiz
rm(list = ls()); gc(reset = T)
# Library -----------------------------------------------------------------
library(readxl)
library(stringr)
library(lubridate)

# Data -------------------------------------------------------------------
USB <- "I:/"
dir_raw <- file.path(USB, "database/rawdat/")
dir_cin <- file.path(USB, "database/cin/")

cinFile <- file.path(dir_raw, "DATA_TOTAL_IMARPE_2005 AL 2024.xlsx")
outFile  <- file.path(dir_cin, "/STANDARIZED_DATA_TOTAL_2005_2024.csv")

base <- read_xlsx(path = cinFile, sheet = 1, col_types = NULL)
base <- as.data.frame(base)
names(base)  <- toupper(names(base))

##1. latitud --------
base[, "LAT"] <- (-1)*abs(as.numeric(base$LATITUD))

##2. longitud --------
base[, "LONG"] <- (-1)*abs(as.numeric(base$LONGITUD))

##3. Especie -------
base[, "ESPECIE"] <- "Anchoveta, anchoveta peruana, peladilla"

##4. Captura -------
base$CAPTURA <- gsub(",", ".", base$CAPTURA)
unique(base$CAPTURA[grepl("[a-zA-Z]", base$CAPTURA)])
base[, "CAPTURA (t)"] <- as.numeric(base$CAPTURA)

##5. Region  --------
base$REGION <- NA
base$REGION[base$PUERTO == "Chicama"]        <- "LA LIBERTAD"
base$REGION[base$PUERTO == "Chimbote"]      <- "ANCASH"
base$REGION[base$PUERTO == "Samanco"]       <- "ANCASH"
base$REGION[base$PUERTO == "Supe"]          <- "LIMA"
base$REGION[base$PUERTO == "Vegueta"]       <- "LIMA"
base$REGION[base$PUERTO == "Huacho"]        <- "LIMA"
base$REGION[base$PUERTO == "Chancay"]       <- "LIMA"
base$REGION[base$PUERTO == "Pisco"]         <- "ICA"
base$REGION[base$PUERTO == "Callao"]        <- "CALLAO"
base$REGION[base$PUERTO == "Tambo de Mora"] <- "ICA"
base$REGION[base$PUERTO == "Paita"]         <- "PIURA"
base$REGION[base$PUERTO == "Parachique"]    <- "PIURA"
base$REGION[base$PUERTO == "Ilo"]           <- "MOQUEGUA"
base$REGION[base$PUERTO == "Salaverry"]     <- "LA LIBERTAD"
base$REGION[base$PUERTO == "Atico"]         <- "AREQUIPA"
base$REGION[base$PUERTO == "Mollendo"]      <- "AREQUIPA"
base$REGION[base$PUERTO == "Planchada"]     <- "AREQUIPA"
base$REGION[base$PUERTO == "CPLP"]          <- "ICA"
base$REGION[base$PUERTO == "Caleta Lagunillas"] <- "ICA"
base$REGION[base$PUERTO == "CASMA"]         <- "ANCASH"
base$REGION[base$PUERTO == "Pacasmayo"]     <- "LA LIBERTAD"
base$REGION[base$PUERTO == "caleta Lagunillas"] <- "ICA"
base$REGION[base$PUERTO == "Huarmey"]       <- "ANCASH" 
base$REGION[base$PUERTO == "Quilca"]        <- "AREQUIPA"
base$REGION[ base$PUERTO == "Chala"]        <- "AREQUIPA"
base$REGION[ base$PUERTO == "Bayovar"]      <- "PIURA"
base$REGION[ base$PUERTO == "Coishco"]      <- "ANCASH" 
base$REGION[ base$PUERTO == "Matarani"]     <- "AREQUIPA"
base$REGION[ base$PUERTO == "Puerto Rico"]  <- "PIURA"
base$REGION[ base$PUERTO == "ILO"]          <- "MOQUEGUA"
base$REGION[ base$PUERTO == "Máncora"]      <- "PIURA"
base$REGION[ base$PUERTO == "T. Mora"]      <- "ICA"
base$REGION[ base$PUERTO == "Malabrigo"]    <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "SUPE"]         <- "LIMA"
base$REGION[ base$PUERTO == "CALLAO"]       <- "CALLAO"
base$REGION[ base$PUERTO == "CHIMBOTE"]     <- "ANCASH"
base$REGION[ base$PUERTO == "VEGUETA"]      <- "LIMA"
base$REGION[ base$PUERTO == "HUACHO"]       <- "LIMA"
base$REGION[ base$PUERTO == "CHANCAY"]      <- "LIMA"
base$REGION[ base$PUERTO == "PISCO"]        <- "ICA"
base$REGION[ base$PUERTO == "TAMBO DE MORA"] <- "ICA"
base$REGION[ base$PUERTO == "CALLAO"]       <- "CALLAO"
base$REGION[ base$PUERTO == "CHIMBOTE"]     <- "ANCASH"
base$REGION[ base$PUERTO == "COISHCO"]      <- "ANCASH"
base$REGION[ base$PUERTO == "MOLLENDO"]     <- "AREQUIPA"
base$REGION[ base$PUERTO == "MALABRIGO"]    <- "LA LIBERTAD"
base$REGION[ base$PUERTO == "BAYOVAR"]      <- "PIURA"
base$REGION[ base$PUERTO == "CARQUIN"]      <- "LIMA"
base$REGION[ base$PUERTO == "parachique"]   <- "PIURA"
base$REGION[ base$PUERTO == "PAITA"]        <- "PIURA"
base$REGION[ base$PUERTO == "PARACHIQUE"]   <- "PIURA"
base$REGION[ base$PUERTO == "Casma"]        <- "ANCASH"
base$REGION[ base$PUERTO == "Carquin"]      <- "LIMA"

unique(base$PUERTO); unique(base$REGION)
unique(base$PUERTO[is.na(base$REGION)])

##6. Fabrica ------
base[,"FABRICA"]      <- as.character(NA)

##7 Embarcacion ----
base[,"EMBARCACION"]  <- toupper(trimws(base$EMBARCACIONES))

##8 Matricula ----
base[,"MATRICULA"]  <- toupper(trimws(base$MATRICULA))
base$MATRICULA      <- gsub("[^a-zA-Z0-9-]", "", base$MATRICULA)
base$MATRICULA[base$MATRICULA == "RSW"] <- NA
base$MATRICULA[base$MATRICULA == "IND"] <- NA
base$MATRICULA[base$MATRICULA == "0"] <- NA

##9 Capacidad de bodega ----
base[,"CB"]           <- as.numeric(NA)

##10 Tipo de flota ----
base$TIPO[base$TIPO == "Ind"]              <- "ACERO NAVAL"
base$TIPO[base$TIPO == "Ind Mad"]          <- "MADERA"
base$TIPO[base$TIPO == "Art"]              <- "ARTESANAL"
base$TIPO[base$TIPO == "menor escala"]     <- "ARTESANAL"
base$TIPO[base$TIPO == "Art."]             <- "ARTESANAL"
base$TIPO[base$TIPO == "Ind."]             <- "ACERO NAVAL"
base$TIPO[base$TIPO == "Art/Med Esc."]     <- "ARTESANAL"
base$TIPO[base$TIPO == "Artesanal"]        <- "ARTESANAL"
base$TIPO[base$TIPO == "Ind. Mad"]         <- "MADERA"
base$TIPO[base$TIPO == "ARTESANAL"]        <- "ARTESANAL"
base$TIPO[base$TIPO == "INDUSTRIAL DE ACERO"] <- "ACERO NAVAL"
base$TIPO[base$TIPO == "INDUSTRIAL DE MADER"] <- "MADERA"
base$TIPO[base$TIPO == "Industrial"]          <- "ACERO NAVAL"
base$TIPO[base$TIPO == "IND"]                 <- "ACERO NAVAL"
base$TIPO[base$TIPO == "IND MAD"]             <- "MADERA"
base$TIPO[base$TIPO == "ACERO NAVAL"]         <- "ACERO NAVAL"
base$TIPO[base$TIPO == "MADERA"]              <- "MADERA"
base$TIPO[base$TIPO == "Rsw"]                 <- "ACERO NAVAL"
base$TIPO[base$TIPO == "RSW"]                 <- "ACERO NAVAL"

base[,"TIPO DE FLOTA"]               <- base[, "TIPO"]

unique(base$TIPO)
unique(base$TIPO[is.na(base$`TIPO DE FLOTA`)])

##11 Puerto ----
base[ ,"PUERTO"] <- toupper(trimws(base$PUERTO))
base$PUERTO <- gsub("T. MORA", "TAMBO DE MORA", base$PUERTO)
base$PUERTO <- gsub("MÁNCORA", "MANCORA", base$PUERTO)
base$PUERTO <- gsub("PLANCHADA", "LA PLANCHADA", base$PUERTO)
base$PUERTO <- gsub("MALABRIGO", "CHICAMA", base$PUERTO)
base$PUERTO <- gsub("MATARANI", "MOLLENDO", base$PUERTO)
unique(base$PUERTO)

##12 Area Isoparalitoral ----
base[,"AREA"] <- as.numeric(base$AREA)
unique(base$AREA)

##13 Fecha ----
base$DIA <- as.Date(paste(base$DIA, base$MES, base$YEAR, sep = "-"), 
                    format = "%d-%m-%Y")

##14 Marcas anchoveta -----
names(base[, 18:48]); df <- base[, 18:48]; names(df) <- seq(5, 20, .5)

base2 <- base[, c("LONG", "LAT", "ESPECIE", "CAPTURA (t)", "REGION", "FABRICA", 
                 "EMBARCACION", "MATRICULA", "CB", "TIPO DE FLOTA", 
                 "PUERTO", "AREA", "DIA")]
base2 <- cbind(base2, df)
base2$'19' <- as.numeric(base2$'19')
base2$'19.5' <- as.numeric(base2$'19.5')
base2$'20' <- as.numeric(base2$'20')

write.csv(base2, outFile, row.names = F)
