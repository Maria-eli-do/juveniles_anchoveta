rm(list = ls()); gc(reset = T)
# LIBRARY -----------------------------------------------------------------------------------------------------------------
library(lubridate); library(devtools)
require(ggplot2); require(cowplot)
require(ggmap); require(rnaturalearth); 
require(tidyverse); require(dplyr)

# SOURCE ------------------------------------------------------------------

#source("R/1_standardized_data_Ilo.R")
#source("R/2_standardized_data_arequipa.R")
#source("R/3_standardized_data_1972_2014.R")
#source("R/4_standardized_data_2005-2024.R")

marks <- seq(5,20,.5)

# FILES -----------------------------------------------------------------------------------------------------------------
file1 <- "cin/STANDARIZED_DATA_AREQUIPA_2010_2014.csv"
file2 <- "cin/STANDARIZED_DATA_ILO_2010_2014.csv"
file3 <- "cin/STANDARIZED_DATA_1972_2014.csv"
file4 <- "cin/STANDARIZED_DATA_TOTAL_2005_2024.csv"

sf_land <- ne_countries(scale = "medium", returnclass = "sf", 
                        continent = c("South America", "North America")) 

# 1. Standardized file 1 ----------------------------------------------------------------------------------------------
data <- read.csv(file1, header = T)
names(data)

## LONG LAT ---------------------
data$LONG  <- (-1 * abs( as.numeric(data$LONG)))
data$LAT  <- (-1 * abs( as.numeric(data$LAT)))

r1 <- range(data$LONG, na.rm = TRUE); r1

r2 <- range(data$LAT, na.rm = TRUE); r2

ggplot() +
  geom_sf(data = sf_land, fill = "white", color = "black") +
  geom_point(data = data, aes(x = LONG, y = LAT), 
             size = 1, color = "blue") +
  labs(title = "", x = "Longitud", y = "Latitud") +
  theme_bw() +
  scale_x_continuous(breaks = seq(trunc(r1[1]) - 3, trunc(r2[2]) + 3, 10)) +
  scale_y_continuous(breaks = seq(trunc(r1[2]) - 1, trunc(r2[2]) + 1, 5))  + 
  coord_sf(xlim = r1 + c(-10, +10), ylim = r2 + c(-1, +1))

unique(data$DIA[data$LAT < -20])

## ESPECIE ---------------------
unique(data$ESPECIE)

## CAPTURA ----------------------------------------- 
data$CAPTURA.t. <- abs(as.numeric(data$CAPTURA.t.))

range(data$CAPTURA.t.)

ggplot(data = data) +
  geom_histogram(aes(x = CAPTURA.t.), binwidth = 30, 
                 fill = "blue", color = "black") +
  labs(x = "CAPTURA (t)", y = "Frecuencia") +
  theme_minimal()

names(data)
id0 <-  which(data$CAPTURA.t. == 0)
tmp <- (data[id0, paste0("X", seq(5,20,.5))])
tmp <- as.data.frame(lapply(tmp, as.numeric))
str(tmp)
rowSums(tmp, na.rm = TRUE)


## REGION  ------------------------------------------------------------------
data$REGION <- toupper(trimws(as.character(data$REGION)))
unique(data$REGION)
table(data$REGION, useNA = "ifany")

## FABRICA -----------------------------------------------------------------
data$FABRICA <- toupper(trimws(as.character(data$FABRICA)))
unique(data$FABRICA)
table(data$FABRICA, useNA = "ifany")

## MATRICULA ---------------------
data$MATRICULA <- toupper(trimws(as.character(data$MATRICULA)))
unique(data$MATRICULA)

## CAPACIDAD DE BODEGA -----------------------------------------------------
data$CB  <- abs(as.numeric(data$CB))
range(data$CB, na.rm = TRUE)
ggplot(data = data) +
  geom_histogram(aes(x = CB), binwidth = 30, 
                 fill = "red", color = "black") +
  labs(x = "Capacidad de bodega (m3)", y = "Frecuencia") +
  theme_minimal()

names(data)

## FLOTA -----------------------------------------------------------------
data$TIPO.DE.FLOTA <- toupper(trimws(as.character(data$TIPO.DE.FLOTA)))
unique(data$TIPO.DE.FLOTA)
table(data$TIPO.DE.FLOTA, useNA = "ifany")
data$MATRICULA[is.na(data$TIPO.DE.FLOTA)]
data[is.na(data$TIPO.DE.FLOTA),]

names(data)

## PUERTO ------------------------------------------------------------------
data$PUERTO <- toupper(trimws(as.character(data$PUERTO)))
unique(data$PUERTO)
table(data$PUERTO, useNA = "ifany")
data$LAT[is.na(data$PUERTO)]
data$REGION[is.na(data$PUERTO)]

names(data)

## AREA --------------------------------------------------------------------
data$AREA <- abs(as.numeric(data$AREA))
unique(data$AREA)
data$AREA[data$AREA == 0] <- NA
data$AREA[data$AREA == 116] <- NA
table(data$AREA, useNA = "ifany")
data$LAT[is.na(data$AREA)]
data$LONG[is.na(data$AREA)]

# RECORDAR AL WILD LAS FUNCIONES 

names(data)

## DIA ---------------------------------------------------------------------

data$DIA  <- as.Date(data$DIA,format = "%Y-%m-%d")
unique(data$DIA)

## BIOMETRICO -------------------------------------------------------------
tmp <- data[, paste0("X", marks)]
tmp_numeric <- tmp[, sapply(tmp, is.numeric)]
nbiom <- rowSums(tmp_numeric, na.rm = TRUE)

range(nbiom, na.rm = TRUE)

ggplot() +
  geom_histogram(aes(x = nbiom), binwidth = 30, 
                 fill = "darkgreen", color = "black") +
  labs(x = "n ejemplares", y = "Frecuencia") +
  theme_minimal()

data_1 <- data

# 2. Standardized file 2 ----------------------------------------------------------------------------------------------
data <- read.csv(file2, header = T)
names(data)

## LONG LAT ---------------------
data$LONG  <- (-1 * abs( as.numeric(data$LONG)))
data$LAT  <- (-1 * abs( as.numeric(data$LAT)))

r1 <- range(data$LONG, na.rm = TRUE); r1
r2 <- range(data$LAT, na.rm = TRUE); r2

ggplot() +
  geom_sf(data = sf_land, fill = "white", color = "black") +
  geom_point(data = data, aes(x = LONG, y = LAT), 
             size = 1, color = "blue") +
  labs(title = "", x = "Longitud", y = "Latitud") +
  theme_bw() +
  scale_x_continuous(breaks = seq(trunc(r1[1]) - 3, trunc(r2[2]) + 3, 10)) +
  scale_y_continuous(breaks = seq(trunc(r1[2]) - 1, trunc(r2[2]) + 1, 5))  + 
  coord_sf(xlim = r1 + c(-10, +10), ylim = r2 + c(-1, +1))

unique(data$DIA[data$LONG > -86])

## ESPECIE ---------------------
unique(data$ESPECIE)

## CAPTURA --------------------------------
data$CAPTURA.t. <- abs(as.numeric(data$CAPTURA.t.))

ggplot(data = data) +
  geom_histogram(aes(x = CAPTURA.t.), binwidth = 30, 
                 fill = "blue", color = "black") +
  labs(x = "CAPTURA (t)", y = "Frecuencia") +
  theme_minimal()

names(data)
data[data$CAPTURA.t. == 0,]

## REGION  ------------------------------------------------------------------
data$REGION <- toupper(trimws(as.character(data$REGION)))
unique(data$REGION)
table(data$REGION, useNA = "ifany")

## FABRICA -----------------------------------------------------------------
data$FABRICA <- toupper(trimws(as.character(data$FABRICA)))
unique(data$FABRICA)
table(data$FABRICA, useNA = "ifany")

## MATRICULA ---------------------
data$MATRICULA <- toupper(trimws(as.character(data$MATRICULA)))
unique(data$MATRICULA)

## CAPACIDAD DE BODEGA -----------------------------------------------------
data$CB  <- abs(as.numeric(data$CB))
range(data$CB, na.rm = TRUE)
ggplot(data = data) +
  geom_histogram(aes(x = CB), binwidth = 30, 
                 fill = "red", color = "black") +
  labs(x = "Capacidad de bodega (m3)", y = "Frecuencia") +
  theme_minimal()

names(data)

## FLOTA -----------------------------------------------------------------
data$TIPO.DE.FLOTA <- toupper(trimws(as.character(data$TIPO.DE.FLOTA)))
unique(data$TIPO.DE.FLOTA)
table(data$TIPO.DE.FLOTA, useNA = "ifany")
data$MATRICULA[is.na(data$TIPO.DE.FLOTA)]
data$CB[is.na(data$TIPO.DE.FLOTA)]
names(data)

## PUERTO ------------------------------------------------------------------
data$PUERTO <- toupper(trimws(as.character(data$PUERTO)))
unique(data$PUERTO)
table(data$PUERTO, useNA = "ifany")
data$LAT[is.na(data$PUERTO)]
data$REGION[is.na(data$PUERTO)]
names(data)

## AREA --------------------------------------------------------------------
data$AREA <- abs(as.numeric(data$AREA))
unique(data$AREA)
table(data$AREA, useNA = "ifany")
data$LAT[is.na(data$AREA)]
data$LONG[is.na(data$AREA)]

# RECORDAR AL WILD LAS FUNCIONES 

names(data)

## DIA ---------------------------------------------------------------------
data$DIA  <- as.Date(data$DIA,format = "%Y-%m-%d")
unique(data$DIA)

## BIOMETRICO -------------------------------------------------------------
tmp <- data[, paste0("X", marks)]
tmp_numeric <- sapply(tmp, as.numeric)
nbiom <- rowSums(tmp_numeric, na.rm = TRUE)

range(nbiom, na.rm = TRUE)

ggplot() +
  geom_histogram(aes(x = nbiom), binwidth = 10, 
               fill = "darkgreen", color = "black") +
  labs(x = "n ejemplares", y = "Frecuencia") +
  theme_minimal()

data_2 <- data

# 3. Standardized file 3 ----------------------------------------------------------------------------------------------
data <- read.csv(file3, header = T)
names(data)

## LONG LAT ---------------------
data$LONG  <- (-1 * abs( as.numeric(data$LONG)))
data$LAT  <- (-1 * abs( as.numeric(data$LAT)))

r1 <- range(data$LONG, na.rm = TRUE); r1
r2 <- range(data$LAT, na.rm = TRUE); r2

ggplot() +
  geom_sf(data = sf_land, fill = "white", color = "black") +
  geom_point(data = data, aes(x = LONG, y = LAT), 
             size = 1, color = "blue") +
  labs(title = "", x = "Longitud", y = "Latitud") +
  theme_bw() +
  scale_x_continuous(breaks = seq(trunc(r1[1]) - 3, trunc(r2[2]) + 3, 10)) +
  scale_y_continuous(breaks = seq(trunc(r1[2]) - 1, trunc(r2[2]) + 1, 5))  + 
  coord_sf(xlim = r1 + c(-10, +10), ylim = r2 + c(-1, +1))

unique(data$DIA[data$LAT == 0])
unique(data$DIA[data$LONG == -819])

## ESPECIE ----------LONG## ESPECIE ---------------------
unique(data$ESPECIE)

## CAPTURA --------------------------------
data$CAPTURA..t. <- abs(as.numeric(data$CAPTURA..t.))

range(data$CAPTURA..t.)                       

ggplot(data = data) +
  geom_histogram(aes(x = CAPTURA..t.), binwidth = 30, 
                 fill = "blue", color = "black") +
  labs(x = "CAPTURA (t)", y = "Frecuencia") +
  theme_minimal()

names(data)
data[data$CAPTURA.t. == 0,]

## REGION  ------------------------------------------------------------------
data$REGION <- toupper(trimws(as.character(data$REGION)))
unique(data$REGION)
table(data$REGION, useNA = "ifany")

## FABRICA -----------------------------------------------------------------
data$FABRICA <- toupper(trimws(as.character(data$FABRICA)))
unique(data$FABRICA)
table(data$FABRICA, useNA = "ifany")

## MATRICULA ---------------------
data$MATRICULA <- toupper(trimws(as.character(data$MATRICULA)))
data$MATRICULA <- gsub(pattern = " ", replacement = "", data$MATRICULA)
unique(data$MATRICULA)

## CAPACIDAD DE BODEGA -----------------------------------------------------
data$CB  <- abs(as.numeric(data$CB))
range(data$CB, na.rm = TRUE)
ggplot(data = data) +
  geom_histogram(aes(x = CB), binwidth = 30, 
                 fill = "red", color = "black") +
  labs(x = "Capacidad de bodega (m3)", y = "Frecuencia") +
  theme_minimal()

names(data)

## FLOTA -----------------------------------------------------------------
data$TIPO.DE.FLOTA <- toupper(trimws(as.character(data$TIPO.DE.FLOTA)))
unique(data$TIPO.DE.FLOTA)
table(data$TIPO.DE.FLOTA, useNA = "ifany")
data$MATRICULA[is.na(data$TIPO.DE.FLOTA)]
data$CB[is.na(data$TIPO.DE.FLOTA)]
names(data)

## PUERTO ------------------------------------------------------------------
data$PUERTO <- toupper(trimws(as.character(data$PUERTO)))
unique(data$PUERTO)
table(data$PUERTO, useNA = "ifany")
data$LAT[is.na(data$PUERTO)]
data$REGION[is.na(data$PUERTO)]
names(data)

## AREA --------------------------------------------------------------------
data$AREA <- abs(as.numeric(data$AREA))
unique(data$AREA)
table(data$AREA, useNA = "ifany")
data$LAT[is.na(data$AREA)]
data$LONG[is.na(data$AREA)]

names(data)

## DIA ---------------------------------------------------------------------
data$DIA  <- as.Date(data$DIA,format = "%Y-%m-%d")
unique(data$DIA)

## BIOMETRICO --------------------------------------------------------------
tmp <- data[, paste0("X", marks)]
tmp_numeric <- sapply(tmp, as.numeric)
nbiom <- rowSums(tmp_numeric, na.rm = TRUE)

range(nbiom, na.rm = TRUE)

ggplot() +
  geom_histogram(aes(x = nbiom), binwidth = 10, 
                 fill = "darkgreen", color = "black") +
  labs(x = "n ejemplares", y = "Frecuencia") +
  theme_minimal()

data_3 <- data

# 4. Standardized file 4 ----------------------------------------------------------------------------------------------
data <- read.csv(file4, header = T)
names(data)

## LONG LAT ---------------------
data$LONG  <- (-1 * abs( as.numeric(data$LONG)))
data$LAT  <- (-1 * abs( as.numeric(data$LAT)))

r1 <- range(data$LONG, na.rm = TRUE); r1
r2 <- range(data$LAT, na.rm = TRUE); r2

ggplot() +
  geom_sf(data = sf_land, fill = "white", color = "black") +
  geom_point(data = data, aes(x = LONG, y = LAT), 
             size = 1, color = "blue") +
  labs(title = "", x = "Longitud", y = "Latitud") +
  theme_bw() +
  scale_x_continuous(breaks = seq(trunc(r1[1]) - 3, trunc(r2[2]) + 3, 10)) +
  scale_y_continuous(breaks = seq(trunc(r1[2]) - 1, trunc(r2[2]) + 1, 5))  + 
  coord_sf(xlim = r1 + c(-10, +10), ylim = r2 + c(-1, +1))

data_LAT <- data[data$LAT >= -18.23 & data$LAT <= -3, ]
data_LAT_NOT <- data[!(data$LAT >= -18.23 & data$LAT <= -3), ]

data_LAT_NOT$DIA <- as.Date(data_LAT_NOT$DIA)
unique_years <- unique(format(data_LAT_NOT$DIA, "%Y"))
unique(unique_years)
unique(data_LAT_NOT$PUERTO)

atico_fil <- data_LAT_NOT[data_LAT_NOT$LONG >= -17 & data_LAT_NOT$LONG <= -16 
                               & data_LAT_NOT$LAT >= -75 & data_LAT_NOT$LAT <= -73 
                               & data_LAT_NOT$puerta == "ATICO", ]
atico_fil$LAT <- atico_fil$LONG
atico_fil$LONG <- atico_fil$LAT



## ESPECIE ----------
unique(data$ESPECIE)

## CAPTURA --------------------------------
data$CAPTURA..t. <- abs(as.numeric(data$CAPTURA..t.))
range(data$CAPTURA.t.)

solo_texto <- data[is.na(data$CAPTURA), ]
print(solo_texto)
unique(solo_texto$CAPTURA..t.)

data_clean <- data[is.finite(data$CAPTURA.t.), ]
range(data_clean$CAPTURA.t.)

ggplot(data = data) +
  geom_histogram(aes(x = CAPTURA..t.), binwidth = 30, 
                 fill = "blue", color = "black") +
  labs(x = "CAPTURA (t)", y = "Frecuencia") +
  theme_minimal()

names(data)
data[data$CAPTURA.t. == 0,]

## REGION  ------------------------------------------------------------------
data$REGION <- toupper(trimws(as.character(data$REGION)))
unique(data$REGION)
table(data$REGION, useNA = "ifany")

## FABRICA -----------------------------------------------------------------
data$FABRICA <- toupper(trimws(as.character(data$FABRICA)))
unique(data$FABRICA)
table(data$FABRICA, useNA = "ifany")

## MATRICULA ---------------------
data$MATRICULA <- toupper(trimws(as.character(data$MATRICULA)))
unique(data$MATRICULA)

## CAPACIDAD DE BODEGA -----------------------------------------------------
data$CB  <- abs(as.numeric(data$CB))
range(data$CB, na.rm = TRUE)
ggplot(data = data) +
  geom_histogram(aes(x = CB), binwidth = 30, 
                 fill = "red", color = "black") +
  labs(x = "Capacidad de bodega (m3)", y = "Frecuencia") +
  theme_minimal()

names(data)

## FLOTA -----------------------------------------------------------------
data$TIPO.DE.FLOTA <- toupper(trimws(as.character(data$TIPO.DE.FLOTA)))
unique(data$TIPO.DE.FLOTA)
table(data$TIPO.DE.FLOTA, useNA = "ifany")
data$MATRICULA[is.na(data$TIPO.DE.FLOTA)]
data$CB[is.na(data$TIPO.DE.FLOTA)]
names(data)

## PUERTO ------------------------------------------------------------------
data$PUERTO <- toupper(trimws(as.character(data$PUERTO)))
unique(data$PUERTO)
table(data$PUERTO, useNA = "ifany")
data$LAT[is.na(data$PUERTO)]
data$REGION[is.na(data$PUERTO)]
names(data)

## AREA --------------------------------------------------------------------
data$AREA <- abs(as.numeric(data$AREA))
unique(data$AREA)
table(data$AREA, useNA = "ifany")
data$LAT[is.na(data$AREA)]
data$LONG[is.na(data$AREA)]

# RECORDAR AL WILD LAS FUNCIONES 

names(data)

## DIA ---------------------------------------------------------------------
data$DIA  <- as.Date(data$DIA,format = "%Y-%m-%d")
unique(data$DIA)

## BIOMETRICO 
tmp <- data[, paste0("X", marks)]
tmp_numeric <- sapply(tmp, as.numeric)
nbiom <- rowSums(tmp_numeric, na.rm = TRUE)

range(nbiom, na.rm = TRUE)

ggplot() +
  geom_histogram(aes(x = nbiom), binwidth = 10, 
                 fill = "darkgreen", color = "black") +
  labs(x = "n ejemplares", y = "Frecuencia") +
  theme_minimal()


# fig <- tmp_numeric[which(nbiom == 498), ]
# fig[is.na(fig)] <- 0
# plot(marks, (fig), type = "l", lwd = 3)
