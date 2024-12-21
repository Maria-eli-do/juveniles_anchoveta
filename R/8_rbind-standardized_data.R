rm(list = ls()); gc(reset = T)
# LIBRARY -----------------------------------------------------------------------------------------------------------------
require(lubridate); require(devtools)
require(ggplot2); require(cowplot)
require(ggmap); require(rnaturalearth); 
require(tidyverse); require(dplyr)
require(r4fish); require(readxl)
library(modeest)
library(data.table)

# install.packages("modeest")

# SOURCE ----------------------------------------------------------------------------
#source("R/3_standardized_data_1972_2014.R")
#source("R/4_standardized_data_2005-2024.R")
outFile    <- "cin/STANDARIZED_DATA_TOTAL_1972_2024.csv"
# rbind -----------------------------------------------------
fileone <- "cin/STANDARIZED_DATA_TOTAL_2005_2024.csv"
dataone <- read.csv(fileone, header = T, encoding = "UTF-8")
baseone <- as.data.frame(dataone)

filetwo <- "cin/STANDARIZED_DATA_1972_2014.csv"
datatwo <- read.csv(filetwo, header = T, encoding = "UTF-8")
basetwo <- as.data.frame(datatwo)

filethree <- "cin/STANDARIZED_IMARSIS_2016-2019.csv"
datathree <- read.csv(filethree, header = T, encoding = "UTF-8")
basethree <- as.data.frame(datathree)

filefour <- "cin/STANDARIZED_IMARSIS_2018-2021.csv"
datafour <- read.csv(filefour, header = T, encoding = "UTF-8")
basefour <- as.data.frame(datafour)

colnames(basetwo) <- colnames(baseone)
colnames(basethree) <- colnames(baseone)
colnames(basefour) <- colnames(baseone)
basetwo$MissingColumn <- NA
basethree$MissingColumn <- NA
basefour$MissingColumn <- NA

basetwo <- select(basetwo, colnames(baseone))
basethree <- select(basethree, colnames(baseone))
basefour <- select(basefour, colnames(baseone))
basetwo[, setdiff(colnames(baseone), colnames(basetwo))] <- NA
basethree[, setdiff(colnames(baseone), colnames(basethree))] <- NA
basefour[, setdiff(colnames(baseone), colnames(basefour))] <- NA

data <- rbind(baseone, basetwo)
#datareal <- <- rbind(baseone, basetwo, basethree, basefour)

data$DIA <- as.Date(data$DIA)
data$MES <- month(data$DIA)
sum(is.na(data$EMBARCACION)); sum(is.na(data$MATRICULA)); 

data$KEY <- paste(data$MATRICULA, data$PUERTO, data$DIA, sep = "-")
idk <- duplicated(data$KEY)
data <- data[!idk, ]

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
data$PUERTO[data$PUERTO == "QUILCA"] <- "MOLLENDO"
data$PUERTO[data$PUERTO == "ILO(GRAU)"] <- "ILO"
data$PUERTO[data$PUERTO == "SAMANCOS"] <- "SAMANCO"
unique(data$PUERTO)

#LIMPIEZA PARA IDONEIDAD DEL MUESTREO-------------------------------------------
nobs <- nrow(data)
data <- data[!is.na(data$AREA) & !is.na(data$LONG) & !is.na(data$LAT), ]
nobs2 <- nrow(data)

marks <- seq(5, 20, .5)
tmp <- data[, paste0("X", marks)]
tmp <- as.data.frame.list((lapply(tmp, as.numeric)))
data[, paste0("X", marks)] <- tmp
data$nAnc <- rowSums(data[, paste0("X", marks)], na.rm = T)
data <- data[data$nAnc >= 120, ]
nobs3 <- nrow(data)

data$CAPTURA..t. <- gsub(",", ".", data$CAPTURA..t.)
data$CAPTURA..t.[grepl("[a-zA-Z]", data$CAPTURA..t.)]
data$CAPTURA..t. <- as.numeric(data$CAPTURA..t.)
data <- data[which(!is.na(data$CAPTURA..t.) | data$CAPTURA..t. != 0), ]
nobs4 <- nrow(data)


sum(is.na(data$AREA))
data$LAT[data$LAT == 0] <- NA
data$LONG[data$LONG == 0] <- NA

areaISO <- read_xlsx("rawdat/areaISOEXCEL.xlsx")
data$AREA <- as.numeric(data$AREA)
areaISO$AREA <- as.numeric(areaISO$AREA)

idx <- which(areaISO$AREA %in% data$AREA)
data$LONG <- ifelse(is.na(data$LONG), areaISO$LONG[idx], data$LONG)
data$LAT <- ifelse(is.na(data$LAT), areaISO$LAT[idx], data$LAT)

data$LAT  <- -1*abs(data$LAT)
data$LONG <- -1*abs(data$LONG)

sum(is.na(data$LONG))
sum(is.na(data$LAT))

conditionLAT <- is.na(data$LAT) & !is.na(data$AREA)
conditionLONG <- is.na(data$LONG) & !is.na(data$AREA)
fileISO <- "rawdat/areaISO.csv"
dataISO <- read.csv(fileone, header = T, encoding = "UTF-8")
baseISO <- as.data.frame(dataISO)

data$LAT[conditionLAT] <- baseISO$LAT[which(baseISO$AREA %in% data$AREA[conditionLAT])]
data$LONG[conditionLONG] <- baseISO$LONG[which(baseISO$AREA %in% data$AREA[conditionLONG])]



#summary(data)
nobs; ((nobs2-nobs)/nobs)*100; ((nobs3-nobs)/nobs)*100; ((nobs4-nobs)/nobs)*100

moda <- as.matrix.data.frame(data[, paste0("X", marks)])
dimnames(moda) <- NULL
data$moda <- marks[apply(moda, 1, function(x) which.max(as.vector(x)) )]


# captura de juv
a = 0.0036
b = 3.2380

peso <- a*(marks^b)
plot(marks, peso)  

freqsim_num <- data[, paste0("X", marks)]
freqsim_pes <- sweep(freqsim_num, 2, peso, "*")
# freqsim_pes[852, ]; freqsim_num[852,]*peso 
peso_m <- rowSums(freqsim_pes, na.rm = T)
fx <- (data$CAPTURA..t.)*1e6/peso_m
hist((log(fx+1)))
freqsim_num_pon <- sweep(freqsim_num, 1, fx, "*")
# sum(freqsim_num_pon[654, ]*peso, na.rm = TRUE)/1E6; data$CAPTURA..t.[654]

data$CAPTURA..n. <- rowSums(freqsim_num_pon, na.rm = TRUE)  
data$juv_n <- rowSums(freqsim_num_pon[, marks < 12], na.rm = TRUE)  
temp_peso <- freqsim_num_pon[, marks < 12]  
temp_peso <- sweep(temp_peso, 2, peso[marks < 12], "*")
data$juv_p  <- rowSums(temp_peso, na.rm = TRUE)/1E6

#Eliminando datos anomalos LAT/LONG-----------------
require(fenix)
data$dc <- estima_dc2(lon = data$LONG, lat = data$LAT, polygon = PERU_SP)
hist(data$dc, border = "blue")

data$LAT[data$dc > 1000]
data$LONG[data$dc > 1000]

hist(data$dc[data$dc <= 160])
sum(data$dc > 160)*100 /nrow(data)

data <- data[data$dc <= 160, ]

write.csv(data, outFile, row.names = F)

year <- year(data$DIA) + (month(data$DIA)/12-(1/12))
year <- as.numeric(paste0((year %/% 10), "0"))
year <- paste0(year, "-", (year+10-1))
year[year == "2020-2029"] <- "2020-2024"

xx <- (data$juv_n/data$CAPTURA..n.)*100
yy <- (data$juv_p/data$CAPTURA..t.)*100


# plot
par(mfrow = c(3,1), mar = c(2.5,4,1,1))
boxplot(data$moda ~ year, col = "blue", lwd = 2, cex = 0.2, ylab = "Moda (cm)")
boxplot(xx~year, col = "red", lwd = 2, cex = 0.2, ylab = "% juv Numero")
boxplot(yy~year, col = "darkgreen", lwd = 2, cex = 0.2, ylab = "% juv Peso")

dev.copy(png, filename="cout/plot_juv.png", width=1000, height=1500, res=200)
dev.off()

plot(data$LAT , data$moda)


# Biometricos: año, puerto -----------------------------------------------------------------------------------------------------------------

str(data)
data$YEAR <- year(data$DIA)
# data$PUERTO <- factor(data$PUERTO)
# data$YEAR <- factor(data$YEAR)
# data$MES <- factor(data$MES)
t_year_puerto <- table(data$YEAR, data$PUERTO,  useNA = "ifany")
df_year_puerto <- as.data.frame(t_year_puerto)
years <- range(data$YEAR, na.rm = T)[1]:range(data$YEAR, na.rm = T)[2]
all_combinations <- expand.grid(Var1 = (years), Var2 = unique(data$PUERTO))

df_complete <- merge(all_combinations, df_year_puerto, by = c("Var1", "Var2"), all.x = TRUE)
df_complete$Freq[is.na(df_complete$Freq)] <- 0

df_wide <- df_complete %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = list(Freq = 0))

file <- "cout/matrix_año_puerto_biometricos_1972_2024.csv"
write.csv(df_wide, file, row.names = T)

df_complete$year <- as.numeric(as.character(df_complete$Var1))
df_complete$puerto <- (df_complete$Var2)

pp <- unique(df_complete$puerto)

vec <- c("Paita", "Parachique", "Pimentel", "Chicama", "Salaverry",  "Chimbote",
         "Samanco", "Casma", "Huarmey", "Supe", "Vegueta", "Huacho",
         "Chancay", "Callao", "Pucusana", "Tambo de Mora"  ,"Pisco", "Atico",
         "LA Planchada", "Mollendo",  "ILO", NA)
vec <- toupper(vec)
df_complete$puerto <- factor(df_complete$puerto, levels = vec)


g1 <- ggplot(df_complete, aes(x = year, y = Freq, color = puerto)) + 
  geom_line() + 
  facet_wrap(~puerto, scales = "free", ncol = 3) +
  theme_minimal() + 
  labs(title = "", x = "Año", y = "Número de muestreos biometricos") +
  theme(legend.position = "none")

ggsave("cout/muestreos_biometricos.png", plot = g1, width = 6*1.2, height = 9*1.2, bg = "white")


# Captura: año, mes y puerto -----------------------------------------------------------------------------------------------------------------
length(data$DIA[is.na(data$TIPO.DE.FLOTA)])

t_year_puerto <- table(data$YEAR, data$MES,  data$PUERTO, useNA = "ifany")
df_year_puerto <- as.data.frame(t_year_puerto)

df_wide <- df_year_puerto %>%
  pivot_wider(names_from = Var3, values_from = Freq)

years <- range(data$YEAR, na.rm = T)[1] : range(data$YEAR, na.rm = T)[2]
months <- 1:12  
all_combinations <- expand.grid(Var1 = (years), Var2 = months)
df_complete <- merge(all_combinations, df_wide, by = c("Var1", "Var2"), all.x = TRUE)

df_complete[which(is.na(df_complete), arr.ind = T)] <- 0
df_complete <- df_complete %>%
  arrange(Var1, Var2)

colnames(df_complete)[1:2] <- c("year", "month")



file <- "cout/matrix_año_mes_puerto_biometricos_1972_2024.csv"
write.csv(df_complete, file, row.names = T)

# data$PUERTO[which(data$YEAR == 1972)]

