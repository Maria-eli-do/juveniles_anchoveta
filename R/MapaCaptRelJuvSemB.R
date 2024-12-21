rm(list = ls()); gc(reset = T)
# Cargar archivos y paqueterías necesarias
require(grDevices)
require(fenix)
require(ggplot2)
#install.packages("lubridate")
require(lubridate)
# devtools::install_github("PabloMBooster/fenix")
#base -----------------------------------------------------
file <- "cin/STANDARIZED_DATA_TOTAL_1972_2024.csv"
data <- read.csv(file, header = T, encoding = "UTF-8")
data$YEAR <- year(data$DIA)
data$MONTH <- month(data$DIA)
data$YEAR_SB <- data$YEAR
data$YEAR_SB[data$MONTH %in% 1:3] <- rep(data$YEAR - 1, length.out = sum(data$MONTH %in% 1:3))
data$SEMESTRE <- 1
data$SEMESTRE[data$MES %in% c(1:3,10:12)] <- 2
data$SemB <- paste0("Sem. ",data$YEAR_SB,"-",data$SEMESTRE)

#data <- data[data$SemB == "Sem. 2001-1",]

#juveniles------------------------------------------------------------------
a = 0.0036
b = 3.2380

marks <- seq(5, 20, .5)
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

base <- as.data.frame(data)

# Cargar datos Mapa Perú --------------------------------------------------
rbPal = colorRampPalette(c("#99CCCC", "#99CC33", "#00FF00","#006633",
                           "#FFFF00", "#FF8000", "#FF0000", "#800000", "black"))
PalCols = rbPal(101)


areaPeru = data.frame(x = c(linePeru$lon[1], -50, -50, linePeru$lon[23513:2], 
                            linePeru$lon[1]), y = c(linePeru$lat[1], -24, 0, 
                                                    linePeru$lat[23513:2], 
                                                    linePeru$lat[1]))
puertos = as.data.frame(puertosPeru)
datosPuertos = data.frame(lon = puertos$lon, 
                          lat = puertos$lat, 
                          names = as.character(puertos$puertos))
datosPuertos$names[15] <- "B. Independencia"

base$LONG = abs(base$LONG)
base$LAT = abs(base$LAT)

SB = unique(base$SemB)

data_Mapas_SemBiol = NULL
for (s in seq_along(SB)) {
  
  dato_SemBiol = base[base$SemB %in% SB[s],]
  
  
  grid = 1/12 # tamaño de grilla
  
  # Mapa relación Juv/Total
  
  xbins <- seq(70, 92, by = grid)
  ybins <- seq(4, 19, by = grid)
  
  
  require(dplyr)
  # Create a 2D binned statistic
  Resumen <- dato_SemBiol %>%
    mutate(
      bin_x = cut(LONG, breaks = xbins, labels = xbins[1:(length(xbins)-1)] + (grid)/2),
      bin_y = cut(LAT, breaks = ybins, labels = ybins[1:(length(ybins)-1)] + (grid)/2)
    ) %>%
    dplyr::group_by(bin_x, bin_y) %>%
    summarise(
      sum_Juv = sum(juv_p), #Nombre de columna de captura de juveniles
      sum_Ton = sum(CAPTURA..t.) #Nombre de columna de captura total
    ) %>%
    mutate(
      juvR = ifelse(sum_Juv == 0, NA, ifelse(sum_Ton != 0, sum_Juv / sum_Ton, NA))
    ) %>%
    ungroup()
  
  Resumen$bin_x <- as.numeric(as.character.factor(Resumen$bin_x))*-1
  Resumen$bin_y <- as.numeric(as.character.factor(Resumen$bin_y))*-1
  Resumen$juvR <- round(Resumen$juvR*100, 0)
  Resumen$SemB = SB[s]
  
  data_Mapas_SemBiol = rbind.data.frame(data_Mapas_SemBiol, Resumen)
}

#plot(data$LONG, data$LAT, xlim = c(-78.5, -78.0), ylim = c(-12, -10), xaxt = "n")
#axis(1, seq(-78.5, 78.2, .01), las = 2)
#abline(v = data_Mapas_SemBiol$bin_x, col = "red")
#abline(h = data_Mapas_SemBiol$bin_y, col = "red")


# Obtener los valores únicos de Semestre biológicos y dividirlos en grupos de 0
weeks_unicas <- as.vector(sort(unique(base$SemB)))
grupos_week <- split(weeks_unicas, ceiling(seq_along(weeks_unicas) / 16))

# Crear una lista para almacenar los gráficos
graficos <- list()

# Generar gráficos para cada grupo de SemB
for (i in seq_along(grupos_week)) {

  SemB_grupo <- grupos_week[[i]]
  
  # Filtrar los datos para el grupo de SemB
  datos_filtrados <- data_Mapas_SemBiol %>% filter(SemB %in% SemB_grupo)

  graficos[[i]] <- datos_filtrados %>% ggplot()+
  stat_summary_2d(aes(x = bin_x, y = bin_y, z = juvR),
                  fun = "sum", binwidth= 1/12)+
  facet_wrap(~SemB, ncol = 4, nrow = 4)+ 
  coord_fixed(xlim = c(-81,-72), ylim = c(-18,-4)) +
  geom_polygon(data = areaPeru, aes(x = x, y = y),
               fill = "khaki1", colour = "black") +
  geom_text(data = datosPuertos,
            mapping = aes(x = lon+0.15, y = lat, label = names), size = 6,
            hjust = 0, colour = "gray20")+
  scale_fill_gradientn(colours = PalCols, name = "Porc. (%)", limits = c(0,100))+
  theme_bw() +
  labs(title = "Captura relativa de juveniles por semana", x = "Longitud", y = "Latitud") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.caption = element_text(hjust = 0),
        legend.text = element_text(), 
        legend.title = element_text(),
        legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(1, 'cm')) +
  scale_y_continuous(breaks = c(-4,-6,-8,-10,-12,-14,-16,-18),
                     labels = c("4°S","6°S","8°S","10°S","12°S","14°S","16°S","18°S"))+
  scale_x_continuous(breaks = c(-81,-78,-75,-72),
                     labels = c("81°W","78°W","75°W","72°W")) +
  theme(text = element_text(size = 30))
  axis.text.x = element_text(, size = 24)
}  
 
# Guardar los gráficos
for (i in seq_along(graficos)) {
  ggsave(paste0("cout/MapaJuvenil_SemB_p", i, ".png"), plot = graficos[[i]], width = 16.5, height = 23.4, 
         units = "in")
}
