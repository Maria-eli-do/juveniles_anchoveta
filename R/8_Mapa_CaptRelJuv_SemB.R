rm(list = ls()); gc(reset = T)
# -----------------------------------------------------------------------------------------------------------------
# Cargar archivos y paqueterías necesarias
require(grDevices)
require(fenix)
require(ggplot2)
require(lubridate)
require(dplyr)
# devtools::install_github("PabloMBooster/fenix")
# -----------------------------------------------------------------------------------------------------------------
USB <- "I:/"
dir_cin <- file.path(USB, "database/cin/")
dir_out <- file.path("cout/")
File   <- file.path(dir_cin, "DATA_JUVENILES_1972_2024.csv")
base   <- read.csv(File, encoding = "latin1", header = TRUE)

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
SB = unique(base$SEMB)

data_Mapas_SemBiol = NULL
for (s in seq_along(SB)) {
  print(SB[s])
  dato_SemBiol = base[base$SEMB %in% SB[s],]
  grid = 1/12 # tamaño de grilla
  
  # Mapa relación Juv/Total
  xbins <- seq(70, 92, by = grid)
  ybins <- seq(4, 19, by = grid)

  # Create a 2D binned statistic
  Resumen <- dato_SemBiol %>%
    mutate(
      bin_x = cut(LONG, breaks = xbins, labels = xbins[1:(length(xbins)-1)] + (grid)/2),
      bin_y = cut(LAT, breaks = ybins, labels = ybins[1:(length(ybins)-1)] + (grid)/2)
    ) %>%
    dplyr::group_by(bin_x, bin_y) %>%
    summarise(
      sum_Juv = sum(JUV_P), #Nombre de columna de captura de juveniles
      sum_Ton = sum(CAPTURA_P) #Nombre de columna de captura total
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
weeks_unicas <- as.vector(sort(unique(base$SEMB)))
grupos_week <- split(weeks_unicas, ceiling(seq_along(weeks_unicas) / 16))

# Crear una lista para almacenar los gráficos
graficos <- list()

# Generar gráficos para cada grupo de SemB
for (i in seq_along(grupos_week)) {

  SemB_grupo <- grupos_week[[i]]
  
  # Filtrar los datos para el grupo de SemB
  datos_filtrados <- data_Mapas_SemBiol %>% filter(SemB %in% SemB_grupo)

  graficos[[i]] <- datos_filtrados %>% ggplot() +
  stat_summary_2d(aes(x = bin_x, y = bin_y, z = juvR),
                  fun = "sum", binwidth= 1/12) +
  facet_wrap(~SemB, ncol = 4, nrow = 4)+ 
  coord_fixed(xlim = c(-84,-70), ylim = c(-18, -4)) + 
  geom_polygon(data = areaPeru, aes(x = x, y = y),
               fill = "khaki1", colour = "black") +
  geom_text(data = datosPuertos,
            mapping = aes(x = lon+ 0.15, y = lat, label = names), size = 3,
            hjust = 0, colour = "gray20") +
  scale_fill_gradientn(colours = PalCols, name = "Porc. (%)", limits = c(0,100)) +
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
    theme(text = element_text(size = 30), 
          axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 20))
}  
 
# Guardar los gráficos
for (i in seq_along(graficos)) {
  ggsave(file.path(dir_out, paste0("Mapas_Juveniles_SemB_p", i, ".png")), plot = graficos[[i]], 
         width = 16.5, height = 23.4, units = "in")
}
