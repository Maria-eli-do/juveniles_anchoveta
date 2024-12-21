
# Cargar archivos y paqueterías necesarias
require(grDevices)
require(fenix)
require(ggplot2)
# En caso de no tener fenix, instalarlo
#devtools::install_github("PabloMBooster/fenix")


#base = read....

# Paleta de colores de moda
rbPalTM = colorRampPalette(c("#FF0000", "#FF0C00", "#FF2200", "#FF3C00",
                             "#FF5300", "#FF6D00", "#FF8400", "#FF9C00",
                             "#FFB500", "#FFCC00", "#FFE600", "#FFFF00",
                             "#5CFF00", "#00F20C", "#00D22C", "#00B34B",
                             "#00936B", "#006B8D", "#0050AE", "#0033CB"))
PalColsTM = rbPalTM(180)

# Cargar mapa del Peru
areaPeru = data.frame(x = c(linePeru$lon[1], -50, -50, linePeru$lon[23513:2], 
                            linePeru$lon[1]), y = c(linePeru$lat[1], -24, 0, 
                                                    linePeru$lat[23513:2], 
                                                    linePeru$lat[1]))
puertos = as.data.frame(puertosPeru)
datosPuertos = data.frame(lon = puertos$lon, 
                          lat = puertos$lat, 
                          names = as.character(puertos$puertos))
datosPuertos$names[15] <- "B. Independencia"


# Obtener los valores únicos de Semestre biológicos y dividirlos en grupos de 20
weeks_unicas <- unique(base$SemB)
grupos_week <- split(weeks_unicas, ceiling(seq_along(weeks_unicas) / 20))

# Crear una lista para almacenar los gráficos
graficos <- list()

# Generar gráficos para cada grupo de SemB
for (i in seq_along(grupos_week)) {
  SemB_grupo <- grupos_week[[i]]
  
  # Filtrar los datos para el grupo de SemB
  datos_filtrados <- base %>% filter(SemB %in% SemB_grupo)
  
  # Crear el gráfico
  graficos[[i]] <- datos_filtrados %>%
  geom_point(aes(long, lati, fill = moda), # colocar nombres de columnas de lon, lat y moda en la base
             size = 2, shape=21, col = "black") +   
  coord_fixed(xlim = c(-84,-70), ylim = c(-18.5,-4)) + 
  geom_polygon(data = areaPeru, aes(x = x, y = y),
               fill = "khaki1", colour = "black") +
  geom_text(data = datosPuertos,
            mapping = aes(x = lon+0.15, y = lat, label = names), size = 4.5, #ajustar tamaño del texto de los puertos
            hjust = 0, colour = "gray20")+
  scale_fill_gradientn(colours = PalColsTM,
                       name = "Moda (cm)",
                       limits = c(0,20), breaks = c(4,8,12,16), labels = c(4,8,12,16))+    
  theme_bw() +
  labs(title = "Modas ", x = "Longitud", y = "Latitud") +
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
  facet_wrap(~SemB, ncol = 5, nrow = 4)+ #colocar en lugar de SemB el semestre biológico
  theme(text = element_text(size = 30))
}

# Guardar los gráficos
for (i in seq_along(graficos)) {
  ggsave(paste0("MapaModa_SemB_p", i, ".png"), plot = graficos[[i]], width = 15, height = 10)
}

