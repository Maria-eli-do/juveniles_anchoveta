rm(list = ls()); gc(reset = T)
# LIBRARY -----------------------------------------------------------------------------------------------------------------
require(lubridate); require(data.table)
require(ggplot2); require(cowplot)
require(ggmap); require(rnaturalearth)
require(tidyverse); require(dplyr)
require(r4fish); require(readxl)
library(modeest); require(fenix)

# SOURCE ----------------------------------------------------------------------------
USB <- "I:/"
dir_cin <- file.path(USB, "database/cin/")
dir_out <- file.path("cout/")
File   <- file.path(dir_cin, "DATA_JUVENILES_1972_2024.csv")
data   <- read.csv(File, encoding = "latin1", header = TRUE)
head(data)

## Figura por decadas

year <- data$YEAR + ((data$MES/12) - (1/12))
year <- (year %/% 10)*10
year <- paste0(year, "-", (year + 10 - 1))
year[year == "2020-2029"] <- "2020-2024"
table(year)

xx <- (data$JUV_N/data$CAPTURA_N)*100
yy <- (data$JUV_P/data$CAPTURA_P)*100
ggdf <- data.frame(year, año = factor(data$YEAR), 
                   mes = factor(data$MES), moda = data$MODA, xx, yy)

p1 <- ggplot(ggdf, aes(x = year, y = moda)) +
      geom_boxplot(fill = "#1F77B4", color = "black", outlier.size = 0.2, width = 0.5 ) +
      stat_summary( fun.data = function(x) {
        return(data.frame(y = max(x), label = length(x)))},
        geom = "text", color = "black", hjust = 0.5, vjust = -0.5) +
      labs(y = "Moda (cm)", x = "") +
      theme_bw() +
      ylim(5, 20.0) +
      theme(axis.text.x = element_blank(), 
            plot.margin = margin(t = 0.5, r = 2, b = 1, l = 4) )

p2 <- ggplot(ggdf, aes(x = year, y = xx)) +
      geom_boxplot(fill = "#FF7F0E", color = "black", outlier.size = 0.2, width = 0.5) +
      labs(y = "Juv numero (%)", x = "") +
      theme_bw() +
      theme(axis.text.x = element_blank(), 
            plot.margin = margin(t = 0.5, r = 2, b = 1, l = 4))

p3 <- ggplot(data, aes(x = year, y = yy)) +
      geom_boxplot(fill = "#2CA02C", color = "black", outlier.size = 0.2, width = 0.5) +
      labs(y = "Juv peso (%)", x = "") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 8), 
            plot.margin = margin(t = 0.5, r = 2, b = 1, l = 4))

p <- plot_grid(p1, p2, p3, ncol = 1, labels = c("a", "b", "c"))

filename <- file.path(dir_out, "boxplot_juv_modas_decades.png")
ggsave(filename, plot = p, width = 6.75*1.9, height = 9*1.9, dpi = 300, 
       units = "cm", bg = "white")


# Biometricos: año, puerto -----------------------------------------------------------------------------------------------------------------
t_year_puerto <- table(data$YEAR, data$PUERTO,  useNA = "ifany")
df_year_puerto <- as.data.frame(t_year_puerto)
years <- range(data$YEAR, na.rm = T)[1]:range(data$YEAR, na.rm = T)[2]
all_combinations <- expand.grid(Var1 = (years), Var2 = unique(data$PUERTO))
df_complete <- merge(all_combinations, df_year_puerto, by = c("Var1", "Var2"), all.x = TRUE)
df_complete$Freq[is.na(df_complete$Freq)] <- 0

df_wide <- df_complete %>%
  pivot_wider(names_from = Var2, values_from = Freq,
              values_fill = list(Freq = 0))

file <- "cout/matrix_año_puerto_biometricos_1972_2024.csv"
write.csv(df_wide, file, row.names = T)

df_complete$year <- as.numeric(as.character(df_complete$Var1))
df_complete$puerto <- (df_complete$Var2)

pp <- unique(df_complete$puerto)

vec <- c("Paita", "Parachique", "Pimentel", "Chicama", "Salaverry",  "Chimbote",
         "Samanco", "Casma", "Huarmey", "Supe", "Vegueta", "Huacho",
         "Chancay", "Callao", "Pucusana", "Tambo de Mora"  ,"Pisco", "Atico",
         "LA Planchada", "Mollendo",  "Ilo", NA)
vec <- toupper(vec)
df_complete$puerto <- factor(df_complete$puerto, levels = vec)

g1 <- ggplot(df_complete, aes(x = year, y = Freq, color = puerto)) + 
  geom_line() + 
  facet_wrap(~puerto, scales = "free", ncol = 3) +
  theme_minimal() + 
  labs(title = "", x = "Año", y = "Número de muestreos biometricos") +
  theme(legend.position = "none")

ggsave("cout/series_muestreos_biometricos.png", plot = g1, width = 6*1.2, height = 9*1.2, bg = "white")


# Captura: año, mes y puerto -----------------------------------------------------------------------------------------------------------------
length(data$DIA[is.na(data$TIPO_FLOTA)])

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

