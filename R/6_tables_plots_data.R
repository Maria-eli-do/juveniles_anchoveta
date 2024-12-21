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

