library(extrafont)
library(readr)
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(lubridate)
library(treemapify)
library(viridis)
library(RColorBrewer)
library(waffle)
library(ggplot2)
library(eeptools)
library(sf)
library(janitor)
library(ggthemes)

options(scipen = 999) #"scipen": notación cientifica (una forma de abreviar numeros). Con esta linea de código configuramos R, de modo tal que solo muestre los valores en formato decimal

MI_DAS2_AX02 <- read_excel("data/nueva/MI_DAS2_AX02.xlsx")
#View(MI_DAS2_AX02)

#Cambio de nombre por facilidad
CABA_mt2 <- MI_DAS2_AX02

#elimino las filas que no interesan
CABA_mt2_2 <- CABA_mt2[-c(1,3,52,53,54,55), ]
#View(CABA_mt2_2)
#renombrar las celdas de la fila 1
CABA_mt2_2[1, 1] <- "Fecha"
CABA_mt2_2[1, 2] <- format(as.Date ("2013-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 3] <- format(as.Date ("2013-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 4] <- format(as.Date ("2013-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 5] <- format(as.Date ("2013-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 6] <- format(as.Date ("2013-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 7] <- format(as.Date ("2013-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 8] <- format(as.Date ("2014-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 9] <- format(as.Date ("2014-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 10] <- format(as.Date ("2014-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 11] <- format(as.Date ("2014-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 12] <- format(as.Date ("2014-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 13] <- format(as.Date ("2014-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 14] <- format(as.Date ("2014-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 15] <- format(as.Date ("2014-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 16] <- format(as.Date ("2014-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 17] <- format(as.Date ("2014-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 18] <- format(as.Date ("2014-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 19] <- format(as.Date ("2014-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 20] <- format(as.Date ("2015-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 21] <- format(as.Date ("2015-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 22] <- format(as.Date ("2015-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 23] <- format(as.Date ("2015-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 24] <- format(as.Date ("2015-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 25] <- format(as.Date ("2015-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 26] <- format(as.Date ("2015-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 27] <- format(as.Date ("2015-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 28] <- format(as.Date ("2015-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 29] <- format(as.Date ("2015-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 30] <- format(as.Date ("2015-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 31] <- format(as.Date ("2015-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 32] <- format(as.Date ("2016-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 33] <- format(as.Date ("2016-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 34] <- format(as.Date ("2016-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 35] <- format(as.Date ("2016-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 36] <- format(as.Date ("2016-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 37] <- format(as.Date ("2016-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 38] <- format(as.Date ("2016-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 39] <- format(as.Date ("2016-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 40] <- format(as.Date ("2016-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 41] <- format(as.Date ("2016-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 42] <- format(as.Date ("2016-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 43] <- format(as.Date ("2016-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 44] <- format(as.Date ("2017-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 45] <- format(as.Date ("2017-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 46] <- format(as.Date ("2017-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 47] <- format(as.Date ("2017-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 48] <- format(as.Date ("2017-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 49] <- format(as.Date ("2017-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 50] <- format(as.Date ("2017-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 51] <- format(as.Date ("2017-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 52] <- format(as.Date ("2017-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 53] <- format(as.Date ("2017-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 54] <- format(as.Date ("2017-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 55] <- format(as.Date ("2017-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 56] <- format(as.Date ("2018-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 57] <- format(as.Date ("2018-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 58] <- format(as.Date ("2018-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 59] <- format(as.Date ("2018-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 60] <- format(as.Date ("2018-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 61] <- format(as.Date ("2018-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 62] <- format(as.Date ("2018-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 63] <- format(as.Date ("2018-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 64] <- format(as.Date ("2018-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 65] <- format(as.Date ("2018-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 66] <- format(as.Date ("2018-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 67] <- format(as.Date ("2018-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 68] <- format(as.Date ("2019-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 69] <- format(as.Date ("2019-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 70] <- format(as.Date ("2019-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 71] <- format(as.Date ("2019-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 72] <- format(as.Date ("2019-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 73] <- format(as.Date ("2019-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 74] <- format(as.Date ("2019-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 75] <- format(as.Date ("2019-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 76] <- format(as.Date ("2019-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 77] <- format(as.Date ("2019-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 78] <- format(as.Date ("2019-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 79] <- format(as.Date ("2019-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 80] <- format(as.Date ("2020-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 81] <- format(as.Date ("2020-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 82] <- format(as.Date ("2020-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 83] <- format(as.Date ("2020-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 84] <- format(as.Date ("2020-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 85] <- format(as.Date ("2020-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 86] <- format(as.Date ("2020-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 87] <- format(as.Date ("2020-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 88] <- format(as.Date ("2020-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 89] <- format(as.Date ("2020-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 90] <- format(as.Date ("2020-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 91] <- format(as.Date ("2020-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 92] <- format(as.Date ("2021-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 93] <- format(as.Date ("2021-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 94] <- format(as.Date ("2021-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 95] <- format(as.Date ("2021-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 96] <- format(as.Date ("2021-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 97] <- format(as.Date ("2021-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 98] <- format(as.Date ("2021-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 99] <- format(as.Date ("2021-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 100] <- format(as.Date ("2021-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 101] <- format(as.Date ("2021-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 102] <- format(as.Date ("2021-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 103] <- format(as.Date ("2021-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 104] <- format(as.Date ("2022-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 105] <- format(as.Date ("2022-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 106] <- format(as.Date ("2022-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 107] <- format(as.Date ("2022-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 108] <- format(as.Date ("2022-5-1"), "%Y-%m-%d")
CABA_mt2_2[1, 109] <- format(as.Date ("2022-6-1"), "%Y-%m-%d")
CABA_mt2_2[1, 110] <- format(as.Date ("2022-7-1"), "%Y-%m-%d")
CABA_mt2_2[1, 111] <- format(as.Date ("2022-8-1"), "%Y-%m-%d")
CABA_mt2_2[1, 112] <- format(as.Date ("2022-9-1"), "%Y-%m-%d")
CABA_mt2_2[1, 113] <- format(as.Date ("2022-10-1"), "%Y-%m-%d")
CABA_mt2_2[1, 114] <- format(as.Date ("2022-11-1"), "%Y-%m-%d")
CABA_mt2_2[1, 115] <- format(as.Date ("2022-12-1"), "%Y-%m-%d")
CABA_mt2_2[1, 116] <- format(as.Date ("2023-1-1"), "%Y-%m-%d")
CABA_mt2_2[1, 117] <- format(as.Date ("2023-2-1"), "%Y-%m-%d")
CABA_mt2_2[1, 118] <- format(as.Date ("2023-3-1"), "%Y-%m-%d")
CABA_mt2_2[1, 119] <- format(as.Date ("2023-4-1"), "%Y-%m-%d")
CABA_mt2_2[1, 120] <- format(as.Date ("2023-5-1"), "%Y-%m-%d")
#View(CABA_mt2_2)

#transponer el df
CABA_mt2_transpuesta <- t(CABA_mt2_2)
#View(CABA_mt2_transpuesta)

#view(CABA_mt2_2)
#ahora
Nuevos_nombres_columnas <- as.character(CABA_mt2_transpuesta[1, ])
# Funcion colnames para cambiar el nombre de columnas
colnames(CABA_mt2_transpuesta) <- Nuevos_nombres_columnas
#View(CABA_mt2_transpuesta)
#eliminar la fila 1
CABA_mt2_transpuesta_2 <- CABA_mt2_transpuesta[-c(1), ]
#View(CABA_mt2_transpuesta_2)

# Convertir la matriz de caracteres en un dataframe
df_CABA_mt2_transpuesta <- as.data.frame(CABA_mt2_transpuesta_2, stringsAsFactors = FALSE)

colnames(df_CABA_mt2_transpuesta)
#que se lean las columnas como valores numericos
df_CABA_mt2_transpuesta$Agronomia <- as.numeric(df_CABA_mt2_transpuesta$Agronomia)
df_CABA_mt2_transpuesta$Almagro <- as.numeric(df_CABA_mt2_transpuesta$Almagro)
df_CABA_mt2_transpuesta$Balvanera <- as.numeric(df_CABA_mt2_transpuesta$Balvanera)
df_CABA_mt2_transpuesta$Barracas <- as.numeric(df_CABA_mt2_transpuesta$Barracas)
df_CABA_mt2_transpuesta$Belgrano <- as.numeric(df_CABA_mt2_transpuesta$Belgrano)
df_CABA_mt2_transpuesta$Boca <- as.numeric(df_CABA_mt2_transpuesta$Boca)
df_CABA_mt2_transpuesta$Boedo <- as.numeric(df_CABA_mt2_transpuesta$Boedo)
df_CABA_mt2_transpuesta$Caballito <- as.numeric(df_CABA_mt2_transpuesta$Caballito)
df_CABA_mt2_transpuesta$Chacarita <- as.numeric(df_CABA_mt2_transpuesta$Chacarita)
df_CABA_mt2_transpuesta$Coghlan <- as.numeric(df_CABA_mt2_transpuesta$Coghlan)
df_CABA_mt2_transpuesta$Colegiales <- as.numeric(df_CABA_mt2_transpuesta$Colegiales)
df_CABA_mt2_transpuesta$Constitución <- as.numeric(df_CABA_mt2_transpuesta$Constitución)
df_CABA_mt2_transpuesta$Flores <- as.numeric(df_CABA_mt2_transpuesta$Flores)
df_CABA_mt2_transpuesta$Floresta <- as.numeric(df_CABA_mt2_transpuesta$Floresta)
df_CABA_mt2_transpuesta$"La Paternal" <- as.numeric(df_CABA_mt2_transpuesta$"La Paternal")
df_CABA_mt2_transpuesta$Liniers <- as.numeric(df_CABA_mt2_transpuesta$Liniers)
df_CABA_mt2_transpuesta$Mataderos <- as.numeric(df_CABA_mt2_transpuesta$Mataderos)
df_CABA_mt2_transpuesta$"Monte Castro" <- as.numeric(df_CABA_mt2_transpuesta$"Monte Castro")
df_CABA_mt2_transpuesta$Montserrat <- as.numeric(df_CABA_mt2_transpuesta$Montserrat)
df_CABA_mt2_transpuesta$"Nueva Pompeya" <- as.numeric(df_CABA_mt2_transpuesta$"Nueva Pompeya")
df_CABA_mt2_transpuesta$Núñez <- as.numeric(df_CABA_mt2_transpuesta$Núñez)
df_CABA_mt2_transpuesta$Palermo <- as.numeric(df_CABA_mt2_transpuesta$Palermo)
df_CABA_mt2_transpuesta$"Parque Avellaneda" <- as.numeric(df_CABA_mt2_transpuesta$"Parque Avellaneda")
df_CABA_mt2_transpuesta$"Parque Chacabuco" <- as.numeric(df_CABA_mt2_transpuesta$"Parque Chacabuco")
df_CABA_mt2_transpuesta$"Parque Chas" <- as.numeric(df_CABA_mt2_transpuesta$"Parque Chas")
df_CABA_mt2_transpuesta$"Parque Patricios" <- as.numeric(df_CABA_mt2_transpuesta$"Parque Patricios")
df_CABA_mt2_transpuesta$"Puerto Madero" <- as.numeric(df_CABA_mt2_transpuesta$"Puerto Madero")
df_CABA_mt2_transpuesta$Recoleta <- as.numeric(df_CABA_mt2_transpuesta$Recoleta)
df_CABA_mt2_transpuesta$Retiro <- as.numeric(df_CABA_mt2_transpuesta$Retiro)
df_CABA_mt2_transpuesta$Saavedra <- as.numeric(df_CABA_mt2_transpuesta$Saavedra)
df_CABA_mt2_transpuesta$"San Cristobal" <- as.numeric(df_CABA_mt2_transpuesta$"San Cristobal")
df_CABA_mt2_transpuesta$"San Nicolás" <- as.numeric(df_CABA_mt2_transpuesta$"San Nicolás")
df_CABA_mt2_transpuesta$"San Telmo" <- as.numeric(df_CABA_mt2_transpuesta$"San Telmo")
df_CABA_mt2_transpuesta$"Vélez Sársfield" <- as.numeric(df_CABA_mt2_transpuesta$"Vélez Sársfield")
df_CABA_mt2_transpuesta$Versalles <- as.numeric(df_CABA_mt2_transpuesta$Versalles)
df_CABA_mt2_transpuesta$"Villa Crespo" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Crespo")
df_CABA_mt2_transpuesta$"Villa del Parque" <- as.numeric(df_CABA_mt2_transpuesta$"Villa del Parque")
df_CABA_mt2_transpuesta$"Villa Devoto" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Devoto")
df_CABA_mt2_transpuesta$"Villa Gral. Mitre" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Gral. Mitre")
df_CABA_mt2_transpuesta$"Villa Lugano" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Lugano")
df_CABA_mt2_transpuesta$"Villa Luro" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Luro")
df_CABA_mt2_transpuesta$"Villa Ortúzar" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Ortúzar")
df_CABA_mt2_transpuesta$"Villa Pueyrredón" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Pueyrredón")
df_CABA_mt2_transpuesta$"Villa Real" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Real")
df_CABA_mt2_transpuesta$"Villa Riachuelo" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Riachuelo")
df_CABA_mt2_transpuesta$"Villa Santa Rita" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Santa Rita")
df_CABA_mt2_transpuesta$"Villa Soldati" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Soldati")
df_CABA_mt2_transpuesta$"Villa Urquiza" <- as.numeric(df_CABA_mt2_transpuesta$"Villa Urquiza")
#que se lea la columna "Fecha" como fecha
df_CABA_mt2_transpuesta$Fecha <- as.Date(df_CABA_mt2_transpuesta$Fecha)

#intento fallido: sumar al df una columna con el promedio
#df_CABA_mt2_transpuesta$Mts_Promedio <- rowMeans(df_CABA_mt2_transpuesta[, -1], na.rm = TRUE, start = 2)
#df_CABA_mt2_transpuesta$Mts_Promedio <- apply(df_CABA_mt2_transpuesta[, -1], 1, function(row) mean(row[-1], na.rm = TRUE))

#crear un df con los promedios de mts2 en caba
df_CABA_mt2_transpuesta_menos_fecha <- subset(df_CABA_mt2_transpuesta, select = -Fecha)
#View(df_CABA_mt2_transpuesta_menos_fecha)
df_CABA_mt2_transpuesta_menos_fecha$Promedio <- rowMeans(df_CABA_mt2_transpuesta_menos_fecha)
#View(df_CABA_mt2_transpuesta_menos_fecha)
df_CABA_promedios_mts2 <- select(df_CABA_mt2_transpuesta_menos_fecha, "Promedio")

df_CABA_mt2_final <- cbind(df_CABA_mt2_transpuesta,df_CABA_promedios_mts2)
View(df_CABA_mt2_final)

MT2_PALERMO_LINEA <- ggplot() +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Palermo, group = 1, color = "Palermo")) +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Recoleta, group = 1, color = "Recoleta")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = San Nicolas, group = 1, color = "San Nicolas")) +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Belgrano, group = 1, color = "Belgrano")) +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Retiro, group = 1, color = "Retiro")) +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Promedio, group = 1, color = "Promedio")) +
  labs(title = "Evolución de la oferta de mts2 de alquiler en CABA",
       x = "Tiempo",
       y = "Mt2",
       caption = "Fuente: Base de datos de Estadísticas y Censos de Ciudad de Buenos Aires") +
  scale_color_manual(values = c("black", "blue", "green", "red", "orange"), guide = FALSE) +
  guides(color = guide_legend(title = "")) +
  theme_bw() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y")


MT2_PALERMO_LINEA


'''
nos metemos con datos de AIRBNB
'''
mar23<-Fcantidad_x_barrio %>%
  st_drop_geometry() %>%
  select(-comuna) %>%
  rename(mar23= cantidad)
#View(mar23)
#View(Fcantidad_x_barrio)
dic22 <- read_csv('data/nueva/diciembre22.csv') %>%
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood=tolower(.$neighbourhood)) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad) %>%
  rename(dic22= cantidad)

sep22<- read_csv('data/nueva/septiembre22.csv') %>%
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood=tolower(.$neighbourhood)) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad)%>%
  rename(sep22= cantidad)

jun22<- read_csv('data/nueva/junio22.csv') %>%
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood=tolower(.$neighbourhood)) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad)%>%
  rename(jun22= cantidad)

air2021<- read_csv('data/nueva/2021.csv') %>%  #rotooooo
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood=tolower(.$neighbourhood_cleansed)) %>%
  select(-neighbourhood_cleansed) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad) %>%
  rename('2021'= cantidad)


air2020<- read_csv('data/nueva/dic 2020.csv') %>%  #rarooo
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood=tolower(.$neighbourhood)) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad) %>%
  rename('2020'= cantidad)

air2019<- read_csv('data/nueva/2019.csv') %>%  #parece funcar
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood = stri_trans_general(neighbourhood, "Latin-ASCII")) %>%
  mutate(neighbourhood=tolower(.$neighbourhood)) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad) %>%
  rename('2019'= cantidad)

#se decide no usar datasets de 2018 porque muchos registros vienen sin barrio, lo que sesga el analisis
'''
  air2018<- read_csv('data/nueva/2018.csv')  
  filter(room_type =='Entire home/apt') %>%
  mutate(neighbourhood=tolower(.$address)) %>%
  select(-neighborhood,-country,-city,-address)  
 
 '''

air2017 <- read_csv('data/nueva/jul2017.csv') %>%  #este dataset tiene barrios de mas que se van a perder cuando hagamos left join y eso es lo que se busca
  filter(room_type == 'Entire home/apt') %>%
  mutate(neighbourhood = stri_trans_general(neighbourhood, "Latin-ASCII")) %>%
  mutate(neighbourhood=tolower(.$neighbourhood)) %>%
  select(-country, -city, -address) %>%
  group_by(neighbourhood) %>%
  summarise(cantidad = n(), .groups = 'drop') %>%
  arrange(-cantidad) %>%
  rename('2017'= cantidad)


#joinsss

evolucion <- air2017 %>% #se perdio nuñez, y se por que es. seguir viendo errores
  right_join(air2019, by='neighbourhood') %>%
  right_join(air2020, by='neighbourhood') %>%
  right_join(air2021, by='neighbourhood') %>%
  right_join(jun22, by='neighbourhood') %>%
  right_join(sep22, by='neighbourhood') %>%
  right_join(dic22, by='neighbourhood') %>%
  right_join(mar23, by='neighbourhood')
View(evolucion)
#####################################
#####################################

#graficar airbnb (evolucion de oferta)
#1er paso: crear un nuevo df con el nombre de las columnas (las fechas) (en el df final la fecha de la medicion será un valor dentro de la columna "Fecha")
#colnames(evolucion)
Fechas <- c("Fechas","2017-1-1","2019-1-1","2020-1-1","2021-1-1","2022-6-1","2022-9-1","2022-12-1", "2023-3-1")
#2do paso: transponer el df original
evolucion_transpuesta <- t(evolucion)
#3er paso: unir ambos df
EVOLUCION_CON_FECHAS <- cbind(Fechas, evolucion_transpuesta)
#4to paso: reemplazar el nombre de las columnas por los valores de la primera fila
Nuevos_nombres_columnas <- as.character(unlist(EVOLUCION_CON_FECHAS[1, ]))
colnames(EVOLUCION_CON_FECHAS) <- Nuevos_nombres_columnas
#5to paso: eliminar la primera fila del df
EVOLUCION_CON_FECHAS <- EVOLUCION_CON_FECHAS[-1, ]
#View(EVOLUCION_CON_FECHAS)

# Convertir la matriz de caracteres en un dataframe
df_EVOLUCION_CON_FECHAS <- as.data.frame(EVOLUCION_CON_FECHAS, stringsAsFactors = FALSE)
colnames(df_EVOLUCION_CON_FECHAS)

# hacer que los valores de la columna "Fechas" sean leidos como tales
df_EVOLUCION_CON_FECHAS$Fechas <- as.Date(df_EVOLUCION_CON_FECHAS$Fechas)
# Hacer que los valores de las columnas restantes sean leidos como valores numericos
df_EVOLUCION_CON_FECHAS$palermo <- as.numeric(df_EVOLUCION_CON_FECHAS$palermo)
df_EVOLUCION_CON_FECHAS$recoleta <- as.numeric(df_EVOLUCION_CON_FECHAS$recoleta)
df_EVOLUCION_CON_FECHAS$retiro <- as.numeric(df_EVOLUCION_CON_FECHAS$retiro)
df_EVOLUCION_CON_FECHAS$"san nicolas" <- as.numeric(df_EVOLUCION_CON_FECHAS$"san nicolas")
df_EVOLUCION_CON_FECHAS$"san telmo" <- as.numeric(df_EVOLUCION_CON_FECHAS$"san telmo")
df_EVOLUCION_CON_FECHAS$monserrat <- as.numeric(df_EVOLUCION_CON_FECHAS$monserrat)
df_EVOLUCION_CON_FECHAS$belgrano <- as.numeric(df_EVOLUCION_CON_FECHAS$belgrano)
df_EVOLUCION_CON_FECHAS$balvanera <- as.numeric(df_EVOLUCION_CON_FECHAS$balvanera)
df_EVOLUCION_CON_FECHAS$almagro <- as.numeric(df_EVOLUCION_CON_FECHAS$almagro)
df_EVOLUCION_CON_FECHAS$colegiales <- as.numeric(df_EVOLUCION_CON_FECHAS$colegiales)
df_EVOLUCION_CON_FECHAS$caballito <- as.numeric(df_EVOLUCION_CON_FECHAS$caballito)
df_EVOLUCION_CON_FECHAS$chacarita <- as.numeric(df_EVOLUCION_CON_FECHAS$chacarita)
df_EVOLUCION_CON_FECHAS$barracas <- as.numeric(df_EVOLUCION_CON_FECHAS$barracas)
df_EVOLUCION_CON_FECHAS$saavedra <- as.numeric(df_EVOLUCION_CON_FECHAS$saavedra)
df_EVOLUCION_CON_FECHAS$boedo <- as.numeric(df_EVOLUCION_CON_FECHAS$boedo)
df_EVOLUCION_CON_FECHAS$"parque chas" <- as.numeric(df_EVOLUCION_CON_FECHAS$"parque chas")
df_EVOLUCION_CON_FECHAS$coghlan <- as.numeric(df_EVOLUCION_CON_FECHAS$coghlan)
df_EVOLUCION_CON_FECHAS$flores <- as.numeric(df_EVOLUCION_CON_FECHAS$flores)
df_EVOLUCION_CON_FECHAS$"villa crespo" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa crespo")
df_EVOLUCION_CON_FECHAS$"puerto madero" <- as.numeric(df_EVOLUCION_CON_FECHAS$"puerto madero")
df_EVOLUCION_CON_FECHAS$"villa urquiza" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa urquiza")
df_EVOLUCION_CON_FECHAS$floresta <- as.numeric(df_EVOLUCION_CON_FECHAS$floresta)
df_EVOLUCION_CON_FECHAS$"san cristobal" <- as.numeric(df_EVOLUCION_CON_FECHAS$"san cristobal")
df_EVOLUCION_CON_FECHAS$agronomia <- as.numeric(df_EVOLUCION_CON_FECHAS$agronomia)
df_EVOLUCION_CON_FECHAS$mataderos <- as.numeric(df_EVOLUCION_CON_FECHAS$mataderos)
df_EVOLUCION_CON_FECHAS$constitucion <- as.numeric(df_EVOLUCION_CON_FECHAS$constitucion)
df_EVOLUCION_CON_FECHAS$"villa ortuzar" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa ortuzar")
df_EVOLUCION_CON_FECHAS$"villa real" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa real")
df_EVOLUCION_CON_FECHAS$"monte castro" <- as.numeric(df_EVOLUCION_CON_FECHAS$"monte castro")
df_EVOLUCION_CON_FECHAS$"parque avellaneda" <- as.numeric(df_EVOLUCION_CON_FECHAS$"parque avellaneda")
df_EVOLUCION_CON_FECHAS$"parque patricios" <- as.numeric(df_EVOLUCION_CON_FECHAS$"parque patricios")
df_EVOLUCION_CON_FECHAS$"villa devoto" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa devoto")
df_EVOLUCION_CON_FECHAS$"villa pueyrredon" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa pueyrredon")
df_EVOLUCION_CON_FECHAS$"villa del parque" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa del parque")
df_EVOLUCION_CON_FECHAS$"parque chacabuco" <- as.numeric(df_EVOLUCION_CON_FECHAS$"parque chacabuco")
df_EVOLUCION_CON_FECHAS$liniers <- as.numeric(df_EVOLUCION_CON_FECHAS$liniers)
df_EVOLUCION_CON_FECHAS$"velez sarsfield" <- as.numeric(df_EVOLUCION_CON_FECHAS$"velez sarsfield")
df_EVOLUCION_CON_FECHAS$"villa luro" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa luro")
df_EVOLUCION_CON_FECHAS$"villa santa rita" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa santa rita")
df_EVOLUCION_CON_FECHAS$"nueva pompeya" <- as.numeric(df_EVOLUCION_CON_FECHAS$"nueva pompeya")
df_EVOLUCION_CON_FECHAS$versalles <- as.numeric(df_EVOLUCION_CON_FECHAS$versalles)
df_EVOLUCION_CON_FECHAS$"villa lugano" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa lugano")
df_EVOLUCION_CON_FECHAS$"villa riachuelo" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa riachuelo")
df_EVOLUCION_CON_FECHAS$nuñez <- as.numeric(df_EVOLUCION_CON_FECHAS$nuñez)
df_EVOLUCION_CON_FECHAS$boca <- as.numeric(df_EVOLUCION_CON_FECHAS$boca)
df_EVOLUCION_CON_FECHAS$"villa gral. mitre" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa gral. mitre")
df_EVOLUCION_CON_FECHAS$paternal <- as.numeric(df_EVOLUCION_CON_FECHAS$paternal)
df_EVOLUCION_CON_FECHAS$"villa soldati" <- as.numeric(df_EVOLUCION_CON_FECHAS$"villa soldati")

View(df_EVOLUCION_CON_FECHAS)


AIRBNB_EVOLUCION_OFERTA <- ggplot() +
  geom_line(data = df_EVOLUCION_CON_FECHAS, aes(x = Fechas, y = palermo, group = 1, color = "Cant. Airbnb's")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Recoleta, group = 1, color = "Recoleta")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = San Nicolas, group = 1, color = "San Nicolas")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Belgrano, group = 1, color = "Belgrano")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Retiro, group = 1, color = "Retiro")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Promedio, group = 1, color = "Promedio")) +
  labs(title = "Evolución de la oferta de airbnb's en CABA",
       x = "Tiempo",
       y = "Cantidad",
       caption = "Fuente: Inside airbnb") +
  scale_color_manual(values = c("black", "blue", "green", "red", "orange"), guide = FALSE) +
  guides(color = guide_legend(title = "")) +
  theme_bw()
#scale_x_date(date_breaks = "12 months", date_labels = "%b %Y")


AIRBNB_EVOLUCION_OFERTA <- ggplot() +
  geom_line(data = df_EVOLUCION_CON_FECHAS, aes(x = Fechas, y = palermo, group = 1, color = "Cant. Airbnb's")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Recoleta, group = 1, color = "Recoleta")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = San Nicolas, group = 1, color = "San Nicolas")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Belgrano, group = 1, color = "Belgrano")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Retiro, group = 1, color = "Retiro")) +
  #geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Promedio, group = 1, color = "Promedio")) +
  labs(title = "Evolución de la oferta de airbnb's en CABA",
       x = "Tiempo",
       y = "Cantidad",
       caption = "Fuente: Inside airbnb") +
  scale_color_manual(values = c("black", "blue", "green", "red", "orange"), guide = FALSE) +
  guides(color = guide_legend(title = "")) +
  theme_bw()
#scale_x_date(date_breaks = "12 months", date_labels = "%b %Y")


####Cruzamiento de variables

'''
Llegados este punto, cruzaremos m2 de departamentos publicados segun el GCBA y cantidad de AIRBNB 
para Palermo y Recoleta a traves del tiempo
'''

####PALERMO####

#analizamos los rangos de nuestras variables para encontrar un "factor de conversion"
range(df_CABA_mt2_final$Palermo)/3
range(df_EVOLUCION_CON_FECHAS$palermo)

#definimos el factor de conversion para nuestras escalas en Palermo
factorP<-3

GRAFICO_UNIDO_PRUEBA_PALERMO <- ggplot() +
  geom_line(data = df_EVOLUCION_CON_FECHAS, aes(x = Fechas, y = palermo, group = 1, color = "Cant. Airbnb's")) +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Palermo/factorP, group = 1, color = "Nuevos mts2 publicados para alquiler convencional")) + #dividimos la variable por el factor de conversion
  labs(x = "Fecha", y = "Cantidad de AIRBNBs publicados") +
  scale_y_continuous(breaks = c(1000,2000,3000,4000,5000,6000,7000),sec.axis = sec_axis(trans = ~.*factorP, name = "Nuevos mts2 publicados para alquiler convencional")) + #creamos un segundo eje y le aplicamos el factor de conversion.
  guides(color = guide_legend(title = "")) +
  ggtitle("Evolución oferta de alquileres en Palermo") +
  theme_bw()
GRAFICO_UNIDO_PRUEBA_PALERMO

####RECOLETA####
range(df_CABA_mt2_final$Recoleta)/5
range(df_EVOLUCION_CON_FECHAS$recoleta)

#definimos el factor de conversion para nuestras escalas en Recoleta
factorR<-5

GRAFICO_UNIDO_PRUEBA_RECOLETA <- ggplot() +
  geom_line(data = df_EVOLUCION_CON_FECHAS, aes(x = Fechas, y = recoleta, group = 1, color = "Cant. Airbnb's")) +
  geom_line(data = df_CABA_mt2_final, aes(x = Fecha, y = Recoleta/factorR, group = 1, color = "Nuevos mts2 publicados para alquiler convencional")) +
  labs(x = "Fecha", y = "Cantidad de AIRBNBs publicados") +
  scale_y_continuous(breaks = c(1000,2000,3000),sec.axis = sec_axis(trans = ~.*factorR, name = "Nuevos mts2 publicados para alquiler convencional")) +
  guides(color = guide_legend(title = "")) +
  ggtitle("Evolución oferta de alquileres en Recoleta") +
  theme_bw()
GRAFICO_UNIDO_PRUEBA_RECOLETA


#Modificamos el grafico palercaba de la entrega anterior para quedarnos solo con palermo
QUE_CONVIENE2 <- read.csv("data/QUE_CONVIENE.csv") %>% #levantamos la tabla elaborada en el script PRINCIPAL
filter(neighbourhood=='palermo') %>% 
  select(-X)

plot_QUE_CONVIENE2<-QUE_CONVIENE2%>% #ploteamos para poder comparar
  ggplot(aes(reorder(neighbourhood, precio_promedio),precio_promedio,fill=tipo ))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  coord_flip()+
  labs(x='',y="ALQUILER MENSUAL PROMEDIO EN PESOS ARGENTINOS",
       title = "Precio promedio de alquiler mensual en Palermo",
       fill= "Tipo",
       caption = "Fuente: inside Airbnb y GCBA",
       legend ="")+
  theme_economist()

ggsave(plot = plot_QUE_CONVIENE2, filename = "output/queconviene2.png", 
       width = 40, height = 20, units = "cm")


##comprobacion del bajo nivel de control sobre el alquiler temporario
#cargamos datos caba
Registro_de_ATTs <- read_excel("data/nueva/Registro de ATTs - 31-5-23.xlsx")
#nos interesa la cantidad registrada en recoleta y palermo
Cantidad_recoleta_palermo <-Registro_de_ATTs %>%
  filter(BARRIO=='PALERMO'|BARRIO=='RECOLETA') %>% 
  group_by(BARRIO) %>%
  summarise(Registrados = n(),.groups = 'drop') %>% 
  mutate(BARRIO=.$BARRIO<-tolower(.$BARRIO)) 
#View(Cantidad_ATTs_registro)

#lo cruzamos con la cantidad de departamentos para los mismos barrios publicados en AIRBNB
REGISTRO_VS_AIRBNB<-mar23 %>% 
  rename(BARRIO='neighbourhood') %>%
  rename(AIRBNB='mar23') %>% 
  right_join(Cantidad_recoleta_palermo, by='BARRIO') %>% 
  pivot_longer(cols = c(AIRBNB, Registrados), names_to = "Tipo", values_to = "Cantidad")


#lo visualizamos

plot_cantidad_atts_CABA_AIRBNB <- ggplot(REGISTRO_VS_AIRBNB, aes(x = BARRIO, y = Cantidad, fill = Tipo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("AIRBNB" = "blue", "Registrados" = "red")) +
  scale_y_continuous(breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500))+
  labs(x = "Barrio",
       y = "Cantidad",
       title = "Cantidad de ATT's registrados vs cantidad de Airbnb",
       caption = "Fuente: Insideairbnb y Ente de Turismo GCBA") +
  geom_text(data =REGISTRO_VS_AIRBNB,
            aes(label = Cantidad, x = BARRIO, y = Cantidad), vjust = -0.5, position = position_dodge(width = 0.9), hjust = -1)+
  theme_economist()

plot_cantidad_atts_CABA_AIRBNB
