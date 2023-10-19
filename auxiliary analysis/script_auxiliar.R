library(dplyr)
library(readxl)
library(stargazer)
library(treemapify)
library(viridis)
library(RColorBrewer)
library(waffle)
library(ggplot2)
library(eeptools)
library(tidyverse)
library(stringr)
library(sf)
library(xlsx)
library(janitor)
library(lubridate)

####PRIMERA PARTE: GENERAR DATOS DE PRECIOS DE ALQUILER PROMEDIO MENSUALES EN CABA:

#importar las bases de datos
CABA_PROMEDIOS_1_AMBIENTE_BARRIO <- read_excel("../data/CABA_PROMEDIOS_1_AMBIENTE_BARRIO.xlsx")
CABA_PROMEDIOS_2_AMBIENTE_BARRIO <- read_excel("../data/CABA_PROMEDIOS_2_AMBIENTE_BARRIO.xlsx")
CABA_PROMEDIOS_3_AMBIENTE_BARRIO <- read_excel("../data/CABA_PROMEDIOS_3_AMBIENTE_BARRIO.xlsx")

#cambiar el nombre
df_promedios_x_BARRIO_1_ambiente <- CABA_PROMEDIOS_1_AMBIENTE_BARRIO
df_promedios_x_BARRIO_2_ambiente <- CABA_PROMEDIOS_2_AMBIENTE_BARRIO
df_promedios_x_BARRIO_3_ambiente <- CABA_PROMEDIOS_3_AMBIENTE_BARRIO
#View(df_promedios_x_BARRIO_1_ambiente)
#View(df_promedios_x_BARRIO_2_ambiente)
#View(df_promedios_x_BARRIO_3_ambiente)
glimpse(df_promedios_x_BARRIO_3_ambiente)

#crear una unica base de datos a partir de las tres anteriores
##problema: dejan de aparecer los valores de la columna "BARRIO"
##problema: no coinciden los nombres de las columnas (diferencia en la cantidad de ambientes de cada dataset)
#renombrar esa columan
df_promedios_x_BARRIO_1_ambiente_1 <- df_promedios_x_BARRIO_1_ambiente %>%
  rename(BARRIO = "Precio promedio de publicación (pesos) de departamentos en alquiler de 1 ambiente usados por barrio. Ciudad de Buenos Aires. 1er. trimestre 2018/1er. trimestre 2023")
df_promedios_x_BARRIO_2_ambiente_1 <- df_promedios_x_BARRIO_2_ambiente %>%
  rename(BARRIO = "Precio promedio de publicación (pesos) de departamentos en alquiler de 2 ambientes usados por barrio. Ciudad de Buenos Aires. 1er. trimestre 2018/1er. trimestre 2023")
df_promedios_x_BARRIO_3_ambiente_1 <- df_promedios_x_BARRIO_3_ambiente %>%
  rename(BARRIO = "Precio promedio de publicación (pesos) de departamentos en alquiler de 3 ambientes usados por barrio. Ciudad de Buenos Aires. 1er. trimestre 2018/1er. trimestre 2023")
#View(df_promedios_x_BARRIO_1_ambiente_1)
#View(df_promedios_x_BARRIO_2_ambiente_1)
#View(df_promedios_x_BARRIO_3_ambiente_1)


df_promedios_x_BARRIO <- rbind(df_promedios_x_BARRIO_1_ambiente_1, df_promedios_x_BARRIO_2_ambiente_1, df_promedios_x_BARRIO_3_ambiente_1)
#View(df_promedios_x_BARRIO)

##la nueva base de datos presenta filas y columnas no deseadas.Voy a crear una nueva base de datos que solo considere los valores de la ultima columna (1er trimestre 2023)
colnames(df_promedios_x_BARRIO) #identificar los nombres de las columnas
##Solo considero la ultima columna, omitiendo el numero de BARRIO (de nuevo)

#voy a modificar el nombre de la columna dentro de una nueva base de datos (que contenga solo los datos de 2023).
df_promedios_x_BARRIO_2023 <- select(df_promedios_x_BARRIO, BARRIO, ...22)
df_promedios_x_BARRIO_2023 <-  df_promedios_x_BARRIO_2023 %>%
  rename("1er Trimestre 2023" = "...22")
#View(df_promedios_x_BARRIO_2023)

##en el nuevo script hace falta: eliminar filas que no corresponden, transponer columnas y filas, limpiar la tabla asociando los valores de las mismas BARRIOs
#limpiar:
#son mas las filas a eliminar que las filas con datos (8,22,28,50,51,53,56,61,70,96,98,104,118,124,144)
#filas con valores:11,25,31,61,62,64,67,72,81,107,117,123,137,143,163
df_promedios_x_BARRIO_2023_limpia <- df_promedios_x_BARRIO_2023[-c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,63,65,66,68,69,70,71,73,74,75,76,77,78,79,80,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,108,109,110,111,112,113,114,115,116,118,119,120,121,122,124,125,126,127, 128, 129, 130, 131, 132, 133, 134, 135, 136,138,139, 140, 141, 142,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162, 164, 165, 166, 167, 168), ]
View(df_promedios_x_BARRIO_2023_limpia)



df_ordenado <- arrange(df_promedios_x_BARRIO_2023_limpia, BARRIO)
#View(df_ordenado)
#colnames(df_ordenado)

# Convertir la columna a tipo numérico
df_ordenado$"1er Trimestre 2023" <- as.numeric(df_ordenado$"1er Trimestre 2023")
#operacion para calcular promedios
df_promedio <- df_ordenado %>%
  group_by(BARRIO) %>%
  summarize(Promedio = mean(`1er Trimestre 2023`, na.rm = TRUE))
View(df_promedio)

'''
Solo Palermo y Caballito son los barrios que presentaban informacíon para 1,2 y 3 ambientes en el 1er trim. del 2023 por eso nos quedamos
solo con ellos
'''

palercaba <- df_promedio[c(4,6), ] %>% 
  mutate(BARRIO = tolower(BARRIO)) %>% 
  rename("precio_promedio"="Promedio") %>% 
  rename("neighbourhood"="BARRIO")


write.csv(alquileres_promedio_palercaba, "../data/palercaba.csv")

###RESULTADO### OBTUVIMOS ´PALERCABA´ PARA SEGUIRLO TRABAJANDO EN EL SCRIPT PRINCIPAL

rm(df_promedios_x_BARRIO_2023_limpia,df_promedios_x_BARRIO_2023,df_ordenado)

####PARTE 2: GENERAR GRAFICOS DE SERIES DE TIEMPO####
#EVOLUCION DEL SALARIO MINIMO VS VALORES DE ALQUILER


CABA_PROMEDIOS_1_AMBIENTE_BARRIO <- read_excel("../data/CABA_PROMEDIOS_1_AMBIENTE_BARRIO.xlsx")
CABA_PROMEDIOS_2_AMBIENTE_BARRIO <- read_excel("../data/CABA_PROMEDIOS_2_AMBIENTE_BARRIO.xlsx")
CABA_PROMEDIOS_3_AMBIENTE_BARRIO <- read_excel("../data/CABA_PROMEDIOS_3_AMBIENTE_BARRIO.xlsx")

#cambiar el nombre
df_promedios_x_barrio_1_ambiente <- CABA_PROMEDIOS_1_AMBIENTE_BARRIO
df_promedios_x_barrio_2_ambiente <- CABA_PROMEDIOS_2_AMBIENTE_BARRIO
df_promedios_x_barrio_3_ambiente <- CABA_PROMEDIOS_3_AMBIENTE_BARRIO

#crear una unica base de datos a partir de las tres anteriores
##problema: dejan de aparecer los valores de la columna "Barrio"
##problema: no coinciden los nombres de las columnas (diferencia en la cantidad de ambientes de cada dataset)
#renombrar esa columan
df_promedios_x_barrio_1_ambiente_1 <- df_promedios_x_barrio_1_ambiente %>%
  rename(Barrio = "Precio promedio de publicación (pesos) de departamentos en alquiler de 1 ambiente usados por barrio. Ciudad de Buenos Aires. 1er. trimestre 2018/1er. trimestre 2023")
df_promedios_x_barrio_2_ambiente_1 <- df_promedios_x_barrio_2_ambiente %>%
  rename(Barrio = "Precio promedio de publicación (pesos) de departamentos en alquiler de 2 ambientes usados por barrio. Ciudad de Buenos Aires. 1er. trimestre 2018/1er. trimestre 2023")
df_promedios_x_barrio_3_ambiente_1 <- df_promedios_x_barrio_3_ambiente %>%
  rename(Barrio = "Precio promedio de publicación (pesos) de departamentos en alquiler de 3 ambientes usados por barrio. Ciudad de Buenos Aires. 1er. trimestre 2018/1er. trimestre 2023")

#---------------------------------#
#para graficar la evolucion de los datos a lo largo del tiempo es necesario modificar la tabla.
##completar la info anterior (sintetizar año y trimestre en el nombre de las columnas) y transponer la tabla

#primero: cambiar el nombre de las columnas
colnames(df_promedios_x_barrio_1_ambiente_1) <- c("Barrio", "2018-3-1", "2018-6-1", "2018-9-1", "2018-12-1", "2019-3-1", "2019-6-1", "2019-9-1", "2019-12-1", "2020-3-1", "2020-6-1", "2020-9-1", "2020-12-1", "2021-3-1", "2021-6-1", "2021-9-1", "2021-12-1", "2022-3-1", "2022-6-1", "2022-9-1", "2022-12-1", "2023-3-1")
colnames(df_promedios_x_barrio_2_ambiente_1) <- c("Barrio", "2018-3-1", "2018-6-1", "2018-9-1", "2018-12-1", "2019-3-1", "2019-6-1", "2019-9-1", "2019-12-1", "2020-3-1", "2020-6-1", "2020-9-1", "2020-12-1", "2021-3-1", "2021-6-1", "2021-9-1", "2021-12-1", "2022-3-1", "2022-6-1", "2022-9-1", "2022-12-1", "2023-3-1")
colnames(df_promedios_x_barrio_3_ambiente_1) <- c("Barrio", "2018-3-1", "2018-6-1", "2018-9-1", "2018-12-1", "2019-3-1", "2019-6-1", "2019-9-1", "2019-12-1", "2020-3-1", "2020-6-1", "2020-9-1", "2020-12-1", "2021-3-1", "2021-6-1", "2021-9-1", "2021-12-1", "2022-3-1", "2022-6-1", "2022-9-1", "2022-12-1", "2023-3-1")

#segundo: eliminar las primeras 3 filas
df_promedios_x_barrio_1_ambiente_2 <- df_promedios_x_barrio_1_ambiente_1[-c(1,2,3,52,53,54,55,56), ]
df_promedios_x_barrio_2_ambiente_2 <- df_promedios_x_barrio_2_ambiente_1[-c(1,2,3,52,53,54,55,56), ]
df_promedios_x_barrio_3_ambiente_2 <- df_promedios_x_barrio_3_ambiente_1[-c(1,2,3,52,53,54,55,56), ]


#tercero calcular el promedio, para cada columna, de las filas que contienen datos numericos (no considerar las celdas con valor "///", ni aquellas con datos metodológicos)
colnames(df_promedios_x_barrio_1_ambiente_2)
colnames(df_promedios_x_barrio_2_ambiente_2)
colnames(df_promedios_x_barrio_3_ambiente_2)


#----------------------------#
##Convertir los valores a numéricos + calcular promedios por separado (para cada cuatrimestre de cada año :) )
#1 AMBIENTE ("1A")
#2018.1er trim
filas_2018_1_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2018-3-1")
##View(filas_2018_1_1A)
filas_2018_1_1A_2 <- filas_2018_1_1A[-c(1,4,6,7,9,10,11,12,15,16,17,19,20,21,23,25,26,27,29,30,31,33,34,35,39,40,42,44,45,46,47), ]
##View(filas_2018_1_1A_2)
columna_2018_1_1A <- "2018-3-1"
filas_2018_1_1A_2[[columna_2018_1_1A]] <- as.numeric(filas_2018_1_1A_2[[columna_2018_1_1A]])
##calcular promedio
promedio_2018_1_1A <- mean(filas_2018_1_1A_2[["2018-3-1"]], na.rm = TRUE)
##View(promedio_2018_1_1A)

#2018.2do trim
filas_2018_2_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2018-6-1")
##View(filas_2018_2_1A)
filas_2018_2_1A_2 <- filas_2018_2_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,19,20,23,25,26,27,29,30,31,33,34,35,39,40,41,42,44,45,46,47), ]
##View(filas_2018_2_1A_2)
columna_2018_2_1A <- "2018-6-1"
filas_2018_2_1A_2[[columna_2018_2_1A]] <- as.numeric(filas_2018_2_1A_2[[columna_2018_2_1A]])
##calcular promedio
promedio_2018_2_1A <- mean(filas_2018_2_1A_2[["2018-6-1"]], na.rm = TRUE)
#View(promedio_2018_2_1A)

#2018.3er trim
filas_2018_3_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2018-9-1")
##View(filas_2018_3_1A)
filas_2018_3_1A_2 <- filas_2018_3_1A[-c(1,3,4,6,7,9,10,11,12,15,17,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,39,40,41,44,45,46,47), ]
##View(filas_2018_2_1A_2)
columna_2018_3_1A <- "2018-9-1"
filas_2018_3_1A_2[[columna_2018_3_1A]] <- as.numeric(filas_2018_3_1A_2[[columna_2018_3_1A]])
##calcular promedio
promedio_2018_3_1A <- mean(filas_2018_3_1A_2[["2018-9-1"]], na.rm = TRUE)
#View(promedio_2018_3_1A)

#2018.4to trim
filas_2018_4_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2018-12-1")
#View(filas_2018_4_1A)
filas_2018_4_1A_2 <- filas_2018_4_1A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,21,23,24,25,26,27,29,31,33,34,35,37,38,39,40,41,42,44,45,46,47), ]
##View(filas_2018_2_1A_2)
columna_2018_4 <- "2018-12-1"
filas_2018_4_1A_2[[columna_2018_4]] <- as.numeric(filas_2018_4_1A_2[[columna_2018_4]])
##calcular promedio
promedio_2018_4_1A <- mean(filas_2018_4_1A_2[["2018-12-1"]], na.rm = TRUE)
#View(promedio_2018_4_1A)

#2019.1er trim
filas_2019_1_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2019-3-1")
#View(filas_2019_1_1A)
filas_2019_1_1A_2 <- filas_2019_1_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,23,24,25,26,27,29,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_1_1A_2)
columna_2019_1_1A <- "2019-3-1"
filas_2019_1_1A_2[[columna_2019_1_1A]] <- as.numeric(filas_2019_1_1A_2[[columna_2019_1_1A]])
##calcular promedio
promedio_2019_1_1A <- mean(filas_2019_1_1A_2[["2019-3-1"]], na.rm = TRUE)
#View(promedio_2019_1_1A)

#2019. 2do trim
filas_2019_2_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2019-6-1")
##View(filas_2019_2_1A)
filas_2019_2_1A_2 <- filas_2019_2_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,23,24,25,26,27,29,30,31,33,34,35,37,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_2_1A_2)
columna_2019_2_1A <- "2019-6-1"
filas_2019_2_1A_2[[columna_2019_2_1A]] <- as.numeric(filas_2019_2_1A_2[[columna_2019_2_1A]])
##calcular promedio
promedio_2019_2_1A <- mean(filas_2019_2_1A_2[["2019-6-1"]], na.rm = TRUE)
#View(promedio_2019_2_1A)

#2019. 3er trim
filas_2019_3_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2019-9-1")
##View(filas_2019_3_1A)
filas_2019_3_1A_2 <- filas_2019_3_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,19,20,23,24,25,26,27,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_3_1A_2)
columna_2019_3_1A <- "2019-9-1"
filas_2019_3_1A_2[[columna_2019_3_1A]] <- as.numeric(filas_2019_3_1A_2[[columna_2019_3_1A]])
##calcular promedio
promedio_2019_3_1A <- mean(filas_2019_3_1A_2[["2019-9-1"]], na.rm = TRUE)
#View(promedio_2019_3_1A)

#2019. 4to trim
filas_2019_4_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2019-12-1")
##View(filas_2019_4_1A)
filas_2019_4_1A_2 <- filas_2019_4_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,33,34,35,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_4_1A_2)
columna_2019_4_1A <- "2019-12-1"
filas_2019_4_1A_2[[columna_2019_4_1A]] <- as.numeric(filas_2019_4_1A_2[[columna_2019_4_1A]])
##calcular promedio
promedio_2019_4_1A <- mean(filas_2019_4_1A_2[["2019-12-1"]], na.rm = TRUE)
#View(promedio_2019_4_1A)

#2020. 1er trim
filas_2020_1_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2020-3-1")
##View(filas_2020_1_1A)
filas_2020_1_1A_2 <- filas_2020_1_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,21,23,24,25,26,27,29,30,31,33,34,35,37,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_1_1A_2)
columna_2020_1_1A <- "2020-3-1"
filas_2020_1_1A_2[[columna_2020_1_1A]] <- as.numeric(filas_2020_1_1A_2[[columna_2020_1_1A]])
##calcular promedio
promedio_2020_1_1A <- mean(filas_2020_1_1A_2[["2020-3-1"]], na.rm = TRUE)
#View(promedio_2020_1_1A)

#2020. 2do trim
filas_2020_2_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2020-6-1")
##View(filas_2020_2_1A)
filas_2020_2_1A_2 <- filas_2020_2_1A[-c(1,4,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_2_1A_2)
columna_2020_2_1A <- "2020-6-1"
filas_2020_2_1A_2[[columna_2020_2_1A]] <- as.numeric(filas_2020_2_1A_2[[columna_2020_2_1A]])
##calcular promedio
promedio_2020_2_1A <- mean(filas_2020_2_1A_2[["2020-6-1"]], na.rm = TRUE)
#View(promedio_2020_2_1A)

#2020. 3er trim
filas_2020_3_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2020-9-1")
##View(filas_2020_3_1A)
filas_2020_3_1A_2 <- filas_2020_3_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,23,24,25,26,27,29,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_3_1A_2)
columna_2020_3_1A <- "2020-9-1"
filas_2020_3_1A_2[[columna_2020_3_1A]] <- as.numeric(filas_2020_3_1A_2[[columna_2020_3_1A]])
##calcular promedio
promedio_2020_3_1A <- mean(filas_2020_3_1A_2[["2020-9-1"]], na.rm = TRUE)
#View(promedio_2020_3_1A)

#2020. 4to trim
filas_2020_4_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2020-12-1")
##View(filas_2020_4_1A)
filas_2020_4_1A_2 <- filas_2020_4_1A[-c(1,4,6,7,9,10,11,12,13,14,15,16,17,18,20,23,24,25,26,27,30,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_4_1A_2)
columna_2020_4_1A <- "2020-12-1"
filas_2020_4_1A_2[[columna_2020_4_1A]] <- as.numeric(filas_2020_4_1A_2[[columna_2020_4_1A]])
##calcular promedio
promedio_2020_4_1A <- mean(filas_2020_4_1A_2[["2020-12-1"]], na.rm = TRUE)
#View(promedio_2020_4_1A)

#2021. 1er trim
filas_2021_1_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2021-3-1")
##View(filas_2021_1_1A)
filas_2021_1_1A_2 <- filas_2021_1_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,23,24,25,26,27,30,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2021_1_1A_2)
columna_2021_1_1A <- "2021-3-1"
filas_2021_1_1A_2[[columna_2021_1_1A]] <- as.numeric(filas_2021_1_1A_2[[columna_2021_1_1A]])
##calcular promedio
promedio_2021_1_1A <- mean(filas_2021_1_1A_2[["2021-3-1"]], na.rm = TRUE)
#View(promedio_2021_1_1A)

#2021. 2do trim
filas_2021_2_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2021-6-1")
##View(filas_2021_2_1A)
filas_2021_2_1A_2 <- filas_2021_2_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,23,24,25,26,27,30,31,33,34,35,37,38,39,40,41,42,44,45,46,47), ]
##View(filas_2021_2_1A_2)
columna_2021_2_1A <- "2021-6-1"
filas_2021_2_1A_2[[columna_2021_2_1A]] <- as.numeric(filas_2021_2_1A_2[[columna_2021_2_1A]])
##calcular promedio
promedio_2021_2_1A <- mean(filas_2021_2_1A_2[["2021-6-1"]], na.rm = TRUE)
#View(promedio_2021_2_1A)

#2021. 3er trim
filas_2021_3_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2021-9-1")
##View(filas_2021_3_1A)
filas_2021_3_1A_2 <- filas_2021_3_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,20,23,24,25,26,27,29,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2021_3_1A_2)
columna_2021_3_1A <- "2021-9-1"
filas_2021_3_1A_2[[columna_2021_3_1A]] <- as.numeric(filas_2021_3_1A_2[[columna_2021_3_1A]])
##calcular promedio
promedio_2021_3_1A <- mean(filas_2021_3_1A_2[["2021-9-1"]], na.rm = TRUE)
#View(promedio_2021_3_1A)

#2021. 4to trim
filas_2021_4_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2021-12-1")
##View(filas_2021_4_1A)
filas_2021_4_1A_2 <- filas_2021_4_1A[-c(1,4,6,7,9,10,11,14,15,16,17,18,20,23,24,25,26,27,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2021_4_1A_2)
columna_2021_4_1A <- "2021-12-1"
filas_2021_4_1A_2[[columna_2021_4_1A]] <- as.numeric(filas_2021_4_1A_2[[columna_2021_4_1A]])
##calcular promedio
promedio_2021_4_1A <- mean(filas_2021_4_1A_2[["2021-12-1"]], na.rm = TRUE)
#View(promedio_2021_4_1A)

#2022. 1er trim
filas_2022_1_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2022-3-1")
##View(filas_2022_1_1A)
filas_2022_1_1A_2 <- filas_2022_1_1A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_1_1A_2)
columna_2022_1_1A <- "2022-3-1"
filas_2022_1_1A_2[[columna_2022_1_1A]] <- as.numeric(filas_2022_1_1A_2[[columna_2022_1_1A]])
##calcular promedio
promedio_2022_1_1A <- mean(filas_2022_1_1A_2[["2022-3-1"]], na.rm = TRUE)
#View(promedio_2022_1_1A)

#2022. 2do trim
filas_2022_2_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2022-6-1")
##View(filas_2022_2_1A)
filas_2022_2_1A_2 <- filas_2022_2_1A[-c(1,4,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_2_1A_2)
columna_2022_2_1A <- "2022-6-1"
filas_2022_2_1A_2[[columna_2022_2_1A]] <- as.numeric(filas_2022_2_1A_2[[columna_2022_2_1A]])
##calcular promedio
promedio_2022_2_1A <- mean(filas_2022_2_1A_2[["2022-6-1"]], na.rm = TRUE)
#View(promedio_2022_2_1A)

#2022. 3er trim
filas_2022_3_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2022-9-1")
##View(filas_2022_3_1A)
filas_2022_3_1A_2 <- filas_2022_3_1A[-c(1,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_3_1A_2)
columna_2022_3_1A <- "2022-9-1"
filas_2022_3_1A_2[[columna_2022_3_1A]] <- as.numeric(filas_2022_3_1A_2[[columna_2022_3_1A]])
##calcular promedio
promedio_2022_3_1A <- mean(filas_2022_3_1A_2[["2022-9-1"]], na.rm = TRUE)
#View(promedio_2022_3_1A)

#2022. 4to trim
filas_2022_4_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2022-12-1")
##View(filas_2022_4_1A)
filas_2022_4_1A_2 <- filas_2022_4_1A[-c(1,2,3,4,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_4_1A_2)
columna_2022_4_1A <- "2022-12-1"
filas_2022_4_1A_2[[columna_2022_4_1A]] <- as.numeric(filas_2022_4_1A_2[[columna_2022_4_1A]])
##calcular promedio
promedio_2022_4_1A <- mean(filas_2022_4_1A_2[["2022-12-1"]], na.rm = TRUE)
#View(promedio_2022_4_1A)

#2023. 1er trim
filas_2023_1_1A <- select(df_promedios_x_barrio_1_ambiente_2, "2023-3-1")
#View(filas_2023_1_1A)
filas_2023_1_1A_2 <- filas_2023_1_1A[-c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48), ]
##View(filas_2023_1_1A_2)
columna_2023_1_1A <- "2023-3-1"
filas_2023_1_1A_2[[columna_2023_1_1A]] <- as.numeric(filas_2023_1_1A_2[[columna_2023_1_1A]])
##calcular promedio
promedio_2023_1_1A <- mean(filas_2023_1_1A_2[["2023-3-1"]], na.rm = TRUE)
#View(promedio_2023_1_1A)


#2AMBIENTES ("2A")
#2018.1er trim
filas_2018_1_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2018-3-1")
#View(filas_2018_1_2A)
filas_2018_1_2A_2 <- filas_2018_1_2A[-c(1,4,6,7,10,12,15,17,20,25,26,27,33,35,39,40,44,45,47), ]
##View(filas_2018_1_2A_2)
columna_2018_1_2A <- "2018-3-1"
filas_2018_1_2A_2[[columna_2018_1_2A]] <- as.numeric(filas_2018_1_2A_2[[columna_2018_1_2A]])
##calcular promedio
promedio_2018_1_2A <- mean(filas_2018_1_2A_2[["2018-3-1"]], na.rm = TRUE)
#View(promedio_2018_1_2A)

#2018.2do trim
filas_2018_2_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2018-6-1")
#View(filas_2018_2_2A)
filas_2018_2_2A_2 <- filas_2018_2_2A[-c(1,6,10,12,15,17,20,25,26,29,33,34,35,40,44,45,47), ]
#View(filas_2018_2_2A_2)
columna_2018_2_2A <- "2018-6-1"
filas_2018_2_2A_2[[columna_2018_2_2A]] <- as.numeric(filas_2018_2_2A_2[[columna_2018_2_2A]])
##calcular promedio
promedio_2018_2_2A <- mean(filas_2018_2_2A_2[["2018-6-1"]], na.rm = TRUE)
#View(promedio_2018_2_2A)

#2018.3er trim
filas_2018_3_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2018-9-1")
#View(filas_2018_3_2A)
filas_2018_3_2A_2 <- filas_2018_3_2A[-c(1,4,6,7,9,10,12,15,17,20,23,24,25,26,29,33,35,40,44,45,47), ]
#View(filas_2018_2_2A_2)
columna_2018_3_2A <- "2018-9-1"
filas_2018_3_2A_2[[columna_2018_3_2A]] <- as.numeric(filas_2018_3_2A_2[[columna_2018_3_2A]])
##calcular promedio
promedio_2018_3_2A <- mean(filas_2018_3_2A_2[["2018-9-1"]], na.rm = TRUE)
#View(promedio_2018_3_2A)

#2018.4to trim
filas_2018_4_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2018-12-1")
#View(filas_2018_4_2A)
filas_2018_4_2A_2 <- filas_2018_4_2A[-c(1,4,6,9,10,12,15,17,18,20,23,25,26,27,29,34,35,40,41,42,43,44,45,46,47), ]
#View(filas_2018_2_2A_2)
columna_2018_4_2A <- "2018-12-1"
filas_2018_4_2A_2[[columna_2018_4_2A]] <- as.numeric(filas_2018_4_2A_2[[columna_2018_4_2A]])
##calcular promedio
promedio_2018_4_2A <- mean(filas_2018_4_2A_2[["2018-12-1"]], na.rm = TRUE)
#View(promedio_2018_4_2A)

#2019.1er trim
filas_2019_1_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2019-3-1")
#View(filas_2019_1_2A)
filas_2019_1_2A_2 <- filas_2019_1_2A[-c(1,6,9,10,12,15,16,17,18,19,20,23,25,26,27,33,34,35,40,42,43,44,45,46,47), ]
#View(filas_2019_1_2A_2)
columna_2019_1_2A <- "2019-3-1"
filas_2019_1_2A_2[[columna_2019_1_2A]] <- as.numeric(filas_2019_1_2A_2[[columna_2019_1_2A]])
##calcular promedio
promedio_2019_1_2A <- mean(filas_2019_1_2A_2[["2019-3-1"]], na.rm = TRUE)
#View(promedio_2019_1_2A)

#2019. 2do trim
filas_2019_2_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2019-6-1")
#View(filas_2019_2_2A)
filas_2019_2_2A_2 <- filas_2019_2_2A[-c(1,6,10,12,15,16,17,18,20,23,25,26,27,33,35,40,42,44,45,46,47), ]
#View(filas_2019_2_2A_2)
columna_2019_2_2A <- "2019-6-1"
filas_2019_2_2A_2[[columna_2019_2_2A]] <- as.numeric(filas_2019_2_2A_2[[columna_2019_2_2A]])
##calcular promedio
promedio_2019_2_2A <- mean(filas_2019_2_2A_2[["2019-6-1"]], na.rm = TRUE)
#View(promedio_2019_2_2A)

#2019. 3er trim
filas_2019_3_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2019-9-1")
#View(filas_2019_3_2A)
filas_2019_3_2A_2 <- filas_2019_3_2A[-c(1,4,6,9,10,12,15,17,18,20,23,25,26,31,33,34,35,40,42,44,45,46,47), ]
#View(filas_2019_3_2A_2)
columna_2019_3_2A <- "2019-9-1"
filas_2019_3_2A_2[[columna_2019_3_2A]] <- as.numeric(filas_2019_3_2A_2[[columna_2019_3_2A]])
##calcular promedio
promedio_2019_3_2A <- mean(filas_2019_3_2A_2[["2019-9-1"]], na.rm = TRUE)
#View(promedio_2019_3_2A)

#2019. 4to trim
filas_2019_4_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2019-12-1")
#View(filas_2019_4_2A)
filas_2019_4_2A_2 <- filas_2019_4_2A[-c(1,6,10,12,15,16,17,18,19,20,23,25,26,30,34,35,39,40,41,44,45,46,47), ]
#View(filas_2019_4_2A_2)
columna_2019_4_2A <- "2019-12-1"
filas_2019_4_2A_2[[columna_2019_4_2A]] <- as.numeric(filas_2019_4_2A_2[[columna_2019_4_2A]])
##calcular promedio
promedio_2019_4_2A <- mean(filas_2019_4_2A_2[["2019-12-1"]], na.rm = TRUE)
#View(promedio_2019_4_2A)

#2020. 1er trim
filas_2020_1_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2020-3-1")
#View(filas_2020_1_2A)
filas_2020_1_2A_2 <- filas_2020_1_2A[-c(1,6,10,12,14,15,16,17,18,20,23,25,26,27,33,34,35,39,40,42,44,45,46,47), ]
#View(filas_2020_1_2A_2)
columna_2020_1_2A <- "2020-3-1"
filas_2020_1_2A_2[[columna_2020_1_2A]] <- as.numeric(filas_2020_1_2A_2[[columna_2020_1_2A]])
##calcular promedio
promedio_2020_1_2A <- mean(filas_2020_1_2A_2[["2020-3-1"]], na.rm = TRUE)
#View(promedio_2020_1_2A)

#2020. 2do trim
filas_2020_2_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2020-6-1")
#View(filas_2020_2_2A)
filas_2020_2_2A_2 <- filas_2020_2_2A[-c(1,4,6,7,9,10,12,14,15,16,17,18,20,23,24,25,26,27,30,31,32,34,35,39,40,41,42,44,45,46,47), ]
#View(filas_2020_2_2A_2)
columna_2020_2_2A <- "2020-6-1"
filas_2020_2_2A_2[[columna_2020_2_2A]] <- as.numeric(filas_2020_2_2A_2[[columna_2020_2_2A]])
##calcular promedio
promedio_2020_2_2A <- mean(filas_2020_2_2A_2[["2020-6-1"]], na.rm = TRUE)
#View(promedio_2020_2_2A)

#2020. 3er trim
filas_2020_3_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2020-9-1")
#View(filas_2020_3_2A)
filas_2020_3_2A_2 <- filas_2020_3_2A[-c(1,4,6,7,10,12,14,15,16,17,18,20,23,24,25,26,27,30,34,35,39,40,41,42,43,44,45,46,47), ]
#View(filas_2020_3_2A_2)
columna_2020_3_2A <- "2020-9-1"
filas_2020_3_2A_2[[columna_2020_3_2A]] <- as.numeric(filas_2020_3_2A_2[[columna_2020_3_2A]])
##calcular promedio
promedio_2020_3_2A <- mean(filas_2020_3_2A_2[["2020-9-1"]], na.rm = TRUE)
#View(promedio_2020_3_2A)

#2020. 4to trim
filas_2020_4_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2020-12-1")
#View(filas_2020_4_2A)
filas_2020_4_2A_2 <- filas_2020_4_2A[-c(1,4,6,10,12,14,15,16,17,18,20,23,24,25,26,27,30,33,34,35,39,40,41,42,43,44,45,46,47), ]
#View(filas_2020_4_2A_2)
columna_2020_4_2A <- "2020-12-1"
filas_2020_4_2A_2[[columna_2020_4_2A]] <- as.numeric(filas_2020_4_2A_2[[columna_2020_4_2A]])
##calcular promedio
promedio_2020_4_2A <- mean(filas_2020_4_2A_2[["2020-12-1"]], na.rm = TRUE)
#View(promedio_2020_4_2A)

#2021. 1er trim
filas_2021_1_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2021-3-1")
#View(filas_2021_1_2A)
filas_2021_1_2A_2 <- filas_2021_1_2A[-c(1,4,6,9,10,12,14,15,16,17,18,20,23,24,25,26,27,34,35,38,39,40,41,42,43,44,45,46,47), ]
#View(filas_2021_1_2A_2)
columna_2021_1_2A <- "2021-3-1"
filas_2021_1_2A_2[[columna_2021_1_2A]] <- as.numeric(filas_2021_1_2A_2[[columna_2021_1_2A]])
##calcular promedio
promedio_2021_1_2A <- mean(filas_2021_1_2A_2[["2021-3-1"]], na.rm = TRUE)
#View(promedio_2021_1_2A)

#2021. 2do trim
filas_2021_2_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2021-6-1")
#View(filas_2021_2_2A)
filas_2021_2_2A_2 <- filas_2021_2_2A[-c(1,6,9,10,12,14,15,16,17,18,20,23,25,26,27,30,33,34,35,38,39,40,41,42,43,44,45,46,47), ]
#View(filas_2021_2_2A_2)
columna_2021_2_2A <- "2021-6-1"
filas_2021_2_2A_2[[columna_2021_2_2A]] <- as.numeric(filas_2021_2_2A_2[[columna_2021_2_2A]])
##calcular promedio
promedio_2021_2_2A <- mean(filas_2021_2_2A_2[["2021-6-1"]], na.rm = TRUE)
#View(promedio_2021_2_2A)

#2021. 3er trim
filas_2021_3_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2021-9-1")
#View(filas_2021_3_2A)
filas_2021_3_2A_2 <- filas_2021_3_2A[-c(1,4,6,9,10,12,15,16,17,18,20,23,25,26,27,30,34,35,39,40,41,42,43,44,45,46,47), ]
#View(filas_2021_3_2A_2)
columna_2021_3_2A <- "2021-9-1"
filas_2021_3_2A_2[[columna_2021_3_2A]] <- as.numeric(filas_2021_3_2A_2[[columna_2021_3_2A]])
##calcular promedio
promedio_2021_3_2A <- mean(filas_2021_3_2A_2[["2021-9-1"]], na.rm = TRUE)
#View(promedio_2021_3_2A)

#2021. 4to trim
filas_2021_4_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2021-12-1")
#View(filas_2021_4_2A)
filas_2021_4_2A_2 <- filas_2021_4_2A[-c(1,4,7,9,10,12,14,15,17,18,20,23,25,26,27,30,33,34,35,38,39,40,41,42,44,45,46,47), ]
#View(filas_2021_4_2A_2)
columna_2021_4_2A <- "2021-12-1"
filas_2021_4_2A_2[[columna_2021_4_2A]] <- as.numeric(filas_2021_4_2A_2[[columna_2021_4_2A]])
##calcular promedio
promedio_2021_4_2A <- mean(filas_2021_4_2A_2[["2021-12-1"]], na.rm = TRUE)
#View(promedio_2021_4_2A)

#2022. 1er trim
filas_2022_1_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2022-3-1")
#View(filas_2022_1_2A)
filas_2022_1_2A_2 <- filas_2022_1_2A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,23,25,26,27,29,30,31,33,34,35,39,40,41,42,43,44,45,47), ]
#View(filas_2022_1_2A_2)
columna_2022_1_2A <- "2022-3-1"
filas_2022_1_2A_2[[columna_2022_1_2A]] <- as.numeric(filas_2022_1_2A_2[[columna_2022_1_2A]])
##calcular promedio
promedio_2022_1_2A <- mean(filas_2022_1_2A_2[["2022-3-1"]], na.rm = TRUE)
#View(promedio_2022_1_2A)

#2022. 2do trim
filas_2022_2_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2022-6-1")
#View(filas_2022_2_2A)
filas_2022_2_2A_2 <- filas_2022_2_2A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,32,33,34,35,39,40,41,42,43,44,45,46,47), ]
#View(filas_2022_2_2A_2)
columna_2022_2_2A <- "2022-6-1"
filas_2022_2_2A_2[[columna_2022_2_2A]] <- as.numeric(filas_2022_2_2A_2[[columna_2022_2_2A]])
##calcular promedio
promedio_2022_2_2A <- mean(filas_2022_2_2A_2[["2022-6-1"]], na.rm = TRUE)
#View(promedio_2022_2_2A)

#2022. 3er trim
filas_2022_3_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2022-9-1")
#View(filas_2022_3_2A)
filas_2022_3_2A_2 <- filas_2022_3_2A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,33,34,35,38,39,40,41,42,43,44,45,46,47), ]
#View(filas_2022_3_2A_2)
columna_2022_3_2A <- "2022-9-1"
filas_2022_3_2A_2[[columna_2022_3_2A]] <- as.numeric(filas_2022_3_2A_2[[columna_2022_3_2A]])
##calcular promedio
promedio_2022_3_2A <- mean(filas_2022_3_2A_2[["2022-9-1"]], na.rm = TRUE)
#View(promedio_2022_3_2A)

#2022. 4to trim
filas_2022_4_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2022-12-1")
#View(filas_2022_4_2A)
filas_2022_4_2A_2 <- filas_2022_4_2A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,23,24,25,26,27,29,30,31,33,34,35,39,40,41,42,43,44,45,46,47), ]
#View(filas_2022_4_2A_2)
columna_2022_4_2A <- "2022-12-1"
filas_2022_4_2A_2[[columna_2022_4_2A]] <- as.numeric(filas_2022_4_2A_2[[columna_2022_4_2A]])
##calcular promedio
promedio_2022_4_2A <- mean(filas_2022_4_2A_2[["2022-12-1"]], na.rm = TRUE)
#View(promedio_2022_4_2A)

#2023. 1er trim
filas_2023_1_2A <- select(df_promedios_x_barrio_2_ambiente_2, "2023-3-1")
#View(filas_2023_1_2A)
filas_2023_1_2A_2 <- filas_2023_1_2A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
#View(filas_2023_1_2A_2)
columna_2023_1_2A <- "2023-3-1"
filas_2023_1_2A_2[[columna_2023_1_2A]] <- as.numeric(filas_2023_1_2A_2[[columna_2023_1_2A]])
##calcular promedio
promedio_2023_1_2A <- mean(filas_2023_1_2A_2[["2023-3-1"]], na.rm = TRUE)
#View(promedio_2023_1_2A)

#3 AMBIENTES ("3A")
#2018.1er trim
filas_2018_1_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2018-3-1")
##View(filas_2018_1_3A)
filas_2018_1_3A_2 <- filas_2018_1_3A[-c(1,4,6,7,10,12,14,15,16,17,18,19,20,23,24,25,26,30,31,32,33,34,35,37,39,40,41,42,43,44,45,46,47), ]
##View(filas_2018_1_3A_2)
columna_2018_1_3A <- "2018-3-1"
filas_2018_1_3A_2[[columna_2018_1_3A]] <- as.numeric(filas_2018_1_3A_2[[columna_2018_1_3A]])
##calcular promedio
promedio_2018_1_3A <- mean(filas_2018_1_3A_2[["2018-3-1"]], na.rm = TRUE)
##View(promedio_2018_1_3A)

#2018.2do trim
filas_2018_2_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2018-6-1")
##View(filas_2018_2_3A)
filas_2018_2_3A_2 <- filas_2018_2_3A[-c(1,4,6,7,9,10,12,15,16,17,18,19,20,23,25,26,30,31,32,33,34,35,39,40,42,43,44,45,46,47), ]
##View(filas_2018_2_3A_2)
columna_2018_2_3A <- "2018-6-1"
filas_2018_2_3A_2[[columna_2018_2_3A]] <- as.numeric(filas_2018_2_3A_2[[columna_2018_2_3A]])
##calcular promedio
promedio_2018_2_3A <- mean(filas_2018_2_3A_2[["2018-6-1"]], na.rm = TRUE)
##View(promedio_2018_2_3A)

#2018.3er trim
filas_2018_3_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2018-9-1")
##View(filas_2018_3_3A)
filas_2018_3_3A_2 <- filas_2018_3_3A[-c(1,4,6,7,9,10,12,14,15,17,18,19,20,23,24,25,26,27,29,30,31,32,33,34,35,40,42,43,44,45,47), ]
##View(filas_2018_3_3A_3A_2)
columna_2018_3_3A <- "2018-9-1"
filas_2018_3_3A_2[[columna_2018_3_3A]] <- as.numeric(filas_2018_3_3A_2[[columna_2018_3_3A]])
##calcular promedio
promedio_2018_3_3A <- mean(filas_2018_3_3A_2[["2018-9-1"]], na.rm = TRUE)
##View(promedio_2018_3_3A)

#2018.4to trim
filas_2018_4_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2018-12-1")
##View(filas_2018_4_3A)
filas_2018_4_3A_2 <- filas_2018_4_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,32,33,34,35,39,40,41,42,43,44,45,46,47,48), ]
##View(filas_2018_4_3A_3A_2)
columna_2018_4_3A <- "2018-12-1"
filas_2018_4_3A_2[[columna_2018_4_3A]] <- as.numeric(filas_2018_4_3A_2[[columna_2018_4_3A]])
##calcular promedio
promedio_2018_4_3A <- mean(filas_2018_4_3A_2[["2018-12-1"]], na.rm = TRUE)
##View(promedio_2018_4_3A)

#2019.1er trim
filas_2019_1_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2019-3-1")
##View(filas_2019_1_3A)
filas_2019_1_3A_2 <- filas_2019_1_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,23,25,26,27,30,31,33,34,35,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_1_3A_2)
columna_2019_1_3A <- "2019-3-1"
filas_2019_1_3A_2[[columna_2019_1_3A]] <- as.numeric(filas_2019_1_3A_2[[columna_2019_1_3A]])
##calcular promedio
promedio_2019_1_3A <- mean(filas_2019_1_3A_2[["2019-3-1"]], na.rm = TRUE)
##View(promedio_2019_1_3A)

#2019. 2do trim
filas_2019_2_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2019-6-1")
##View(filas_2019_2_3A)
filas_2019_2_3A_2 <- filas_2019_2_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,20,23,25,26,27,30,31,33,34,35,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_2_3A_2)
columna_2019_2_3A <- "2019-6-1"
filas_2019_2_3A_2[[columna_2019_2_3A]] <- as.numeric(filas_2019_2_3A_2[[columna_2019_2_3A]])
##calcular promedio
promedio_2019_2_3A <- mean(filas_2019_2_3A_2[["2019-6-1"]], na.rm = TRUE)
##View(promedio_2019_2_3A)

#2019. 3er trim
filas_2019_3_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2019-9-1")
##View(filas_2019_3_3A)
filas_2019_3_3A_2 <- filas_2019_3_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,20,23,25,26,27,30,31,32,33,34,35,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_3_3A_2)
columna_2019_3_3A <- "2019-9-1"
filas_2019_3_3A_2[[columna_2019_3_3A]] <- as.numeric(filas_2019_3_3A_2[[columna_2019_3_3A]])
##calcular promedio
promedio_2019_3_3A <- mean(filas_2019_3_3A_2[["2019-9-1"]], na.rm = TRUE)
##View(promedio_2019_3_3A)

#2019. 4to trim
filas_2019_4_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2019-12-1")
##View(filas_2019_4_3A)
filas_2019_4_3A_2 <- filas_2019_4_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,23,25,26,27,30,31,33,34,35,39,40,41,42,43,44,45,46,47), ]
##View(filas_2019_4_3A_2)
columna_2019_4_3A <- "2019-12-1"
filas_2019_4_3A_2[[columna_2019_4_3A]] <- as.numeric(filas_2019_4_3A_2[[columna_2019_4_3A]])
##calcular promedio
promedio_2019_4_3A <- mean(filas_2019_4_3A_2[["2019-12-1"]], na.rm = TRUE)
##View(promedio_2019_4_3A)

#2020. 1er trim
filas_2020_1_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2020-3-1")
##View(filas_2020_1_3A)
filas_2020_1_3A_2 <- filas_2020_1_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,29,30,31,33,34,35,37,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_1_3A_2)
columna_2020_1_3A <- "2020-3-1"
filas_2020_1_3A_2[[columna_2020_1_3A]] <- as.numeric(filas_2020_1_3A_2[[columna_2020_1_3A]])
##calcular promedio
promedio_2020_1_3A <- mean(filas_2020_1_3A_2[["2020-3-1"]], na.rm = TRUE)
##View(promedio_2020_1_3A)

#2020. 2do trim
filas_2020_2_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2020-6-1")
##View(filas_2020_2_3A)
filas_2020_2_3A_2 <- filas_2020_2_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_2_3A_2)
columna_2020_2_3A <- "2020-6-1"
filas_2020_2_3A_2[[columna_2020_2_3A]] <- as.numeric(filas_2020_2_3A_2[[columna_2020_2_3A]])
##calcular promedio
promedio_2020_2_3A <- mean(filas_2020_2_3A_2[["2020-6-1"]], na.rm = TRUE)
##View(promedio_2020_2_3A)

#2020. 3er trim
filas_2020_3_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2020-9-1")
##View(filas_2020_3_3A)
filas_2020_3_3A_2 <- filas_2020_3_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_3_3A_2)
columna_2020_3_3A <- "2020-9-1"
filas_2020_3_3A_2[[columna_2020_3_3A]] <- as.numeric(filas_2020_3_3A_2[[columna_2020_3_3A]])
##calcular promedio
promedio_2020_3_3A <- mean(filas_2020_3_3A_2[["2020-9-1"]], na.rm = TRUE)
##View(promedio_2020_3_3A)

#2020. 4to trim
filas_2020_4_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2020-12-1")
##View(filas_2020_4_3A)
filas_2020_4_3A_2 <- filas_2020_4_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2020_4_3A_2)
columna_2020_4_3A <- "2020-12-1"
filas_2020_4_3A_2[[columna_2020_4_3A]] <- as.numeric(filas_2020_4_3A_2[[columna_2020_4_3A]])
##calcular promedio
promedio_2020_4_3A <- mean(filas_2020_4_3A_2[["2020-12-1"]], na.rm = TRUE)
##View(promedio_2020_4_3A)

#2021. 1er trim
filas_2021_1_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2021-3-1")
##View(filas_2021_1_3A)
filas_2021_1_3A_2 <- filas_2021_1_3A[-c(1,4,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,45,46,47), ]
##View(filas_2021_1_3A_2)
columna_2021_1_3A <- "2021-3-1"
filas_2021_1_3A_2[[columna_2021_1_3A]] <- as.numeric(filas_2021_1_3A_2[[columna_2021_1_3A]])
##calcular promedio
promedio_2021_1_3A <- mean(filas_2021_1_3A_2[["2021-3-1"]], na.rm = TRUE)
##View(promedio_2021_1_3A)

#2021. 2do trim
filas_2021_2_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2021-6-1")
##View(filas_2021_2_3A)
filas_2021_2_3A_2 <- filas_2021_2_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2021_2_3A_2)
columna_2021_2_3A <- "2021-6-1"
filas_2021_2_3A_2[[columna_2021_2_3A]] <- as.numeric(filas_2021_2_3A_2[[columna_2021_2_3A]])
##calcular promedio
promedio_2021_2_3A <- mean(filas_2021_2_3A_2[["2021-6-1"]], na.rm = TRUE)
##View(promedio_2021_2_3A)

#2021. 3er trim
filas_2021_3_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2021-9-1")
##View(filas_2021_3_3A)
filas_2021_3_3A_2 <- filas_2021_3_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2021_3_3A_2)
columna_2021_3_3A <- "2021-9-1"
filas_2021_3_3A_2[[columna_2021_3_3A]] <- as.numeric(filas_2021_3_3A_2[[columna_2021_3_3A]])
##calcular promedio
promedio_2021_3_3A <- mean(filas_2021_3_3A_2[["2021-9-1"]], na.rm = TRUE)
#View(promedio_2021_3_3A)

#2021. 4to trim
filas_2021_4_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2021-12-1")
##View(filas_2021_4_3A)
filas_2021_4_3A_2 <- filas_2021_4_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2021_4_3A_2)
columna_2021_4_3A <- "2021-12-1"
filas_2021_4_3A_2[[columna_2021_4_3A]] <- as.numeric(filas_2021_4_3A_2[[columna_2021_4_3A]])
##calcular promedio
promedio_2021_4_3A <- mean(filas_2021_4_3A_2[["2021-12-1"]], na.rm = TRUE)
##View(promedio_2021_4_3A)

#2022. 1er trim
filas_2022_1_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2022-3-1")
##View(filas_2022_1_3A)
filas_2022_1_3A_2 <- filas_2022_1_3A[-c(1,4,6,7,9,10,12,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_1_3A_2)
columna_2022_1_3A <- "2022-3-1"
filas_2022_1_3A_2[[columna_2022_1_3A]] <- as.numeric(filas_2022_1_3A_2[[columna_2022_1_3A]])
##calcular promedio
promedio_2022_1_3A <- mean(filas_2022_1_3A_2[["2022-3-1"]], na.rm = TRUE)
##View(promedio_2022_1_3A)

#2022. 2do trim
filas_2022_2_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2022-6-1")
##View(filas_2022_2_3A)
filas_2022_2_3A_2 <- filas_2022_2_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_2_3A_2)
columna_2022_2_3A <- "2022-6-1"
filas_2022_2_3A_2[[columna_2022_2_3A]] <- as.numeric(filas_2022_2_3A_2[[columna_2022_2_3A]])
##calcular promedio
promedio_2022_2_3A <- mean(filas_2022_2_3A_2[["2022-6-1"]], na.rm = TRUE)
##View(promedio_2022_2_3A)

#2022. 3er trim
filas_2022_3_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2022-9-1")
##View(filas_2022_3_3A)
filas_2022_3_3A_2 <- filas_2022_3_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_3_3A_2)
columna_2022_3_3A <- "2022-9-1"
filas_2022_3_3A_2[[columna_2022_3_3A]] <- as.numeric(filas_2022_3_3A_2[[columna_2022_3_3A]])
##calcular promedio
promedio_2022_3_3A <- mean(filas_2022_3_3A_2[["2022-9-1"]], na.rm = TRUE)
#View(promedio_2022_3_3A)

#2022. 4to trim
filas_2022_4_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2022-12-1")
##View(filas_2022_4_3A)
filas_2022_4_3A_2 <- filas_2022_4_3A[-c(1,4,6,7,9,10,11,12,14,15,16,17,18,19,20,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2022_4_3A_2)
columna_2022_4_3A <- "2022-12-1"
filas_2022_4_3A_2[[columna_2022_4_3A]] <- as.numeric(filas_2022_4_3A_2[[columna_2022_4_3A]])
##calcular promedio
promedio_2022_4_3A <- mean(filas_2022_4_3A_2[["2022-12-1"]], na.rm = TRUE)
##View(promedio_2022_4_3A)

#2023. 1er trim
filas_2023_1_3A <- select(df_promedios_x_barrio_3_ambiente_2, "2023-3-1")
##View(filas_2023_1_3A)
filas_2023_1_3A_2 <- filas_2023_1_3A[-c(1,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47), ]
##View(filas_2023_1_3A_2)
columna_2023_1_3A <- "2023-3-1"
filas_2023_1_3A_2[[columna_2023_1_3A]] <- as.numeric(filas_2023_1_3A_2[[columna_2023_1_3A]])
##calcular promedio
promedio_2023_1_3A <- mean(filas_2023_1_3A_2[["2023-3-1"]], na.rm = TRUE)
##View(promedio_2023_1_3A)

#---------------------#
#UNIR DF'S
EVOLUCION_PROMEDIOS_1_AMBIENTE <- cbind(promedio_2018_1_1A,promedio_2018_2_1A,promedio_2018_3_1A,promedio_2018_4_1A,promedio_2019_1_1A,promedio_2019_2_1A,promedio_2019_3_1A,promedio_2019_4_1A,promedio_2020_1_1A,promedio_2020_2_1A,promedio_2020_3_1A,promedio_2020_4_1A,promedio_2021_1_1A,promedio_2021_2_1A,promedio_2021_3_1A,promedio_2021_4_1A,promedio_2022_1_1A,promedio_2022_2_1A,promedio_2022_3_1A,promedio_2022_4_1A,promedio_2023_1_1A)
EVOLUCION_PROMEDIOS_2_AMBIENTE <- cbind(promedio_2018_1_2A,promedio_2018_2_2A,promedio_2018_3_2A,promedio_2018_4_2A,promedio_2019_1_2A,promedio_2019_2_2A,promedio_2019_3_2A,promedio_2019_4_2A,promedio_2020_1_2A,promedio_2020_2_2A,promedio_2020_3_2A,promedio_2020_4_2A,promedio_2021_1_2A,promedio_2021_2_2A,promedio_2021_3_2A,promedio_2021_4_2A,promedio_2022_1_2A,promedio_2022_2_2A,promedio_2022_3_2A,promedio_2022_4_2A,promedio_2023_1_2A)
EVOLUCION_PROMEDIOS_3_AMBIENTE <- cbind(promedio_2018_1_3A,promedio_2018_2_3A,promedio_2018_3_3A,promedio_2018_4_3A,promedio_2019_1_3A,promedio_2019_2_3A,promedio_2019_3_3A,promedio_2019_4_3A,promedio_2020_1_3A,promedio_2020_2_3A,promedio_2020_3_3A,promedio_2020_4_3A,promedio_2021_1_3A,promedio_2021_2_3A,promedio_2021_3_3A,promedio_2021_4_3A,promedio_2022_1_3A,promedio_2022_2_3A,promedio_2022_3_3A,promedio_2022_4_3A,promedio_2023_1_3A)

#excluir la columna barrio
df_promedios_x_barrio_1_ambiente_1_fechas <- df_promedios_x_barrio_1_ambiente_1%>%
  select("2018-3-1", "2018-6-1", "2018-9-1", "2018-12-1", "2019-3-1", "2019-6-1", "2019-9-1", "2019-12-1", "2020-3-1", "2020-6-1", "2020-9-1", "2020-12-1", "2021-3-1", "2021-6-1", "2021-9-1", "2021-12-1", "2022-3-1", "2022-6-1", "2022-9-1", "2022-12-1", "2023-3-1")
df_promedios_x_barrio_2_ambiente_1_fechas <- df_promedios_x_barrio_2_ambiente_1%>%
  select("2018-3-1", "2018-6-1", "2018-9-1", "2018-12-1", "2019-3-1", "2019-6-1", "2019-9-1", "2019-12-1", "2020-3-1", "2020-6-1", "2020-9-1", "2020-12-1", "2021-3-1", "2021-6-1", "2021-9-1", "2021-12-1", "2022-3-1", "2022-6-1", "2022-9-1", "2022-12-1", "2023-3-1")
df_promedios_x_barrio_3_ambiente_1_fechas <- df_promedios_x_barrio_3_ambiente_1%>%
  select("2018-3-1", "2018-6-1", "2018-9-1", "2018-12-1", "2019-3-1", "2019-6-1", "2019-9-1", "2019-12-1", "2020-3-1", "2020-6-1", "2020-9-1", "2020-12-1", "2021-3-1", "2021-6-1", "2021-9-1", "2021-12-1", "2022-3-1", "2022-6-1", "2022-9-1", "2022-12-1", "2023-3-1")

#Crear un nuevo df con los nombres de las columnas (las fechas)
fila_fechas_1A <- colnames(df_promedios_x_barrio_1_ambiente_1_fechas)
fila_fechas_2A <- colnames(df_promedios_x_barrio_2_ambiente_1_fechas)
fila_fechas_3A <- colnames(df_promedios_x_barrio_3_ambiente_1_fechas)

# Transponer el dataframe
EVOLUCION_PROMEDIOS_1_AMBIENTE_transpuesta <- t(EVOLUCION_PROMEDIOS_1_AMBIENTE)
EVOLUCION_PROMEDIOS_2_AMBIENTE_transpuesta <- t(EVOLUCION_PROMEDIOS_2_AMBIENTE)
EVOLUCION_PROMEDIOS_3_AMBIENTE_transpuesta <- t(EVOLUCION_PROMEDIOS_3_AMBIENTE)

#crear un nuevo df que una los valores promedios transpuestos y las fechas
EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha <- cbind(fila_fechas_1A,EVOLUCION_PROMEDIOS_1_AMBIENTE_transpuesta)
EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha <- cbind(fila_fechas_2A,EVOLUCION_PROMEDIOS_2_AMBIENTE_transpuesta)
EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha <- cbind(fila_fechas_3A,EVOLUCION_PROMEDIOS_3_AMBIENTE_transpuesta)

colnames(EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha)
colnames(EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha)
colnames(EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha)
#la funcion colnames muestra que la segunda columna de nuestro df no posee nombre. vamos a crearle uno
colnames(EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha)[2] <- "Valor_promedio"
colnames(EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha)
colnames(EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha)[2] <- "Valor_promedio"
colnames(EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha)
colnames(EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha)[2] <- "Valor_promedio"
colnames(EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha)

#por ultimo hacer que R interprete los datos de la columna "fila_fechas_1A" (/2A/3A) como fechas
#EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha <- EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha %>%
#mutate(fila_fechas_1A = ymd(fila_fechas_1A))
#no funciono, aparentemente no es un df: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

#Convertir en data frame
EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha <- as.data.frame(EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha)
EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha <- as.data.frame(EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha)
EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha <- as.data.frame(EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha)

#intentar de nuevo
EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha <- EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha %>%
  mutate(fila_fechas_1A = ymd(fila_fechas_1A))
EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha$Valor_promedio <- as.numeric(EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha$Valor_promedio)

EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha <- EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha %>%
  mutate(fila_fechas_2A = ymd(fila_fechas_2A))
EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha$Valor_promedio <- as.numeric(EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha$Valor_promedio)

EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha <- EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha %>%
  mutate(fila_fechas_3A = ymd(fila_fechas_3A))
EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha$Valor_promedio <- as.numeric(EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha$Valor_promedio)

#Agregar una segunda linea que contextualice a la primera.
#Para eso tengo que cargar los datos correspondientes a la evolución del salario minimo vital y movil
#ademas, tengo que crear una columna con los valores correspondientes al momento de la medicion

#-----------------------#
#EVOLUCION DEL SALARIO MINIMO VITAL Y MOVIL

salario_minimo_1988 <- read_csv("../data/indice-salario-minimo-vital-movil-valores-mensuales-pesos-corrientes-desde-1988.csv")
#View(salario_minimo_1988)
#no hicieron falta las aclaraciones de "sep =',', encoding = 'UTF-8'"

salario_minimo_mensual_1988 <- salario_minimo_1988%>%
  clean_names() %>%
  select(indice_tiempo, salario_minimo_vital_movil_mensual) #el df posee variables del sal-vit-movil, solo nos interesa la mensual
#View(salario_minimo_mensual_1988)


#hacemos coincidir el periodo de tiempo con aquel de nuestro df relativo a los precios promedios de los alquileres
salario_minimo_mensual_2018 <- salario_minimo_mensual_1988[-c(1:636), ]
#View(salario_minimo_mensual_2018)
salario_minimo_mensual_2018_2023 <- salario_minimo_mensual_2018[-c(64:66), ]
#colnames(salario_minimo_mensual_2018_2023)

#volver numericos los valores de la columna "salario_minimo_vital_movil_mensual"
salario_minimo_mensual_2018_2023$salario_minimo_vital_movil_mensual <- as.numeric(salario_minimo_mensual_2018_2023$salario_minimo_vital_movil_mensual)
#volver fechas los valores de la columna "indice_tiempo"
salario_minimo_mensual_2018_2023 <- salario_minimo_mensual_2018_2023 %>%
  mutate(indice_tiempo = ymd(indice_tiempo))
colnames(salario_minimo_mensual_2018_2023)


#graficar
SALARIO_VS_ALQUILER <- ggplot() +
  geom_line(data = EVOLUCION_PROMEDIOS_1_AMBIENTE_con_fecha, aes(x = fila_fechas_1A, y = Valor_promedio, group = 1, color = "Promedio alquiler 1 ambiente")) +
  geom_line(data = EVOLUCION_PROMEDIOS_2_AMBIENTE_con_fecha, aes(x = fila_fechas_2A, y = Valor_promedio, group = 1, color = "Promedio alquiler 2 ambientes")) +
  geom_line(data = EVOLUCION_PROMEDIOS_3_AMBIENTE_con_fecha, aes(x = fila_fechas_3A, y = Valor_promedio, group = 1, color = "Promedio alquiler 3 ambientes")) +
  geom_line(data = salario_minimo_mensual_2018_2023, aes(x = indice_tiempo, y = salario_minimo_vital_movil_mensual, color = "Salario mínimo vital y móvil")) +
  labs(title = "Evolución del precio promedio del alquiler en Ciudad de Buenos Aires",
       x = "Tiempo",
       y = "Valor en ARS",
       caption = "Fuente: Base de datos de Estadísticas y Censos de Ciudad de Buenos Aires") +
  scale_color_manual(values = c("black", "blue", "green", "red"), guide = FALSE) +
  guides(color = guide_legend(title = "")) +
  theme_minimal()

#Obtenemos el siguiente grafico para elaborar un analisis

SALARIO_VS_ALQUILER

ggsave(plot = SALARIO_VS_ALQUILER, filename = "../output/SALARIO_VS_ALQUILER.png", 
        width = 30, height = 20, units = "cm")
