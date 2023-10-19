


library(stargazer)
library(treemapify)
library(viridis)
library(RColorBrewer)

library(ggplot2)
library(eeptools)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)

#cargamos la encuesta
encuesta <-read_excel("./data/EncuestaLab.xlsx",sheet =1, col_names= TRUE)

#mejoramos los nombres de las columnas
encuesta<- encuesta %>% 
  rename(edad= colnames(encuesta)[1],
         genero= colnames(encuesta)[2],
         estudiante = colnames(encuesta)[3],
         alquila = colnames(encuesta)[5],
         F1_pq_no_alquila= colnames(encuesta)[6],
         vive_con= colnames(encuesta)[7],
         barrio_caba= colnames(encuesta)[8],
         quiere_mudarse= colnames(encuesta)[9],
         F2_pq_no_muda= colnames(encuesta)[10],
         trabaja= colnames(encuesta)[11],
         q_trabajo =colnames(encuesta)[12],
         t_trabajo =colnames(encuesta)[13],
         alquiler_sobre_salario = colnames(encuesta)[14],
         puede_recibir_ayuda = colnames(encuesta)[15],
         problema= colnames(encuesta)[16],
         desarrolle=colnames(encuesta)[17])

#La encuesta base tenia 99 respuestas. Recopilamos la respuesta numero 100 y agregaos el registro con codigo

rta100<- c("20-23","Masculino","Si","C.A.B.A.","No","Propietario","Con amigue/amigues","Monserrat",NA,NA,"Si",1,"Relación de dependencia",NA,"Si",NA,NA)
rta100
encuesta100<- rbind(encuesta, rta100)
encuesta<-encuesta100
rm(encuesta100)

#veo los tipos de datos
glimpse(encuesta)

#genero 2 subsets de datos con quienes alquilan y quienes no
df_inquilinos <-encuesta %>%
  filter(alquila=="Si")
#solo el 33% de los encuestados es inquilino

df_no_inquilinos<-encuesta %>%
  filter(alquila=="No")
#el 66% no es inquilino
###ANALISIS DE LOS NO INQUILINOS###

##preguntamos a los no inquilinos, por que no alquilan
#primero corregimos un temita con una variable
df_no_inquilinos$F1_pq_no_alquila[df_no_inquilinos$F1_pq_no_alquila== "Vivo en un depto. propio"] <- "Propietario"

pq_no_inquilinos <- df_no_inquilinos %>% 
  group_by(F1_pq_no_alquila) %>% 
  summarise(cantidad=n()) %>% 
  arrange(-cantidad)

pq_no_inquilinos<-na.omit(pq_no_inquilinos) %>% 
  mutate(prob = round(prop.table(cantidad),4)*100)

#pedimos que indiquen su mayor problema
df_problemas <-df_no_inquilinos %>%
  group_by(problema) %>% 
  summarise(cantidad=n()) %>% 
  arrange(-cantidad)
#lo vmeos en porcentaje
df_problemas_prob<- na.omit(df_problemas) %>% 
  mutate(prob = round(prop.table(cantidad),4)*100)
#sobrescribo mi variable inical y borro est aultima
df_problemas<-df_problemas_prob
rm(df_problemas_prob)

####Analisis de los Inquilinos####

df_trabaja<-encuesta %>%
  filter(trabaja=="Si") %>%
  filter(alquila=="Si") %>% 
  select(edad,genero,estudiante,alquila,barrio_caba,q_trabajo,t_trabajo,alquiler_sobre_salario,puede_recibir_ayuda,problema)

df_peso_alquiler<-df_trabaja %>% 
  group_by(alquiler_sobre_salario) %>% 
  summarise(cantidad=n()) %>% 
  arrange(-cantidad) %>% 
  mutate(prob = round(prop.table(cantidad),4)*100)

#vemos que nos dijo la gente que desarrolló
unique(na.omit(encuesta$desarrolle))


#prints
stargazer(pq_no_inquilinos,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "pq_no_inquilinos_txt.txt")

stargazer(df_problemas,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "df_problemas_txt.txt")

stargazer(df_peso_alquiler,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "df_peso_alquiler_txt.txt")
