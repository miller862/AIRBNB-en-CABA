
library(ggthemes)
library(stargazer)
library(treemapify)
library(ggplot2)
library(eeptools)
library(tidyverse)
library(stringr)
library(sf)
library(geojson)
library(rgdal)
library(dplyr)
library(geoAr)
library(ggmap)
library(ggrepel)
library(janitor)
library(geojsonsf)
library(viridis)
library(viridisLite)

#Primero tomamos prestado un poco del codigo de la clase 7

options(scipen = 999)


df_barrios <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")%>%
st_transform(crs = 4326) #transformo el sistema de coordenadas 

comunas <-st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.csv') %>% 
  mutate(comuna =as.integer(COMUNAS)) %>% 
  mutate(comuna=as.factor(comuna)) %>% 
  select(comuna)


st_crs(df_barrios) #chequeo el sistema de coords

df_airbnb <- read.csv("./data/listings.csv", 
sep = ",", #Defino el separador de las columnas
encoding = "UTF-8") #El encoding de las letras


df_airbnb2 <- st_as_sf(df_airbnb, 
coords = c("longitude", "latitude"), 
crs = 4326) #transformo el objeto y el sistema de coords
st_crs(df_airbnb2) #chequeo el sistema de coords



df_airbnb2_barrios <- st_join(df_airbnb2, df_barrios)


df_airbnb2_barrios <- df_airbnb2_barrios %>%
  clean_names() %>% #de janitor Pone todos los nombres en minuscula, elimina espacios etc.
  mutate(last_review = ymd(last_review)) %>% # con la función "ymd" formateo las fechas y le digo a r como leerla
  mutate(comuna = as.factor(comuna)) %>%  #Le decimos a R que el número de columna es una categorización y no un número
  mutate(neighbourhood=.$neighbourhood<-tolower(df_airbnb2_barrios$neighbourhood))

#empezamos

propietarios <- df_airbnb %>% 
  select(host_id,host_name) %>% 
  group_by(host_id,host_name) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(-cantidad) %>% 
  as.data.frame()

rm(df_airbnb)

####limpiando algun outliaer y emprolijando la tabla y nombre de variables####

#calculamos el precio promedio por barrio
precio_x_barrio<-df_airbnb2_barrios %>% 
  group_by(neighbourhood) %>% 
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% 
  arrange(-precio_promedio)

#nos hace ruido coghlan asi que lo inspeccionamos
COGHLAN <- df_airbnb2 %>% 
  filter(neighbourhood=='Coghlan') %>% 
  arrange(-price)

#eliminamos un gran outlier y ordenamos por precio, y le ponemos el nombre final que va a tener la tabla con todos los datos
AIRBNB_TODO<-df_airbnb2_barrios %>%  
  arrange(-price) %>% 
  .[-c(1), ]

rm(COGHLAN,df_airbnb2_barrios)
#Que pasa con la comuna 4?

AIRBNB_TODO %>% 
  filter(comuna=='4') %>% 
  arrange(-price) %>% 
  view(.)

#Borramos el unico departamento que se ofertaba a 2 millones la noche
AIRBNB_TODO<- AIRBNB_TODO [! grepl ('7801998', AIRBNB_TODO$id),] 

####Generando las primeras tablas ####

#volvemos a calcular el PRECIO PROMEDIO POR BARRIO sin el outlier y en base a nuestra nueva tabla
precio_x_barrio<-AIRBNB_TODO %>% 
  group_by(neighbourhood) %>% 
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% 
  arrange(-precio_promedio)

#PRECIO PROMEDIO POR COMUNA
precio_x_comuna<-AIRBNB_TODO %>% 
  group_by(comuna) %>% 
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% 
  arrange(-precio_promedio)

#CANTIDAD DE AIRBN POR BARRIO
cantidad_x_barrio<-AIRBNB_TODO %>% 
  group_by(neighbourhood) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(-cantidad)

#CANTIDAD DE AIRBNB POR COMUNA
cantidad_x_comuna<-AIRBNB_TODO %>% 
  group_by(comuna) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(-cantidad)

##generamos unas vistas de nuestra data deglosada por tipo de habitacion

#precio promedio por tipo
AIRBNB_TODO %>% 
  group_by(room_type) %>% 
  summarise(precio_promedio = mean(price))%>% 
  st_drop_geometry()
#cantitad por tipo
  AIRBNB_TODO %>% 
    group_by(room_type) %>% 
    summarise(cantidad= n(),.groups = 'drop')%>% 
    st_drop_geometry()

  
#TIPO DE AIRBN POR BARRIO DESGLOSADO
tipo_x_barrio<-AIRBNB_TODO %>% 
  group_by(neighbourhood,room_type) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(neighbourhood)

#TIPO DE AIRBNB POR COMUNA DESGLOSADO
tipo_x_comuna<-AIRBNB_TODO %>% 
  group_by(comuna,room_type) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(comuna)

#graficamos
plot1_cantidad_x_comuna <- ggplot(tipo_x_comuna,
                                  aes(x = reorder(comuna, cantidad), #le decimos que reordene una columna según los valores de otra 
                                      y = cantidad,
                                      fill = room_type)) + #relleno de acuerdo a una columna
  geom_bar(stat = "identity", 
           position = "stack") + #stak apiladas
  #coord_flip() + #cambiamos coordenadas (la x en vertical por ejemplo)
  labs(title = "Oferta por tipo y comuna")+
  scale_fill_manual(name = "Tipo de inmueble", 
                     values = c("salmon", "#F6FA70", "#00DFA2","#0079FF"))+
  theme_economist()+
  xlab("COMUNA") +
  ylab("CANTIDAD")



#lo guardamos
ggsave(plot1_cantidad_x_comuna, filename="output/ofertaxtipoycomuna.png",
       width = 20, height = 20, units = "cm")

'''
A fines de nuestro trabajo, resulta pertinente trabajar solo con el tipo: Entire home/apt
que representa un departamento que podria utilizarse por un residente de la ciudad de Buenos Aires.
A su vez genramos una tabla con las habitaciones de hotel, en la que no profundizaremos por el momento
'''
#AL MISMO TIEMPO BORRAMOS LAS GEOMETRIAS

##CODIGO QUE SE ROMPIO, LUEGO ARRGLE CON EL CODIGO QUE LE SIGUE, PERO LO DEJO POR LAS DUDAS.
'''
df_barrios<- df_barrios %>% 
  mutate(BARRIO=.$BARRIO<-tolower(df_barrios$BARRIO)) %>% 
  str_to_title(.$neighbourhood) %>%                           <-----------ACA SE ROMPIO
  rename(neighbourhood= BARRIO) %>% 
  rename(comuna=COMUNA)
'''

df_barrios<- df_barrios %>% 
  mutate(BARRIO=.$BARRIO<-tolower(df_barrios$BARRIO)) %>% 
  rename(neighbourhood= BARRIO) %>% 
  rename(comuna=COMUNA)

AIRBNB_FILTRADO<-AIRBNB_TODO%>%
  st_drop_geometry() %>% 
  filter(room_type=="Entire home/apt") %>% 
  .[! grepl ('46055003', .$id),] #resulta gracioso el unico departamento entero publicado en villa lugano a 30mil la noche 'ideal extranjeros' Hay que borrarlo porque nos jode el grafico

hotelrooms<-AIRBNB_TODO%>%
  st_drop_geometry() %>% 
  filter(room_type=="Hotel room")

#vamos a generar versiones filradas de las tablas que ya tenia, donde solo me quedo con la categoria Entire Home/apt

Fprecio_x_barrio<-AIRBNB_FILTRADO %>%
  group_by(neighbourhood) %>% 
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% 
  left_join(x=df_barrios,y= .,by='neighbourhood') %>% 
  select(neighbourhood, precio_promedio,comuna) %>%
  arrange(-precio_promedio)

  
Fprecio_x_comuna<-AIRBNB_FILTRADO %>% 
  group_by(comuna) %>% 
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% 
  arrange(-precio_promedio)


Fcantidad_x_barrio<-AIRBNB_FILTRADO %>% 
  group_by(neighbourhood) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  left_join(x=df_barrios,y= .,by='neighbourhood') %>% 
  select(neighbourhood, cantidad,comuna) %>% 
  arrange(-cantidad)


Fcantidad_x_comuna<-AIRBNB_FILTRADO %>% 
  group_by(comuna) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(-cantidad)

####PLOTEOS####

### MAPAS ####
#MAPA DE AIRBNB EN CABA DESGLOSADO POR TIPO
mapa_tipos<-ggplot() +
  geom_sf(data = df_barrios) + 
  geom_sf(data = AIRBNB_TODO,aes(color=room_type),size=0.1,alpha = 0.5)+
  labs(x="Longitud",
       y="Latitud",
       title = "Airbnb en CABA por tipo",
       fill= "",
       caption = "Fuente: inside Airbnb")+
  theme_void()+
  scale_color_manual(name = "Tipo de inmueble", 
                     values = c("salmon", "#F6FA70", "#00DFA2","#0079FF"))+
  guides(color = guide_legend(override.aes = list(size = 4)))
mapa_tipos
#Guardo el mapa
ggsave(plot = mapa_tipos, filename = "output/mapa_tipos.png", 
       width = 20, height = 20, units = "cm")

#MAPA DE AIRBNB EN CABA FILTRADO POR ENTIRE HOME/APT
mapa_solo_entire <-AIRBNB_TODO %>% 
  filter(room_type=='Entire home/apt') %>% 
  ggplot()+
  geom_sf(data = df_barrios) + 
  geom_sf(aes(color=room_type),size=0.05)+
  labs(x="Longitud",
       y="Latitud",
       title = "AIRBNB, Deptos completos",
       fill= "",
       caption = "Fuente: inside Airbnb")+
  scale_color_manual(name = "Tipo de inmueble", 
                     values = "salmon")+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  theme_void()
mapa_solo_entire
#Guardo el mapa
ggsave(plot = mapa_solo_entire, filename = "output/mapa_entires.png", 
       width = 20, height = 20, units = "cm")

#Cantidad de AIRBNB x barrio (solo departamentos enteros o casas)

mapa_cantidad_barrio<-ggplot(data = Fcantidad_x_barrio) +
  geom_sf(aes(fill = cantidad))+ 
  scale_fill_viridis_c(trans = "log", breaks=c(50,100,500,1000,4000)) + 
  labs(title = "Cantidad de Airbnb x barrio (solo 'entire home/apt')",
        subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "cantidad",
       caption = "Fuente: inside Airbnb")+
  theme_void()+
  geom_sf_label(aes(label=neighbourhood),size=2.5)
mapa_cantidad_barrio

dir.create("output")
  
  #Guardo el mapa
ggsave(plot = mapa_cantidad_barrio, filename = "output/mapa_airb_x_barrio_1.png", 
         width = 20, height = 20, units = "cm")
    
#precio promedio por barrio (solo departamentos enteros o casas)
mapa_precio_barrio<-ggplot(data = Fprecio_x_barrio) +
  geom_sf(aes(fill = precio_promedio))+ 
  scale_fill_viridis_c(option = 'B') + 
  labs(title = "Precio promedio de Airbnb por noche x barrio (solo 'entire home/apt')",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "Precio promedio en pesos",
       caption = "Fuente: inside Airbnb")+
  theme_void()+
  geom_sf_label(aes(label=neighbourhood),size=2.5,)
mapa_precio_barrio
#Guardo el mapa
ggsave(plot = mapa_precio_barrio, filename = "output/mapa_$_x_barrio_1.png", 
       width = 20, height = 20, units = "cm")

#exportando tablas en txt
Fprecio_x_barrio %>% 
  st_drop_geometry() %>% 
  stargazer(.,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "df_$_barrio_txt.txt")

Fcantidad_x_barrio %>% 
  st_drop_geometry() %>% 
  stargazer(.,                 # Export txt
            summary = FALSE,
            type = "text",
            out = "df_cantidad_x_barrio_txt.txt")


####Creo nuevas tablas que cruzan las tablas generadas previamente

cantidad_vs_precio_barrio <-Fcantidad_x_barrio %>% 
  st_drop_geometry() %>% 
  right_join(Fprecio_x_barrio, by='neighbourhood') %>% 
  select(-comuna.y) %>% 
  arrange(-precio_promedio)


cantidad_vs_precio_comuna <- comunas %>%
  left_join(Fcantidad_x_comuna,by='comuna') %>% 
  left_join(Fprecio_x_comuna,by='comuna') %>% 
  arrange(-precio_promedio)
  
##grafico Scatterplot de precio promedio por comuna en funcion de la cantidad de Airbnb por comuna
scattercomunas<- ggplot(cantidad_vs_precio_comuna %>%
         mutate(comuna = as.factor(comuna)),
       aes(x = cantidad, y = precio_promedio, color = comuna)) +
  geom_point(
    aes(),
    alpha = 0.6
  ) +
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "#576CBC"))+
  labs(x = "CANTIDAD",
       y = "PRECIO PROMEDIO POR NOCHE EN PESOS",
       title = "PRECIO PROMEDIO DE AIRBNB SEGUN SU CANTIDAD POR COMUNA") +
  geom_label(aes(label = comuna, color = comuna), show.legend = FALSE) +
  scale_color_manual(values = unique(cantidad_vs_precio_comuna$comuna)) +
  guides(color = guide_legend(title = "Comuna"))

#lo guardamos
ggsave(plot = scattercomunas, filename = "output/scattercomunas.png", 
       width = 20, height = 20, units = "cm")


##estudio de la distribucion territorial de la concentracion##
head(propietarios,10)

#Elabaroamos una tabla con todos los 394 departamentos concentrados por los 3 mayores concentradores de AIRBNB
TOP3 <- AIRBNB_TODO %>% 
  filter(host_id %in% c(1021694, 55553719,3469227)) %>% 
  view()

#contamos y agrupamos
TOP3agregado<-TOP3 %>% 
  group_by(neighbourhood,host_name) %>% 
  summarise(cantidad= n(),.groups = 'drop') %>% 
  arrange(host_name)

#Graficamos
plot_TOP3 <- ggplot(TOP3agregado,
                                  aes(x = reorder(neighbourhood,cantidad), #le decimos que reordene una columna según los valores de otra 
                                      y = cantidad,
                                      fill = host_name)) + #relleno de acuerdo a una columna
  geom_bar(stat = "identity", 
           position = "stack") + #stak apiladas
  #coord_flip() + #cambiamos coordenadas (la x en vertical por ejemplo)
  labs(y="CANTIDAD",
       x="BARRIO",
       title = "TOP 3 concentradores: distribución por barrio",
       fill='Propietario',
       caption = "Fuente: inside Airbnb")+
  theme_economist()
plot_TOP3

#guardamos
ggsave(plot = plot_TOP3, filename = "output/plottop3.png", 
       width = 40, height = 20, units = "cm")

#ploteo del prototipo#
#ANTES#

'''
################PROTOTIPO######################
'''
geopalermo <- geojson_sf("http://data.insideairbnb.com/argentina/ciudad-aut%C3%B3noma-de-buenos-aires/buenos-aires/2023-03-29/visualisations/neighbourhoods.geojson") %>% 
  select(-neighbourhood_group) %>% 
  filter(neighbourhood=='Palermo')

st_crs(geopalermo)

#Creamos a palermo filtrando por barrio y casas/deptos enteros
palermo<-AIRBNB_TODO %>% 
  filter(room_type=='Entire home/apt') %>% 
  filter(neighbourhood=='palermo')

#genreamos un mapa de palermo y todos sus deptos enteros en airbnb
mapa_palermo_antes <- ggplot(data=palermo)+
  geom_sf(data=geopalermo) + 
  geom_sf(aes(color=room_type),size=0.05)+
  labs(x="",
       y="",
       title = "Mapa de Palermo ANTES",
       fill= "",
       caption = "7255 viviendas solo para AIRBNB")+
  scale_color_manual(name = "Tipo", 
                     values = c("salmon"))+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  theme_void()
mapa_palermo_antes

#guardamos el mapa
ggsave(plot = mapa_palermo_antes, filename = "output/palermoantesraw.png", 
       width = 20, height = 20, units = "cm")

#Funcion con ayuda de Google Bard (toni consultado)
#lo que hace es convertir el valor de la columna  'room_type' de 'entire home/apt' a 'libre'.
#la funcion se ejecuta sobre el 70% de las propiedades de cada host
mutate_disponible <- function(x) {
  for (i in unique(x$host_id)) {
    idx <- sample(which(x$host_id == i), 0.7 * length(which(x$host_id == i)))
    x$room_type[idx] <- "libre"
  }
  x
}
#aplicamos la función para crear la visa de lo que sería palermo despues del registro unico
PALERMO_DESPUES<- palermo %>% group_by(host_id) %>% mutate_disponible()

PALERMO_DESPUES %>% 
  filter(host_id=='1021694') %>% 
  view()

PALERMO_DESPUES %>% 
  group_by(room_type) %>% 
  summarise(cantida= n(),.groups = 'drop')
  view()

mapa_palermo_despues <- ggplot(data=PALERMO_DESPUES)+
  geom_sf(data=geopalermo) + 
  geom_sf(aes(color=room_type),size=0.05)+
  labs(x="",
       y="",
       title = "Mapa de Palermo DESPUES",
       fill= "TIPO",
       caption = "2633 nuevas viviendas para Alquier de residentes de un total de 7255")+
  scale_color_manual(name = "Tipo", 
                     values = c("salmon","green"))+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  theme_void()
mapa_palermo_despues

ggsave(plot = mapa_palermo_despues, filename = "output/palermodespuesraw.png", 
       width = 20, height = 20, units = "cm")


'''
CRUCE DE DATOS
'''
#precios de alquiler promedio informados por el gobierno de caba
#para el primer trimestre del 2023. en Palermo y caballito
#acá tenemos precios mensuales promedio para estos 2 barrios
palercaba <- read.csv("data/palercaba.csv") %>% #levantamos la tabla elaborada en el script auxiliar
  mutate(tipo='alquiler convencional') %>% 
  select(-X)

#Hacemos el ejercicio de calcular el dinero que entra a una persona por alquilar un AIRBNB en Palermo y caballito en promedio.
MES_AIRBNB_palercaba<-Fprecio_x_barrio %>% 
  select(-comuna) %>% 
  st_drop_geometry() %>% 
  filter(neighbourhood== 'palermo'| neighbourhood=='caballito') %>% 
  mutate(precio_promedio = precio_promedio*30) %>% 
  mutate(tipo= 'AIRBNB')


QUE_CONVIENE <- rbind(palercaba,MES_AIRBNB_palercaba) #cruzamos los datos

write.csv(QUE_CONVIENE, "data/QUE_CONVIENE.csv")

plot_QUE_CONVIENE<-QUE_CONVIENE%>% #ploteamos para poder comparar
  ggplot(aes(reorder(neighbourhood, precio_promedio),precio_promedio,fill=tipo ))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  coord_flip()+
  labs(x="BARRIO",
       y="ALQUILER MENSUAL PROMEDIO",
       title = "Precio promedio de alquiler mensual en Palermo y Caballito",
       fill= "Tipo",
       caption = "Fuente: inside Airbnb y GCBA",
       legend ="")+
  theme_economist()

ggsave(plot = plot_QUE_CONVIENE, filename = "output/QUECONVIENE.png", 
       width = 30, height = 20, units = "cm")


#ejemplo de como exportar un grafico con fondo transparente para usar en la presentacion
 
plot_QUE_CONVIENE<-QUE_CONVIENE%>% #ploteamos para poder comparar
  ggplot(aes(reorder(neighbourhood, precio_promedio),precio_promedio,fill=tipo ))+
  geom_bar(stat = "identity",
           position=position_dodge())+
  coord_flip()+
  labs(x="BARRIO",
       y="ALQUILER MENSUAL PROMEDIO",
       title = "Precio promedio de alquiler mensual en Palermo y Caballito",
       fill= "Tipo",
       caption = "Fuente: inside Airbnb y GCBA",
       legend ="")+
  theme_minimal()+
  theme(legend.background = element_rect(fill="transparent", color = NA),
        legend.box.background = element_rect(fill="transparent", color = NA),
        panel.background =element_rect(fill="transparent",color = NA),
        plot.background = element_rect(fill="transparent",color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
                                       )

ggsave(plot = plot_QUE_CONVIENE, filename = "output/trnasp.png", bg="transparent",
       width = 30, height = 20, units = "cm")

##imprimiendo data con stargazer para incorporar al brief##

Fcantidad_x_barrio %>% st_drop_geometry() %>% 
  select(-comuna)%>% 
  stargazer(                # Export txt
          summary = FALSE,
          type = "text",
          out = "output/Fcantidad_x_barrio.txt")

Fprecio_x_barrio %>% st_drop_geometry() %>% 
  select(-comuna)%>% 
  stargazer(                # Export txt
    summary = FALSE,
    type = "text",
    out = "output/Fprecio_x_barrio.txt")

QUE_CONVIENE%>% st_drop_geometry() %>% 
  stargazer(                # Export txt
    summary = FALSE,
    type = "text",
    out = "output/queconviene.txt")

cantidad_vs_precio_comuna%>% st_drop_geometry() %>% 
  stargazer(                # Export txt
    summary = FALSE,
    type = "text",
    out = "output/Fprecio_x_barrio.txt")


TOP3 %>% filter(neighbourhood=="san nicolas") %>% view()
