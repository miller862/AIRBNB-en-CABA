#Cargar librerias (poder acceder a funciones que estan fuera de las basicas de Rstudio)
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

options(scipen = 999) #"scipen": notación cientifica (una forma de abreviar numeros). Con esta linea de código configuramos R, de modo tal que solo muestre los valores en formato decimal

#crea nuevo objeto ("df_barrios"), producto de las 2 operaciones siguientes. La info que nos interesa son las coordenadas ("geometry") de cada barrio, y su comuna ("COMUNA")

df_barrios <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")%>% #crea nuevo objeto ("df_barrios"), producto de las 2 operaciones siguientes. La info que nos interesa son las coordenadas ("geometry") de cada barrio, y su comuna ("COMUNA") #la funcion "st_read" es para leer datos geoespaciales (aquellos presentes en la pagina del link)
st_transform(crs = 4326) #transformo el sistema de coordenadas #"st_transform": transforma el sistema de coordenadas. (en este caso a un sistema denominado "4326")


comunas <-st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.csv') %>% #la funcion "st_read" es para leer datos geoespaciales (aquellos presentes en la pagina del link)
  mutate(comuna =as.integer(COMUNAS)) %>% #la funcion "mutate" sirve para crear/modificar columnas. En este caso tecnicamente creamos una nueva columna ("comuna"), que es igual a la preexistente columna "COMUNA", pero en valores enteros ("as.integer")
  mutate(comuna=as.factor(comuna)) %>% #mutate: se modifican los valores de la recien creada columna "comuna" como factores ("as.factor")
  select(comuna) #la función "select" sirve para seleccionar columnas (en este caso una única, la columna "comuna", con las modificaciones previas)


st_crs(df_barrios) #chequeo el sistema de coords

df_airbnb <- read.csv("./data/listings.csv", #creo un nuevo objeto ("df_airbnb") compuesto por el archivo "listings.csv" (leído gracias a la función "read.csv"), sobre el cual se realizan las operaciones que siguen
sep = ",", #Al leer el archivo se definen a las "," como separadores de columnas
encoding = "UTF-8") #El encoding de las letras


df_airbnb2 <- st_as_sf(df_airbnb, #st_as_sf: sirve para convertir un objeto (en este caso "df_airbnb") en un objeto espacial
coords = c("longitude", "latitude"), #parte de la operacion anterior, para crear un objeto espacial determino que se va a tomar como las coordenadas, primero, la data de la columna "longitude", seguida de la data de la columna "latitude". Si nos fijaramos con la funcion "colnames" en el nuevo objeto, ambas columnas son reemplazadas por una nueva ("geometry")
crs = 4326) #transformo el objeto y el sistema de coords
st_crs(df_airbnb2) #chequeo el sistema de coords



df_airbnb2_barrios <- st_join(df_airbnb2, df_barrios) #la funcion "st_join" permite unir ambos objetos (aquel derivado de la data scrapeada de airbnb, y aquella correspondiente a los barrios de CABA), a partir de las coincidencias en las columnas "geometry". El nuevo objeto contiene las filas y columnas de "df_airbnb_2", pero se le suma la info correspondiente al resto de las columnas de "df_barrio"


df_airbnb2_barrios <- df_airbnb2_barrios %>% #se modifica el objeto "df_airbnb2_barrios" a partir de las operaciones que siguen
  clean_names() %>% #de janitor Pone todos los nombres en minuscula, elimina espacios etc.
  mutate(last_review = ymd(last_review)) %>% # con la función "ymd" formateo las fechas y le digo a r como leerla (dentro de la columna "last_review")
  mutate(comuna = as.factor(comuna)) %>%  #Le decimos a R que el número de columna es una categorización y no un número
  mutate(neighbourhood=.$neighbourhood<-tolower(df_airbnb2_barrios$neighbourhood)) #modificamos la columna "neighbourhood": le aplicamos la funcion "tolower" para que los valores dentro de esta columna esten en minuscula

#empezamos

propietarios <- df_airbnb %>% #nuevo objeto ("propietarios"), basado en el objeto "df_airbnb"
  select(host_id,host_name) %>% #el nuevo objeto estará compuesto solo por las columnas "host_id" y "host_name" de "df_airbnb"
  group_by(host_id,host_name) %>% #la funcion "group_by" permite agrupar (unir) las filas de "df_airbnb" en funcion de valores identicos en "host_id" y "host_name"
  summarise(cantidad= n(),.groups = 'drop') %>% #se suma una nueva columna ("cantidad"). El valor que presente para cada fila sera igual a "n()" (la cantidad de filas en "df_airbnb" que comparten al mismo tiempo "host_id" y "host_name")
  arrange(-cantidad) %>% #arrange: la funcion permite determinar el orden de las filas. En este caso se toma la columna "cantidad" y se aplica un orden descendiente
  as.data.frame() #"as.data.frame": que el objeto ("propietarios") sea un data frame

rm(df_airbnb) #rm: "remove", quita del environment al objeto
'''
rm(headpropietarios)
headpropietarios <-(head(propietarios,20))

stargazer(headpropietarios,                 # Export txt                                                             este choclo es un comentario
          summary = FALSE,
          type = "text",
          out = "data_stargazer_txt.txt")
'''
####limpiando algun outliaer y emprolijando la tabla y nombre de variables####

#calculamos el precio promedio por barrio
precio_x_barrio<-df_airbnb2_barrios %>%
  group_by(neighbourhood) %>% #group_by: unimos las filas que compartan valor en la columna "neighbourhood"
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% #creamos la columna "precio_promedio", igual a "mean(price)" (el promedio de la columna precio)
  arrange(-precio_promedio) #orden de las filas: descendiente según la categoría "precio_promedio"

#nos hace ruido coghlan asi que lo inspeccionamos
COGHLAN <- df_airbnb2 %>%
  filter(neighbourhood=='Coghlan') %>% #filter: filtrar, seleccionar solo las filas cuyo valor en la columna "neighbourhood" sea "Coghlan"
  arrange(-price) #orden de las filas: descendiente según la categoría "price", para identificar outliers

#eliminamos un gran outlier (".[-c(1), ]") y ordenamos por precio, y le ponemos el nombre final que va a tener la tabla con todos los datos ("AIRBNB_TODO")
AIRBNB_TODO<-df_airbnb2_barrios %>%  
  arrange(-price) %>%
  .[-c(1), ]

rm(COGHLAN,df_airbnb2_barrios) #elimina del environment "COGHLAN" y "df_airbnb2_barrios"

#Que pasa con la comuna 4?
AIRBNB_TODO %>%
  filter(comuna=='4') %>% #filtramos, seleccionamos solo las filas cuyo valor en la columna "comuna" es "4"
  arrange(-price) %>%
  view(.)

#Borramos el unico departamento que se ofertaba a 2 millones la noche (otro outlier)
AIRBNB_TODO<- AIRBNB_TODO [! grepl ('7801998', AIRBNB_TODO$id),] #! grepl: excluir dentro del objeto "AIRBNB_TODO" el valor "7801998" correspondiente a la columna "id" = 7801998

####Generando las primeras tablas ####

#volvemos a calcular el PRECIO PROMEDIO POR BARRIO sin el outlier y en base a nuestra nueva tabla
precio_x_barrio<-AIRBNB_TODO %>%
  group_by(neighbourhood) %>%
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% #se crea una nueva columna ("precio_promedio") que expresa el promedio entre los valores de la columna "price" en las filas que coincidan en el valor de la columna "neighbourhood"
  arrange(-precio_promedio)

#PRECIO PROMEDIO POR COMUNA
precio_x_comuna<-AIRBNB_TODO %>%
  group_by(comuna) %>%
  summarise(precio_promedio = mean(price),.groups = 'drop') %>%
  arrange(-precio_promedio)

#CANTIDAD DE AIRBN POR BARRIO
cantidad_x_barrio<-AIRBNB_TODO %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>% #se crea una nueva columna ("cantidad") que expresa la cantidad de airbnb ofertados para cada barrio (la sumatoria de filas unidas por coincidencias en la columna "neighbourhood")
  arrange(-cantidad)

#CANTIDAD DE AIRBNB POR COMUNA
cantidad_x_comuna<-AIRBNB_TODO %>%
  group_by(comuna) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad)

##generamos unas vistas de nuestra data deglosada por tipo de habitacion
#precio promedio por tipo

AIRBNB_TODO %>%
  group_by(room_type) %>% #se agrupa en función del tipo de vivienda ofertada
  summarise(precio_promedio = mean(price))%>% #se crea una nueva columna ("precio_promedio"), igual al promedio de las filas que comparten el mismo valor en la columna "room_type"
  st_drop_geometry()  #DROP GEOMETRY = elimina la columna geometri
#cantitad por tipo
  AIRBNB_TODO %>%
    group_by(room_type) %>%
    summarise(cantidad= n(),.groups = 'drop')%>% #se crea una nueva columna ("cantidad"), igual a la cantidad de filas que presenta cada valor en la columna "room_type"
    st_drop_geometry()
 
#TIPO DE AIRBN POR BARRIO DESGLOSADO
tipo_x_barrio<-AIRBNB_TODO %>% #se crea un nuevo objeto: "tipo_x_barrio"
  group_by(neighbourhood,room_type) %>% #se agrupan las filas con valores identicos en las columnas "neighbouhood" y "room_type" (como estas no coinciden perfectamente hay multiples filas con cada valor)
  summarise(cantidad= n(),.groups = 'drop') %>% #se crea una nueva columna ("cantidad"), igual a la cantidad de filas que coinciden simultáneamente en los valores de las columnas "neighbourhood" y "room_type"
  arrange(neighbourhood) #orden ascendente, según la columna "neighbourhood"

#TIPO DE AIRBNB POR COMUNA DESGLOSADO
tipo_x_comuna<-AIRBNB_TODO %>%
  group_by(comuna,room_type) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(comuna)

#graficamos
plot1_cantidad_x_comuna <- ggplot(tipo_x_comuna, #ggplot: la función crea un gráfico (en este caso, basado sobre la data contenida en el objeto "tipo_x_comuna")
                                  aes(x = reorder(comuna, cantidad), #"aes": el "mapeo estetico" del grafico #le decimos que reordene una columna según los valores de otra
                                      y = cantidad,
                                      fill = room_type)) + #relleno de acuerdo a una columna
  geom_bar(stat = "identity", #geombar: nueva capa de barras. #los valores de las barras comparten identidad con los valores de "cantidad"
           position = "stack") + #stak apiladas
  #coord_flip() + #cambiamos coordenadas (la x en vertical por ejemplo)
  labs(title = "Oferta por tipo y comuna")+ #LABS PARA TODAS LAS ETIQUETAS. aca solo titulo del grafico
  scale_fill_manual(name = "Tipo de inmueble", #scale_fill_manual: se definen manualmente las caracteristicas de las barras. En este caso, los colores de cada valor correspondiente a la variable "Tipo de inmueble"
                     values = c("salmon", "#F6FA70", "#00DFA2","#0079FF"))+
  theme_economist()+ #seleccion de un "tema" para el gráfico de barras
  xlab("COMUNA") + #nombre del eje x
  ylab("CANTIDAD") #nombre del eje y

dir.create("output") #Le digo que cree la carpeta output en la misma carpeta en que esta el proyecto

#lo guardamos
ggsave(plot1_cantidad_x_comuna, filename="output/ofertaxtipoycomuna.png",
       width = 20, height = 20, units = "cm")

'''
A fines de nuestro trabajo, resulta pertinente trabajar solo con el tipo: Entire home/apt
que representa un departamento que podria utilizarse por un residente de la ciudad de Buenos Aires.
A su vez genramos una tabla con las habitaciones de hotel, en la que no profundizaremos por el momento
'''

##CODIGO QUE SE ROMPIO, LUEGO ARREGLE CON EL CODIGO QUE LE SIGUE, PERO LO DEJO POR LAS DUDAS.
'''
df_barrios<- df_barrios %>%
  mutate(BARRIO=.$BARRIO<-tolower(df_barrios$BARRIO)) %>%
  str_to_title(.$neighbourhood) %>%                           <-----------ACA SE ROMPIO
  rename(neighbourhood= BARRIO) %>%
  rename(comuna=COMUNA)
'''

df_barrios<- df_barrios %>% #modificamos "df_barrios"
  mutate(BARRIO=.$BARRIO<-tolower(df_barrios$BARRIO)) %>% #tolower: modificamos el valor de la columna "BARRIO" a minuscula
  rename(neighbourhood= BARRIO) %>% #rename: modificacion del nombre de la columna (BARRIO --> neighbourhood)
  rename(comuna=COMUNA)

AIRBNB_FILTRADO<-AIRBNB_TODO%>% #nuevo objeto
  st_drop_geometry() %>% #st_drop_geometry: elimna los datos de geometria
  filter(room_type=="Entire home/apt") %>% #filter: solo considera las filas en los que el valor de la columna "room_type" es "Entire home/apt"
  .[! grepl ('46055003', .$id),] #resulta gracioso el unico departamento entero publicado en villa lugano a 30mil la noche 'ideal extranjeros' Hay que borrarlo porque nos jode el grafico
#"Error in is.factor(x) : objeto 'AIRBNB_FILTRADO' no encontrado". creo que hay un conflicto porque llamas a "AIRBNB_FILTRADO" en la operacion para crear al dataframe "AIRBNB_FILTRADO"
#es por la linea 206. si se comenta el "%>%" y la linea 206 desaparece el error

hotelrooms<-AIRBNB_TODO%>%
  st_drop_geometry() %>%
  filter(room_type=="Hotel room")

#vamos a generar versiones filradas de las tablas que ya tenia, donde solo me quedo con la categoria Entire Home/apt

Fprecio_x_barrio<-AIRBNB_FILTRADO %>%
  group_by(neighbourhood) %>% #une las filas que comparten el mismo valor en la columna "neighbourhood"
  summarise(precio_promedio = mean(price),.groups = 'drop') %>% #crea nueva columna ("precio promedio"), igual al promedio entre las filas agrupadas
  left_join(x=df_barrios,y= .,by='neighbourhood') %>% #left_join: une dos data frames ("df_barrios" y aquel producto de las operaciones anteriores -simbolizado por el "."-), a partir de coincidencias en la columna "neighbourhood". Es un "left_join", asique mantiene todas las filas de "df_barrios", aunque no coincidan con una del dataframe recien creado
  select(neighbourhood, precio_promedio,comuna) %>%
  arrange(-precio_promedio)

 
Fprecio_x_comuna<-AIRBNB_FILTRADO %>%
  group_by(comuna) %>%
  summarise(precio_promedio = mean(price),.groups = 'drop') %>%
  arrange(-precio_promedio)


Fcantidad_x_barrio<-AIRBNB_FILTRADO %>%
  group_by(neighbourhood) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  left_join(x=df_barrios,y= .,by='neighbourhood') %>% #left_join: une dos dataframes ("df_barrios" y el resultante de las operacioes anteriores), en funcion de coincidencias dentro de la columna "neighbourhood" (se mantienen aquellas filas de "df_barrios" sin coincidencia, pero se eliminan aquellas del dataframe anterior sin coincidencia)
  select(neighbourhood, cantidad,comuna) %>%
  arrange(-cantidad)


Fcantidad_x_comuna<-AIRBNB_FILTRADO %>%
  group_by(comuna) %>%
  summarise(cantidad= n(),.groups = 'drop') %>%
  arrange(-cantidad)

####PLOTEOS####

### MAPAS ####
#MAPA DE AIRBNB EN CABA DESGLOSADO POR TIPO
mapa_tipos<-ggplot() + #para el ggplot se usan "+" en vez de "%>%" para indicar una secuencia de operaciones
  geom_sf(data = df_barrios) + #toma la "data" de las geometrias del dataframe "df_barrios" (concretamente para la construccion de los "polígonos", que representaran a los barrios)
  geom_sf(data = AIRBNB_TODO,aes(color=room_type),size=0.1,alpha = 0.5)+ #toma la "data" de las geometrias del dataframe "AIRBNB_TODO" (concretamente para ubicar en el gráfico la locacion de los alquileres ofertados) #"aes(color=room_type)": el color será determinado segun el valor en la columna "room_type"
  labs(x="Longitud", #"labs": determinacion de los nombres dentro del gráfico. #el eje "x" se llama "Longitud", etc.
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
  filter(room_type=='Entire home/apt') %>% #se filtran, excluyen las filas que no tienen el valor "Entire home/apt" como valor en la columna "room_type"
  ggplot()+ #se grafica
  geom_sf(data = df_barrios) + #se especifica de que dataframe tomar la data
  geom_sf(aes(color=room_type),size=0.05)+ #se especifica que varie el color en funcion del valor en la columna "room_type"
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
  geom_sf(aes(fill = cantidad))+ #se especifica que varie el color en funcion del valor en la columna "cantidad"
  scale_fill_viridis_c(trans = "log", breaks=c(50,100,500,1000,4000)) + #scale_fill_viridis_c: se toma una paleta de colores. #breaks=c(50,100,500,1000,4000): marca el punto en que cambia de color dentro de la paleta
  labs(title = "Cantidad de Airbnb x barrio (solo 'entire home/apt')",
        subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "cantidad",
       caption = "Fuente: inside Airbnb")+
  theme_void()+
  geom_sf_label(aes(label=neighbourhood),size=2.5) #geom_sf_label: se customizan las etiquetas que acompañan a los barrios (e indica que tomen el valor de la columna "neighbourhood")
mapa_cantidad_barrio


 
  #Guardo el mapa
ggsave(plot = plot2, filename = "output/mapa_airb_x_barrio_1.png",
         width = 20, height = 20, units = "cm")
   
#precio promedio por barrio (solo departamentos enteros o casas)
mapa_precio_barrio<-ggplot(data = Fprecio_x_barrio) + #se especifica que tome la data para el grafico del dataframe "Fprecio_x_barrio"
  geom_sf(aes(fill = precio_promedio))+ #complete el color en funcion del valor de la columna "precio promedio"
  scale_fill_viridis_c(option = 'B') + #el color se completa tomando la paleta viridis #especificar "(option = 'B')"
  labs(title = "Precio promedio de Airbnb por noche x barrio (solo 'entire home/apt')",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "Precio promedio en pesos",                        #añado las etiquetas
       caption = "Fuente: inside Airbnb")+
  theme_void()+
  geom_sf_label(aes(label=neighbourhood),size=2.5,) #geom_sf_label: se customizan las etiquetas que acompañan a los barrios (e indica que tomen el valor de la columna "neighbourhood")
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
            out = "df_cantidad_x_barrio_txt.txt")  #nombre del archivo


####Creo nuevas tablas que cruzan las tablas generadas previamente

cantidad_vs_precio_barrio <-Fcantidad_x_barrio %>%
  st_drop_geometry() %>%
  right_join(Fprecio_x_barrio, by='neighbourhood') %>% #join sobre la tabla de la derecha
  select(-comuna.y) %>%    #se generan 2 columnas comuna, asi que hay que eliminar 1
  rename("comuna"="comuna.x")

cantidad_vs_precio_comuna <- comunas %>%                #aca hice 2 joins. al df comunas
  left_join(Fcantidad_x_comuna,by='comuna') %>%         #le uni cantidades filtradas
  left_join(Fprecio_x_comuna,by='comuna')               #y precios filtrados
 
##grafico Scatterplot de precio promedio por comuna en funcion de la cantidad de Airbnb por comuna
scattercomunas<- ggplot(cantidad_vs_precio_comuna %>%
         mutate(comuna = as.factor(comuna)),
       aes(x = cantidad, y = precio_promedio, color = comuna)) +
  geom_point(
    aes(),
    alpha = 0.6 #transparencia
  ) +
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "#577CBC"))+ #fondo del grafico
  labs(x = "CANTIDAD",
       y = "PRECIO PROMEDIO POR NOCHE EN PESOS",
       title = "PRECIO PROMEDIO DE AIRBNB SEGUN SU CANTIDAD POR COMUNA") +
  geom_label(aes(label = comuna, color = comuna), show.legend = FALSE) + #agrego las etiquetitas d elas comunas
  scale_color_manual(values = unique(cantidad_vs_precio_comuna$comuna)) + #corrijo un problema en la leyenda
  guides(color = guide_legend(title = "Comuna")) #titulo de la leyenda
scattercomunas
#lo guardamos
ggsave(plot = scattercomunas, filename = "output/scattercomunas.png",
       width = 20, height = 20, units = "cm")


##estudio de la distribucion territorial de la concentracion##
head(propietarios,10)

#Elabaroamos una tabla con todos los 394 departamentos concentrados por los 3 mayores concentradores de AIRBNB
TOP3 <- AIRBNB_TODO %>%
  filter(host_id %in% c(1021694, 55553719,3469227)) %>% #filtro por una lista
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
#levanto un geojson de la ciudad y lo filtro para quedarme solo con palermo
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
  guides(color = guide_legend(override.aes = list(size = 4)))+ #esto aumenta el tamaño d elos puntos en la leyenda
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
#aplicamos la función para crear la vista de lo que sería palermo despues del registro unico
PALERMO_DESPUES<- palermo %>% group_by(host_id) %>% mutate_disponible()


#en esta vista chequeo como cambiaria el uso de la propiedad de este propietario
PALERMO_DESPUES %>%
  filter(host_id=='1021694') %>%  
  view()

#corroboro que 2633 vviendas pasaria a estar libres para ser alquiladas por residentes argentinos
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
  filter(neighbourhood== 'palermo'| neighbourhood=='caballito') %>% #filtro al mismo tiempo diciendo que quiero 2 barrios
  mutate(precio_promedio = precio_promedio*30) %>%    #al preico promedio por dia lo multiplico x 30 para tenr un precio promedio mensual
  mutate(tipo= 'AIRBNB')


QUE_CONVIENE <- rbind(palercaba,MES_AIRBNB_palercaba) #cruzamos los datos    (rbindo o row bind pega dos tablas una abajo d ela otra. deben tener mismo numeor de columnas)


plot_QUE_CONVIENE<-QUE_CONVIENE%>% #ploteamos para poder comparar
  ggplot(aes(reorder(neighbourhood, precio_promedio),precio_promedio,fill=tipo ))+
  geom_bar(stat = "identity",
           position=position_dodge())+ #position dodge al contratio de stack hace que las barras no se apilen
  coord_flip()+
  labs(x="BARRIO",
       y="ALQUILER MENSUAL PROMEDIO",
       title = "Precio promedio de alquiler mensual en Palermo y Caballito",
       fill= "Tipo",
       caption = "Fuente: inside Airbnb y GCBA",
       legend ="")+
  theme_economist()
plot_QUE_CONVIENE
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
  #todo esto para garantizar la trnasparencia
  theme(legend.background = element_rect(fill="transparent", color = NA),
        legend.box.background = element_rect(fill="transparent", color = NA),
        panel.background =element_rect(fill="transparent",color = NA),
        plot.background = element_rect(fill="transparent",color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
                                       )

ggsave(plot = plot_QUE_CONVIENE, filename = "output/trnasp.png", bg="transparent",
       width = 30, height = 20, units = "cm")