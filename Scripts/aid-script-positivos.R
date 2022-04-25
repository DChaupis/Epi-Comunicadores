#############################################################################
#############################################################################
###                                                                       ###
###                      ANÁLISIS INICIAL DE LA DATA                      ###
###             FUNDACIÓN TELEFÓNICA - ALTA TECNOLOGÍA ANDINA             ###
###                              FUTURO SALUD                             ###
###                                                                       ###
###  Epidemiología para comunicadores con datos abiertos, caso COVID-19.  ###
###                              ABRIL, 2022                              ###
###                                                                       ###
#############################################################################
#############################################################################

##------------------------------------------------------------------------------
"Tiene como principal objetivo brindar un conocimiento confiable de los datos que 
se estén analizando e interpretando, valiéndose de una seria de procedimientos que 
aseguren la replicabilidad de resultados y su transparencia basado en una visión 
de ciencia abierta."

## Basado en The STRATOS Iniciative - Topic Group 3.
## https://stratos-initiative.org/group_3
##------------------------------------------------------------------------------



### PASO 1  
### Preparando la metadata ("...are data that describe other data.")
### Accedemos a nuestro directorio e identificamos los tipos de variables

set.seed(9)
dataepi <- read.csv("positivos_covid_minsa.csv", sep = ";", header = T, 
                    encoding = "UTF-8")
#View(dataepi)
#file.info("positivos_covid_minsa.csv")$size

#install.packages("DataExplorer")

library(tidyverse, dplyr) # gestión de datos y visualización
library(DataExplorer) # exploración estadística descriptiva en plots

attach(dataepi) # adherir las variables paraa cada function()

### Identificamos los tipos de variables: Numéricas || Categóricas 

dim(dataepi) # filas y columnas
names(dataepi) #nombre de variables
glimpse(dataepi) # que tipo de variables: <chr> o <int> 
plot_str(dataepi) # podemos graficar la organización de la data
head(dataepi,n = 10) # descubramos las n primeras o últimas (tail)
summary(dataepi) # cuales son las variables (n columnas)
class(dataepi) #identifica el formato configurado


unique() # para cada variable solo las observaciones únicas 
is.na() # permite identificar los valores perdidos en la variable de interés


##------------------------------------------------------------------------------

### PASO 2   
### Data cleaning
### Una limpieza de los valores perdidos y/o valores atípicos (outliers)

plot_missing(dataepi) # determinar el % de missing value por cada variable

### ¿Cuantos missing values o NA - Not Attribible existen?

dataepi %>%
  select(EDAD)%>% # seleccionar la variable
  filter(is.na(EDAD))%>% #aqui podemos identificar los NA o valores perdidos 
  count(EDAD)%>% # contabilizar lo identificado
  #arrange(!desc(n))%>% #ordenar no descendente
  View()

dataepi %>%
  select(FECHA_RESULTADO)%>% # seleccionar la variable
  filter(is.na(FECHA_RESULTADO))%>% #aqui podemos identificar los NA o valores perdidos 
  count(FECHA_RESULTADO)%>% # contabilizar lo identificado
  #arrange(!desc(n))%>% #ordenar no descendente
  View()

### Removemos los valores perdidos en nuestras variables de interés
dataepi2 <- dataepi %>% 
  drop_na(c(EDAD, FECHA_RESULTADO)) # repetir linea 63

duplicated(id_persona) # evaluamos si existen valores duplicados
dataepi3 <- dataepi2 %>% 
  distinct(id_persona, .keep_all = T) # solo quedaron los valores únicos, linea 63 again
  
summary(dataepi3) # un resumen del avance logrado

### Identificamos y removemos valores atípicos en nuestras variables de interés

### Solo para datos numéricos, ej. edad
dataepi3%>%
  #drop_na(EDAD)%>%
  ggplot(aes(x=EDAD))+
  geom_histogram(binwidth = 0.5)+
  xlim(0,120)+
  theme_bw()

### Solo para datos categóricos, ej. departamentos
dataepi3%>%
  drop_na(DEPARTAMENTO)%>%
  ggplot(aes(x=DEPARTAMENTO))+
  geom_bar(fill = "#3C963C")+
  coord_flip()+
  theme_bw()

dataepi3%>% # Ahora evaluamos los valores atípicos en la variables de interés
  #drop_na(EDAD)%>%
  ggplot(aes(x=EDAD, y = DEPARTAMENTO))+
  geom_boxplot(fill= "white",
               colour = "#3366FF",
               outlier.color = "red",
               #outlier.shape = NA,
               notch = T, varwidth = TRUE)+
  #stat_boxplot(na.rm = F)+
  #geom_jitter(width = 0.2)+
  #coord_flip()+
  xlim(0,120)+
  theme_gray()

dataepi4 <- dataepi3 %>% 
  filter(EDAD>18 & EDAD<100) # de esta forma solo nos quedamos con lo filtrado // 
  # correr linea 109 cambiando la base nueva de dataepi3 -> dataepi4


##------------------------------------------------------------------------------


### PASO 3
### Data screening   
### Manipulación de la data, revisión rápida de las propiedades de la data


### Conversion de valores numericos del formato fecha 

library(lubridate)
library(zoo)

glimpse(dataepi4$FECHA_RESULTADO)
head(dataepi4$FECHA_RESULTADO)
str(dataepi4$FECHA_RESULTADO)
typeof(dataepi4$FECHA_RESULTADO)
class(dataepi4$FECHA_RESULTADO)

# Formato year-month-day - POSIXct tipo list
fecha_resul = strptime(dataepi4$FECHA_RESULTADO, format = "%Y%m%d") 
fecha_corte = strptime(dataepi4$FECHA_CORTE, format = "%Y%m%d")

typeof(fecha_resul) # reconocer el tipo de variable  
class(c(fecha_resul, fecha_corte)) # identificar el tipo de formato
glimpse(c(fecha_resul, fecha_corte))

# Conversión de list a date, fecha (ymd) 
dataepi4$FECHA_RESULTADO = as.Date(fecha_resul) 
dataepi4$FECHA_CORTE = as.Date(fecha_corte) 

typeof(dataepi4$FECHA_RESULTADO) # reconocer el tipo de variable  
class(dataepi4$FECHA_RESULTADO) # identificar el tipo de formato 

# Segumos con la manipulación de la variable DEPARTAMENTO -> Macroregiones
glimpse(dataepi4$DEPARTAMENTO)
plot_bar(dataepi4$DEPARTAMENTO) # Identificando las n categorías de la variable

# Filtramos en una nueva data los valores de dos categorías que no deberían estar
dataepi5 <- dataepi4 %>% 
  filter(dataepi4$DEPARTAMENTO != "ARICA" & dataepi4$DEPARTAMENTO != "") #correr linea 164
sort(summary(dataepi5$DEPARTAMENTO), decreasing = T)

# Eliminamos las variables que no son de interés, solo los 25 departamentos (L/Callao)
dataepi5$DEPARTAMENTO <- fct_drop(dataepi5$DEPARTAMENTO, only = c("","ARICA"))
sort(summary(dataepi5$DEPARTAMENTO), decreasing = T)

# Recodificamos las demás variables de interés como Macroregiones
dataepi5$MACRO <- recode(dataepi5$DEPARTAMENTO, 
                                LIMA = "L/Callao",
                                CALLAO = "L/Callao",
                                ANCASH = "Norte",
                                "LA LIBERTAD" = "Norte",
                                PIURA = "Norte",
                                CAJAMARCA = "Norte",
                                LAMBAYEQUE = "Norte",
                                TUMBES = "Norte",
                                AREQUIPA = "Sur",
                                APURIMAC = "Sur",
                                CUSCO = "Sur",
                                MOQUEGUA = "Sur",
                                PUNO = "Sur",
                                TACNA = "Sur",
                                ICA = "Centro",
                                JUNIN = "Centro",
                                AYACUCHO = "Centro",
                                PASCO = "Centro",
                                HUANCAVELICA = "Centro",
                                HUANUCO = "Centro",
                                "MADRE DE DIOS" = "Selva",
                                LORETO = "Selva",
                                "SAN MARTIN" = "Selva",
                                AMAZONAS = "Selva",
                                UCAYALI = "Selva")
                                       
sort(summary(dataepi5$MACRO), decreasing = T)
glimpse(dataepi5$MACRO)
plot_bar(dataepi5$MACRO)

# Revisión rápida de patrones y tendencias

#install.packages("incidence2")
library(incidence2)


# Contabilizar el n de fallecidos por cada macroregión
data_conteo <- dataepi5 %>% 
  group_by(MACRO, FECHA_RESULTADO) %>% 
  summarize(n_casos = dplyr::n()) %>% 
  #filter(FECHA_RESULTADO > as.Date("2013-06-01")) %>% 
  ungroup()


summary(data_conteo)
plot(data_conteo$n_casos)

# create the incidence object, aggregating cases by day
dias_epi <- incidence(       # create incidence object
  x = dataepi5,             # dataset
  date_index = FECHA_RESULTADO,  # date column
  interval = "weeks",          # date grouping interval
  groups = MACRO
)

class(dias_epi)
plot(dias_epi)
summary(dias_epi)

plot(dias_epi,             # incidence object with age_cat as group
    fill = MACRO)+          # age_cat is used for bar fill color (must have been set as a groups column above)
    labs(fill = "Macroregiones") # change legend title from default "age_cat" (this is a ggplot2 modification)

