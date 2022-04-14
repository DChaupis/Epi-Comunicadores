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
datamorte <- read.csv("fallecidos_covid.csv", sep = ";", header = T, 
                    encoding = "UTF-8")
#View(dataepi)
#file.info("positivos_covid_minsa.csv")$size

#install.packages("DataExplorer")

library(tidyverse, dplyr) # gestión de datos y visualización
library(DataExplorer) # exploración estadística descriptiva en plots

attach(datamorte) # adherir las variables paraa cada function()

### Identificamos los tipos de variables: Numéricas || Categóricas 

dim(datamorte) # filas y columnas
names(datamorte) #nombre de variables
glimpse(datamorte) # que tipo de variables: <chr> o <int> 
plot_str(datamorte) # podemos graficar la organización de la data
head(datamorte,n = 10) # descubramos las n primeras o últimas (tail)
summary(datamorte) # cuales son las variables (n columnas)
class(datamorte) #identifica el formato configurado


unique() # para cada variable solo las observaciones únicas 
is.na() # permite identificar los valores perdidos en la variable de interés


##------------------------------------------------------------------------------

### PASO 2   
### Data cleaning
### Una limpieza de los valores perdidos y/o valores atípicos (outliers)

plot_missing(datamorte) # determinar el % de missing value por cada variable

### ¿Cuantos missing values o NA - Not Attribible existen?

datamorte %>%
  select(EDAD_DECLARADA)%>% # seleccionar la variable
  filter(is.na(EDAD_DECLARADA))%>% #aqui podemos identificar los NA o valores perdidos 
  count(EDAD_DECLARADA)%>% # contabilizar lo identificado
  #arrange(!desc(n))%>% #ordenar no descendente
  View()

datamorte %>%
  select(FECHA_FALLECIMIENTO)%>% # seleccionar la variable
  filter(is.na(FECHA_FALLECIMIENTO))%>% #aqui podemos identificar los NA o valores perdidos 
  count(FECHA_FALLECIMIENTO)%>% # contabilizar lo identificado
  #arrange(!desc(n))%>% #ordenar no descendente
  View()

### Removemos los valores perdidos en nuestras variables de interés
datamorte2 <- datamorte %>% 
  drop_na(c(EDAD_DECLARADA, FECHA_FALLECIMIENTO)) # repetir linea 63

duplicated(UUID) # evaluamos si existen valores duplicados
datamorte3 <- datamorte2 %>% 
  distinct(UUID, .keep_all = T) # solo quedaron los valores únicos, linea 63 again
  
summary(datamorte3) # un resumen del avance logrado

### Identificamos y removemos valores atípicos en nuestras variables de interés

### Solo para datos numéricos, ej. edad
datamorte3%>%
  #drop_na(EDAD)%>%
  ggplot(aes(x=EDAD_DECLARADA))+
  geom_histogram(binwidth = 0.5)+
  xlim(0,120)+
  theme_bw()

### Solo para datos categóricos, ej. departamentos
datamorte3%>%
  drop_na(DEPARTAMENTO)%>%
  ggplot(aes(x=DEPARTAMENTO))+
  geom_bar(fill = "#3C963C")+
  coord_flip()+
  theme_bw()

datamorte4%>% # Ahora evaluamos los valores atípicos en la variables de interés
  #drop_na(EDAD)%>%
  ggplot(aes(x=EDAD_DECLARADA, y = DEPARTAMENTO))+
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

datamorte4 <- datamorte3 %>% 
  filter(EDAD_DECLARADA>18 & EDAD_DECLARADA<120) # de esta forma solo nos quedamos con lo filtrado // 
  # correr linea 109 cambiando la base nueva de dataepi3 -> dataepi4


##------------------------------------------------------------------------------


### PASO 3
### Data screening   
### Manipulación de la data, revisión rápida de las propiedades de la data


### Conversion de valores numericos del formato fecha 

library(lubridate)
library(zoo)

glimpse(datamorte4$FECHA_FALLECIMIENTO)
head(datamorte4$FECHA_FALLECIMIENTO)
str(datamorte4$FECHA_FALLECIMIENTO)
typeof(datamorte4$FECHA_FALLECIMIENTO)
class(datamorte4$FECHA_FALLECIMIENTO)

# Formato year-month-day - POSIXct tipo list
fecha_resul_morte = strptime(datamorte4$FECHA_FALLECIMIENTO, format = "%Y%m%d") 

typeof(fecha_resul_morte) # reconocer el tipo de variable  
class(fecha_resul_morte) # identificar el tipo de formato
glimpse(fecha_resul_morte)

# Conversión de list a date, fecha (ymd) 
datamorte4$FECHA_FALLECIMIENTO = as.Date(fecha_resul_morte) 

typeof(datamorte4$FECHA_FALLECIMIENTO) # reconocer el tipo de variable  
class(datamorte4$FECHA_FALLECIMIENTO) # identificar el tipo de formato 

# Segumos con la manipulación de la variable DEPARTAMENTO -> Macroregiones
glimpse(datamorte4$DEPARTAMENTO)
plot_bar(datamorte4$DEPARTAMENTO) # Identificando las n categorías de la variable

# Recodificamos las demás variables de interés como Macroregiones
datamorte4$MACRO <- recode(datamorte4$DEPARTAMENTO, 
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
                                       
sort(summary(datamorte4$MACRO), decreasing = T)
glimpse(datamorte4$MACRO)
plot_bar(datamorte4$MACRO)

# Revisión rápida de patrones y tendencias

# install.packages("incidence2")
library(incidence2)

# create the incidence object, aggregating cases by day
dias_epi_morte <- incidence(       # create incidence object
  x = datamorte4,             # dataset
  date_index = FECHA_FALLECIMIENTO,  # date column
  interval = "weeks",          # date grouping interval
  groups = MACRO)

summary(dias_epi_morte)

plot(dias_epi_morte,             # incidence object with age_cat as group
    fill = MACRO)+          # age_cat is used for bar fill color (must have been set as a groups column above)
    labs(fill = "Macroregiones") # change legend title from default "age_cat" (this is a ggplot2 modification)



##------------------------------------------------------------------------------


### PASO 4 
### Data Reporting







