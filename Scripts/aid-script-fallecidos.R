#############################################################################
#############################################################################
###                                                                       ###
###                      ANÁLISIS INICIAL DE LA DATA                      ###
###                                                                       ###
###  Epidemiología para comunicadores con datos abiertos, caso COVID-19.  ###
###                              ABRIL, 2022                              ###
###                                                                       ###
#############################################################################
#############################################################################

##------------------------------------------------------------------------------
"
Tiene como principal objetivo brindar un conocimiento confiable de los datos que 
se estén analizando e interpretando, valiéndose de una seria de procedimientos que 
aseguren la replicabilidad de resultados y su transparencia basado en una visión 
de ciencia abierta.
"
## Basado en The STRATOS Iniciative - Topic Group 3.
## https://stratos-initiative.org/group_3
"
Objetivos:
    
    1. Importación de la dataset revisando la metadata.
    2. Limpieza de datos de valores perdidos y atípicos.
    3. Manipulación de datos para una rápida evaluación.
    4. Presentación de datos en un reporte de resultados.
"
##------------------------------------------------------------------------------

### PRESETEO DE LIBRERIAS

install.packages("pacman")
library(pacman)
pacman::p_load(
  tidyverse, # data management + ggplot2 graphics 
  dplyr, # data management
  DataExplorer, # descriptive statistics
  lubridate, # manipulate dates
  zoo, # alternative manipulate dates 
  incidence2) # epicurves of linelist data) 

### OBJETIVO 1  
### Preparando la metadata ("...are data that describe other data.")
### Accedemos a nuestro directorio e identificamos los tipos de variables

set.seed(9)
datamorte <- read.csv("fallecidos_covid.csv", sep = ";", header = T, 
                      encoding = "UTF-8")
#View(dataepi)
#file.info("positivos_covid_minsa.csv")$size


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

### OBJETIVO 2   
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

### Removemos los valores duplicados en caso existan
duplicated(UUID) # evaluamos si existen valores duplicados
datamorte3 <- datamorte2 %>% 
  distinct(UUID, .keep_all = T) # solo quedaron los valores únicos, linea 63 again

summary(datamorte3) # un resumen del avance logrado

### Identificamos y removemos valores atípicos en nuestras variables de interés

### Solo para datos numéricos, ej. edad
datamorte3%>%
  drop_na(EDAD_DECLARADA)%>%
  ggplot(aes(x=as.integer(EDAD_DECLARADA)))+
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

datamorte3%>% # Ahora evaluamos los valores atípicos en la variables de interés
  #drop_na(EDAD)%>%
  ggplot(aes(x=as.integer(EDAD_DECLARADA), y = DEPARTAMENTO))+
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
  filter(as.integer(EDAD_DECLARADA)>18 & as.integer(EDAD_DECLARADA)<120) # de esta forma solo nos quedamos con lo filtrado // 

datamorte4%>% # Ahora evaluamos los valores atípicos en la variables de interés
  #drop_na(EDAD)%>%
  ggplot(aes(x=as.integer(EDAD_DECLARADA), y = DEPARTAMENTO))+
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

##------------------------------------------------------------------------------

### OBJETIVO 3
### Data screening   
### Manipulación de la data, revisión rápida de las propiedades de la data


### Conversion de valores numericos del formato fecha 

glimpse(datamorte4$FECHA_FALLECIMIENTO)
head(datamorte4$FECHA_FALLECIMIENTO)
typeof(datamorte4$FECHA_FALLECIMIENTO)

# Formato year-month-day - POSIXct tipo list
fecha_resul_morte = strptime(datamorte4$FECHA_FALLECIMIENTO, format = "%Y%m%d") 
typeof(datamorte4$FECHA_FALLECIMIENTO) # reconocer el tipo de variable  
class(datamorte4$FECHA_FALLECIMIENTO) # identificar el tipo de formato 
glimpse(fecha_resul_morte)

# Conversión de list a date, fecha (ymd) 
datamorte4$FECHA_FALLECIMIENTO = as.Date(fecha_resul_morte) 
typeof(datamorte4$FECHA_FALLECIMIENTO) # reconocer el tipo de variable  
class(datamorte4$FECHA_FALLECIMIENTO) # identificar el tipo de formato 

# Segumos con la manipulación de la variable DEPARTAMENTO -> Macroregiones
glimpse(datamorte4$DEPARTAMENTO)
plot_bar(datamorte4$DEPARTAMENTO) # Identificando las n categorías

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

#sort(summary(datamorte4$MACRO), decreasing = T)
glimpse(datamorte4$MACRO)
plot_bar(datamorte4$MACRO)

# Una revisión rápida de patrones y tendencias

# Generar la curva epidémica de casos por semanas epi
week_epi_morte <- incidence(    # crear una función de incidencia
  x = datamorte4,               # seleccionamos el dataset
  date_index = FECHA_FALLECIMIENTO,  # seleccionamos la columna fecha
  interval = "weeks",           # agrupamos por macroregiones
  groups = MACRO)

summary(week_epi_morte)

plot(week_epi_morte,           # curva epidémica generada por sem-epi
     fill = MACRO)+            # coloreado por cada macroregión (preseteado)
  labs(fill = "Macroregiones") # legenda para cada macroregión


# También podemos generar una data conteo de casos/sem-epi (opcional)
data_conteo <- datamorte4 %>% 
  group_by(MACRO, FECHA_FALLECIMIENTO) %>% 
  summarize(n_casos = dplyr::n()) %>% 
  #filter(date_hospitalisation > as.Date("2013-06-01")) %>% 
  ungroup() 

##------------------------------------------------------------------------------


### OBJETIVO 4 
### Data Reporting - Resultados preliminares


## Para evaluar la curva epidémica de cada MACRO
macro1 <- datamorte4 %>% # Empezamos creando una nueva dataset para Macro1
  filter(MACRO == "L/Callao") # La cual solo incluirá a "L/Callao"

# Creamos una secuencia de fechas semanales empezando desde el Lunes
weekly_breaks_macro1 <- seq.Date(
  from = floor_date(min(macro1$FECHA_FALLECIMIENTO, na.rm=T),   "week", 
                    week_start = 1), # primer lunes antes del primer caso
  to   = ceiling_date(max(macro1$FECHA_FALLECIMIENTO, na.rm=T), "week", 
                      week_start = 1), # último lunes después del último caso
  by   = "week") # por intervalos de semana


## Generamos la gráfica de la Curva Epidémica para L/Callao 
## Periodo de tres olas pandémicas que van del 2020 al 2022

ggplot(macro1) + 
  
  # Configurando el histograma
  geom_histogram(
    mapping = aes(x = FECHA_FALLECIMIENTO),
    breaks = weekly_breaks_macro1, # secuencia de fechas semanales
    closed = "left", # contar casos desde el punto de corte
    color = "darkblue",
    fill = "lightblue") +
  
  # escalando las semanas en meses
  scale_y_continuous(expand = c(0,0))+ # escala de relleno y
  scale_x_date(
    expand = c(0,0),                   # escala de relleno x
    date_breaks = "1 month",           # primero de cada mes
    date_minor_breaks = "1 month",     # primero de cada mes
    date_labels = "%b\n'%y")+          # formato de fechas (mes/año)
  
  # Legenda y tema de la gráfica
  labs(
    title = "Curva epidémica de Fallecidos en la Macroregión Lima/Callao",
    x = "Semanas epidemiológicas agrupado en meses",
    y = "N° de Fallecidos por COVID19")+ 
  theme_minimal()+
  
## Añadir una linea vertical y una aclaratoria de dicha linea
annotate(
  "segment",
  x     = max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 110, # 110 días 
  xend  = max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 110, # 110 días
  y     = 0,           # linea ajustada en y = 0
  yend  = Inf,         # linea hasta el top del plot
  size  = 1.5,         # grosor de la linea
  color = "red",
  lty   = "dashed")+   # Tipo de linea e.g. "solid", "dashed"
  
annotate(
  "segment",
  x     = max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 520, # 21 días
  xend  = max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 520, # 21 días
  y     = 0,          # linea ajustada en y = 0
  yend  = Inf,        # linea hasta el top del plot
  size  = 1.5,        # grosor de la linea
  color = "red",
  lty   = "dashed")+  # Tipo de linea e.g. "solid", "dashed"
  
# Añadiendo nota aclaratoria para cada linea
annotate(
  "text",
  x = max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 100,
  y = 1500,
  label = "Transición a la tercera ola pandémica",
  angle = 90)+
  
annotate(
  "text",
  x = max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 510,
  y = 1500,
  label = "Transición a la segunda ola pandémica",
  angle = 90)

# rango de advertencia
#annotate(
#  "rect",
#  xmin  = as.Date(max(macro1$FECHA_FALLECIMIENTO, na.rm = T) - 21), 
#  xmax  = as.Date(Inf), # debe ser leido como as.Date()
#  ymin  = 0,
#  ymax  = Inf,
#  alpha = 0.2,          # parametro para annotate()
#  fill  = "red")+
