## Epidemiología para comunicadores usando datos abiertos, caso COVID19. 


## 🌻 Motivación

Este es un tutorial práctico para aquellos interesados en la comunicación científica usando datos abiertos, dirigido especialmente a los periodistas y divulgadores científicos y en general a todos los curiosos en esta área donde se junta la ciencia con el periodismo de datos para desarrollar lo que sería el periodismo científico, para ello se piesa utilizar una serie de herramientas de programación estadística y control de cambios, asi analizar y reportar los resultados de nuestra investigacción. 

## ¿Cómo utilizo este tutorial?

Es importante clonar o copiar este repositorio en una carpeta de su elección en su computadora.

Una vez descargada la carpeta /DataProject se podrá obtejer el dataset de interés, ejemplo:

```{r}
set.seed(9)
datamorte <- read.csv("fallecidos_covid.csv", sep = ";", header = T, 
                    encoding = "UTF-8")       
```
Las librerías que serán usadas son:
```{r}

library(tidyverse, dplyr) # gestión de datos y visualización
library(DataExplorer) # exploración estadística descriptiva en plots    
library(lubridate) # modificación de formatos de fechas
library(zoo) # modificación de formatos de fechas
library(incidence2) # analisis de datos en epidemiología

```
<br/>

## Objetivo

Tiene como principal objetivo brindar un conocimiento confiable de los datos que se estén analizando e interpretando, valiéndose de una seria de procedimientos que 
aseguren la replicabilidad de resultados y su transparencia basado en una visión de ciencia abierta.

El principal objetivo es tener un protocolo básico para el análisis inicial de la data usando R.

Algunos objetivos específicos son:
  
    1. Preparación y revisión inicial de la metadata.
    2. Limpieza de datos - Data cleaning.
    3. Manipulación de datos - Data Screening.
    4. Presentación de datos - Data Reporting.

Apoya este proyecto dejando tu ⭐.

<br/>

## 👨‍💻 Autoria 

# David Chaupis Meza

[Linkedin](https://www.linkedin.com/in/davidchaupis/)

<br/>

## 🍁 Licence

**Creative Commons to Open Science**
