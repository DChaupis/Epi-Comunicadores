# ***Epidemiología para comunicadores usando datos abiertos, caso COVID19.***


## 🌻 Motivación

Este es un tutorial práctico para aquellos interesados en la comunicación científica usando *datos abiertos*, dirigido especialmente a los periodistas y divulgadores científicos y en general a todos los curiosos de esta área, donde se junta la ciencia con el periodismo de datos para desarrollar de los cuales se nutre el periodismo científico. Para lograr generar buenas prácticas en el manejo de datos para la comunicación científica, se piesa utilizar una serie de funciones de programación estadística (en R y RStudio) y control de cambios (en Git y Github), asi analizar y reportar los resultados de nuestra investigacción. 

## ✅ Objetivos

Tiene como principal objetivo brindar un conocimiento confiable de los datos que se estén analizando e interpretando, valiéndose de una seria de procedimientos que 
aseguren la replicabilidad de resultados y su transparencia basado en una visión de ciencia abierta.

El principal objetivo es: 

  1. Desaroollar un protocolo básico para el análisis inicial de la data usando R.

Los objetivos específicos son:
  
  1. Preparación y revisión inicial de la metadata.
  2. Limpieza de datos - Data cleaning.
  3. Manipulación de datos - Data Screening.
  4. Presentación de datos - Data Reporting.

## 🙋🏻‍♂️ ¿Cómo utilizo este tutorial?

Es importante descargar el formato comprimible o **zip** de este repositorio y ubicarlo en la carpeta de su elección en su propia computadora, lo recomemdable sería lo descargue en `../Documentos/EpiComunicadores-main`. 

Una vez descargada en la carpeta `/Epicomunicadores-main` se podrá abrir el proyecto-R llamado **OpenData-CovidPeru**, al abrirse se visualizará en la sección de *Files* ("la zona de ficheros") donde podrá hacer click para abrir el código ubicada en la carpeta `/Scripts`. Una vez abierto el código, por ejemplo, se muestra la ruta de como importar la dataset de [Fallecidos por Covid19](https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa/resource/4b7636f3-5f0c-4404-8526). 

```{r}
set.seed(9)
datamorte <- read.csv("fallecidos_covid.csv", sep = ";", header = T, 
                    encoding = "UTF-8")       

```
Para importar tanto la base de datos de Fallecidos y Casos positivos de COVID19 será a partir de la [Plataforma Nacional de Datos Abiertos](https://www.datosabiertos.gob.pe/). Estos datos primarios proceden de la fuente del **Centro Nacional de Epidemiologia, prevención y Control de Enfermedades – MINSA**, de esta plataforma se deberá descargar los siguientes datasets:

  1. [**Dataset de Fallecidos de Covid19**](https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa/resource/4b7636f3-5f0c-4404-8526)
  2. [**Dataset de Casos positivos de Covid19**](https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa/resource/690e57a6-a465-47d8-86fd)

> Nota: Deberás descargar los datasets directamente a la carperta principal: `/Epicomunicadores-main`.

Las librerías que se utilizan son:
```{r}

install.packages("pacman")
library(pacman)
pacman::p_load(
  tidyverse, # data management + ggplot2 graphics 
  dplyr, # data management
  DataExplorer, # descriptive statistics
  lubridate, # manipulate dates
  zoo, # alternative manipulate dates 
  incidence2 # epicurves of linelist data) 

```

Apoya este proyecto dejando tu ⭐.

## 👨‍💻 Autoria 

  - [David Chaupis Meza](https://www.linkedin.com/in/davidchaupis/)

> Nota: Si tu también estás interesado en formar parte del desarrollo de este tutorial forkea y dale al pull request, así todos podemos colaborar con el proyecto.

## 🔓 Licencia de uso

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Licencia Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br />Esta obra está bajo una <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Licencia Creative Commons Atribución-CompartirIgual 4.0 Internacional</a>.

**Creative Commons to Open Science**


# >>> **Por una Ciencia Abierta, Reproducible y Colaborativa** 🙌🏻
