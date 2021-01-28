# Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 
# de la primera divisi?n de la liga espa?ola a R,los datos los puedes encontrar en el 
# siguiente enlace: https://www.football-data.co.uk/spainm.php

setwd("/home/marco/Documentos/BEDU/github/BEDU-Data-Science-/Módulo 2/Sesion2/files")
division17 <- read.csv("SP1.csv")
division18 <- read.csv("SP2.csv")
division19 <- read.csv("SP3.csv")

# Obten una mejor idea de las caracter?sticas de los data frames al usar las funciones: 
# str, head, View y summary

str(division17)
head(division17)
View(division17)
summary(division17)


str(division18)
head(division18)
View(division18)
summary(division18)


str(division19)
head(division19)
View(division19)
summary(division19)

# Con la funci?n select del paquete dplyr selecciona ?nicamente las columnas 
# Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
# esto para cada uno de los data frames. 

suppressMessages(suppressWarnings(library(dplyr)))
division17 <- select(division17,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)
division18 <- select(division18,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)
division19 <- select(division19,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)

# Aseg?rate de que los elementos de las columnas correspondientes de los nuevos 
# data frames sean del mismo tipo. Con ayuda de la funci?n rbind forma un ?nico 
# data frame que contenga las seis columnas mencionadas en el punto 3. 

division17 <- mutate(division17, Date = as.Date(Date, "%d/%m/%y"))
division18 <- mutate(division18, Date = as.Date(Date, "%d/%m/%y"))
division19 <- mutate(division19, Date = as.Date(Date, "%d/%m/%y"))

FinalFrame <- rbind(division17,division18,division19)

View(FinalFrame)

