install.packages("janitor")
library(janitor)
library(tidyr)
install.packages("prettydoc")
library(prettydoc)
#Importa los datos de soccer de la temporada 2019/2020 de la primera divisi?n de la liga espa?ola a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
#setwd("C:/Users/Jacqueline Julian/Documents/BEDU/Modulo2/sesion1/postwork") 
soccer <-read.csv("SP1.csv")
#Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los n?meros de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
suppressMessages(suppressWarnings(library(dplyr)))
partidos <- select(soccer,FTHG, FTAG)
#Consulta c?mo funciona la funci?n table en R al ejecutar en la consola ?table
?table

#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
num_partidos <- length(partidos$FTHG)
goles_casa <- sum(partidos$FTHG)
goles_visitantes <-sum(partidos$FTAG)
total_goles <- goles_casa+goles_visitantes;
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
#tabla de frecuencias para el equipo de casa (FTHG)

#frec_casa <- table(casa_visitantes$FTHG)
#fCasa<-as.data.frame(frec_casa)
frec2<- tabyl(partidos, FTHG) #tabyl ya saca la frencuencia y la probabilidad marginal
frec_casa <- partidos %>%group_by(FTHG) %>% summarise(frequency = n())  # para hacer esto se utiliza janitor
frec_visitantes <- partidos %>%group_by(FTAG) %>% summarise(frequency = n())  # para hacer esto se utiliza janitor
p_casa_marginal = frec_casa$frequency / (sum(frec_casa$frequency,frec_visitantes$frequency)) #dividir la frecuencia/el total de partidos
tabla_casa_marginal <- data.frame(frec_casa$FTHG, frec_casa$frequency, p_casa_marginal) #unir los goles, la frecuencia y la probabilidad
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
p_visitantes_marginal = frec_visitantes$frequency / (sum(frec_casa$frequency,frec_visitantes$frequency))  #dividir la frecuencia/el total de partidos
tabla_visitantes_marginal <- data.frame(frec_visitantes$FTAG, frec_visitantes$frequency, p_visitantes_marginal) #unir los goles, la frecuencia y la probabilidad
########### Conjunta#############
partidos2<-unite(partidos, variables,c(1:2),sep="",remove=TRUE)
tconjunta<-partidos2 %>%group_by(variables) %>% summarise(frequency=n())
p_conjunta<-tconjunta$frequency/(sum(tconjunta$frequency))
tabla_conjunta<-data.frame(tconjunta,p_conjunta)



View(tabla_conjunta)
View(tabla_casa_marginal)
View(tabla_visitantes_marginal)
