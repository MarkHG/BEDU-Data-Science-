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
p_casa_marginal = frec_casa$frequency / total_goles #dividir la frecuencia/el total de partidos
tabla_casa_marginal <- data.frame(frec_casa$FTHG, frec_casa$frequency, p_casa_marginal) #unir los goles, la frecuencia y la probabilidad
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
frec_visitantes <- partidos %>%group_by(FTAG) %>% summarise(frequency = n())  # para hacer esto se utiliza janitor
p_visitantes_marginal = frec_visitantes$frequency / total_goles #dividir la frecuencia/el total de partidos
tabla_visitantes_marginal <- data.frame(frec_visitantes$FTAG, frec_visitantes$frequency, p_visitantes_marginal) #unir los goles, la frecuencia y la probabilidad
########### Conjunta#############
#suma de los goles de local y visitante cuando son 0,1,2,..
goles_conjunto00=frec_casa$frequency[1]+frec_visitantes$frequency[1]
goles_conjunto01=frec_casa$frequency[1]+frec_visitantes$frequency[2]
goles_conjunto02=frec_casa$frequency[1]+frec_visitantes$frequency[3]
goles_conjunto03=frec_casa$frequency[1]+frec_visitantes$frequency[4]
goles_conjunto04=frec_casa$frequency[1]+frec_visitantes$frequency[5]
goles_conjunto05=frec_casa$frequency[1]+frec_visitantes$frequency[6]

goles_conjunto10=frec_casa$frequency[2]+frec_visitantes$frequency[1]
goles_conjunto11=frec_casa$frequency[2]+frec_visitantes$frequency[2]
goles_conjunto12=frec_casa$frequency[2]+frec_visitantes$frequency[3]
goles_conjunto13=frec_casa$frequency[2]+frec_visitantes$frequency[4]
goles_conjunto14=frec_casa$frequency[2]+frec_visitantes$frequency[5]
goles_conjunto15=frec_casa$frequency[2]+frec_visitantes$frequency[6]

goles_conjunto20=frec_casa$frequency[3]+frec_visitantes$frequency[1]
goles_conjunto21=frec_casa$frequency[3]+frec_visitantes$frequency[2]
goles_conjunto22=frec_casa$frequency[3]+frec_visitantes$frequency[3]
goles_conjunto23=frec_casa$frequency[3]+frec_visitantes$frequency[4]
goles_conjunto24=frec_casa$frequency[3]+frec_visitantes$frequency[5]
goles_conjunto25=frec_casa$frequency[3]+frec_visitantes$frequency[6]

goles_conjunto30=frec_casa$frequency[4]+frec_visitantes$frequency[1]
goles_conjunto31=frec_casa$frequency[4]+frec_visitantes$frequency[2]
goles_conjunto32=frec_casa$frequency[4]+frec_visitantes$frequency[3]
goles_conjunto33=frec_casa$frequency[4]+frec_visitantes$frequency[4]
goles_conjunto34=frec_casa$frequency[4]+frec_visitantes$frequency[5]
goles_conjunto35=frec_casa$frequency[4]+frec_visitantes$frequency[6]

goles_conjunto40=frec_casa$frequency[5]+frec_visitantes$frequency[1]
goles_conjunto41=frec_casa$frequency[5]+frec_visitantes$frequency[2]
goles_conjunto42=frec_casa$frequency[5]+frec_visitantes$frequency[3]
goles_conjunto43=frec_casa$frequency[5]+frec_visitantes$frequency[4]
goles_conjunto44=frec_casa$frequency[5]+frec_visitantes$frequency[5]
goles_conjunto45=frec_casa$frequency[5]+frec_visitantes$frequency[6]


goles_conjunto50=frec_casa$frequency[6]+frec_visitantes$frequency[1]
goles_conjunto51=frec_casa$frequency[6]+frec_visitantes$frequency[2]
goles_conjunto52=frec_casa$frequency[6]+frec_visitantes$frequency[3]
goles_conjunto53=frec_casa$frequency[6]+frec_visitantes$frequency[4]
goles_conjunto54=frec_casa$frequency[6]+frec_visitantes$frequency[5]
goles_conjunto55=frec_casa$frequency[6]+frec_visitantes$frequency[6]


goles_conjunto60=frec_casa$frequency[7]+frec_visitantes$frequency[1]
goles_conjunto61=frec_casa$frequency[7]+frec_visitantes$frequency[2]
goles_conjunto62=frec_casa$frequency[7]+frec_visitantes$frequency[3]
goles_conjunto63=frec_casa$frequency[7]+frec_visitantes$frequency[4]
goles_conjunto64=frec_casa$frequency[7]+frec_visitantes$frequency[5]
goles_conjunto65=frec_casa$frequency[7]+frec_visitantes$frequency[6]


goles_conjunto0<-rbind(goles_conjunto00,goles_conjunto01,goles_conjunto02,goles_conjunto03,goles_conjunto04,goles_conjunto05)
goles_conjunto1<-rbind(goles_conjunto10,goles_conjunto11,goles_conjunto12,goles_conjunto13,goles_conjunto14,goles_conjunto15)
goles_conjunto2<-rbind(goles_conjunto20,goles_conjunto21,goles_conjunto22,goles_conjunto23,goles_conjunto24,goles_conjunto25)
goles_conjunto3<-rbind(goles_conjunto30,goles_conjunto31,goles_conjunto32,goles_conjunto33,goles_conjunto34,goles_conjunto35)
goles_conjunto4<-rbind(goles_conjunto40,goles_conjunto41,goles_conjunto42,goles_conjunto43,goles_conjunto44,goles_conjunto45)
goles_conjunto5<-rbind(goles_conjunto50,goles_conjunto51,goles_conjunto52,goles_conjunto53,goles_conjunto54,goles_conjunto55)
goles_conjunto6<-rbind(goles_conjunto60,goles_conjunto61,goles_conjunto62,goles_conjunto63,goles_conjunto64,goles_conjunto65)

tabla_p_conjunta1<-data.frame(frec_casa$FTHG[1],frec_visitantes$FTAG,goles_conjunto0,goles_conjunto0/total_goles)
tabla_p_conjunta2<-data.frame(frec_casa$FTHG[2],frec_visitantes$FTAG,goles_conjunto1,goles_conjunto1/total_goles)
tabla_p_conjunta3<-data.frame(frec_casa$FTHG[3],frec_visitantes$FTAG,goles_conjunto2,goles_conjunto2/total_goles)
tabla_p_conjunta4<-data.frame(frec_casa$FTHG[4],frec_visitantes$FTAG,goles_conjunto3,goles_conjunto3/total_goles)
tabla_p_conjunta5<-data.frame(frec_casa$FTHG[5],frec_visitantes$FTAG,goles_conjunto4,goles_conjunto4/total_goles)
tabla_p_conjunta6<-data.frame(frec_casa$FTHG[6],frec_visitantes$FTAG,goles_conjunto5,goles_conjunto5/total_goles)
tabla_p_conjunta7<-data.frame(frec_casa$FTHG[7],frec_visitantes$FTAG,goles_conjunto6,goles_conjunto6/total_goles)

names(tabla_p_conjunta1)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")
names(tabla_p_conjunta2)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")

names(tabla_p_conjunta3)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")
names(tabla_p_conjunta4)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")
names(tabla_p_conjunta5)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")
names(tabla_p_conjunta6)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")
names(tabla_p_conjunta7)=c("Goles Local","Goles Visitante","Suma de goles","P_Conjunta")
tabla_p_conjunta <-rbind(tabla_p_conjunta1,tabla_p_conjunta2,tabla_p_conjunta3,tabla_p_conjunta4,tabla_p_conjunta5,tabla_p_conjunta6,tabla_p_conjunta7)
View(tabla_p_conjunta)


names(tabla_casa_marginal)= c("Goles","Frecuencia","P_MarginalLocal")
names(tabla_visitantes_marginal)= c("Goles","Frecuencia","P_MarginalVisitante")

View(tabla_casa_marginal)
View(tabla_visitantes_marginal)
View(tabla_p_conjunta)
