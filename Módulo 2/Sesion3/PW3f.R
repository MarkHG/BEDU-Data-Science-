setwd("/home/marco/Documentos/BEDU/github/BEDU-Data-Science-/Módulo 2/Sesion3/files")

division17 <- read.csv("SP1.csv")
division18 <- read.csv("SP2.csv")
division19 <- read.csv("SP3.csv")
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

library(janitor)
#Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el n?mero de goles que 
#anotan en un partido el equipo de casa o el equipo visitante.

#Con el ?ltimo data frame obtenido en el postwork de la sesi?n 2, elabora tablas de frecuencias relativas 
#para estimar las siguientes probabilidades:
str(FinalFrame)
FinalFrame <- rename(FinalFrame, goles_casa = FTHG , goles_visitantes = FTAG)

goles_casa <- sum(FinalFrame$goles_casa)
goles_visitantes <-sum(FinalFrame$goles_visitantes)
total_goles <- goles_casa+goles_visitantes;
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
frec2<- tabyl(FinalFrame, goles_casa) #tabyl ya saca la frencuencia y la probabilidad marginal
frec_casa <- FinalFrame %>%group_by(goles_casa) %>% summarise(frequency = n())  # para hacer esto se utiliza janitor
frec2<- tabyl(FinalFrame, goles_visitantes) #tabyl ya saca la frencuencia y la probabilidad marginal
frec_visitantes <- FinalFrame %>%group_by(goles_visitantes) %>% summarise(frequency = n())

p_casa_marginal = frec_casa$frequency /(sum(frec_casa$frequency,frec_visitantes$frequency)) #dividir la frecuencia/el total de partidos
tabla_casa_marginal <- data.frame(frec_casa$goles_casa, frec_casa$frequency, p_casa_marginal) #unir los goles, la frecuencia y la probabilidad
tabla_casa_marginal <- rename(tabla_casa_marginal, goles_casa = frec_casa.goles_casa , frecuencia = frec_casa.frequency)

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
  # para hacer esto se utiliza janitor
p_visitantes_marginal = frec_visitantes$frequency /(sum(frec_casa$frequency,frec_visitantes$frequency)) #dividir la frecuencia/el total de partidos
tabla_visitantes_marginal <- data.frame(frec_visitantes$goles_visitantes, frec_visitantes$frequency, p_visitantes_marginal) #u
tabla_visitantes_marginal <- rename(tabla_visitantes_marginal, goles_visitantes = frec_visitantes.goles_visitantes , frecuencia = frec_visitantes.frequency)
########### Conjunta#############

partidos2<-unite(FinalFrame, variables,c(4:5),sep=",",remove=TRUE)
tconjunta<-partidos2 %>%group_by(variables) %>% summarise(frequency=n())
p_conjunta<-tconjunta$frequency/(sum(tconjunta$frequency))
tabla_conjunta<-data.frame(tconjunta,p_conjunta)
tabla_conjunta<- within(data=tabla_conjunta,variables<-data.frame(do.call('rbind',strsplit(variables,",",fixed = TRUE))))

#Realiza lo siguiente:
#Un gr?fico de barras para las probabilidades marginales estimadas del n?mero de goles que anota el 
#equipo de casa
class(tabla_casa_marginal)
library(ggplot2)
ggplot(tabla_casa_marginal, aes(x = goles_casa, y = p_casa_marginal)) + 
  geom_col(colour = "black", fill= "green") +
  geom_text(aes(y = round(p_casa_marginal, 4), label = round(p_casa_marginal, 4)), 
            position = "identity", size=3, vjust = -1, hjust=0.5 ,col="black") +
  ggtitle("Probabilidades marginales de goles de equipos de casa") + 
  xlab("Goles") +
  ylab("Probabilidades") +
  theme_minimal()


#Un gr?fico de barras para las probabilidades marginales estimadas del n?mero de goles que anota el 
#equipo visitante.
ggplot(tabla_visitantes_marginal, aes(x = goles_visitantes, y = p_visitantes_marginal)) + 
  geom_col(colour = "black", fill= "red") +
  geom_text(aes(y = round(p_visitantes_marginal, 4), label = round(p_visitantes_marginal, 4)), 
            position = "identity", size=3, vjust = -1, hjust=0.5 ,col="black") +
  ggtitle("Probabilidades marginales de goles de equipos de visitante") + 
  xlab("Goles") +
  ylab("Probabilidades") +
  theme_minimal()

#Un HeatMap para las probabilidades conjuntas estimadas de los n?meros de goles que anotan el 
#equipo de casa y el equipo visitante en un partido.

ggplot(data = data.frame(tabla_conjunta), aes(x =variables$X1, y =variables$X2, fill= p_conjunta )) + geom_tile()

