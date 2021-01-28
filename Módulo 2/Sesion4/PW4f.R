division17 <- read.csv("SP1.csv")
division18 <- read.csv("SP2.csv")
division19 <- read.csv("SP3.csv")
suppressMessages(suppressWarnings(library(dplyr)))
division17 <- select(division17,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)
division18 <- select(division18,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)
division19 <- select(division19,Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR)

# Asegúrate de que los elementos de las columnas correspondientes de los nuevos 
# data frames sean del mismo tipo. Con ayuda de la función rbind forma un único 
# data frame que contenga las seis columnas mencionadas en el punto 3. 

division17 <- mutate(division17, Date = as.Date(Date, "%d/%m/%y"))
division18 <- mutate(division18, Date = as.Date(Date, "%d/%m/%y"))
division19 <- mutate(division19, Date = as.Date(Date, "%d/%m/%y"))

FinalFrame <- rbind(division17,division18,division19)

View(FinalFrame)



library(janitor)
#Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que 
#anotan en un partido el equipo de casa o el equipo visitante.

#Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas 
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
tabla_conjunta1<-data.frame(tabla_conjunta$variables$X1,tabla_conjunta$variables$X2,tabla_conjunta$frequency,tabla_conjunta$p_conjunta)
tabla_conjunta1<-rename(tabla_conjunta1,Local=tabla_conjunta.variables.X1,Visitante=tabla_conjunta.variables.X2, Frecuencia=tabla_conjunta.frequency, PConjunta=tabla_conjunta.p_conjunta)

k<-1
golesc<-NULL
goles_v<-NULL
for(i in 1:length(frec_casa$frequency)){
  for(j in 1:length(frec_visitantes$frequency)){
    golesc[k]<-frec_casa$goles_casa[i]
    goles_v[k]<-frec_visitantes$goles_visitantes[j]
    k<-k+1
  }}
tabla_apoyo<-data.frame(golesc,goles_v)
tabla_apoyo<-rename(tabla_apoyo,Local=golesc,Visitante=goles_v)
View(tabla_apoyo)


k<-1
mult<-NULL
for(i in 1:length(p_casa_marginal)){
  for(j in 1:length(p_visitantes_marginal)){
    mult[k]<-(p_casa_marginal[i]*p_visitantes_marginal[j])
    k<-k+1
}}
tabla_apoyo<-cbind(tabla_apoyo,mult)
View(tabla_apoyo)
View(tabla_conjunta1)


t<-merge(x=tabla_conjunta1, y=tabla_apoyo, by=c("Local","Visitante"), all.x=TRUE)
tabla_final<-data.frame(t,t$PConjunta/t$mult)
tabla_final<-rename(tabla_final,Cociente=t.PConjunta.t.mult)


#BOOTSTRAP
library(rsample)
set.seed(839287482)
computos_boot <- bootstraps(tabla_final, times = 100)
first_computos_boot <- computos_boot$splits[[1]]
x<-as.data.frame(first_computos_boot)

#Se observa que el valor nunca es 1 por lo que son dependientes 