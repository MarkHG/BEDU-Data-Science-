library(janitor)
setwd("/home/marco/Documentos/BEDU/github/BEDU-Data-Science-/Módulo 2/Sesion3/files")
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
p_casa_marginal = frec_casa$frequency / total_goles #dividir la frecuencia/el total de partidos
tabla_casa_marginal <- data.frame(frec_casa$goles_casa, frec_casa$frequency, p_casa_marginal) #unir los goles, la frecuencia y la probabilidad
tabla_casa_marginal <- rename(tabla_casa_marginal, goles_casa = frec_casa.goles_casa , frecuencia = frec_casa.frequency)

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
frec2<- tabyl(FinalFrame, goles_visitantes) #tabyl ya saca la frencuencia y la probabilidad marginal
frec_visitantes <- FinalFrame %>%group_by(goles_visitantes) %>% summarise(frequency = n())  # para hacer esto se utiliza janitor
p_visitantes_marginal = frec_visitantes$frequency / total_goles #dividir la frecuencia/el total de partidos
tabla_visitantes_marginal <- data.frame(frec_visitantes$goles_visitantes, frec_visitantes$frequency, p_visitantes_marginal) #u
tabla_visitantes_marginal <- rename(tabla_visitantes_marginal, goles_visitantes = frec_visitantes.goles_visitantes , frecuencia = frec_visitantes.frequency)
########### Conjunta#############
#suma de los goles de local y visitante cuando son 0,1,2,..
goles_conjunto00=frec_casa$frequency[1]+frec_visitantes$frequency[1]
goles_conjunto01=frec_casa$frequency[1]+frec_visitantes$frequency[2]
goles_conjunto02=frec_casa$frequency[1]+frec_visitantes$frequency[3]
goles_conjunto03=frec_casa$frequency[1]+frec_visitantes$frequency[4]
goles_conjunto04=frec_casa$frequency[1]+frec_visitantes$frequency[5]
goles_conjunto05=frec_casa$frequency[1]+frec_visitantes$frequency[6]
goles_conjunto06=frec_casa$frequency[1]+frec_visitantes$frequency[7]

goles_conjunto10=frec_casa$frequency[2]+frec_visitantes$frequency[1]
goles_conjunto11=frec_casa$frequency[2]+frec_visitantes$frequency[2]
goles_conjunto12=frec_casa$frequency[2]+frec_visitantes$frequency[3]
goles_conjunto13=frec_casa$frequency[2]+frec_visitantes$frequency[4]
goles_conjunto14=frec_casa$frequency[2]+frec_visitantes$frequency[5]
goles_conjunto15=frec_casa$frequency[2]+frec_visitantes$frequency[6]
goles_conjunto16=frec_casa$frequency[2]+frec_visitantes$frequency[7]

goles_conjunto20=frec_casa$frequency[3]+frec_visitantes$frequency[1]
goles_conjunto21=frec_casa$frequency[3]+frec_visitantes$frequency[2]
goles_conjunto22=frec_casa$frequency[3]+frec_visitantes$frequency[3]
goles_conjunto23=frec_casa$frequency[3]+frec_visitantes$frequency[4]
goles_conjunto24=frec_casa$frequency[3]+frec_visitantes$frequency[5]
goles_conjunto25=frec_casa$frequency[3]+frec_visitantes$frequency[6]
goles_conjunto26=frec_casa$frequency[3]+frec_visitantes$frequency[7]

goles_conjunto30=frec_casa$frequency[4]+frec_visitantes$frequency[1]
goles_conjunto31=frec_casa$frequency[4]+frec_visitantes$frequency[2]
goles_conjunto32=frec_casa$frequency[4]+frec_visitantes$frequency[3]
goles_conjunto33=frec_casa$frequency[4]+frec_visitantes$frequency[4]
goles_conjunto34=frec_casa$frequency[4]+frec_visitantes$frequency[5]
goles_conjunto35=frec_casa$frequency[4]+frec_visitantes$frequency[6]
goles_conjunto36=frec_casa$frequency[4]+frec_visitantes$frequency[7]

goles_conjunto40=frec_casa$frequency[5]+frec_visitantes$frequency[1]
goles_conjunto41=frec_casa$frequency[5]+frec_visitantes$frequency[2]
goles_conjunto42=frec_casa$frequency[5]+frec_visitantes$frequency[3]
goles_conjunto43=frec_casa$frequency[5]+frec_visitantes$frequency[4]
goles_conjunto44=frec_casa$frequency[5]+frec_visitantes$frequency[5]
goles_conjunto45=frec_casa$frequency[5]+frec_visitantes$frequency[6]
goles_conjunto46=frec_casa$frequency[5]+frec_visitantes$frequency[7]

goles_conjunto50=frec_casa$frequency[6]+frec_visitantes$frequency[1]
goles_conjunto51=frec_casa$frequency[6]+frec_visitantes$frequency[2]
goles_conjunto52=frec_casa$frequency[6]+frec_visitantes$frequency[3]
goles_conjunto53=frec_casa$frequency[6]+frec_visitantes$frequency[4]
goles_conjunto54=frec_casa$frequency[6]+frec_visitantes$frequency[5]
goles_conjunto55=frec_casa$frequency[6]+frec_visitantes$frequency[6]
goles_conjunto56=frec_casa$frequency[6]+frec_visitantes$frequency[7]

goles_conjunto60=frec_casa$frequency[7]+frec_visitantes$frequency[1]
goles_conjunto61=frec_casa$frequency[7]+frec_visitantes$frequency[2]
goles_conjunto62=frec_casa$frequency[7]+frec_visitantes$frequency[3]
goles_conjunto63=frec_casa$frequency[7]+frec_visitantes$frequency[4]
goles_conjunto64=frec_casa$frequency[7]+frec_visitantes$frequency[5]
goles_conjunto65=frec_casa$frequency[7]+frec_visitantes$frequency[6]
goles_conjunto66=frec_casa$frequency[7]+frec_visitantes$frequency[7]


goles_conjunto70=frec_casa$frequency[8]+frec_visitantes$frequency[1]
goles_conjunto71=frec_casa$frequency[8]+frec_visitantes$frequency[2]
goles_conjunto72=frec_casa$frequency[8]+frec_visitantes$frequency[3]
goles_conjunto73=frec_casa$frequency[8]+frec_visitantes$frequency[4]
goles_conjunto74=frec_casa$frequency[8]+frec_visitantes$frequency[5]
goles_conjunto75=frec_casa$frequency[8]+frec_visitantes$frequency[6]
goles_conjunto76=frec_casa$frequency[8]+frec_visitantes$frequency[7]


goles_conjunto80=frec_casa$frequency[9]+frec_visitantes$frequency[1]
goles_conjunto81=frec_casa$frequency[9]+frec_visitantes$frequency[2]
goles_conjunto82=frec_casa$frequency[9]+frec_visitantes$frequency[3]
goles_conjunto83=frec_casa$frequency[9]+frec_visitantes$frequency[4]
goles_conjunto84=frec_casa$frequency[9]+frec_visitantes$frequency[5]
goles_conjunto85=frec_casa$frequency[9]+frec_visitantes$frequency[6]
goles_conjunto86=frec_casa$frequency[9]+frec_visitantes$frequency[7]


goles_conjunto0<-rbind(goles_conjunto00,goles_conjunto01,goles_conjunto02,goles_conjunto03,goles_conjunto04,goles_conjunto05,goles_conjunto06)
goles_conjunto1<-rbind(goles_conjunto10,goles_conjunto11,goles_conjunto12,goles_conjunto13,goles_conjunto14,goles_conjunto15,goles_conjunto16)
goles_conjunto2<-rbind(goles_conjunto20,goles_conjunto21,goles_conjunto22,goles_conjunto23,goles_conjunto24,goles_conjunto25,goles_conjunto26)
goles_conjunto3<-rbind(goles_conjunto30,goles_conjunto31,goles_conjunto32,goles_conjunto33,goles_conjunto34,goles_conjunto35,goles_conjunto36)
goles_conjunto4<-rbind(goles_conjunto40,goles_conjunto41,goles_conjunto42,goles_conjunto43,goles_conjunto44,goles_conjunto45,goles_conjunto46)
goles_conjunto5<-rbind(goles_conjunto50,goles_conjunto51,goles_conjunto52,goles_conjunto53,goles_conjunto54,goles_conjunto55,goles_conjunto56)

goles_conjunto6<-rbind(goles_conjunto60,goles_conjunto61,goles_conjunto62,goles_conjunto63,goles_conjunto64,goles_conjunto65,goles_conjunto66)

goles_conjunto7<-rbind(goles_conjunto70,goles_conjunto71,goles_conjunto72,goles_conjunto73,goles_conjunto74,goles_conjunto75,goles_conjunto76)
goles_conjunto8<-rbind(goles_conjunto80,goles_conjunto81,goles_conjunto82,goles_conjunto83,goles_conjunto84,goles_conjunto85,goles_conjunto86)
goles_conjunto<-rbind(goles_conjunto0,goles_conjunto1,goles_conjunto2,goles_conjunto3,goles_conjunto4,goles_conjunto5,goles_conjunto6,goles_conjunto7,goles_conjunto8)

tabla_p_conjunta1<-data.frame(frec_casa$goles_casa[1],frec_visitantes$goles_visitantes,goles_conjunto0,goles_conjunto0/total_goles)
tabla_p_conjunta2<-data.frame(frec_casa$goles_casa[2],frec_visitantes$goles_visitantes,goles_conjunto1,goles_conjunto1/total_goles)
tabla_p_conjunta3<-data.frame(frec_casa$goles_casa[3],frec_visitantes$goles_visitantes,goles_conjunto2,goles_conjunto2/total_goles)
tabla_p_conjunta4<-data.frame(frec_casa$goles_casa[4],frec_visitantes$goles_visitantes,goles_conjunto3,goles_conjunto3/total_goles)
tabla_p_conjunta5<-data.frame(frec_casa$goles_casa[5],frec_visitantes$goles_visitantes,goles_conjunto4,goles_conjunto4/total_goles)

tabla_p_conjunta6<-data.frame(frec_casa$goles_casa[6],frec_visitantes$goles_visitantes,goles_conjunto5,goles_conjunto5/total_goles)
tabla_p_conjunta7<-data.frame(frec_casa$goles_casa[7],frec_visitantes$goles_visitantes,goles_conjunto6,goles_conjunto6/total_goles)
tabla_p_conjunta8<-data.frame(frec_casa$goles_casa[8],frec_visitantes$goles_visitantes,goles_conjunto7,goles_conjunto7/total_goles)
tabla_p_conjunta9<-data.frame(frec_casa$goles_casa[9],frec_visitantes$goles_visitantes,goles_conjunto8,goles_conjunto8/total_goles)
names(tabla_p_conjunta1)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta2)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta3)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta4)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta5)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta6)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")

names(tabla_p_conjunta7)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta8)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")
names(tabla_p_conjunta9)=c("GolesLocal","GolesVisitante","Sumadegoles","P_Conjunta")

tabla_p_conjunta <-rbind(tabla_p_conjunta1,tabla_p_conjunta2,tabla_p_conjunta3,tabla_p_conjunta4,tabla_p_conjunta5,tabla_p_conjunta6,tabla_p_conjunta7,tabla_p_conjunta8,tabla_p_conjunta9)
View(tabla_p_conjunta)
str(tabla_p_conjunta)

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

