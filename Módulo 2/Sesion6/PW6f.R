#Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
#Obtén el promedio por mes de la suma de goles. Crea la serie de tiempo del 
#promedio por mes de la suma de goles hasta diciembre de 2019. Grafica la serie de 

library(dplyr)
data <- read.csv("match.data.csv")
data <- mutate(data, date = as.Date(date, "%Y-%m-%d"))

data<-data.frame(data,data$home.score+data$away.score)
data<-rename(data,Golesporpartido=data.home.score...data.away.score)
data <- mutate(data, Ym = format(date, "%Y-%m"))
data <- data %>% group_by(Ym) %>% summarise(goles = mean(Golesporpartido))
View (data)
(data2 <- ts(data$goles, start = c(1, 1), end = c(95, 2), # Hasta diciembre de 2019
             frequency = 1))
ts.plot((data2), col = c(2, 4), ylim = c(0, 5))
abline(h = mean(data2), lwd = 2, col = 2, lty = 2)
legend(x = 2, y = 5,
       legend = c("GolesPromedio"),
       col = c(2, 4), lty = c(1, 1))

