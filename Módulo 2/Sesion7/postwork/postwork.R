
library(mongolite)

# Conexi�n a la base de datos
con <- mongo("match", url =
               "mongodb+srv://MarcoBEDU:<password>@cluster0.zavzu.mongodb.net/match_games")

# Prueba de conexi�n a la base de datos

mydata <- con$find()
mydata <- con$count()



# Conexi�na la colecci�n
m <- mongo(collection = "match")

# Query : No existen partidos en tal fecha en la base de datos proporcionada

query <- m$find('{"$and": [{"$or": [{"HomeTeam": "Real Madrid"}, {"AwayTeam": "Real Madrid"}]}, {"Date": "2015-12-20"}]}')


rm(con)
gc()
