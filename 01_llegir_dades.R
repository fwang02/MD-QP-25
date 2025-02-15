#leer datos sin añadir NA
data <- read.csv("reduced-data.csv",sep=";")
head(data)

#leer datos con NA
data2 <- read.csv("data_withNA.csv", row.names = NULL)
data2 <- data2[,-1]
head(data2)
