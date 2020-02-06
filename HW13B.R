setwd("C:/Users/USER/Documents/BASM")

data <- read.csv("security_questions.csv",stringsAsFactors = FALSE)

pca <- prcomp(data)

summary(pca)

screeplot(pca, type = "line")

pca$sdev^2

sqrt(eigen(cor(data))$values)
