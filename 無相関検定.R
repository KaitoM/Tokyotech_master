#無相関検定

data <- read.csv("C:/cygwin64/home/kaito/Program/R_PCA/PCA_無相関.csv",head=T,row.names=1)


SK <-　data$SK
CP1 <- data$CP1
CP2 <- data$CP2
CP3 <- data$CP3
Australia <- data$Australia
NS <- data$NS
NAO <- data$NAO
cor.test(SK, NAO)