library(class)

# Dokonczenie labu 5
set.seed(9042021)

w <- read.table("https://mimuw.edu.pl/~dot/resources/sad/wine.csv",
                sep="\t", header=T, as.is=T )

# centrowanie
w[,-1] <- apply(w[,-1], 2, function(x) x-mean(x))
# skalowanie
w[,-1] <- apply(w[,-1], 2, function(x) x/sd(x))

as.factor(w[,1])

# podzielenie zbioru na czesci
indeksy_testowe <- sample(1:nrow(w), 480, replace=F)
zbior_testowy <- w[indeksy_testowe, ]
zbior_treningowy <- w[-indeksy_testowe, ] # Indeksowanie ujemne wiele ułatwia!

# knn 1 (k = 2)
knn1 <- knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k=2)
# knn 2 (k = 10)
knn2 <- knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k=10)
# knn 3 (k =15)
knn3 <- knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k=15)

# macierz konfuzji
confusion1 <- table('True'=zbior_testowy[, 1], 'Label'=knn1)
confusion1

confusion2 <- table('True'=zbior_testowy[, 1], 'Label'=knn2)
confusion2

confusion3 <- table('True'=zbior_testowy[, 1], 'Label'=knn3)
confusion3

# precision
precisions1 <- diag(confusion1)/colSums(confusion1)
precisions2 <- diag(confusion2)/colSums(confusion2)
precisions3 <- diag(confusion3)/colSums(confusion3)
precisions1
precisions2
precisions3
# recall
recalls1 <- diag(confusion1)/rowSums(confusion1)
recalls2 <- diag(confusion2)/rowSums(confusion2)
recalls3 <- diag(confusion3)/rowSums(confusion3)
recalls1
recalls2
recalls3

# Regresja liniowa -- lab 6

# wczytanie pliku
w <- read.table("https://mimuw.edu.pl/~dot/resources/sad/ais.txt",
                sep="\t", header=T, as.is=T )

# obejrzenie zawartosci
summary(w)

library(GGally)
ggpairs(w, aes(col=Sex), columns=c(9, 10, 5, 13))

regresja <- lm(Wt ~ RCC + Hc+Ht + Ferr + SSF + X.Bfat + LBM  ,w)
summary(regresja)
library(car)
qqPlot(regresja$residuals)
