---
title: "Statistical Data Analysis 2020/2021"
author: "Emilia Wisnios"
subtitle: "Lab5"
output:   
  html_document:
    keep_md: true
email: e.wisnios@student.uw.edu.pl
---


```r
m = read.csv('https://mimuw.edu.pl/~dot/resources/sad/Zadluzenie_gmin.csv', header=TRUE, sep ='\t')
m$woj <- floor(m$Kod.Teryt/100000)
slownik <- c('2' = 'Dolnośląskie', '4' = 'Kujawsko-pomorskie',
             '6' = 'Lubelskie', '8' = 'Lubuskie',
             '10' = 'Łódzkie', '12' = 'Małopolskie',
             '14' = 'Mazowieckie', '16' = 'Opolskie', 
             '18' = 'Podkarpackie', '20' = 'Podlaskie',
             '22' = 'Pomorskie', '24' = 'Śląskie',
             '26' = 'Świętokrzyskie', '28' = 'Warmińsko-mazurskie',
             '30' = 'Wielkopolskie',  '32' = 'Zachodniopomorskie')
m$woj <- slownik[as.character(m$woj)]
a <- m[m$woj == "Mazowieckie", ]
t = (mean((a$Zadlużenie.gmin) - 25) / sd(a$Zadlużenie.gmin)) * sqrt(nrow(a))
qnorm(0.05)
pnorm(t)
n = nrow(a)
alpha = 0.05
asympt1 = mean(a$Zadłużenie.gmin) + (qnorm((1 - alpha/2))/sqrt(n))*sd(a$Zadłużenie.gmin)
```

```
## Warning in mean.default(a$Zadłużenie.gmin): argument is not numeric or logical:
## returning NA
```

```r
asympt2 = mean(a$Zadłużenie.gmin) - (qnorm((1 - alpha/2))/sqrt(n)) * sd(a$Zadłużenie.gmin)
```

```
## Warning in mean.default(a$Zadłużenie.gmin): argument is not numeric or logical:
## returning NA
```


```r
w <- read.table("https://mimuw.edu.pl/~dot/resources/sad/wine.csv",
                sep="\t", header=T, as.is=T )
w[,-1] <- apply(w[,-1], 2, function(x) x -mean(x))
w[,-1] <- apply(w[,-1], 2, function(x) x/sd(x))
w[,1] <- as.factor(w[,1])
```

Klasyfikacja krok po kroku

```r
x <- c(0.42, 0.03, -0.90, 0.15, -1.25, -0.15, -0.01, 0.73, 0.90, -0.82, -0.69)
```
Obliczenie macierzy odleglosci

```r
distances <- apply(w[,-1], 1, function(y) sqrt(sum((x-y)^2)))
```
Uporzadkowane klasy pod wzgledem odlegosci od x

```r
k <- 3
najblizsze_wiersze <- order(distances)[1:k]
```
Przypisanie najblizszych klas

```r
najblizsze_klasy <- w[najblizsze_wiersze, 1]
```
Szukamy najliczniejszej klasy

```r
czestosc_klas <- table(najblizsze_klasy)
czestosc_klas
```

```
## najblizsze_klasy
## 3 4 5 6 7 8 9 
## 0 0 1 2 0 0 0
```

```r
najczestsza_klasa <- which.max(czestosc_klas)
najczestsza_klasa
```

```
## 6 
## 4
```

```r
najczestsza_klasa <- levels(w$Quality)[najczestsza_klasa]
```

knn(treningowe, testowe, wektor prawdziwych wartosci, k)

```r
library(class)
# wynik <- knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k=3)
```
