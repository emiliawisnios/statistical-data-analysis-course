---
title: "Statistical Data Analysis 2020/2021"
author: "Emilia Wisnios"
subtitle: "Lab2"
output:   
  html_document:
    keep_md: true
email: e.wisnios@student.uw.edu.pl
---


# ZADANIE 1

Ustaw n: 

```r
n <- sample(100:5000, 1)
n
```

```
## [1] 3474
```

Wylosuj n obserwacji:

```r
x <- rnorm(n,10,1)
```

Oblicz srednia m:

```r
srednia = sum(x)/n
```

Estymator wariancji nieobciazony:

```r
EWN <- (1/(n-1))*sum((x-srednia)^2)
```
Estymator wariancji obciazony:

```r
EWO <- (1/(n))*sum((x-srednia)^2)
```
Odchylenie std na podstawie estymatora wariancji obciazonego:

```r
ENWOS = sqrt(EWO)
SD2 <- sqrt(EWN)
```


```r
print(c("estymator nieobciazony:", EWN))
```

```
## [1] "estymator nieobciazony:" "1.00445458529942"
```

```r
print(c("estymator obciazony:", EWO))
```

```
## [1] "estymator obciazony:" "1.00416545041592"
```

```r
print(c("estymator sd:", ENWOS))
```

```
## [1] "estymator sd:"    "1.00208056084125"
```

```r
print(c("var():", var(x)))
```

```
## [1] "var():"           "1.00445458529942"
```

```r
print(c("sd", sd(x)))
```

```
## [1] "sd"               "1.00222481774272"
```

```r
print(c("sd_nieob", SD2))
```

```
## [1] "sd_nieob"         "1.00222481774272"
```

# ZADANIE 2

Ustaw odchylenie std:

```r
std = 0.5
```
Wylosuj 5000 obs z rozkladu normalnego:

```r
n=5000
x <- rnorm(n, 0, std)
```
Przeksztalc wektor do postaci macierzy 10 x 500:

```r
m <- matrix(x, 10, 500)
```
Wyestymuj dla kazdej kolumny nieobciazony estymator wariancji:

```r
S <- apply(m, 2, var)
```
Oblicz obciazony estymator wariancji:

```r
S1 <- ((n-1)/n) * S
```
Oblicz trzeci estymator wariancji:

```r
S2 <- ((n-1)/(n+1)) * S
```
Oblicz obciazenie dla estymatorow wariancji:
<br>
E(theta_n) - theta
<br>
E(theta_n) = mean(S)
<br>
theta -- sd^2 (chcielismy miec taka wariancje)

```r
b_S=mean(S)-std^2
b_S1=mean(S1)-std^2
b_S2=mean(S2)-std^2

print(c("b_S:", b_S ))
```

```
## [1] "b_S:"                 "0.000898058536638657"
```

```r
print(c("b_S1:", b_S1 ))
```

```
## [1] "b_S1:"                "0.000847878924931322"
```

```r
print(c("b_S2:", b_S2 ))
```

```
## [1] "b_S2:"                "0.000797719381055106"
```

Oblicz rmse dla trzech estymatorow wariancji:

```r
RMSE_S=sqrt(1/n*sum((S-std^2)^2))
RMSE_S1=sqrt(1/n*sum((S1-std^2)^2))
RMSE_S2=sqrt(1/n*sum((S2-std^2)^2))


print(c("RMSE_S:", RMSE_S ))
```

```
## [1] "RMSE_S:"            "0.0387914269596924"
```

```r
print(c("RMSE_S1:", RMSE_S1 ))
```

```
## [1] "RMSE_S1:"           "0.0387835561423739"
```

```r
print(c("RMSE_S2:", RMSE_S2 ))
```

```
## [1] "RMSE_S2:"           "0.0387756949171864"
```


