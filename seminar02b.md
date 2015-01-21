# seminar02b
Ali  
Wednesday, January 21, 2015  

tosses = times the coin is flipped in one try
tries = toss events
p = probability of getting a head
turn coinflips into a function



```r
coinflips <- function(tosses, tries, p){
  tab <- matrix(runif(tosses * tries) > p,
              nrow = length(tosses), ncol=tries)
  y <- rowSums(tab)
  y.mean <- mean(y)
  prop <- (y.mean/tries)
  return(prop)
}
coinflips(1, 100, 0.5)
```

```
## [1] 0.48
```

loop over many tries

B <- 1000
n <- 1
track <- matrix(nrow = B, ncol = n, dimnames = list(c(1:B),names(n)))
for (i in 1:B) {
  track[i,] <- coinflips(1, i, 0.5)
}
tail(track)

```r
B <- 1000
n <- 1
track <- matrix(nrow = B, ncol = n, dimnames = list(c(1:B),names(n)))
for (i in 1:B) {
  track[i,] <- coinflips(1, i, 0.5)
}
tail(track)
```

```
##           [,1]
## 995  0.4814070
## 996  0.4809237
## 997  0.4934804
## 998  0.4669339
## 999  0.4904905
## 1000 0.4920000
```
expect convergence around y = 0
make scatter plot

plot(track, xlab="flips", col="red")
legend("topright", c("p = 0.5"))

``{r, echo=FALSE}
plot(track, xlab="flips", col="red")
legend("topright", c("p = 0.5"))
``
or make a boxplot

boxplot(track)
legend("topright", c("p = 0.5"))

``{r, echo=FALSE}
boxplot(track)
legend("topright", c("p = 0.5"))
``
