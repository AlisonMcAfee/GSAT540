# seminar02b
Ali  
Wednesday, January 21, 2015  

tosses = times the coin is flipped in one try,

tries = toss events,

p = probability of getting a head,

turn coinflips into a function:



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
## [1] 0.49
```

loop over many tries


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
## 995  0.4804020
## 996  0.4849398
## 997  0.5085256
## 998  0.5340681
## 999  0.5085085
## 1000 0.4930000
```
expect convergence around y = 0,

make scatter plot

![](seminar02b_files/figure-html/unnamed-chunk-3-1.png) 

or make a boxplot

![](seminar02b_files/figure-html/unnamed-chunk-4-1.png) 
