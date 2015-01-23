# seminar02c
Ali  
Thursday, January 22, 2015  

---
title: "seminar02c"
author: "Ali"
date: "Thursday, January 22, 2015"
output: html_document
---
Exercise:

Generate some data from a different normal distribution, i.e. where the mean is not 0 and/or the standard deviation is not 1. Make the new parameter values extreme, so the data will be obviously different.



```r
rnorm(n = 10, sd = 1, mean = 10)
```

```
##  [1] 10.754898  9.278052 10.606115 11.317203  8.913661  9.467586 10.555665
##  [8] 10.743025 10.258854  9.114199
```

```r
n <- 5
B <- c(5)
x <- matrix(rnorm(n * B, sd = 1, mean = 10), nrow = n)
rownames(x) <- sprintf("obs%02d", 1:n)
colnames(x) <- sprintf("samp%02d", 1:B)
x
```

```
##          samp01   samp02    samp03    samp04    samp05
## obs01 10.337879 9.577065  8.411064 11.200125 11.206123
## obs02 10.291230 9.877826 10.898040  9.921760  9.953012
## obs03  9.969952 9.980475  8.402578  8.268627 11.338994
## obs04  9.198290 9.376109  9.214325 10.494063 10.570653
## obs05 10.898401 9.808009  9.616105  9.651125  9.389259
```
Exercise:
"Repeat the above for a different distribution."



```r
set.seed(10)
rnorm(n = 10, sd = 1, mean = 10)
```

```
##  [1] 10.018746  9.815747  8.628669  9.400832 10.294545 10.389794  8.791924
##  [8]  9.636324  8.373327  9.743522
```

```r
n <- 5
B <- c(5)
x <- matrix(rnorm(n * B, sd = 1, mean = 10), nrow = n)
rownames(x) <- sprintf("obs%02d", 1:n)
colnames(x) <- sprintf("samp%02d", 1:B)
x
```

```
##          samp01    samp02   samp03   samp04    samp05
## obs01 11.101780 10.089347 9.403689 9.626338  8.146260
## obs02 10.755782  9.045056 7.814713 9.312445  9.922054
## obs03  9.761766  9.804850 9.325134 9.127841 10.968566
## obs04 10.987445 10.925521 7.880939 9.898239 10.184926
## obs05 10.741390 10.482979 8.734802 9.746219  8.620056
```

Exercise: 

Recall the claim that the expected value of the sample mean is the true mean. Compute the average of the 4 sample means we have. Is it (sort of) close the true mean? Feel free to change n or B at any point.


```r
means <- colMeans(x)
mean(means) 
```

```
## [1] 9.696325
```

```r
#answer: yes, close to 10!
```

Exercise: 

Recall the Weak Law of Large Numbers said that, as the sample size gets bigger, the distribution of the sample means gets more concentrated around the true mean.


```r
#had to look at the source code for this one
B <- 1000
x10 <- matrix(rnorm(10 * B), nrow = 10)
x100 <- matrix(rnorm(100 * B), nrow = 100)
x1000 <- matrix(rnorm(1000 * B), nrow = 1000)
x10000 <- matrix(rnorm(10000 * B), nrow = 10000)
xBar10 <- colMeans(x10)
xBar100 <- colMeans(x100)
xBar1000 <- colMeans(x1000)
xBar10000 <- colMeans(x10000)
xBarSd10 <- sd(colMeans(x10))
xBarSd100 <- sd(colMeans(x100))
xBarSd1000 <- sd(colMeans(x1000))
xBarSd10000 <- sd(colMeans(x10000))
cbind(sampSize = c(10, 100, 1000, 10000),
      trueSEM = 1 / sqrt(c(10, 100, 1000, 10000)),
      obsSEM = c(xBarSd10, xBarSd100, xBarSd1000, xBarSd10000))
```

```
##      sampSize    trueSEM     obsSEM
## [1,]       10 0.31622777 0.31526638
## [2,]      100 0.10000000 0.09809276
## [3,]     1000 0.03162278 0.03150052
## [4,]    10000 0.01000000 0.01023597
```

Exercise: 

Generate a reasonably large sample from some normal distribution (it need not be standard normal!). Pick a threshhold. What is the CDF at that threshhold, i.e. what's the true probability of seeing an observation less than or equal to the threshhold? Use your large sample to compute the observed proportion of observations that are less than threshhold. Are the two numbers sort of close? Hint: If x is a numeric vector, then mean(x <= threshhold) computes the proportion of values less than or equal to threshhold


```r
x <- rpois(10, lambda = 5)
ppois(6, lambda = 5)
```

```
## [1] 0.7621835
```

```r
mean(x <= 6)
```

```
## [1] 0.8
```

```r
#sort of close!
```

Exercise: 

Do the same for a variety of sample sizes. Do the two numbers tend to be closer for larger samples?


```r
ppois(6, lambda = 5)
```

```
## [1] 0.7621835
```

```r
x <- rpois(10, lambda = 5)
mean(x <= 6)
```

```
## [1] 0.5
```

```r
x <- rpois(100, lambda = 5)
mean(x <= 6)
```

```
## [1] 0.81
```

```r
x <- rpois(1000, lambda = 5)
mean(x <= 6)
```

```
## [1] 0.75
```

```r
x <- rpois(100000000, lambda = 5)
mean(x <= 6)
```

```
## [1] 0.76218
```

```r
#yes, the two numbers tend to be closer with higher n
```
Exercise: 

Do the same for a different distribution.

Exercise: 

Instead of focusing on tail probabilities, focus on the probability of the observed values falling in an interval.


```r
bin <- rbinom(10, size = 100, prob = 0.78)
range <- (mean(bin >= 78) - mean(bin >= 80))
range
```

```
## [1] 0.4
```

```r
pbinom(78, size = 100, prob = .78)
```

```
## [1] 0.5391073
```

