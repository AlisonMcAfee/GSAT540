# seminar04b
Ali  
Monday, February 02, 2015  
Task: determine which country has the sharpest rise in GDP over 5 years, and between which years
I will use the normalized representation of GDP

steps:

- get data
- normalize to Canada as benchmark


```
## Observations: 1704
## Variables:
## $ country   (fctr) Afghanistan, Afghanistan, Afghanistan, Afghanistan,...
## $ year      (int) 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992...
## $ pop       (dbl) 8425333, 9240934, 10267083, 11537966, 13079460, 1488...
## $ continent (fctr) Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asi...
## $ lifeExp   (dbl) 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.8...
## $ gdpPercap (dbl) 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 78...
```

Now calculate delta GDP and arrange in descending order, printing the top 5


```r
gtbl %>%
  group_by(country) %>%
  select(country, year, gdpPercapRel) %>%
  mutate(gdp_delta = gdpPercapRel - lag(gdpPercapRel)) %>% 
  summarize(highest_gdp_delta = max(gdp_delta, na.rm = TRUE)) %>%
  arrange(desc(highest_gdp_delta )) %>% head(5)
```

```
## Source: local data frame [5 x 2]
## 
##        country highest_gdp_delta
## 1       Kuwait         0.7322349
## 2        Libya         0.6657919
## 3        Gabon         0.3833352
## 4         Oman         0.2660578
## 5 Saudi Arabia         0.2578533
```



Now we know that Kuwait experienced the sharpest GDP growth. Let's inspect Kuwait in more detail:


```r
gtbl %>%
  filter(country=="Kuwait") %>%
  select(gdpPercapRel, year) 
```

```
## Source: local data frame [12 x 2]
## 
##    gdpPercapRel year
## 1      9.534690 1952
## 2      9.089158 1957
## 3      7.090675 1962
## 4      5.031844 1967
## 5      5.764079 1972
## 6      2.682803 1977
## 7      1.369244 1982
## 8      1.056031 1987
## 9      1.326086 1992
## 10     1.391840 1997
## 11     1.053441 2002
## 12     1.302533 2007
```

The observed maximum increase (~0.7) is the difference between years 1972 and 1967.

I recognize that this approach is not scalable to bigger datasets that you cannot easily inspect visually like this one



  
