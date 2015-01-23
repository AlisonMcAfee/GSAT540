# seminar01b
Ali  
Thursday, January 22, 2015  

Playing with GSE4051_MINI.txt file


```r
read.delim("GSE4051_MINI.txt")
```

```
##      sidChar sidNum devStage gType crabHammer eggBomb poisonFang
## 1  Sample_20     20      E16    wt     10.220   7.462      7.370
## 2  Sample_21     21      E16    wt     10.020   6.890      7.177
## 3  Sample_22     22      E16    wt      9.642   6.720      7.350
## 4  Sample_23     23      E16    wt      9.652   6.529      7.040
## 5  Sample_16     16      E16 NrlKO      8.583   6.470      7.494
## 6  Sample_17     17      E16 NrlKO     10.140   7.065      7.005
## 7   Sample_6      6      E16 NrlKO     10.340   7.017      6.735
## 8  Sample_24     24       P2    wt      8.869   6.587      7.508
## 9  Sample_25     25       P2    wt      9.038   6.170      7.449
## 10 Sample_26     26       P2    wt      9.611   6.870      7.511
## 11 Sample_27     27       P2    wt      8.613   6.800      7.843
## 12 Sample_14     14       P2 NrlKO      9.572   6.138      7.250
## 13  Sample_3      3       P2 NrlKO      9.414   6.166      7.200
## 14  Sample_5      5       P2 NrlKO      8.925   6.269      7.405
## 15  Sample_8      8       P2 NrlKO      9.116   6.264      8.016
## 16 Sample_28     28       P6    wt      8.214   6.530      7.428
## 17 Sample_29     29       P6    wt     10.130   7.574      7.100
## 18 Sample_30     30       P6    wt      8.951   6.269      7.274
## 19 Sample_31     31       P6    wt      8.693   6.211      7.409
## 20  Sample_1      1       P6 NrlKO      8.920   6.286      7.378
## 21 Sample_10     10       P6 NrlKO      9.544   6.347      7.252
## 22  Sample_4      4       P6 NrlKO      8.992   6.270      7.342
## 23  Sample_7      7       P6 NrlKO      8.803   6.188      7.754
## 24 Sample_32     32      P10    wt     10.250   8.173      7.005
## 25 Sample_33     33      P10    wt      9.004   7.082      8.086
## 26 Sample_34     34      P10    wt      8.519   6.757      8.584
## 27 Sample_35     35      P10    wt      8.449   6.155      7.201
## 28 Sample_13     13      P10 NrlKO      9.838   7.228      7.459
## 29 Sample_15     15      P10 NrlKO      9.746   7.226      7.786
## 30 Sample_18     18      P10 NrlKO     10.140   7.438      7.363
## 31 Sample_19     19      P10 NrlKO      9.771   7.081      7.586
## 32 Sample_36     36  4_weeks    wt      9.960   7.866      6.993
## 33 Sample_37     37  4_weeks    wt      9.667   6.992      7.324
## 34 Sample_38     38  4_weeks    wt      9.767   6.608      7.329
## 35 Sample_39     39  4_weeks    wt     10.200   7.003      7.320
## 36 Sample_11     11  4_weeks NrlKO      9.677   7.204      6.981
## 37 Sample_12     12  4_weeks NrlKO      9.129   7.165      7.350
## 38  Sample_2      2  4_weeks NrlKO      9.744   7.107      7.075
## 39  Sample_9      9  4_weeks NrlKO      9.822   6.558      7.043
```

```r
prDat <- read.table("GSE4051_MINI.txt", header = TRUE, row.names = 1)
str(prDat)
```

```
## 'data.frame':	39 obs. of  6 variables:
##  $ sidNum    : int  20 21 22 23 16 17 6 24 25 26 ...
##  $ devStage  : Factor w/ 5 levels "4_weeks","E16",..: 2 2 2 2 2 2 2 4 4 4 ...
##  $ gType     : Factor w/ 2 levels "NrlKO","wt": 2 2 2 2 1 1 1 2 2 2 ...
##  $ crabHammer: num  10.22 10.02 9.64 9.65 8.58 ...
##  $ eggBomb   : num  7.46 6.89 6.72 6.53 6.47 ...
##  $ poisonFang: num  7.37 7.18 7.35 7.04 7.49 ...
```

```r
nrow(prDat) 
```

```
## [1] 39
```

```r
ncol(prDat) 
```

```
## [1] 6
```

```r
head(prDat)
```

```
##           sidNum devStage gType crabHammer eggBomb poisonFang
## Sample_20     20      E16    wt     10.220   7.462      7.370
## Sample_21     21      E16    wt     10.020   6.890      7.177
## Sample_22     22      E16    wt      9.642   6.720      7.350
## Sample_23     23      E16    wt      9.652   6.529      7.040
## Sample_16     16      E16 NrlKO      8.583   6.470      7.494
## Sample_17     17      E16 NrlKO     10.140   7.065      7.005
```

```r
tail(prDat) 
```

```
##           sidNum devStage gType crabHammer eggBomb poisonFang
## Sample_38     38  4_weeks    wt      9.767   6.608      7.329
## Sample_39     39  4_weeks    wt     10.200   7.003      7.320
## Sample_11     11  4_weeks NrlKO      9.677   7.204      6.981
## Sample_12     12  4_weeks NrlKO      9.129   7.165      7.350
## Sample_2       2  4_weeks NrlKO      9.744   7.107      7.075
## Sample_9       9  4_weeks NrlKO      9.822   6.558      7.043
```

```r
names(prDat)
```

```
## [1] "sidNum"     "devStage"   "gType"      "crabHammer" "eggBomb"   
## [6] "poisonFang"
```

```r
str(prDat)
```

```
## 'data.frame':	39 obs. of  6 variables:
##  $ sidNum    : int  20 21 22 23 16 17 6 24 25 26 ...
##  $ devStage  : Factor w/ 5 levels "4_weeks","E16",..: 2 2 2 2 2 2 2 4 4 4 ...
##  $ gType     : Factor w/ 2 levels "NrlKO","wt": 2 2 2 2 1 1 1 2 2 2 ...
##  $ crabHammer: num  10.22 10.02 9.64 9.65 8.58 ...
##  $ eggBomb   : num  7.46 6.89 6.72 6.53 6.47 ...
##  $ poisonFang: num  7.37 7.18 7.35 7.04 7.49 ...
```

```r
levels(prDat$devStage)
```

```
## [1] "4_weeks" "E16"     "P10"     "P2"      "P6"
```

```r
summary(prDat$devStage)
```

```
## 4_weeks     E16     P10      P2      P6 
##       8       7       8       8       8
```

```r
summary(prDat$gType)
```

```
## NrlKO    wt 
##    19    20
```

```r
table(prDat$devStage, prDat$gType)
```

```
##          
##           NrlKO wt
##   4_weeks     4  4
##   E16         3  4
##   P10         4  4
##   P2          4  4
##   P6          4  4
```
Intended experimental design? Maybe to see how developmetn/life expectancy of the KO mouse differs from wt. Looks like it's the same, though


```r
summary(prDat$crabHammer) #...etc
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   8.214   8.938   9.611   9.428   9.830  10.340
```

```r
weeDat <- subset(prDat, subset = poisonFang > 7.5)
summary(weeDat)
```

```
##      sidNum         devStage   gType     crabHammer       eggBomb     
##  Min.   : 7.00   4_weeks:0   NrlKO:4   Min.   :8.519   Min.   :6.188  
##  1st Qu.:15.00   E16    :0   wt   :5   1st Qu.:8.803   1st Qu.:6.587  
##  Median :24.00   P10    :4             Median :9.004   Median :6.800  
##  Mean   :21.44   P2     :4             Mean   :9.117   Mean   :6.762  
##  3rd Qu.:27.00   P6     :1             3rd Qu.:9.611   3rd Qu.:7.081  
##  Max.   :34.00                         Max.   :9.771   Max.   :7.226  
##    poisonFang   
##  Min.   :7.508  
##  1st Qu.:7.586  
##  Median :7.786  
##  Mean   :7.853  
##  3rd Qu.:8.016  
##  Max.   :8.584
```

```r
str(weeDat$poisonFang)
```

```
##  num [1:9] 7.51 7.51 7.84 8.02 7.75 ...
```

```r
prDat[c("Sample_16", "Sample_38"), "eggBomb"]
```

```
## [1] 6.470 6.608
```

```r
quantile(prDat$eggBomb, .1)
```

```
##    10% 
## 6.1844
```

```r
qDat <- subset(prDat, subset = eggBomb < 6.1844)
rownames(qDat)
```

```
## [1] "Sample_25" "Sample_14" "Sample_3"  "Sample_35"
```
