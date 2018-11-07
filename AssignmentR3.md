---
title: "AssignmentR_3"
author: "Tabea"
date: "2 November 2018"
output: 
  html_document:
      keep_md: yes
---
# 2

```r
rna_counts <- read.csv("C:/Users/Tabea/Downloads/eXpress_dm_counts.csv", row.names = 1, sep = ",", header = TRUE)

mean_expr <- function(x, log = FALSE) {
       if(log) {
               mean(x)} else {
                   z <- x + 0.0001
                   mean(log2(z))
                   }
       }

#> mean_expr(rna_counts[, 5])
# [1] 8.972111
#> mean_expr(rna_counts[, 5], log = TRUE)
# [1] 2105.712
#> mean_expr(rna_counts[, 22])
# [1] 8.348475
#> mean_expr(rna_counts[, 22], log = TRUE)
# [1] 2081.889
#> mean_expr(rna_counts[, 6])
# [1] 8.025615
```

#3

```r
mean_cols <- c(names(rna_counts))
for(i in 1:ncol(rna_counts)) {
        mean_cols[i] <- mean_expr(rna_counts[, i])
        names(mean_cols) <- c(names(rna_counts[-1]))
    }

mean_cols_log2 <- c(names(rna_counts))
for(i in 1:ncol(rna_counts)) {
        mean_cols_log2[i] <- mean_expr(rna_counts[, i], log = TRUE)
        names(mean_cols_log2) <- c(names(rna_counts[-1]))
    }

sort(mean_cols, decreasing = TRUE)
```

```
##  F197_sm_female_hdhorn F197_sm_female_thxhorn F218_lg_female_thxhorn 
##     "9.54287841308877"     "9.53302989712228"     "9.24713332747649" 
##   F131_lg_female_wings    M200_sm_male_hdhorn     M257_lg_male_wings 
##     "9.22460485200084"     "9.21246448161099"     "9.19040460904623" 
##     M200_sm_male_wings     M120_sm_male_wings M200_sm_male_genitalia 
##     "9.14437187740266"     "9.12449378963196"     "9.08597122612734" 
##    M172_sm_male_hdhorn    M257_lg_male_hdhorn   F218_lg_female_wings 
##     "9.06145550987269"       "9.007847956772"      "8.9825734581262" 
## F105_lg_female_thxhorn M125_lg_male_genitalia    M171_sm_male_hdhorn 
##     "8.97211063424517"     "8.95113679468463"     "8.91307271542031" 
## M180_lg_male_genitalia F131_lg_female_thxhorn   F101_lg_female_wings 
##     "8.89766562895116"     "8.86696442798666"     "8.85650723717269" 
## F101_lg_female_thxhorn     M160_lg_male_wings   F197_sm_female_wings 
##     "8.84984898998445"     "8.84136919025785"     "8.81449211632211" 
##    M120_sm_male_hdhorn    M180_lg_male_hdhorn  F136_sm_female_hdhorn 
##     "8.77769321504111"     "8.74193787805923"     "8.72030902235116" 
##    M160_lg_male_hdhorn   M180_lg_male_thxhorn  F196_sm_female_hdhorn 
##     "8.69707320666022"      "8.6873299091011"     "8.68471548954304" 
##   F136_sm_female_wings   M257_lg_male_thxhorn F136_sm_female_thxhorn 
##     "8.67589423901563"     "8.65248827123284"     "8.60812313660682" 
## M160_lg_male_genitalia M171_sm_male_genitalia   F135_sm_female_wings 
##     "8.56969127013491"     "8.56619678768532"      "8.5657870688924" 
##   M160_lg_male_thxhorn    M125_lg_male_hdhorn   M120_sm_male_thxhorn 
##     "8.55860186332222"     "8.55025487946795"     "8.48465048208515" 
##     M125_lg_male_wings M257_lg_male_genitalia M120_sm_male_genitalia 
##     "8.46330532487323"     "8.45163718687766"     "8.42827091386678" 
##     M180_lg_male_wings F196_sm_female_thxhorn  F218_lg_female_hdhorn 
##     "8.38915531940576"     "8.38005463284577"     "8.34847496163504" 
##     M171_sm_male_wings   M200_sm_male_thxhorn   M171_sm_male_thxhorn 
##     "8.32701205283631"     "8.31809980086368"     "8.24889594409076" 
##   M172_sm_male_thxhorn M172_sm_male_genitalia  F131_lg_female_hdhorn 
##     "8.24752384054072"     "8.22255036800592"     "8.22188624688376" 
## F135_sm_female_thxhorn  F135_sm_female_hdhorn     M172_sm_male_wings 
##     "8.14234620661589"     "8.12229523737065"     "8.07994271852859" 
##   F105_lg_female_wings   F196_sm_female_wings                   <NA> 
##     "8.02561472315719"     "7.97084785185446"     "7.96343234942084" 
##  F105_lg_female_hdhorn 
##     "7.94458989682167"
```

```r
#The female's wings and their horns have the highest mean.
```


#4

```r
time.lapply <- system.time(lapply(rna_counts[-1], mean_expr))
```
#> time.lapply
#       User      System verstrichen 
#       0.03        0.00        0.03 

```r
time.mean <- system.time(for(i in 2:ncol(rna_counts)) {
       mean_cols_log2[i] <- mean_expr(rna_counts[, i], log = TRUE)
   })
```
#> time.mean
#       User      System verstrichen 
#       0.01        0.00        0.01 
       
#The loop works faster.

#5

```r
mean_easy <- colMeans(rna_counts[-1])
```


#6

```r
mean_rows <- rowMeans(rna_counts)
```

#7

```r
mean_lg_male_hdhorn <- rowMeans(rna_counts[ ,
grep("M*_lg_male_hdhorn", names(rna_counts), value = TRUE)])

mean_sm_male_hdhorn <- rowMeans(rna_counts[ , grep("M*_sm_male_hdhorn", names(rna_counts), value = TRUE)])

difference_hdhorns <- mean_lg_male_hdhorn - mean_sm_male_hdhorn
```

#8

```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0     v purrr   0.2.5
## v tibble  1.4.2     v dplyr   0.7.7
## v tidyr   0.8.2     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
```

```
## -- Conflicts ---------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
ggplot(rna_counts, aes(x = mean_lg_male_hdhorn + mean_sm_male_hdhorn, y = difference_hdhorns)) + geom_point()
```

![](AssignmentR3_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean_lg_male_hdhorn_log2 <- log2(mean_lg_male_hdhorn)
mean_sm_male_hdhorn_log2 <- log2(mean_sm_male_hdhorn)
difference_hdhorns_log2 <- mean_lg_male_hdhorn_log2 - mean_sm_male_hdhorn_log2

ggplot(rna_counts, aes(x = mean_lg_male_hdhorn_log2 + mean_sm_male_hdhorn_log2, y = difference_hdhorns_log2)) + geom_point()
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![](AssignmentR3_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

#Bonus
#(Just one of the things I tried that didn't work)

rna_counts %>% select(colnames(rna_counts[ , ends_with("lg_male_hdhorn")])) %>% summarise(mean_Hdhorn_lgmale = rowMeans(rna_counts["FBpp*", ]))

rna_counts %>% select(colnames(rna_counts[ , ends_with("sm_male_hdhorn")])) %>% summarise(mean_Hdhorn_smmale = rowMeans(rna_counts["FBpp*", ]))

diff <- mean_Hdhorn_lgmale - mean_Hdhorn_smmale

ggplot(rna_counts, aes(x = mean_lgMaleHdhorn+mean_smMaleHdhorn, y = diff)) + geom_point()
