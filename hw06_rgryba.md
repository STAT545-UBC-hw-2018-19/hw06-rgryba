---
title: "Stat547 Homework 6"
author: "Rowenna Gryba"
date: "2018-11-05"
output:
  html_document:
    keep_md: true
theme: cerulean
number_sections: true
editor_options: 
  chunk_output_type: inline
---
  

```r
suppressPackageStartupMessages(library(tidyverse))
library(repurrrsive)
library(gapminder)
```

##Question 6 - Write a function for gapminder data
This function runs a linear model to model the relationship between two variables using a grouping variable. The output is a dataframe with the variable and associated lm output, for those models that had significant p-values.
The example below uses the gapminder data and compares population to year for each country.


```r
str(gapminder)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

```r
lm_function <- function(df, y, x, group_var) {
  group_var <-  enquo(group_var)
  df %>%
    group_by_(group_var) %>%
    nest() %>%
    mutate(fit = map(data, ~ lm(df[[y]] ~ df[[x]], data=.))) %>%
    mutate(output = map(fit, broom::tidy)) %>%
    unnest(output) %>%
    filter(any(p.value < 0.05)) %>%
    mutate(term = replace(term, term == "df[[x]]", "year"))
}

lm_function(df=gapminder,y="pop", x="year", group_var=country)
```

```
## # A tibble: 284 x 6
##    country     term           estimate  std.error statistic  p.value
##    <fct>       <chr>             <dbl>      <dbl>     <dbl>    <dbl>
##  1 Afghanistan (Intercept) -972185807. 294031308.     -3.31 0.000965
##  2 Afghanistan year            506081.    148533.      3.41 0.000672
##  3 Albania     (Intercept) -972185807. 294031308.     -3.31 0.000965
##  4 Albania     year            506081.    148533.      3.41 0.000672
##  5 Algeria     (Intercept) -972185807. 294031308.     -3.31 0.000965
##  6 Algeria     year            506081.    148533.      3.41 0.000672
##  7 Angola      (Intercept) -972185807. 294031308.     -3.31 0.000965
##  8 Angola      year            506081.    148533.      3.41 0.000672
##  9 Argentina   (Intercept) -972185807. 294031308.     -3.31 0.000965
## 10 Argentina   year            506081.    148533.      3.41 0.000672
## # ... with 274 more rows
```
