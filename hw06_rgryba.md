Stat547 Homework 6
================
Rowenna Gryba
2018-11-05

``` r
suppressPackageStartupMessages(library(tidyverse))
library(repurrrsive)
library(gapminder)
```

Question 6 - Write a function for gapminder data
------------------------------------------------

This function runs a linear model to model the relationship between two variables using a grouping variable. The output is a dataframe with the variable and associated lm output, for those models that had significant p-values. The example below uses the gapminder data and compares population to year for each country.

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
lm_function <- function(df, y, x, group_var) {
  group_var <-  enquo(group_var)
  df %>%
    group_by_(group_var) %>%
    nest() %>%
    mutate(fit = map(data, ~ lm(.[[y]] ~ .[[x]], data=.))) %>%
    mutate(output = map(fit, broom::tidy)) %>%
    unnest(output) %>%
    filter(any(p.value < 0.05)) %>%
    mutate(term = replace(term, term == ".[[x]]", "year"))
}

lm_function(df=gapminder,y="pop", x="year", group_var=country)
```

    ## # A tibble: 284 x 6
    ##    country     term           estimate  std.error statistic  p.value
    ##    <fct>       <chr>             <dbl>      <dbl>     <dbl>    <dbl>
    ##  1 Afghanistan (Intercept) -690631821. 105454007.     -6.55 6.48e- 5
    ##  2 Afghanistan year            356886.     53271.      6.70 5.37e- 5
    ##  3 Albania     (Intercept)  -87538213.   3952656.    -22.1  7.90e-10
    ##  4 Albania     year             45526.      1997.     22.8  5.94e-10
    ##  5 Algeria     (Intercept) -916286165.  42528206.    -21.5  1.04e- 9
    ##  6 Algeria     year            472928.     21484.     22.0  8.39e-10
    ##  7 Angola      (Intercept) -278392508.  21148305.    -13.2  1.22e- 7
    ##  8 Angola      year            144330.     10683.     13.5  9.51e- 8
    ##  9 Argentina   (Intercept) -798638010.  15222684.    -52.5  1.53e-13
    ## 10 Argentina   year            417904.      7690.     54.3  1.08e-13
    ## # ... with 274 more rows
