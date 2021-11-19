Draft\_1
================
Rossina Soyan
10/25/2021

-   [First steps](#first-steps)
-   [An attempt to read at least one .txt
    file](#an-attempt-to-read-at-least-one-txt-file)
-   [Complexity measures](#complexity-measures)
-   [Global complexity - Mean sentence
    length](#global-complexity---mean-sentence-length)
-   [Complexity by coordination - T-units per
    sentence](#complexity-by-coordination---t-units-per-sentence)

``` r
##Set knitr options (show both code and output, show output w/o leading #, make figures smaller, hold figures until after chunk)
knitr::opts_chunk$set(echo=TRUE, include=TRUE, comment=NA, fig.height=3, fig.width=4.2, fig.show="hold")
```

## First steps

``` r
library(tidyverse)
```

    -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    v ggplot2 3.3.5     v purrr   0.3.4
    v tibble  3.1.4     v dplyr   1.0.7
    v tidyr   1.1.3     v stringr 1.4.0
    v readr   2.0.1     v forcats 0.5.1

    -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    x dplyr::filter() masks stats::filter()
    x dplyr::lag()    masks stats::lag()

``` r
#install.packages('readtext') to read a corpus
library(readtext)
#install.packages('quanteda') to work with a corpus
library(quanteda)
```

    Package version: 3.1.0
    Unicode version: 13.0
    ICU version: 69.1

    Parallel computing: 4 of 4 threads used.

    See https://quanteda.io for tutorials and examples.

``` r
#install.packages("htmlwidgets") to work with strings and regular expressions
library(htmlwidgets)
```

## An attempt to read at least one .txt file

I am using guidelines for readtext from this
(source)\[<https://github.com/quanteda/readtext>\]

``` r
text1 <- readtext("data/text1.txt", encoding = "UTF-8")
# I was able to load one text
text1$text
```

    [1] "<U+0414><U+043E><U+0440><U+043E><U+0433><U+0430><U+044F> <U+041B><U+0430><U+0440><U+0430>! <U+042F> <U+0445><U+043E><U+0447><U+0443> <U+0442><U+0435><U+0431><U+0435> <U+0440><U+0430><U+0441><U+0441><U+043A><U+0430><U+0437><U+044B><U+0432><U+0430><U+0442><U+044C> <U+043E> <U+043C><U+043E><U+0435><U+0439> <U+043B><U+0443><U+0447><U+0448><U+0435><U+0439> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0435>. <U+0415><U+0451> <U+0437><U+043E><U+0432><U+0443><U+0442> <U+0412><U+0435><U+0440><U+0430>, <U+0438> <U+043E><U+043D><U+0430> <U+043E><U+0447><U+0435><U+043D><U+044C> <U+0434><U+043E><U+0431><U+0440><U+0430><U+044F>. <U+041E><U+043D><U+0430> <U+0441><U+0440><U+0435><U+0434><U+043D><U+0435><U+0433><U+043E> <U+0440><U+043E><U+0441><U+0442><U+0430> <U+0438> <U+0443> <U+043D><U+0435><U+0451> <U+043A><U+043E><U+0440><U+043E><U+0442><U+043A><U+0438><U+0435> <U+043A><U+0430><U+0448><U+0442><U+0430><U+043D><U+043E><U+0432><U+044B><U+0435> <U+0432><U+043E><U+043B><U+043E><U+0441><U+044B>. <U+0412><U+0435><U+0440><U+0430> <U+043B><U+044E><U+0431><U+0438><U+0442> <U+043C><U+0443><U+0437><U+044B><U+043A><U+0443>, <U+0438> <U+0447><U+0430><U+0441><U+0442><U+043E> <U+043C><U+044B> <U+0434><U+0430><U+0451><U+043C> <U+043D><U+043E><U+0432><U+044B><U+0435> <U+043F><U+0435><U+0441><U+043D><U+0438> <U+0434><U+0440><U+0443><U+0433> <U+043A> <U+0434><U+0440><U+0443><U+0433><U+0443>, <U+043D><U+043E> <U+0447><U+0430><U+0449><U+0435> <U+043E><U+043D><U+0430> <U+043C><U+043D><U+0435> <U+0434><U+0430><U+0451><U+0442> <U+043F><U+0435><U+0441><U+043D><U+0438>, <U+043F><U+043E><U+0442><U+043E><U+043C><U+0443> <U+0447><U+0442><U+043E> <U+043E><U+043D><U+0430> <U+0437><U+043D><U+0430><U+0435><U+0442> <U+0431><U+043E><U+043B><U+044C><U+0448><U+0435> <U+043C><U+0435><U+043D><U+044F> <U+043E> <U+043C><U+0443><U+0437><U+044B><U+043A><U+0435>. <U+0423> <U+0412><U+0435><U+0440><U+044B> <U+0435><U+0441><U+0442><U+044C> <U+0441><U+043F><U+043E><U+0440><U+0442><U+0438><U+0432><U+043D><U+0430><U+044F> <U+0444><U+0438><U+0433><U+0443><U+0440><U+0430>, <U+043F><U+043E><U+0442><U+043E><U+043C><U+0443> <U+0447><U+0442><U+043E> <U+043E><U+043D><U+0430> <U+0438><U+0433><U+0440><U+0430><U+0435><U+0442> <U+0432> <U+0445><U+043E><U+043A><U+043A><U+0435><U+0439>. <U+041E><U+043D><U+0430> <U+0441><U+0438><U+043B><U+044C><U+043D><U+0430><U+044F> <U+0438> unclear <U+043B><U+044E><U+0431><U+0438><U+0442> <U+0431><U+0435><U+0433><U+0430><U+0442><U+044C>. <U+041C><U+044B> <U+043F><U+043E><U+0437><U+043D><U+0430><U+043A><U+043E><U+043C><U+0438><U+043B><U+0438><U+0441><U+044C> <U+0432> <U+0448><U+043A><U+043E><U+043B><U+0435> <U+043F><U+044F><U+0442><U+044C> <U+043B><U+0435><U+0442> <U+043D><U+0430><U+0437><U+0430><U+0434>, <U+043A><U+043E><U+0433><U+0434><U+0430> <U+043D><U+0430><U+043C> <U+0447><U+0435><U+0442><U+044B><U+0440><U+043D><U+0430><U+0434><U+0446><U+0430><U+0442><U+044C> <U+043B><U+0435><U+0442> <U+0431><U+044B><U+043B><U+0438>. <U+041C><U+044B> <U+0431><U+044B><U+043B><U+0438> <U+0432> <U+0431><U+0438><U+0431><U+043B><U+0438><U+043E><U+0442><U+0435><U+043A><U+0435>, <U+0438> <U+044F> <U+0447><U+0438><U+0442><U+0430><U+043B><U+0430> <U+0440><U+043E><U+043C><U+0430><U+043D> <U+0414><U+043E><U+0441><U+0442><U+043E><U+0435><U+0432><U+0441><U+043A><U+043E><U+0433><U+043E>. <U+041E><U+043D><U+0430> <U+0432><U+0438><U+0434><U+0435><U+043B> <U+0440><U+043E><U+043C><U+0430><U+043D> <U+0438> <U+0441><U+043A><U+0430><U+0437><U+0430><U+043B><U+0430>, <U+0447><U+0442><U+043E> <U+0435><U+0439> <U+043D><U+0440><U+0430><U+0432><U+0438><U+0442><U+0441><U+044F> <U+0440><U+0443><U+0441><U+0441><U+043A><U+0430><U+044F> <U+043B><U+0438><U+0442><U+0435><U+0440><U+0430><U+0442><U+0443><U+0440><U+0430>. <U+041C><U+044B> <U+0433><U+043E><U+0432><U+043E><U+0440><U+0438><U+043B><U+0438> <U+0434><U+0432><U+0430> <U+0447><U+0430><U+0441><U+0430>, <U+0434><U+043E> <U+0442><U+043E><U+0433><U+043E> <U+043A><U+0430><U+043A>, <U+0431><U+0438><U+0431><U+043B><U+0438><U+043E><U+0442><U+0435><U+043A><U+0430> <U+0437><U+0430><U+043A><U+0440><U+044B><U+043B><U+0430><U+0441><U+044C>. <U+041C><U+044B> <U+0441><U+0442><U+0430><U+043B><U+0438> <U+043B><U+0443><U+0447><U+0448><U+0438><U+043C><U+0438> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0430><U+043C><U+0438>, <U+0438> <U+0432><U+0441><U+0435><U+0433><U+0434><U+0430> <U+043C><U+044B> <U+0447><U+0438><U+0442><U+0430><U+0435><U+043C> <U+0440><U+043E><U+043C><U+0430><U+043D><U+044B> <U+0432><U+043C><U+0435><U+0441><U+0442><U+0435>. <U+041A><U+0430><U+043A> <U+0437><U+043E><U+0432><U+0443><U+0442> <U+0442><U+0432><U+043E><U+044E> <U+043B><U+0443><U+0447><U+0448><U+0443><U+044E> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0443>? <U+0412><U+044B> <U+0441> <U+043D><U+0435><U+0439> <U+0443><U+0447><U+0438><U+0442><U+0435><U+0441><U+044C> <U+0432> <U+0443><U+043D><U+0438><U+0432><U+0435><U+0440><U+0441><U+0438><U+0442><U+0435><U+0442><U+0435>? <U+0413><U+0434><U+0435> <U+0432><U+044B> <U+043F><U+043E><U+0437><U+043D><U+0430><U+043A><U+043E><U+043C><U+0438><U+043B><U+0438><U+0441><U+044C> <U+0434><U+0440><U+0443><U+0433> <U+0441> <U+0434><U+0440><U+0443><U+0433><U+043E><U+043C>? unclear <U+041A><U+0430><U+043A> <U+043E><U+043D><U+0430> <U+0432><U+044B><U+0433><U+043B><U+044F><U+0434><U+0438><U+0442>? <U+041E><U+043D><U+0430> <U+0437><U+0430><U+043D><U+0438><U+043C><U+0430><U+0435><U+0442><U+0441><U+044F> <U+043A><U+0430><U+043A><U+0438><U+043C>-<U+0442><U+043E> <U+0441><U+043F><U+043E><U+0440><U+0442><U+043E><U+043C>?"

``` r
# I want to exclude the words "unclear" from the text
str_detect(text1$text, "unclear")
```

    [1] TRUE

``` r
text1modified <- char_trim(text1$text, "sentences", exclude_pattern = "unclear")
text1modified #Unfortunately, this command excludes whole sentences containing the word unclear. And I want to only exclude the words "unclear."
```

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   text1 
    "<U+0414><U+043E><U+0440><U+043E><U+0433><U+0430><U+044F> <U+041B><U+0430><U+0440><U+0430>!  <U+042F> <U+0445><U+043E><U+0447><U+0443> <U+0442><U+0435><U+0431><U+0435> <U+0440><U+0430><U+0441><U+0441><U+043A><U+0430><U+0437><U+044B><U+0432><U+0430><U+0442><U+044C> <U+043E> <U+043C><U+043E><U+0435><U+0439> <U+043B><U+0443><U+0447><U+0448><U+0435><U+0439> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0435>.  <U+0415><U+0451> <U+0437><U+043E><U+0432><U+0443><U+0442> <U+0412><U+0435><U+0440><U+0430>, <U+0438> <U+043E><U+043D><U+0430> <U+043E><U+0447><U+0435><U+043D><U+044C> <U+0434><U+043E><U+0431><U+0440><U+0430><U+044F>.  <U+041E><U+043D><U+0430> <U+0441><U+0440><U+0435><U+0434><U+043D><U+0435><U+0433><U+043E> <U+0440><U+043E><U+0441><U+0442><U+0430> <U+0438> <U+0443> <U+043D><U+0435><U+0451> <U+043A><U+043E><U+0440><U+043E><U+0442><U+043A><U+0438><U+0435> <U+043A><U+0430><U+0448><U+0442><U+0430><U+043D><U+043E><U+0432><U+044B><U+0435> <U+0432><U+043E><U+043B><U+043E><U+0441><U+044B>.  <U+0412><U+0435><U+0440><U+0430> <U+043B><U+044E><U+0431><U+0438><U+0442> <U+043C><U+0443><U+0437><U+044B><U+043A><U+0443>, <U+0438> <U+0447><U+0430><U+0441><U+0442><U+043E> <U+043C><U+044B> <U+0434><U+0430><U+0451><U+043C> <U+043D><U+043E><U+0432><U+044B><U+0435> <U+043F><U+0435><U+0441><U+043D><U+0438> <U+0434><U+0440><U+0443><U+0433> <U+043A> <U+0434><U+0440><U+0443><U+0433><U+0443>, <U+043D><U+043E> <U+0447><U+0430><U+0449><U+0435> <U+043E><U+043D><U+0430> <U+043C><U+043D><U+0435> <U+0434><U+0430><U+0451><U+0442> <U+043F><U+0435><U+0441><U+043D><U+0438>, <U+043F><U+043E><U+0442><U+043E><U+043C><U+0443> <U+0447><U+0442><U+043E> <U+043E><U+043D><U+0430> <U+0437><U+043D><U+0430><U+0435><U+0442> <U+0431><U+043E><U+043B><U+044C><U+0448><U+0435> <U+043C><U+0435><U+043D><U+044F> <U+043E> <U+043C><U+0443><U+0437><U+044B><U+043A><U+0435>.  <U+0423> <U+0412><U+0435><U+0440><U+044B> <U+0435><U+0441><U+0442><U+044C> <U+0441><U+043F><U+043E><U+0440><U+0442><U+0438><U+0432><U+043D><U+0430><U+044F> <U+0444><U+0438><U+0433><U+0443><U+0440><U+0430>, <U+043F><U+043E><U+0442><U+043E><U+043C><U+0443> <U+0447><U+0442><U+043E> <U+043E><U+043D><U+0430> <U+0438><U+0433><U+0440><U+0430><U+0435><U+0442> <U+0432> <U+0445><U+043E><U+043A><U+043A><U+0435><U+0439>.  <U+041C><U+044B> <U+043F><U+043E><U+0437><U+043D><U+0430><U+043A><U+043E><U+043C><U+0438><U+043B><U+0438><U+0441><U+044C> <U+0432> <U+0448><U+043A><U+043E><U+043B><U+0435> <U+043F><U+044F><U+0442><U+044C> <U+043B><U+0435><U+0442> <U+043D><U+0430><U+0437><U+0430><U+0434>, <U+043A><U+043E><U+0433><U+0434><U+0430> <U+043D><U+0430><U+043C> <U+0447><U+0435><U+0442><U+044B><U+0440><U+043D><U+0430><U+0434><U+0446><U+0430><U+0442><U+044C> <U+043B><U+0435><U+0442> <U+0431><U+044B><U+043B><U+0438>.  <U+041C><U+044B> <U+0431><U+044B><U+043B><U+0438> <U+0432> <U+0431><U+0438><U+0431><U+043B><U+0438><U+043E><U+0442><U+0435><U+043A><U+0435>, <U+0438> <U+044F> <U+0447><U+0438><U+0442><U+0430><U+043B><U+0430> <U+0440><U+043E><U+043C><U+0430><U+043D> <U+0414><U+043E><U+0441><U+0442><U+043E><U+0435><U+0432><U+0441><U+043A><U+043E><U+0433><U+043E>.  <U+041E><U+043D><U+0430> <U+0432><U+0438><U+0434><U+0435><U+043B> <U+0440><U+043E><U+043C><U+0430><U+043D> <U+0438> <U+0441><U+043A><U+0430><U+0437><U+0430><U+043B><U+0430>, <U+0447><U+0442><U+043E> <U+0435><U+0439> <U+043D><U+0440><U+0430><U+0432><U+0438><U+0442><U+0441><U+044F> <U+0440><U+0443><U+0441><U+0441><U+043A><U+0430><U+044F> <U+043B><U+0438><U+0442><U+0435><U+0440><U+0430><U+0442><U+0443><U+0440><U+0430>.  <U+041C><U+044B> <U+0433><U+043E><U+0432><U+043E><U+0440><U+0438><U+043B><U+0438> <U+0434><U+0432><U+0430> <U+0447><U+0430><U+0441><U+0430>, <U+0434><U+043E> <U+0442><U+043E><U+0433><U+043E> <U+043A><U+0430><U+043A>, <U+0431><U+0438><U+0431><U+043B><U+0438><U+043E><U+0442><U+0435><U+043A><U+0430> <U+0437><U+0430><U+043A><U+0440><U+044B><U+043B><U+0430><U+0441><U+044C>.  <U+041C><U+044B> <U+0441><U+0442><U+0430><U+043B><U+0438> <U+043B><U+0443><U+0447><U+0448><U+0438><U+043C><U+0438> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0430><U+043C><U+0438>, <U+0438> <U+0432><U+0441><U+0435><U+0433><U+0434><U+0430> <U+043C><U+044B> <U+0447><U+0438><U+0442><U+0430><U+0435><U+043C> <U+0440><U+043E><U+043C><U+0430><U+043D><U+044B> <U+0432><U+043C><U+0435><U+0441><U+0442><U+0435>.  <U+041A><U+0430><U+043A> <U+0437><U+043E><U+0432><U+0443><U+0442> <U+0442><U+0432><U+043E><U+044E> <U+043B><U+0443><U+0447><U+0448><U+0443><U+044E> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0443>?  <U+0412><U+044B> <U+0441> <U+043D><U+0435><U+0439> <U+0443><U+0447><U+0438><U+0442><U+0435><U+0441><U+044C> <U+0432> <U+0443><U+043D><U+0438><U+0432><U+0435><U+0440><U+0441><U+0438><U+0442><U+0435><U+0442><U+0435>?  <U+0413><U+0434><U+0435> <U+0432><U+044B> <U+043F><U+043E><U+0437><U+043D><U+0430><U+043A><U+043E><U+043C><U+0438><U+043B><U+0438><U+0441><U+044C> <U+0434><U+0440><U+0443><U+0433> <U+0441> <U+0434><U+0440><U+0443><U+0433><U+043E><U+043C>?  <U+041E><U+043D><U+0430> <U+0437><U+0430><U+043D><U+0438><U+043C><U+0430><U+0435><U+0442><U+0441><U+044F> <U+043A><U+0430><U+043A><U+0438><U+043C>-<U+0442><U+043E> <U+0441><U+043F><U+043E><U+0440><U+0442><U+043E><U+043C>?" 

``` r
# Let's try writing a function as in this link (https://stackoverflow.com/questions/35790652/removing-words-featured-in-character-vector-from-string)
stopwords = "unclear"
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}
text1mod2 <- removeWords(text1$text, stopwords)
text1mod2 #success! I was able to remove the words "unclear" from the text in Russian.
```

    [1] "<U+0414><U+043E><U+0440><U+043E><U+0433><U+0430><U+044F> <U+041B><U+0430><U+0440><U+0430>! <U+042F> <U+0445><U+043E><U+0447><U+0443> <U+0442><U+0435><U+0431><U+0435> <U+0440><U+0430><U+0441><U+0441><U+043A><U+0430><U+0437><U+044B><U+0432><U+0430><U+0442><U+044C> <U+043E> <U+043C><U+043E><U+0435><U+0439> <U+043B><U+0443><U+0447><U+0448><U+0435><U+0439> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0435>. <U+0415><U+0451> <U+0437><U+043E><U+0432><U+0443><U+0442> <U+0412><U+0435><U+0440><U+0430>, <U+0438> <U+043E><U+043D><U+0430> <U+043E><U+0447><U+0435><U+043D><U+044C> <U+0434><U+043E><U+0431><U+0440><U+0430><U+044F>. <U+041E><U+043D><U+0430> <U+0441><U+0440><U+0435><U+0434><U+043D><U+0435><U+0433><U+043E> <U+0440><U+043E><U+0441><U+0442><U+0430> <U+0438> <U+0443> <U+043D><U+0435><U+0451> <U+043A><U+043E><U+0440><U+043E><U+0442><U+043A><U+0438><U+0435> <U+043A><U+0430><U+0448><U+0442><U+0430><U+043D><U+043E><U+0432><U+044B><U+0435> <U+0432><U+043E><U+043B><U+043E><U+0441><U+044B>. <U+0412><U+0435><U+0440><U+0430> <U+043B><U+044E><U+0431><U+0438><U+0442> <U+043C><U+0443><U+0437><U+044B><U+043A><U+0443>, <U+0438> <U+0447><U+0430><U+0441><U+0442><U+043E> <U+043C><U+044B> <U+0434><U+0430><U+0451><U+043C> <U+043D><U+043E><U+0432><U+044B><U+0435> <U+043F><U+0435><U+0441><U+043D><U+0438> <U+0434><U+0440><U+0443><U+0433> <U+043A> <U+0434><U+0440><U+0443><U+0433><U+0443>, <U+043D><U+043E> <U+0447><U+0430><U+0449><U+0435> <U+043E><U+043D><U+0430> <U+043C><U+043D><U+0435> <U+0434><U+0430><U+0451><U+0442> <U+043F><U+0435><U+0441><U+043D><U+0438>, <U+043F><U+043E><U+0442><U+043E><U+043C><U+0443> <U+0447><U+0442><U+043E> <U+043E><U+043D><U+0430> <U+0437><U+043D><U+0430><U+0435><U+0442> <U+0431><U+043E><U+043B><U+044C><U+0448><U+0435> <U+043C><U+0435><U+043D><U+044F> <U+043E> <U+043C><U+0443><U+0437><U+044B><U+043A><U+0435>. <U+0423> <U+0412><U+0435><U+0440><U+044B> <U+0435><U+0441><U+0442><U+044C> <U+0441><U+043F><U+043E><U+0440><U+0442><U+0438><U+0432><U+043D><U+0430><U+044F> <U+0444><U+0438><U+0433><U+0443><U+0440><U+0430>, <U+043F><U+043E><U+0442><U+043E><U+043C><U+0443> <U+0447><U+0442><U+043E> <U+043E><U+043D><U+0430> <U+0438><U+0433><U+0440><U+0430><U+0435><U+0442> <U+0432> <U+0445><U+043E><U+043A><U+043A><U+0435><U+0439>. <U+041E><U+043D><U+0430> <U+0441><U+0438><U+043B><U+044C><U+043D><U+0430><U+044F> <U+0438> <U+043B><U+044E><U+0431><U+0438><U+0442> <U+0431><U+0435><U+0433><U+0430><U+0442><U+044C>. <U+041C><U+044B> <U+043F><U+043E><U+0437><U+043D><U+0430><U+043A><U+043E><U+043C><U+0438><U+043B><U+0438><U+0441><U+044C> <U+0432> <U+0448><U+043A><U+043E><U+043B><U+0435> <U+043F><U+044F><U+0442><U+044C> <U+043B><U+0435><U+0442> <U+043D><U+0430><U+0437><U+0430><U+0434>, <U+043A><U+043E><U+0433><U+0434><U+0430> <U+043D><U+0430><U+043C> <U+0447><U+0435><U+0442><U+044B><U+0440><U+043D><U+0430><U+0434><U+0446><U+0430><U+0442><U+044C> <U+043B><U+0435><U+0442> <U+0431><U+044B><U+043B><U+0438>. <U+041C><U+044B> <U+0431><U+044B><U+043B><U+0438> <U+0432> <U+0431><U+0438><U+0431><U+043B><U+0438><U+043E><U+0442><U+0435><U+043A><U+0435>, <U+0438> <U+044F> <U+0447><U+0438><U+0442><U+0430><U+043B><U+0430> <U+0440><U+043E><U+043C><U+0430><U+043D> <U+0414><U+043E><U+0441><U+0442><U+043E><U+0435><U+0432><U+0441><U+043A><U+043E><U+0433><U+043E>. <U+041E><U+043D><U+0430> <U+0432><U+0438><U+0434><U+0435><U+043B> <U+0440><U+043E><U+043C><U+0430><U+043D> <U+0438> <U+0441><U+043A><U+0430><U+0437><U+0430><U+043B><U+0430>, <U+0447><U+0442><U+043E> <U+0435><U+0439> <U+043D><U+0440><U+0430><U+0432><U+0438><U+0442><U+0441><U+044F> <U+0440><U+0443><U+0441><U+0441><U+043A><U+0430><U+044F> <U+043B><U+0438><U+0442><U+0435><U+0440><U+0430><U+0442><U+0443><U+0440><U+0430>. <U+041C><U+044B> <U+0433><U+043E><U+0432><U+043E><U+0440><U+0438><U+043B><U+0438> <U+0434><U+0432><U+0430> <U+0447><U+0430><U+0441><U+0430>, <U+0434><U+043E> <U+0442><U+043E><U+0433><U+043E> <U+043A><U+0430><U+043A>, <U+0431><U+0438><U+0431><U+043B><U+0438><U+043E><U+0442><U+0435><U+043A><U+0430> <U+0437><U+0430><U+043A><U+0440><U+044B><U+043B><U+0430><U+0441><U+044C>. <U+041C><U+044B> <U+0441><U+0442><U+0430><U+043B><U+0438> <U+043B><U+0443><U+0447><U+0448><U+0438><U+043C><U+0438> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0430><U+043C><U+0438>, <U+0438> <U+0432><U+0441><U+0435><U+0433><U+0434><U+0430> <U+043C><U+044B> <U+0447><U+0438><U+0442><U+0430><U+0435><U+043C> <U+0440><U+043E><U+043C><U+0430><U+043D><U+044B> <U+0432><U+043C><U+0435><U+0441><U+0442><U+0435>. <U+041A><U+0430><U+043A> <U+0437><U+043E><U+0432><U+0443><U+0442> <U+0442><U+0432><U+043E><U+044E> <U+043B><U+0443><U+0447><U+0448><U+0443><U+044E> <U+043F><U+043E><U+0434><U+0440><U+0443><U+0433><U+0443>? <U+0412><U+044B> <U+0441> <U+043D><U+0435><U+0439> <U+0443><U+0447><U+0438><U+0442><U+0435><U+0441><U+044C> <U+0432> <U+0443><U+043D><U+0438><U+0432><U+0435><U+0440><U+0441><U+0438><U+0442><U+0435><U+0442><U+0435>? <U+0413><U+0434><U+0435> <U+0432><U+044B> <U+043F><U+043E><U+0437><U+043D><U+0430><U+043A><U+043E><U+043C><U+0438><U+043B><U+0438><U+0441><U+044C> <U+0434><U+0440><U+0443><U+0433> <U+0441> <U+0434><U+0440><U+0443><U+0433><U+043E><U+043C>? <U+041A><U+0430><U+043A> <U+043E><U+043D><U+0430> <U+0432><U+044B><U+0433><U+043B><U+044F><U+0434><U+0438><U+0442>? <U+041E><U+043D><U+0430> <U+0437><U+0430><U+043D><U+0438><U+043C><U+0430><U+0435><U+0442><U+0441><U+044F> <U+043A><U+0430><U+043A><U+0438><U+043C>-<U+0442><U+043E> <U+0441><U+043F><U+043E><U+0440><U+0442><U+043E><U+043C>?"

## Complexity measures

Let me try to measure complexity measures in one text. By the way, since
I am only working with one text right now, I can actually check the
numbers manually.

## Global complexity - Mean sentence length

``` r
str_length(text1$text) # This is the number of characters in this text with spaces
```

    [1] 850

``` r
str_length(text1modified)
```

    [1] 802

``` r
str_length(text1mod2) #Yes! It worked
```

    [1] 834

``` r
#How do I count the number of sentences?
nsentence(text1$text)
```

    Warning in nsentence.character(text1$text): nsentence() does not correctly count
    sentences in all lower-cased text

    text1 
       17 

``` r
nsentence(text1mod2) #the answers for both texts is 17 but I am not sure what to do with the following warning - Warning in nsentence.character(text1mod2) : nsentence() does not correctly count sentences in all lower-cased text. Can I just ignore it? Another issue is the address in the beginning of the text Дорогая Лара! This is not a real sentence. How do I not count it as a sentence?
```

    Warning in nsentence.character(text1mod2): nsentence() does not correctly count
    sentences in all lower-cased text

    text1 
       17 

``` r
#How do I count the number of words in a sentence? 
ntoken(text1$text, remove_punct = TRUE)
```

    text1 
      144 

``` r
ntoken(text1mod2, remove_punct = TRUE) # Now it works!
```

    text1 
      142 

``` r
(MSLtext1 <- ntoken(text1mod2, remove_punct = TRUE)/ nsentence(text1mod2)) # I have an answer except I do not know how to not count the address in the beginning of the text as not a sentence.
```

    Warning in nsentence.character(text1mod2): nsentence() does not correctly count
    sentences in all lower-cased text

       text1 
    8.352941 

## Complexity by coordination - T-units per sentence

I am not sure how to count the number of T-units. I think I need to
write my own function. If the definition of a sentence is anything which
starts with an upper-case letter and ends with a period or ! or ?, then,
the definition of a T-unit is a little bit more complex. Based on mt
example test, T-unit may start with an upper- or lower-case and it can
end before и if there is a subject and a verb after it or end with a
period, ?, !.

``` r
#nTunit <- function(str, )
```