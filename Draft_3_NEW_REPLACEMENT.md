Draft\_3\_NEW\_REPLACEMENT
================
Rossina Soyan
12/2/2021

-   [What I need to load to run the
    codes?](#what-i-need-to-load-to-run-the-codes)
-   [Upload the corpus, create a dataframe with texts and
    names](#upload-the-corpus-create-a-dataframe-with-texts-and-names)
-   [Lexical complexity measures](#lexical-complexity-measures)
    -   [Lexical density](#lexical-density)
    -   [Lexial variation](#lexial-variation)
    -   [Lexical sophistication](#lexical-sophistication)
-   [A dataframe with all the
    findings](#a-dataframe-with-all-the-findings)
-   [An attempt to do the cluster analysis (no progress
    yet)](#an-attempt-to-do-the-cluster-analysis-no-progress-yet)
-   [Final notes](#final-notes)
    -   [Session info](#session-info)

``` r
##Set knitr options (show both code and output, show output w/o leading #, make figures smaller, hold figures until after chunk)
knitr::opts_chunk$set(echo=TRUE, include=TRUE, comment=NA, fig.height=3, fig.width=4.2, fig.show="hold")
Sys.setlocale("LC_CTYPE", "Russian") #to make sure my text is not gibberish, readable
```

    ## [1] "Russian_Russia.1251"

## What I need to load to run the codes?

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
# install.packages('tidytext') to be able to perform tokenization
library(tidytext)
```

## Upload the corpus, create a dataframe with texts and names

I have decided to upload texts written by only 8 students because I am
running out of time. I have chosen 4 students rated as Intermediate and
4 students rated as Advanced. Each student has submitted 3 texts as part
of their placement examination, it means that the current corpus
consists of 24 texts.

``` r
#How to upload the texts
#From https://www.youtube.com/watch?v=pFinlXYLZ-A
folder <- "C:/Users/Rossina/Documents/CMU_student/3_Fall_2021/Statistics_at_Pitt/data"
filelist_orig <- list.files(path = folder, pattern = ".*.txt")
filelist <- paste(folder, "/", filelist_orig, sep = "")
filelistTexts <- lapply(filelist, FUN = readLines, encoding = "UTF-8", warn = FALSE)
texts <- lapply(filelistTexts, FUN = paste, collapse = " ") %>% 
  str_remove_all("unclear") #I deleted the word "unclear" from the texts
```

Now that I have all the texts uploaded, I want to create a dataframe
with texts and names

``` r
class(texts) #Interesting! It used to show that this is a list, but now the answer is "character"
```

    [1] "character"

``` r
corpus_df <- tibble(text = texts, Student = filelist_orig)
corpus_df #Success
```

    # A tibble: 24 x 2
       text                                                         Student         
       <chr>                                                        <chr>           
     1 "Ìîÿ äîðîãàÿ! Â òâî¸ì ïîñëåäíåì ïèñüìå, òû ñïðîñèëà ìíå î ì~ Student1_Entry1~
     2 "Ðåáÿòà! Â ìî¸ì ãîðîäå ïîÿâèëàñü íîâàÿ ïðîáëåìà - íàøà âîäà~ Student1_Entry2~
     3 "Îäíàæäû, ñèäÿ ñ îòöîì íà ìåçîíèíå, îí íà÷àë êðèòèêîâàòü ìî~ Student1_Entry3~
     4 "Äîðîãîé Æåíÿ, Êàê òû? Õîòåëà ðàçãîâàðèâàòü ñ òîáîé î íàøåé~ Student2_Entry1~
     5 "Ïðèâåò âñåì. Íàäåþñü, ÷òî âñ¸ õîðîøî ñ âàìè. Ðàíüøå, ñåãîä~ Student2_Entry2~
     6 "Â íàøåì âðåìÿ, ó íàñ åñòü ìíîãèå âàðèàíòû, õî÷åòñÿ ëè íàì ~ Student2_Entry3~
     7 "Äîðîãàÿ Èðèíà! Ñïàñèáî çà âàøå ïèñüìî. Êîíå÷íî, ìîé ëó÷øèé~ Student3_Entry1~
     8 "Ó íàñ â ãîðîäå Í.  åñòü îãðîìíàÿ ïðîáëåìà - â ñëîâîì, ó íà~ Student3_Entry2~
     9 "Ãîâîðÿò, ÷òî  äåòè  íàøåãî âðåìåíè íå óìåþò ðàçãîâàðèâàòü ~ Student3_Entry3~
    10 "Ñåãîäíÿ ÿ õî÷ó ñêàçàòü î ñâîåé ñàìîé  áëèçêîé ïîäðóãîé. Å¸~ Student4_Entry1~
    # ... with 14 more rows

I need to separate student ids and student texts

``` r
corpus_df2 <- corpus_df %>% 
  separate(Student, c("Student", "Entry"))
```

    Warning: Expected 2 pieces. Additional pieces discarded in 24 rows [1, 2, 3, 4,
    5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
corpus_df2
```

    # A tibble: 24 x 3
       text                                                           Student  Entry
       <chr>                                                          <chr>    <chr>
     1 "Ìîÿ äîðîãàÿ! Â òâî¸ì ïîñëåäíåì ïèñüìå, òû ñïðîñèëà ìíå î ìî¸~ Student1 Entr~
     2 "Ðåáÿòà! Â ìî¸ì ãîðîäå ïîÿâèëàñü íîâàÿ ïðîáëåìà - íàøà âîäà ñ~ Student1 Entr~
     3 "Îäíàæäû, ñèäÿ ñ îòöîì íà ìåçîíèíå, îí íà÷àë êðèòèêîâàòü ìîåã~ Student1 Entr~
     4 "Äîðîãîé Æåíÿ, Êàê òû? Õîòåëà ðàçãîâàðèâàòü ñ òîáîé î íàøåé ï~ Student2 Entr~
     5 "Ïðèâåò âñåì. Íàäåþñü, ÷òî âñ¸ õîðîøî ñ âàìè. Ðàíüøå, ñåãîäíÿ~ Student2 Entr~
     6 "Â íàøåì âðåìÿ, ó íàñ åñòü ìíîãèå âàðèàíòû, õî÷åòñÿ ëè íàì ãî~ Student2 Entr~
     7 "Äîðîãàÿ Èðèíà! Ñïàñèáî çà âàøå ïèñüìî. Êîíå÷íî, ìîé ëó÷øèé ä~ Student3 Entr~
     8 "Ó íàñ â ãîðîäå Í.  åñòü îãðîìíàÿ ïðîáëåìà - â ñëîâîì, ó íàñ ~ Student3 Entr~
     9 "Ãîâîðÿò, ÷òî  äåòè  íàøåãî âðåìåíè íå óìåþò ðàçãîâàðèâàòü äð~ Student3 Entr~
    10 "Ñåãîäíÿ ÿ õî÷ó ñêàçàòü î ñâîåé ñàìîé  áëèçêîé ïîäðóãîé. Å¸ ç~ Student4 Entr~
    # ... with 14 more rows

I need to add their proficiency ratings

``` r
#I am using something I saw in Winter's textbook + https://rstudio-pubs-static.s3.amazonaws.com/116317_e6922e81e72e4e3f83995485ce686c14.html#/9
corpus_df3 <- mutate(corpus_df2, Proficiency = ifelse(grepl("[1-4]", Student), "Intermediate", "Advanced"))
head(corpus_df3) #Success!
```

    # A tibble: 6 x 4
      text                                                Student  Entry Proficiency
      <chr>                                               <chr>    <chr> <chr>      
    1 "Ìîÿ äîðîãàÿ! Â òâî¸ì ïîñëåäíåì ïèñüìå, òû ñïðîñèë~ Student1 Entr~ Intermedia~
    2 "Ðåáÿòà! Â ìî¸ì ãîðîäå ïîÿâèëàñü íîâàÿ ïðîáëåìà - ~ Student1 Entr~ Intermedia~
    3 "Îäíàæäû, ñèäÿ ñ îòöîì íà ìåçîíèíå, îí íà÷àë êðèòè~ Student1 Entr~ Intermedia~
    4 "Äîðîãîé Æåíÿ, Êàê òû? Õîòåëà ðàçãîâàðèâàòü ñ òîáî~ Student2 Entr~ Intermedia~
    5 "Ïðèâåò âñåì. Íàäåþñü, ÷òî âñ¸ õîðîøî ñ âàìè. Ðàíü~ Student2 Entr~ Intermedia~
    6 "Â íàøåì âðåìÿ, ó íàñ åñòü ìíîãèå âàðèàíòû, õî÷åòñ~ Student2 Entr~ Intermedia~

I need to tokenize my texts

``` r
corpus_df_tidy <- corpus_df3 %>% 
  mutate(text = gsub(x = text, pattern = "\\-\\s", replacement = "")) %>% #to make sure there are no lonely dashes as token
  unnest_tokens(word, text, token = "regex", pattern = "[\\s,\\.\\?!\\(\\)\\:\";]") #let me check if words look like real words in all these 8 texts
corpus_df_tidy %>% 
  filter(str_detect(word, "-")) #Success! 60 words have been identified as 1 word, not two. That's great!
```

    # A tibble: 60 x 4
       Student  Entry  Proficiency  word     
       <chr>    <chr>  <chr>        <chr>    
     1 Student1 Entry1 Intermediate èç-çà    
     2 Student1 Entry2 Intermediate âî-ïåðâûõ
     3 Student1 Entry2 Intermediate ÷òî-òî   
     4 Student1 Entry2 Intermediate èç-çà    
     5 Student1 Entry2 Intermediate ý-ìàéë   
     6 Student1 Entry2 Intermediate âî-ïåðâûõ
     7 Student1 Entry2 Intermediate âî-âòîðûõ
     8 Student1 Entry2 Intermediate â-òðåòüèõ
     9 Student1 Entry3 Intermediate ïî-ìîåìó 
    10 Student1 Entry3 Intermediate èç-çà    
    # ... with 50 more rows

## Lexical complexity measures

### Lexical density

These files are the lists of non-lexical words that I have created

``` r
RusConjCoord2 <- readLines("additional_documents/Russian_conjunctions_COORD.txt", encoding = "UTF-8", warn = FALSE) %>% 
  str_remove_all("<.+>") 
RusPrep2 <- readLines("additional_documents/Russian_prepositions.txt", encoding = "UTF-8", warn = FALSE) %>% 
  str_remove_all("<.+>") 
RusConjSubord2 <- readLines("additional_documents/Russian_conjunctions_SUBORD.txt", encoding = "UTF-8", warn = FALSE) %>% 
  str_remove_all("<.+>")
RusInter2 <- readLines("additional_documents/Russian_interjections.txt", encoding = "UTF-8", warn = FALSE) %>%
  str_remove_all("<.+>")
RusPrtcl2 <- readLines("additional_documents/Russian_particles.txt", encoding = "UTF-8", warn = FALSE) %>% 
  str_remove_all("<.+>") 
AllnonLEX <- c(RusConjCoord2, RusPrep2, RusConjSubord2, RusInter2, RusPrtcl2) 
AllnonLEX_df <- tibble(word = AllnonLEX) #This column name should be the same for using the anti-join()
head(AllnonLEX_df) #Success! 
```

    # A tibble: 6 x 1
      word       
      <chr>      
    1 ""         
    2 "è"        
    3 "äà"       
    4 "íå òîëüêî"
    5 "íî è"     
    6 "òàêæå"    

And now I need to calculate lexical density

``` r
df_LexDens_sep <-corpus_df_tidy %>% 
  group_by(Student, Entry) %>% 
  summarize(total_words = n(),
         lexical_words = sum(!word %in% AllnonLEX_df$word),
         lexical_density = lexical_words/total_words)
```

    `summarise()` has grouped output by 'Student'. You can override using the `.groups` argument.

``` r
df_LexDens_sep
```

    # A tibble: 24 x 5
    # Groups:   Student [8]
       Student  Entry  total_words lexical_words lexical_density
       <chr>    <chr>        <int>         <int>           <dbl>
     1 Student1 Entry1         316           229           0.725
     2 Student1 Entry2         278           219           0.788
     3 Student1 Entry3         282           212           0.752
     4 Student2 Entry1         216           159           0.736
     5 Student2 Entry2         145           108           0.745
     6 Student2 Entry3          92            65           0.707
     7 Student3 Entry1         126           102           0.810
     8 Student3 Entry2         110            83           0.755
     9 Student3 Entry3          57            46           0.807
    10 Student4 Entry1         231           175           0.758
    # ... with 14 more rows

Manual check

``` r
316+278+282 #matches
```

    [1] 876

``` r
(0.7246835 + 0.7877698 + 0.7517730)/3  #  The number here 0.7547421 while the number below is 0.7534247. I need to think about these two numbers
```

    [1] 0.7547421

I want to compare the averaged numbers

``` r
df_LexDens_tog <-corpus_df_tidy %>% 
  group_by(Student) %>% 
  summarize(total_words = n(),
         lexical_words = sum(!word %in% AllnonLEX_df$word),
         lexical_density = lexical_words/total_words)

df_LexDens_tog
```

    # A tibble: 8 x 4
      Student  total_words lexical_words lexical_density
      <chr>          <int>         <int>           <dbl>
    1 Student1         876           660           0.753
    2 Student2         453           332           0.733
    3 Student3         293           231           0.788
    4 Student4         479           363           0.758
    5 Student5         642           455           0.709
    6 Student6         606           451           0.744
    7 Student7         676           496           0.734
    8 Student8         829           617           0.744

### Lexial variation

Lexial variation is measured with type-token ratio but since TTR is
affected by text length, researchers started using MTLD instead.

``` r
#I googled MTLD function in the koRpus package. Let's try it out.
#install.packages("koRpus")
#install.koRpus.lang(c("en","ru"))
#available.koRpus.lang()
library(koRpus) #I hope this package helps me calculate MTLD
```

    Loading required package: sylly

    For information on available language packages for 'koRpus', run

      available.koRpus.lang()

    and see ?install.koRpus.lang()


    Attaching package: 'koRpus'

    The following objects are masked from 'package:quanteda':

        tokens, types

    The following object is masked from 'package:readr':

        tokenize

``` r
library(koRpus.lang.ru)
library(koRpus.lang.en)
#I am expected to tokenize the text within the tokenize function of this package

#My failed attempd to calculate MTLD for all entries and students in one fell swoop. The results is gibberish
Tokens_for_MTLD <- lapply(filelist, FUN = tokenize, fileEncoding = "UTF-8", lang = "ru") 
#I still don't know how to filter out the word "unclear"
lapply(Tokens_for_MTLD, FUN = MTLD) #Success. The MTLD for S1_E1 is 175.36. Let's check manually
```

    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"

    Warning: Text is relatively short (<100 tokens), results are probably not
    reliable!

    Language: "ru"
    Language: "ru"
    Language: "ru"

    Warning: Text is relatively short (<100 tokens), results are probably not
    reliable!

    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"

    Warning: Text is relatively short (<100 tokens), results are probably not
    reliable!

    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"
    Language: "ru"

    [[1]]

    Total number of tokens: 320 
    Total number of types:  204

    Measure of Textual Lexical Diversity
                  MTLD: 175.36 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 59.12 (all factors) 
                        70.71 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[2]]

    Total number of tokens: 284 
    Total number of types:  195

    Measure of Textual Lexical Diversity
                  MTLD: 210.55 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 66.7 (all factors) 
                        7.78 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[3]]

    Total number of tokens: 286 
    Total number of types:  169

    Measure of Textual Lexical Diversity
                  MTLD: 98.27 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 56.36 (all factors) 
                        59.42 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[4]]

    Total number of tokens: 223 
    Total number of types:  147

    Measure of Textual Lexical Diversity
                  MTLD: 152.57 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 47.28 (all factors) 
                        27.58 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[5]]

    Total number of tokens: 147 
    Total number of types:  116

    Measure of Textual Lexical Diversity
                  MTLD: 195.18 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[6]]

    Total number of tokens: 97 
    Total number of types:  63

    Measure of Textual Lexical Diversity
                  MTLD: 65.09 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 11.03 (all factors) 
                        1.41 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[7]]

    Total number of tokens: 127 
    Total number of types:  90

    Measure of Textual Lexical Diversity
                  MTLD: 103.33 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 46.64 (all factors) 
                        14.85 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[8]]

    Total number of tokens: 112 
    Total number of types:  86

    Measure of Textual Lexical Diversity
                  MTLD: 135.09 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[9]]

    Total number of tokens: 62 
    Total number of types:  52

    Measure of Textual Lexical Diversity
                  MTLD: 107.63 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[10]]

    Total number of tokens: 236 
    Total number of types:  162

    Measure of Textual Lexical Diversity
                  MTLD: 195.88 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 81.64 (all factors) 
                        60.1 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[11]]

    Total number of tokens: 136 
    Total number of types:  112

    Measure of Textual Lexical Diversity
                  MTLD: 215.79 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[12]]

    Total number of tokens: 120 
    Total number of types:  88

    Measure of Textual Lexical Diversity
                  MTLD: 126 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[13]]

    Total number of tokens: 391 
    Total number of types:  233

    Measure of Textual Lexical Diversity
                  MTLD: 130.99 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 69.05 (all factors) 
                        74.79 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[14]]

    Total number of tokens: 156 
    Total number of types:  120

    Measure of Textual Lexical Diversity
                  MTLD: 189.28 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[15]]

    Total number of tokens: 99 
    Total number of types:  75

    Measure of Textual Lexical Diversity
                  MTLD: 114.34 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 0 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[16]]

    Total number of tokens: 229 
    Total number of types:  158

    Measure of Textual Lexical Diversity
                  MTLD: 132.16 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 70.15 (all factors) 
                        83.44 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[17]]

    Total number of tokens: 190 
    Total number of types:  125

    Measure of Textual Lexical Diversity
                  MTLD: 110.72 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 35.81 (all factors) 
                        15.56 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[18]]

    Total number of tokens: 212 
    Total number of types:  152

    Measure of Textual Lexical Diversity
                  MTLD: 212 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 112.01 (all factors) 
                        0 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[19]]

    Total number of tokens: 284 
    Total number of types:  164

    Measure of Textual Lexical Diversity
                  MTLD: 85.26 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 34.8 (all factors) 
                        28.91 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[20]]

    Total number of tokens: 202 
    Total number of types:  132

    Measure of Textual Lexical Diversity
                  MTLD: 116.43 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 41.12 (all factors) 
                        39.6 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[21]]

    Total number of tokens: 197 
    Total number of types:  128

    Measure of Textual Lexical Diversity
                  MTLD: 92.46 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 26.07 (all factors) 
                        30.89 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[22]]

    Total number of tokens: 317 
    Total number of types:  200

    Measure of Textual Lexical Diversity
                  MTLD: 138.16 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 51.76 (all factors) 
                        14.5 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[23]]

    Total number of tokens: 294 
    Total number of types:  202

    Measure of Textual Lexical Diversity
                  MTLD: 185.48 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 72.47 (all factors) 
                        17.68 (complete factors only)


    Note: Analysis was conducted case insensitive.


    [[24]]

    Total number of tokens: 226 
    Total number of types:  160

    Measure of Textual Lexical Diversity
                  MTLD: 157.07 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 89.34 (all factors) 
                        106.07 (complete factors only)


    Note: Analysis was conducted case insensitive.

``` r
#I am checking the MTLD manually
S1_E1 <- tokenize("C:/Users/Rossina/Documents/CMU_student/3_Fall_2021/Statistics_at_Pitt/data/Student1_Entry1.txt", fileEncoding = "UTF-8", lang = "ru")  #Success. 
#I tried using filter to remove "unclear" and str_remove_all - it didn't work out. I cannot even find out if there are words "unclear" in the text
#S1_E1 %>% 
#  filter(str_detect(token, "unclear"))
MTLD(S1_E1) #Success! The number matches the number from above - 175.36
```

    Language: "ru"


    Total number of tokens: 320 
    Total number of types:  204

    Measure of Textual Lexical Diversity
                  MTLD: 175.36 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 59.12 (all factors) 
                        70.71 (complete factors only)


    Note: Analysis was conducted case insensitive.

``` r
#I am not sure how to put the results into a tibble but NOT manually. The results look like this
#Language: "ru"

#Total number of tokens: 320 
#Total number of types:  204

#Measure of Textual Lexical Diversity
 #             MTLD: 175.36 
 #Number of factors: NA 
  #     Factor size: 0.72 
  #SD tokens/factor: 59.12 (all factors) 
   #                 70.71 (complete factors only)

#Note: Analysis was conducted case insensitive.
```

### Lexical sophistication

Lexical sophistication is measured using the average word length. I need
to calculate the length of each word, add everything up and divide by
the number of words in each text.

``` r
AWL_sep <-corpus_df_tidy %>% 
  group_by(Student, Entry) %>% 
  summarize(total_words = n(),
         total_word_length = sum(str_length(word)),
         AWL = total_word_length/total_words)
```

    `summarise()` has grouped output by 'Student'. You can override using the `.groups` argument.

``` r
AWL_sep #Success! 
```

    # A tibble: 24 x 5
    # Groups:   Student [8]
       Student  Entry  total_words total_word_length   AWL
       <chr>    <chr>        <int>             <int> <dbl>
     1 Student1 Entry1         316              1484  4.70
     2 Student1 Entry2         278              1441  5.18
     3 Student1 Entry3         282              1409  5.00
     4 Student2 Entry1         216               945  4.38
     5 Student2 Entry2         145               672  4.63
     6 Student2 Entry3          92               395  4.29
     7 Student3 Entry1         126               606  4.81
     8 Student3 Entry2         110               479  4.35
     9 Student3 Entry3          57               300  5.26
    10 Student4 Entry1         231              1089  4.71
    # ... with 14 more rows

``` r
AWL_tog <-corpus_df_tidy %>% 
  group_by(Student) %>% 
  summarize(total_words = n(),
         total_word_length = sum(str_length(word)),
         AWL = total_word_length/total_words)
AWL_tog #Success! Seems acceptable
```

    # A tibble: 8 x 4
      Student  total_words total_word_length   AWL
      <chr>          <int>             <int> <dbl>
    1 Student1         876              4334  4.95
    2 Student2         453              2012  4.44
    3 Student3         293              1385  4.73
    4 Student4         479              2418  5.05
    5 Student5         642              3105  4.84
    6 Student6         606              3082  5.09
    7 Student7         676              3185  4.71
    8 Student8         829              4118  4.97

## A dataframe with all the findings

I need to input my students’ names, their proficiency and numbers for
their lexical complexity measures

``` r
df_for_clustering <- corpus_df3 %>% 
  full_join(df_LexDens_sep) %>% 
  full_join(AWL_sep) #I don't know how to create an MTLD tibble NOT manually yet
```

    Joining, by = c("Student", "Entry")

    Joining, by = c("Student", "Entry", "total_words")

## An attempt to do the cluster analysis (no progress yet)

I am not sure where to begin. What I want to do is to see if lexical
density, lexical sophistication, and lexical variation produce a cluster
typical for Intermediate or Advanced proficiency levels, how lexical
complexity measures match the proficiency levels. I want to conduct
hierarchical cluster analysis similar to Jarvis et al.(2003). They had
more data points and linguistic features, but my project is a pilot
project.

``` r
#From https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
#I need to scale my data points
```

# Final notes

``` r
Sys.setlocale("LC_CTYPE", "English") # to make sure that nothing changes how my other files are read
```

    [1] "English_United States.1252"

## Session info

``` r
sessionInfo()
```

    R version 4.1.1 (2021-08-10)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 10 x64 (build 19043)

    Matrix products: default

    locale:
    [1] LC_COLLATE=English_United States.1252 
    [2] LC_CTYPE=English_United States.1252   
    [3] LC_MONETARY=English_United States.1252
    [4] LC_NUMERIC=C                          
    [5] LC_TIME=English_United States.1252    

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] koRpus.lang.en_0.1-4 koRpus.lang.ru_0.1-2 koRpus_0.13-8       
     [4] sylly_0.1-6          tidytext_0.3.2       htmlwidgets_1.5.4   
     [7] quanteda_3.1.0       forcats_0.5.1        stringr_1.4.0       
    [10] dplyr_1.0.7          purrr_0.3.4          readr_2.0.1         
    [13] tidyr_1.1.3          tibble_3.1.4         ggplot2_3.3.5       
    [16] tidyverse_1.3.1     

    loaded via a namespace (and not attached):
     [1] sylly.ru_0.1-2     httr_1.4.2         jsonlite_1.7.2     modelr_0.1.8      
     [5] RcppParallel_5.1.4 assertthat_0.2.1   cellranger_1.1.0   yaml_2.2.1        
     [9] pillar_1.6.2       backports_1.2.1    lattice_0.20-44    glue_1.4.2        
    [13] digest_0.6.27      rvest_1.0.1        colorspace_2.0-2   htmltools_0.5.2   
    [17] Matrix_1.3-4       pkgconfig_2.0.3    broom_0.7.9        haven_2.4.3       
    [21] scales_1.1.1       tzdb_0.1.2         generics_0.1.0     ellipsis_0.3.2    
    [25] withr_2.4.2        cli_3.0.1          magrittr_2.0.1     crayon_1.4.1      
    [29] readxl_1.3.1       evaluate_0.14      stopwords_2.2      tokenizers_0.2.1  
    [33] janeaustenr_0.1.5  fs_1.5.0           fansi_0.5.0        SnowballC_0.7.0   
    [37] xml2_1.3.2         tools_4.1.1        data.table_1.14.0  hms_1.1.0         
    [41] lifecycle_1.0.0    munsell_0.5.0      reprex_2.0.1       compiler_4.1.1    
    [45] rlang_0.4.11       grid_4.1.1         rstudioapi_0.13    rmarkdown_2.10    
    [49] gtable_0.3.0       DBI_1.1.1          R6_2.5.1           sylly.en_0.1-3    
    [53] lubridate_1.7.10   knitr_1.33         fastmap_1.1.0      utf8_1.2.2        
    [57] fastmatch_1.1-3    stringi_1.7.4      Rcpp_1.0.7         vctrs_0.3.8       
    [61] dbplyr_2.1.1       tidyselect_1.1.1   xfun_0.25         
