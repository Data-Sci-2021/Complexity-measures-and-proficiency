Draft\_2\_clean\_Soyan
================
Rossina Soyan
11/19/2021

-   [What I need to install to run the
    codes?](#what-i-need-to-install-to-run-the-codes)
-   [Loading and tokenizing one text in L2
    Russian](#loading-and-tokenizing-one-text-in-l2-russian)
-   [Lexical complexity measures](#lexical-complexity-measures)
    -   [Lexical density](#lexical-density)
    -   [Lexial variation](#lexial-variation)
    -   [Lexical sophistication](#lexical-sophistication)
    -   [Lexical bundles](#lexical-bundles)
-   [Syntactic complexity](#syntactic-complexity)
    -   [Global syntactic complexity](#global-syntactic-complexity)
    -   [Complexity by coordination](#complexity-by-coordination)
-   [Final notes](#final-notes)
    -   [Session info](#session-info)

``` r
##Set knitr options (show both code and output, show output w/o leading #, make figures smaller, hold figures until after chunk)
knitr::opts_chunk$set(echo=TRUE, include=TRUE, comment=NA, fig.height=3, fig.width=4.2, fig.show="hold")
Sys.setlocale("LC_CTYPE", "Russian") #to make sure my text is not gibberish, readable
```

    ## [1] "Russian_Russia.1251"

## What I need to install to run the codes?

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
#install.packages('corpus')
library(corpus) #To split the text into sentences
#install.packages('spacyr')
library(spacyr) #to be able to count the T-units
```

## Loading and tokenizing one text in L2 Russian

``` r
text1 <- readLines("data/text1.txt", encoding = "UTF-8")
text1 #Success!
```

    [1] "Дорогая Лара! Я хочу тебе рассказывать о моей лучшей подруге. Её зовут Вера, и она очень добрая. Она среднего роста и у неё короткие каштановые волосы. Вера любит музыку, и часто мы даём новые песни друг к другу, но чаще она мне даёт песни, потому что она знает больше меня о музыке. У Веры есть спортивная фигура, потому что она играет в хоккей. Она сильная и unclear любит бегать. Мы познакомились в школе пять лет назад, когда нам четырнадцать лет были. Мы были в библиотеке, и я читала роман Достоевского. Она видел роман и сказала, что ей нравится русская литература. Мы говорили два часа, до того как, библиотека закрылась. Мы стали лучшими подругами, и всегда мы читаем романы вместе. Как зовут твою лучшую подругу? Вы с ней учитесь в университете? Где вы познакомились друг с другом? unclear Как она выглядит? Она занимается каким-то спортом?"

``` r
# I want to exclude the words "unclear" from the text
# Let's try writing a function as in this link (https://stackoverflow.com/questions/35790652/removing-words-featured-in-character-vector-from-string)
stopwords = "unclear"
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}
text1clean <- removeWords(text1, stopwords)
text1clean #success! I was able to remove the words "unclear" from the text in Russian.
```

    [1] "Дорогая Лара! Я хочу тебе рассказывать о моей лучшей подруге. Её зовут Вера, и она очень добрая. Она среднего роста и у неё короткие каштановые волосы. Вера любит музыку, и часто мы даём новые песни друг к другу, но чаще она мне даёт песни, потому что она знает больше меня о музыке. У Веры есть спортивная фигура, потому что она играет в хоккей. Она сильная и любит бегать. Мы познакомились в школе пять лет назад, когда нам четырнадцать лет были. Мы были в библиотеке, и я читала роман Достоевского. Она видел роман и сказала, что ей нравится русская литература. Мы говорили два часа, до того как, библиотека закрылась. Мы стали лучшими подругами, и всегда мы читаем романы вместе. Как зовут твою лучшую подругу? Вы с ней учитесь в университете? Где вы познакомились друг с другом? Как она выглядит? Она занимается каким-то спортом?"

``` r
#Not to the future me: I need to make sure I remove everything between <> in my texts. I don't have comments in text1 but there are comments in other texts. Maybe use str_remove_all("<.+>")?

text1_df <- tibble(text = text1clean) #I use this dataframe for my rsyntax dependency analysis and other syntactic complexity measures. 
text1_df #Success
```

    # A tibble: 1 x 1
      text                                                                          
      <chr>                                                                         
    1 Дорогая Лара! Я хочу тебе рассказывать о моей лучшей подруге. Её зовут Вера, ~

``` r
#Let me create a tidytext
tidy_text1 <- text1_df %>% 
  unnest_tokens(word, text) #I decided to make all words lower-case here, but then I deleted to_lower = FALSE 
tidy_text1 #Success But I need каким-то to be one word. Overall, all punctuation is removed and all words have become lower-case. I can use this tibble for my lexical complexity analysis
```

    # A tibble: 143 x 1
       word        
       <chr>       
     1 дорогая     
     2 лара        
     3 я           
     4 хочу        
     5 тебе        
     6 рассказывать
     7 о           
     8 моей        
     9 лучшей      
    10 подруге     
    # ... with 133 more rows

``` r
#The count function can help me identify stopwords for my analysis
tidy_text1 %>% 
  count(word, sort = TRUE) 
```

    # A tibble: 102 x 2
       word      n
       <chr> <int>
     1 она       9
     2 и         7
     3 мы        6
     4 в         4
     5 что       3
     6 как       3
     7 вера      2
     8 друг      2
     9 вы        2
    10 были      2
    # ... with 92 more rows

Let me now try to measure complexity measures in one text. I have chosen
the list of lexical and syntactic complexity measures based on the book
by Barkaoui and Hadidi (2020). They understand lexical complexity as
“the richness of a writer’s lexicon” (p. 13) and divide it into four
components: *lexical density*, *lexical variation*, *lexical
sophistication*, and *lexical bundles*. As for syntactic complexity, it
is understood as “grammatical variation and sophistication”
(Wolfe-Quintero et al., 1998, p. 45). It is also a multi-dimensional
construct and includes *global complexity*, *complexity by
coordination*, *complexity by subordination*, *clausal complexity*, and
*structural variety*.

Let me try to figure out if I can measure these constructs in one text
in L2 Russian.

## Lexical complexity measures

### Lexical density

Lexical density is understood as the ratio of lexical words. I need to
divide the number of lexical words (nouns, verbs, adjectives, adverbs)
per the total number of words.

``` r
#First, I needed to figure out how to exclude non-lexical words. I looked into the list of stopwords in the corpus package.
?stopwords_ru
```

    starting httpd help server ... done

``` r
stopwords_ru #But this list does not make sense. There are no combinations in the Russian language which are included in this list. I had to create my own list of stopwords.
```

      [1] "Á"       "ÂÅÚ"     "ÂÏÌÅÅ"   "ÂÏÌØÛÅ"  "ÂÕÄÅÔ"   "ÂÕÄÔÏ"   "ÂÙ"     
      [8] "ÂÙÌ"     "ÂÙÌÁ"    "ÂÙÌÉ"    "ÂÙÌÏ"    "ÂÙÔØ"    "ÄÁ"      "ÄÁÖÅ"   
     [15] "ÄÌÑ"     "ÄÏ"      "ÄÒÕÇÏÊ"  "Ä×Á"     "ÅÅ"      "ÅÇÏ"     "ÅÊ"     
     [22] "ÅÍÕ"     "ÅÓÌÉ"    "ÅÓÔØ"    "ÅÝÅ"     "ÇÄÅ"     "ÇÏ×ÏÒÉÌ" "ÈÏÒÏÛÏ" 
     [29] "ÈÏÔØ"    "É"       "ÉÈ"      "ÉÌÉ"     "ÉÍ"      "ÉÎÏÇÄÁ"  "ÉÚ"     
     [36] "Ë"       "ËÁË"     "ËÁËÁÑ"   "ËÁËÏÊ"   "ËÁÖÅÔÓÑ" "ËÏÇÄÁ"   "ËÏÎÅÞÎÏ"
     [43] "ËÔÏ"     "ËÕÄÁ"    "ÌÉ"      "ÌÕÞÛÅ"   "ÍÅÎÑ"    "ÍÅÖÄÕ"   "ÍÎÅ"    
     [50] "ÍÎÏÇÏ"   "ÍÏÊ"     "ÍÏÑ"     "ÍÏÖÅÔ"   "ÍÏÖÎÏ"   "ÍÙ"      "ÎÁ"     
     [57] "ÎÁÄ"     "ÎÁÄÏ"    "ÎÁËÏÎÅÃ" "ÎÁÓ"     "ÎÅ"      "ÎÅÅ"     "ÎÅÇÏ"   
     [64] "ÎÅÊ"     "ÎÅÌØÚÑ"  "ÎÅÔ"     "ÎÉ"      "ÎÉÂÕÄØ"  "ÎÉÈ"     "ÎÉËÏÇÄÁ"
     [71] "ÎÉÍ"     "ÎÉÞÅÇÏ"  "ÎÏ"      "ÎÕ"      "Ï"       "ÏÂ"      "ÏÄÉÎ"   
     [78] "ÏÎ"      "ÏÎÁ"     "ÏÎÉ"     "ÏÐÑÔØ"   "ÏÔ"      "ÐÅÒÅÄ"   "ÐÏ"     
     [85] "ÐÏÄ"     "ÐÏÓÌÅ"   "ÐÏÔÏÍ"   "ÐÏÔÏÍÕ"  "ÐÏÞÔÉ"   "ÐÒÉ"     "ÐÒÏ"    
     [92] "Ñ"       "ÒÁÚ"     "ÒÁÚ×Å"   "Ó"       "ÓÁÍ"     "ÓÅÂÅ"    "ÓÅÂÑ"   
     [99] "ÓÅÇÏÄÎÑ" "ÓÅÊÞÁÓ"  "ÓËÁÚÁÌ"  "ÓËÁÚÁÌÁ" "ÓËÁÚÁÔØ" "ÓÏ"      "ÓÏ×ÓÅÍ" 
    [106] "Ó×ÏÀ"    "ÔÁË"     "ÔÁËÏÊ"   "ÔÁÍ"     "ÔÅÂÑ"    "ÔÅÍ"     "ÔÅÐÅÒØ" 
    [113] "ÔÏ"      "ÔÏÇÄÁ"   "ÔÏÇÏ"    "ÔÏÌØËÏ"  "ÔÏÍ"     "ÔÏÔ"     "ÔÏÖÅ"   
    [120] "ÔÒÉ"     "ÔÕÔ"     "ÔÙ"      "Õ"       "ÕÖ"      "ÕÖÅ"     "Ö"      
    [127] "ÖÅ"      "ÖÉÚÎØ"   "×"       "×ÁÍ"     "×ÁÓ"     "×ÄÒÕÇ"   "×ÅÄØ"   
    [134] "×Ï"      "×ÏÔ"     "×ÐÒÏÞÅÍ" "×ÓÀ"     "×ÓÅ"     "×ÓÅÇÄÁ"  "×ÓÅÇÏ"  
    [141] "×ÓÅÈ"    "×Ù"      "ÚÁ"      "ÚÁÞÅÍ"   "ÚÄÅÓØ"   "ÜÔÉ"     "ÜÔÏÇÏ"  
    [148] "ÜÔÏÊ"    "ÜÔÏÍ"    "ÜÔÏÔ"    "ÜÔÕ"     "ÞÅÇÏ"    "ÞÅÌÏ×ÅË" "ÞÅÍ"    
    [155] "ÞÅÒÅÚ"   "ÞÔÏ"     "ÞÔÏÂ"    "ÞÔÏÂÙ"   "ÞÕÔØ"   

I made my own list of non-lexical words which included a list of
prepositions, conjunctions, interjections and particles.

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
AllnonLEX_df #Success! 
```

    # A tibble: 151 x 1
       word       
       <chr>      
     1 ""         
     2 "è"        
     3 "äà"       
     4 "íå òîëüêî"
     5 "íî è"     
     6 "òàêæå"    
     7 "òîæå"     
     8 "íè"       
     9 "êàê"      
    10 "òàê è"    
    # ... with 141 more rows

Somehow the anti-join command did not work out so I used the code I
found online

``` r
#main_data2 <- main_data[ ! main_data$NAMES %in% NAMES_list, ] From https://stackoverflow.com/questions/13012509/how-to-delete-rows-from-a-data-frame-based-on-an-external-list-using-r/13012618
Lex_text1 <- tidy_text1[ ! tidy_text1$word %in% AllnonLEX, ]
Lex_text1 #Success there is a problem with каким-то. It is counted as two words. I need to work on making it one word. I tried to work with the text using corpus::text_tokens() but the result for now looks ugly.
```

    # A tibble: 112 x 1
       word        
       <chr>       
     1 дорогая     
     2 лара        
     3 я           
     4 хочу        
     5 тебе        
     6 рассказывать
     7 моей        
     8 лучшей      
     9 подруге     
    10 её          
    # ... with 102 more rows

Now I need to come up with a function for measuring lexical density

``` r
LexDens_text1 <- nrow(Lex_text1)/nrow(tidy_text1)
LexDens_text1 #Success - I need to write a function
```

    [1] 0.7832168

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
?MTLD #I am expected to tokenize the text within the tokenize function of this package
koR_text1 <- tokenize("data/text1.txt", fileEncoding = "UTF-8", lang = "ru") #Success. I had to indicate the path to the .txt file, not try to input the existing tibble. 
koR_text1 #BUT I don't know yet how to filter out the words 'unclear'
```

           doc_id      token      tag lemma lttr   wclass desc stop stem idx sntc
    1   text1.txt    Äîðîãàÿ word.kRp          7     word <NA> <NA> <NA>   1    1
    2   text1.txt       Ëàðà word.kRp          4     word <NA> <NA> <NA>   2    1
    3   text1.txt          !     .kRp          1 fullstop <NA> <NA> <NA>   3    1
    4   text1.txt          ß word.kRp          1     word <NA> <NA> <NA>   4    2
    5   text1.txt       õî÷ó word.kRp          4     word <NA> <NA> <NA>   5    2
    6   text1.txt       òåáå word.kRp          4     word <NA> <NA> <NA>   6    2
                                                    [...]                        
    167 text1.txt          ?     .kRp          1 fullstop <NA> <NA> <NA> 167   16
    168 text1.txt        Îíà word.kRp          3     word <NA> <NA> <NA> 168   17
    169 text1.txt çàíèìàåòñÿ word.kRp         10     word <NA> <NA> <NA> 169   17
    170 text1.txt   êàêèì-òî  unk.kRp          8  unknown <NA> <NA> <NA> 170   17
    171 text1.txt    ñïîðòîì word.kRp          7     word <NA> <NA> <NA> 171   17
    172 text1.txt          ?     .kRp          1 fullstop <NA> <NA> <NA> 172   17

``` r
MTLD(koR_text1) #Success! Now I need to figure out how to write a function for a corpus of texts
```

    Language: "ru"


    Total number of tokens: 144 
    Total number of types:  102

    Measure of Textual Lexical Diversity
                  MTLD: 98.89 
     Number of factors: NA 
           Factor size: 0.72 
      SD tokens/factor: 41.64 (all factors) 
                        14.85 (complete factors only)


    Note: Analysis was conducted case insensitive.

### Lexical sophistication

Lexical sophistication is measured using the average word length. I need
to calculate the length of each word, add everything up and divide by
the number of words in each text.

``` r
AWL_text1 <- sum(str_length(tidy_text1$word)) / nrow(tidy_text1)
AWL_text1 #Success! Nowm I need to write function for the corpus of texts
```

    [1] 4.643357

### Lexical bundles

The use of lexical bundles is measures through comparing multi-word
expressions in L2 texts with the list of popular multi-word expressions
from a representative corpus. There is a list of lexical bundles from
the Russian academic corpus, but my texts are non-academic texts. I will
leave this measure unmeasured for the time being. I will come back to it
if I have time and try to figure out how to measure it.

## Syntactic complexity

### Global syntactic complexity

It is measured using the mean sentence length. I need to count the
number of sentences and also the number of words in a text and then
divide words per sentences.

``` r
#How do I calculate the number of sentences in text1?
nsentence(text1) #The answer is more or less correct. I was not able to figure our how to remove the address term in the very beginning of the text (= the sentence without a verb = not a sentence). Let me try to split the text into sentences using the corpus package
```

    Warning in nsentence.character(text1): nsentence() does not correctly count
    sentences in all lower-cased text

    text1 
       17 

``` r
text_split(text1) #Great it works!
```

       parent index text                                                            
    1  1          1 Äîðîãàÿ Ëàðà!                                                   
    2  1          2 ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.                 
    3  1          3 Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.                              
    4  1          4 Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.          
    5  1          5 Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî…
    6  1          6 Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.  
    7  1          7 Îíà ñèëüíàÿ è unclear ëþáèò áåãàòü.                             
    8  1          8 Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü…
    9  1          9 Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.            
    10 1         10 Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.  
    11 1         11 Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.        
    12 1         12 Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.   
    13 1         13 Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?                                  
    14 1         14 Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?                                
    15 1         15 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?                             
    16 1         16 unclear Êàê îíà âûãëÿäèò?                                       
    17 1         17 Îíà çàíèìàåòñÿ êàêèì-òî ñïîðòîì?                                

``` r
nrow(tidy_text1)
```

    [1] 143

``` r
#let me calculate the mean sentence length
(MLStext1<- nrow(tidy_text1) / nsentence(text1)) #Success! Now I need to write the function for a corpus
```

    Warning in nsentence.character(text1): nsentence() does not correctly count
    sentences in all lower-cased text

       text1 
    8.411765 

### Complexity by coordination

I need to calculate T-units or clauses per sentence. The good news is I
can calculate the number of sentences. Now I need to figure out how to
calculate the number of T-units or clauses. Then, I need to divide the
number of T-units per sentences.

``` r
#FYI, the spacyr does not parse Russian texts well. My experience is described below
#I manually installed miniconda
# spacy_install()
#spacy_initialize(model = "en_core_web_sm") #This is for the English lge. 
# spacy_download_langmodel("ru_core_news_sm") # Success!

parsed_text1 <- spacy_parse(text1, lemma = FALSE, entity = TRUE, nounphrase = TRUE) 
```

    Found 'spacy_condaenv'. spacyr will use this environment

    successfully initialized (spaCy Version: 3.1.3, language model: en_core_web_sm)

    (python options: type = "condaenv", value = "spacy_condaenv")

``` r
entity_extract(parsed_text1) # This can be used to anonymize the texts BUT it contains a lot of mistakes
```

      doc_id sentence_id                   entity entity_type
    1  text1           3               çîâóò_Âåðà      PERSON
    2  text1           3             î÷åíü_äîáðàÿ      PERSON
    3  text1           4        êàøòàíîâûå_âîëîñû      PERSON
    4  text1           5                     Âåðà      PERSON
    5  text1           5 íîâûå_ïåñíè_äðóã_ê_äðóãó      PERSON
    6  text1           5               ïîòîìó_÷òî         ORG
    7  text1           8                      ëåò         ORG
    8  text1           9             Äîñòîåâñêîãî      PERSON
    9  text1          14                ó÷èòåñü_â        WORK

``` r
spacy_parse(text1, dependency = TRUE, pos = FALSE) #Looks amazing BUT contains a lot of mistakes
```

        doc_id sentence_id token_id         token         lemma head_token_id
    1    text1           1        1       Äîðîãàÿ       Äîðîãàÿ             1
    2    text1           1        2          Ëàðà          Ëàðà             1
    3    text1           1        3             !             !             1
    4    text1           2        1             ß             ÿ             3
    5    text1           2        2          õî÷ó          õî÷ó             3
    6    text1           2        3          òåáå          òåáå             4
    7    text1           2        4  ðàññêàçûâàòü  ðàññêàçûâàòü             8
    8    text1           2        5             î             î             8
    9    text1           2        6          ìîåé          ìîåé             8
    10   text1           2        7        ëó÷øåé        ëó÷øåé             8
    11   text1           2        8       ïîäðóãå       ïîäðóãå             8
    12   text1           2        9             .             .             8
    13   text1           3        1            Å¸            å¸             1
    14   text1           3        2         çîâóò         çîâóò             3
    15   text1           3        3          Âåðà          Âåðà             1
    16   text1           3        4             ,             ,             3
    17   text1           3        5             è             è             8
    18   text1           3        6           îíà           îíà             8
    19   text1           3        7         î÷åíü         î÷åíü             8
    20   text1           3        8        äîáðàÿ        äîáðàÿ             3
    21   text1           3        9             .             .             1
    22   text1           4        1           Îíà           Îíà             2
    23   text1           4        2      ñðåäíåãî      ñðåäíåãî             3
    24   text1           4        3         ðîñòà         ðîñòà             3
    25   text1           4        4             è             è             3
    26   text1           4        5             ó             ó             9
    27   text1           4        6           íå¸           íå¸             7
    28   text1           4        7      êîðîòêèå      êîðîòêèå             9
    29   text1           4        8    êàøòàíîâûå    êàøòàíîâûå             9
    30   text1           4        9        âîëîñû        âîëîñû             3
    31   text1           4       10             .             .             3
    32   text1           5        1          Âåðà          Âåðà             3
    33   text1           5        2         ëþáèò         ëþáèò             3
    34   text1           5        3        ìóçûêó        ìóçûêó            10
    35   text1           5        4             ,             ,             3
    36   text1           5        5             è             è             6
    37   text1           5        6         ÷àñòî         ÷àñòî             3
    38   text1           5        7            ìû            ìû             3
    39   text1           5        8          äà¸ì          äà¸ì             3
    40   text1           5        9         íîâûå         íîâûå             3
    41   text1           5       10         ïåñíè         ïåñíè            27
    42   text1           5       11          äðóã          äðóã            10
    43   text1           5       12             ê             ê            13
    44   text1           5       13         äðóãó         äðóãó            11
    45   text1           5       14             ,             ,            10
    46   text1           5       15            íî            íî            18
    47   text1           5       16          ÷àùå          ÷àùå            18
    48   text1           5       17           îíà           îíà            18
    49   text1           5       18           ìíå           ìíå            20
    50   text1           5       19          äà¸ò          äà¸ò            20
    51   text1           5       20         ïåñíè         ïåñíè            10
    52   text1           5       21             ,             ,            10
    53   text1           5       22        ïîòîìó        ïîòîìó            25
    54   text1           5       23           ÷òî           ÷òî            24
    55   text1           5       24           îíà           îíà            25
    56   text1           5       25         çíàåò         çíàåò            27
    57   text1           5       26        áîëüøå        áîëüøå            27
    58   text1           5       27          ìåíÿ          ìåíÿ            27
    59   text1           5       28             î             î            29
    60   text1           5       29        ìóçûêå        ìóçûêå            27
    61   text1           5       30             .             .            27
    62   text1           6        1             Ó             Ó             2
    63   text1           6        2          Âåðû          Âåðû             3
    64   text1           6        3          åñòü          åñòü             3
    65   text1           6        4    ñïîðòèâíàÿ    ñïîðòèâíàÿ             5
    66   text1           6        5        ôèãóðà        ôèãóðà             3
    67   text1           6        6             ,             ,             3
    68   text1           6        7        ïîòîìó        ïîòîìó             3
    69   text1           6        8           ÷òî           ÷òî             9
    70   text1           6        9           îíà           îíà             7
    71   text1           6       10        èãðàåò        èãðàåò             7
    72   text1           6       11             â             â            12
    73   text1           6       12        õîêêåé        õîêêåé             7
    74   text1           6       13             .             .             3
    75   text1           7        1           Îíà           Îíà             2
    76   text1           7        2       ñèëüíàÿ       ñèëüíàÿ             2
    77   text1           7        3             è             è             4
    78   text1           7        4       unclear       unclear             6
    79   text1           7        5         ëþáèò         ëþáèò             6
    80   text1           7        6        áåãàòü        áåãàòü             2
    81   text1           7        7             .             .             2
    82   text1           8        1            Ìû            Ìû             2
    83   text1           8        2 ïîçíàêîìèëèñü ïîçíàêîìèëèñü             7
    84   text1           8        3             â             â             4
    85   text1           8        4         øêîëå         øêîëå             7
    86   text1           8        5          ïÿòü          ïÿòü             6
    87   text1           8        6           ëåò           ëåò             7
    88   text1           8        7         íàçàä         íàçàä            11
    89   text1           8        8             ,             ,             7
    90   text1           8        9         êîãäà         êîãäà            10
    91   text1           8       10           íàì           íàì            11
    92   text1           8       11  ÷åòûðíàäöàòü  ÷åòûðíàäöàòü            11
    93   text1           8       12           ëåò           ëåò            13
    94   text1           8       13          áûëè          áûëè            11
    95   text1           8       14             .             .            11
    96   text1           9        1            Ìû            Ìû             2
    97   text1           9        2          áûëè          áûëè             3
    98   text1           9        3             â             â             4
    99   text1           9        4    áèáëèîòåêå    áèáëèîòåêå            10
    100  text1           9        5             ,             ,             4
    101  text1           9        6             è             è            10
    102  text1           9        7             ÿ             ÿ            10
    103  text1           9        8        ÷èòàëà        ÷èòàëà            10
    104  text1           9        9         ðîìàí         ðîìàí            10
    105  text1           9       10  Äîñòîåâñêîãî  Äîñòîåâñêîãî            10
    106  text1           9       11             .             .            10
    107  text1          10        1           Îíà           Îíà             3
    108  text1          10        2         âèäåë         âèäåë             3
    109  text1          10        3         ðîìàí         ðîìàí             7
    110  text1          10        4             è             è             5
    111  text1          10        5       ñêàçàëà       ñêàçàëà             3
    112  text1          10        6             ,             ,             7
    113  text1          10        7           ÷òî           ÷òî            10
    114  text1          10        8            åé            åé             7
    115  text1          10        9      íðàâèòñÿ      íðàâèòñÿ             8
    116  text1          10       10       ðóññêàÿ       ðóññêàÿ            11
    117  text1          10       11    ëèòåðàòóðà    ëèòåðàòóðà            11
    118  text1          10       12             .             .            11
    119  text1          11        1            Ìû            Ìû             2
    120  text1          11        2      ãîâîðèëè      ãîâîðèëè             4
    121  text1          11        3           äâà           äâà             4
    122  text1          11        4          ÷àñà          ÷àñà             4
    123  text1          11        5             ,             ,             4
    124  text1          11        6            äî            äî             4
    125  text1          11        7          òîãî          òîãî             8
    126  text1          11        8           êàê           êàê             6
    127  text1          11        9             ,             ,             8
    128  text1          11       10    áèáëèîòåêà    áèáëèîòåêà            11
    129  text1          11       11     çàêðûëàñü     çàêðûëàñü             8
    130  text1          11       12             .             .             4
    131  text1          12        1            Ìû            Ìû             4
    132  text1          12        2         ñòàëè         ñòàëè             4
    133  text1          12        3       ëó÷øèìè       ëó÷øèìè             4
    134  text1          12        4     ïîäðóãàìè     ïîäðóãàìè             7
    135  text1          12        5             ,             ,             4
    136  text1          12        6             è             è             7
    137  text1          12        7        âñåãäà        âñåãäà             7
    138  text1          12        8            ìû            ìû            11
    139  text1          12        9        ÷èòàåì        ÷èòàåì            10
    140  text1          12       10        ðîìàíû        ðîìàíû            11
    141  text1          12       11        âìåñòå        âìåñòå             7
    142  text1          12       12             .             .             7
    143  text1          13        1           Êàê           Êàê             5
    144  text1          13        2         çîâóò         çîâóò             1
    145  text1          13        3          òâîþ          òâîþ             4
    146  text1          13        4        ëó÷øóþ        ëó÷øóþ             5
    147  text1          13        5       ïîäðóãó       ïîäðóãó             5
    148  text1          13        6             ?             ?             5
    149  text1          14        1            Âû            Âû             4
    150  text1          14        2             ñ             ñ             4
    151  text1          14        3           íåé           íåé             4
    152  text1          14        4       ó÷èòåñü       ó÷èòåñü             4
    153  text1          14        5             â             â             6
    154  text1          14        6  óíèâåðñèòåòå  óíèâåðñèòåòå             4
    155  text1          14        7             ?             ?             4
    156  text1          15        1           Ãäå           Ãäå             3
    157  text1          15        2            âû            âû             3
    158  text1          15        3 ïîçíàêîìèëèñü ïîçíàêîìèëèñü             3
    159  text1          15        4          äðóã          äðóã             3
    160  text1          15        5             ñ             ñ             6
    161  text1          15        6        äðóãîì        äðóãîì             4
    162  text1          15        7             ?             ?             3
    163  text1          16        1       unclear       unclear             4
    164  text1          16        2           Êàê           Êàê             4
    165  text1          16        3           îíà           îíà             4
    166  text1          16        4      âûãëÿäèò      âûãëÿäèò             4
    167  text1          16        5             ?             ?             4
    168  text1          17        1           Îíà           Îíà             6
    169  text1          17        2    çàíèìàåòñÿ    çàíèìàåòñÿ             5
    170  text1          17        3         êàêèì         êàêèì             5
    171  text1          17        4             -             -             5
    172  text1          17        5            òî            òî             1
    173  text1          17        6       ñïîðòîì       ñïîðòîì             6
    174  text1          17        7             ?             ?             6
          dep_rel        entity
    1        ROOT              
    2        dobj              
    3       punct              
    4        amod              
    5    compound              
    6       nsubj              
    7        amod              
    8        nmod              
    9        amod              
    10   compound              
    11       ROOT              
    12      punct              
    13       ROOT              
    14   compound      PERSON_B
    15   npadvmod      PERSON_I
    16      punct              
    17   compound              
    18   compound              
    19   compound      PERSON_B
    20      appos      PERSON_I
    21      punct              
    22   compound              
    23      nsubj              
    24       ROOT              
    25       dobj              
    26        det              
    27   compound              
    28   compound              
    29   compound      PERSON_B
    30       dobj      PERSON_I
    31      punct              
    32       nmod      PERSON_B
    33   compound              
    34      nsubj              
    35      punct              
    36        dep              
    37      appos              
    38       conj              
    39      appos              
    40       conj      PERSON_B
    41  parataxis      PERSON_I
    42       prep      PERSON_I
    43   compound      PERSON_I
    44       pobj      PERSON_I
    45      punct              
    46       amod              
    47   compound              
    48   compound              
    49   compound              
    50   compound              
    51      appos              
    52      punct              
    53   compound         ORG_B
    54   compound         ORG_I
    55   compound              
    56      nsubj              
    57      nsubj              
    58       ROOT              
    59   compound              
    60       dobj              
    61      punct              
    62        det              
    63      nsubj              
    64       ROOT              
    65   compound              
    66       dobj              
    67      punct              
    68        dep              
    69   compound              
    70       dobj              
    71   npadvmod              
    72   compound              
    73       dobj              
    74      punct              
    75      nsubj              
    76       ROOT              
    77     advmod              
    78       amod              
    79   compound              
    80       dobj              
    81      punct              
    82   compound              
    83       nmod              
    84   compound              
    85   compound              
    86   compound              
    87   compound              
    88      nsubj              
    89      punct              
    90       amod              
    91      nsubj              
    92       ROOT              
    93   compound         ORG_B
    94       dobj              
    95      punct              
    96   compound              
    97   compound              
    98   compound              
    99        dep              
    100     punct              
    101       dep              
    102  compound              
    103      amod              
    104  compound              
    105      ROOT      PERSON_B
    106     punct              
    107  compound              
    108  compound              
    109       dep              
    110    advmod              
    111    advmod              
    112     punct              
    113     nsubj              
    114      prep              
    115      pobj              
    116  compound              
    117      ROOT              
    118     punct              
    119  compound              
    120  compound              
    121  compound              
    122      ROOT              
    123     punct              
    124      prep              
    125  compound              
    126      pobj              
    127     punct              
    128  compound              
    129     appos              
    130     punct              
    131      nmod              
    132      amod              
    133  compound              
    134     nsubj              
    135     punct              
    136     nsubj              
    137      ROOT              
    138  compound              
    139  compound              
    140  compound              
    141      dobj              
    142     punct              
    143  compound              
    144      nmod              
    145  compound              
    146  compound              
    147      ROOT              
    148     punct              
    149  compound              
    150  compound              
    151  compound              
    152      ROOT WORK_OF_ART_B
    153  compound WORK_OF_ART_I
    154     appos              
    155     punct              
    156  npadvmod              
    157     nsubj              
    158      ROOT              
    159      prep              
    160  compound              
    161      pobj              
    162     punct              
    163      amod              
    164  compound              
    165  compound              
    166      ROOT              
    167     punct              
    168      nmod              
    169  compound              
    170  npadvmod              
    171     punct              
    172      nmod              
    173      ROOT              
    174     punct              

``` r
entity_consolidate(parsed_text1) # Only some entities are consolidated
```

        doc_id sentence_id token_id                    token    pos entity_type
    1    text1           1        1                  Äîðîãàÿ   VERB            
    2    text1           1        2                     Ëàðà   NOUN            
    3    text1           1        3                        !  PUNCT            
    4    text1           2        1                        ß    ADJ            
    5    text1           2        2                     õî÷ó   NOUN            
    6    text1           2        3                     òåáå   NOUN            
    7    text1           2        4             ðàññêàçûâàòü    ADJ            
    8    text1           2        5                        î      X            
    9    text1           2        6                     ìîåé    ADJ            
    10   text1           2        7                   ëó÷øåé  PROPN            
    11   text1           2        8                  ïîäðóãå  PROPN            
    12   text1           2        9                        .  PUNCT            
    13   text1           3        1                       Å¸   VERB            
    14   text1           3        2               çîâóò_Âåðà ENTITY      PERSON
    15   text1           3        3                        ,  PUNCT            
    16   text1           3        4                        è  PROPN            
    17   text1           3        5                      îíà  PROPN            
    18   text1           3        6             î÷åíü_äîáðàÿ ENTITY      PERSON
    19   text1           3        7                        .  PUNCT            
    20   text1           4        1                      Îíà  PROPN            
    21   text1           4        2                 ñðåäíåãî   NOUN            
    22   text1           4        3                    ðîñòà   VERB            
    23   text1           4        4                        è    DET            
    24   text1           4        5                        ó    DET            
    25   text1           4        6                      íå¸  PROPN            
    26   text1           4        7                 êîðîòêèå  PROPN            
    27   text1           4        8        êàøòàíîâûå_âîëîñû ENTITY      PERSON
    28   text1           4        9                        .  PUNCT            
    29   text1           5        1                     Âåðà ENTITY      PERSON
    30   text1           5        2                    ëþáèò   NOUN            
    31   text1           5        3                   ìóçûêó  PROPN            
    32   text1           5        4                        ,  PUNCT            
    33   text1           5        5                        è      X            
    34   text1           5        6                    ÷àñòî   VERB            
    35   text1           5        7                       ìû  PROPN            
    36   text1           5        8                     äà¸ì   NOUN            
    37   text1           5        9 íîâûå_ïåñíè_äðóã_ê_äðóãó ENTITY      PERSON
    38   text1           5       10                        ,  PUNCT            
    39   text1           5       11                       íî  PROPN            
    40   text1           5       12                     ÷àùå   NOUN            
    41   text1           5       13                      îíà  PROPN            
    42   text1           5       14                      ìíå  PROPN            
    43   text1           5       15                     äà¸ò  PROPN            
    44   text1           5       16                    ïåñíè  PROPN            
    45   text1           5       17                        ,  PUNCT            
    46   text1           5       18               ïîòîìó_÷òî ENTITY         ORG
    47   text1           5       19                      îíà  PROPN            
    48   text1           5       20                    çíàåò  PROPN            
    49   text1           5       21                   áîëüøå  PROPN            
    50   text1           5       22                     ìåíÿ  PROPN            
    51   text1           5       23                        î  PROPN            
    52   text1           5       24                   ìóçûêå  PROPN            
    53   text1           5       25                        .  PUNCT            
    54   text1           6        1                        Ó  PROPN            
    55   text1           6        2                     Âåðû  PROPN            
    56   text1           6        3                     åñòü   VERB            
    57   text1           6        4               ñïîðòèâíàÿ  PROPN            
    58   text1           6        5                   ôèãóðà  PROPN            
    59   text1           6        6                        ,  PUNCT            
    60   text1           6        7                   ïîòîìó   VERB            
    61   text1           6        8                      ÷òî  PROPN            
    62   text1           6        9                      îíà   NOUN            
    63   text1           6       10                   èãðàåò   NOUN            
    64   text1           6       11                        â   NOUN            
    65   text1           6       12                   õîêêåé   NOUN            
    66   text1           6       13                        .  PUNCT            
    67   text1           7        1                      Îíà  PROPN            
    68   text1           7        2                  ñèëüíàÿ   VERB            
    69   text1           7        3                        è    ADV            
    70   text1           7        4                  unclear    ADJ            
    71   text1           7        5                    ëþáèò   NOUN            
    72   text1           7        6                   áåãàòü   NOUN            
    73   text1           7        7                        .  PUNCT            
    74   text1           8        1                       Ìû  PROPN            
    75   text1           8        2            ïîçíàêîìèëèñü   NOUN            
    76   text1           8        3                        â  PROPN            
    77   text1           8        4                    øêîëå   NOUN            
    78   text1           8        5                     ïÿòü  PROPN            
    79   text1           8        6                      ëåò  PROPN            
    80   text1           8        7                    íàçàä   NOUN            
    81   text1           8        8                        ,  PUNCT            
    82   text1           8        9                    êîãäà    ADJ            
    83   text1           8       10                      íàì  PROPN            
    84   text1           8       11             ÷åòûðíàäöàòü   VERB            
    85   text1           8       12                      ëåò ENTITY         ORG
    86   text1           8       13                     áûëè   NOUN            
    87   text1           8       14                        .  PUNCT            
    88   text1           9        1                       Ìû  PROPN            
    89   text1           9        2                     áûëè   NOUN            
    90   text1           9        3                        â   NOUN            
    91   text1           9        4               áèáëèîòåêå   NOUN            
    92   text1           9        5                        ,  PUNCT            
    93   text1           9        6                        è      X            
    94   text1           9        7                        ÿ  PROPN            
    95   text1           9        8                   ÷èòàëà    ADJ            
    96   text1           9        9                    ðîìàí  PROPN            
    97   text1           9       10             Äîñòîåâñêîãî ENTITY      PERSON
    98   text1           9       11                        .  PUNCT            
    99   text1          10        1                      Îíà  PROPN            
    100  text1          10        2                    âèäåë  PROPN            
    101  text1          10        3                    ðîìàí  PROPN            
    102  text1          10        4                        è    ADV            
    103  text1          10        5                  ñêàçàëà   VERB            
    104  text1          10        6                        ,  PUNCT            
    105  text1          10        7                      ÷òî   NOUN            
    106  text1          10        8                       åé    ADP            
    107  text1          10        9                 íðàâèòñÿ  PROPN            
    108  text1          10       10                  ðóññêàÿ   VERB            
    109  text1          10       11               ëèòåðàòóðà  PROPN            
    110  text1          10       12                        .  PUNCT            
    111  text1          11        1                       Ìû  PROPN            
    112  text1          11        2                 ãîâîðèëè   NOUN            
    113  text1          11        3                      äâà  PROPN            
    114  text1          11        4                     ÷àñà  PROPN            
    115  text1          11        5                        ,  PUNCT            
    116  text1          11        6                       äî    ADP            
    117  text1          11        7                     òîãî  PROPN            
    118  text1          11        8                      êàê  PROPN            
    119  text1          11        9                        ,  PUNCT            
    120  text1          11       10               áèáëèîòåêà   NOUN            
    121  text1          11       11                çàêðûëàñü   NOUN            
    122  text1          11       12                        .  PUNCT            
    123  text1          12        1                       Ìû  PROPN            
    124  text1          12        2                    ñòàëè    ADJ            
    125  text1          12        3                  ëó÷øèìè   NOUN            
    126  text1          12        4                ïîäðóãàìè   NOUN            
    127  text1          12        5                        ,  PUNCT            
    128  text1          12        6                        è      X            
    129  text1          12        7                   âñåãäà   VERB            
    130  text1          12        8                       ìû  PROPN            
    131  text1          12        9                   ÷èòàåì  PROPN            
    132  text1          12       10                   ðîìàíû  PROPN            
    133  text1          12       11                   âìåñòå  PROPN            
    134  text1          12       12                        .  PUNCT            
    135  text1          13        1                      Êàê  PROPN            
    136  text1          13        2                    çîâóò  PROPN            
    137  text1          13        3                     òâîþ  PROPN            
    138  text1          13        4                   ëó÷øóþ   NOUN            
    139  text1          13        5                  ïîäðóãó   NOUN            
    140  text1          13        6                        ?  PUNCT            
    141  text1          14        1                       Âû  PROPN            
    142  text1          14        2                        ñ  PROPN            
    143  text1          14        3                      íåé   NOUN            
    144  text1          14        4                ó÷èòåñü_â ENTITY        WORK
    145  text1          14        5             óíèâåðñèòåòå  PROPN            
    146  text1          14        6                        ?  PUNCT            
    147  text1          15        1                      Ãäå  PROPN            
    148  text1          15        2                       âû   PRON            
    149  text1          15        3            ïîçíàêîìèëèñü   VERB            
    150  text1          15        4                     äðóã    ADP            
    151  text1          15        5                        ñ  PROPN            
    152  text1          15        6                   äðóãîì   NOUN            
    153  text1          15        7                        ?  PUNCT            
    154  text1          16        1                  unclear    ADJ            
    155  text1          16        2                      Êàê  PROPN            
    156  text1          16        3                      îíà   NOUN            
    157  text1          16        4                 âûãëÿäèò   NOUN            
    158  text1          16        5                        ?  PUNCT            
    159  text1          17        1                      Îíà  PROPN            
    160  text1          17        2               çàíèìàåòñÿ  PROPN            
    161  text1          17        3                    êàêèì  PROPN            
    162  text1          17        4                        -  PUNCT            
    163  text1          17        5                       òî      X            
    164  text1          17        6                  ñïîðòîì   NOUN            
    165  text1          17        7                        ?  PUNCT            

``` r
nounphrase_extract(parsed_text1) # Mostly incorrect
```

       doc_id sentence_id                                          nounphrase
    1   text1           1                                                Ëàðà
    2   text1           2                                         ß_õî÷ó_òåáå
    3   text1           3                                  è_îíà_î÷åíü_äîáðàÿ
    4   text1           4                                        Îíà_ñðåäíåãî
    5   text1           4                    ó_íå¸_êîðîòêèå_êàøòàíîâûå_âîëîñû
    6   text1           5                                   Âåðà_ëþáèò_ìóçûêó
    7   text1           5                                                  ìû
    8   text1           5                                                äà¸ì
    9   text1           5                                               íîâûå
    10  text1           5                                             ê_äðóãó
    11  text1           5                          íî_÷àùå_îíà_ìíå_äà¸ò_ïåñíè
    12  text1           5                                ïîòîìó_÷òî_îíà_çíàåò
    13  text1           5                                              áîëüøå
    14  text1           5                                            î_ìóçûêå
    15  text1           6                                              Ó_Âåðû
    16  text1           6                                   ñïîðòèâíàÿ_ôèãóðà
    17  text1           6                                             ÷òî_îíà
    18  text1           6                                            â_õîêêåé
    19  text1           7                                                 Îíà
    20  text1           7                              è_unclear_ëþáèò_áåãàòü
    21  text1           8             Ìû_ïîçíàêîìèëèñü_â_øêîëå_ïÿòü_ëåò_íàçàä
    22  text1           8                                           êîãäà_íàì
    23  text1           8                                            ëåò_áûëè
    24  text1           9 Ìû_áûëè_â_áèáëèîòåêå,_è_ÿ_÷èòàëà_ðîìàí_Äîñòîåâñêîãî
    25  text1          10                      Îíà_âèäåë_ðîìàí_è_ñêàçàëà,_÷òî
    26  text1          10                                            íðàâèòñÿ
    27  text1          11                                Ìû_ãîâîðèëè_äâà_÷àñà
    28  text1          11                                            òîãî_êàê
    29  text1          11                                áèáëèîòåêà_çàêðûëàñü
    30  text1          12                          Ìû_ñòàëè_ëó÷øèìè_ïîäðóãàìè
    31  text1          12                             ìû_÷èòàåì_ðîìàíû_âìåñòå
    32  text1          13                       Êàê_çîâóò_òâîþ_ëó÷øóþ_ïîäðóãó
    33  text1          14                                    Âû_ñ_íåé_ó÷èòåñü
    34  text1          14                                      â_óíèâåðñèòåòå
    35  text1          15                                                  âû
    36  text1          15                                            ñ_äðóãîì
    37  text1          16                            unclear_Êàê_îíà_âûãëÿäèò
    38  text1          17                     Îíà_çàíèìàåòñÿ_êàêèì-òî_ñïîðòîì

``` r
nounphrase_consolidate(parsed_text1) #Too many mistakes for the Russian lge
```

       doc_id sentence_id token_id
    1   text1           1        1
    2   text1           1        2
    3   text1           1        3
    4   text1           2        1
    5   text1           2        2
    6   text1           2        3
    7   text1           2        4
    8   text1           2        5
    9   text1           2        6
    10  text1           2        7
    11  text1           3        1
    12  text1           3        2
    13  text1           3        3
    14  text1           3        4
    15  text1           3        5
    16  text1           3        6
    17  text1           4        1
    18  text1           4        2
    19  text1           4        3
    20  text1           4        4
    21  text1           4        5
    22  text1           5        1
    23  text1           5        2
    24  text1           5        3
    25  text1           5        4
    26  text1           5        5
    27  text1           5        6
    28  text1           5        7
    29  text1           5        8
    30  text1           5        9
    31  text1           5       10
    32  text1           5       11
    33  text1           5       12
    34  text1           5       13
    35  text1           5       14
    36  text1           5       15
    37  text1           5       16
    38  text1           5       17
    39  text1           5       18
    40  text1           6        1
    41  text1           6        2
    42  text1           6        3
    43  text1           6        4
    44  text1           6        5
    45  text1           6        6
    46  text1           6        7
    47  text1           6        8
    48  text1           6        9
    49  text1           7        1
    50  text1           7        2
    51  text1           7        3
    52  text1           7        4
    53  text1           8        1
    54  text1           8        2
    55  text1           8        3
    56  text1           8        4
    57  text1           8        5
    58  text1           8        6
    59  text1           9        1
    60  text1           9        2
    61  text1          10        1
    62  text1          10        2
    63  text1          10        3
    64  text1          10        4
    65  text1          10        5
    66  text1          10        6
    67  text1          11        1
    68  text1          11        2
    69  text1          11        3
    70  text1          11        4
    71  text1          11        5
    72  text1          11        6
    73  text1          11        7
    74  text1          12        1
    75  text1          12        2
    76  text1          12        3
    77  text1          12        4
    78  text1          12        5
    79  text1          12        6
    80  text1          13        1
    81  text1          13        2
    82  text1          14        1
    83  text1          14        2
    84  text1          14        3
    85  text1          15        1
    86  text1          15        2
    87  text1          15        3
    88  text1          15        4
    89  text1          15        5
    90  text1          15        6
    91  text1          16        1
    92  text1          16        2
    93  text1          17        1
    94  text1          17        2
                                                     token        pos
    1                                              Äîðîãàÿ       VERB
    2                                                 Ëàðà nounphrase
    3                                                    !      PUNCT
    4                                          ß_õî÷ó_òåáå nounphrase
    5                                         ðàññêàçûâàòü        ADJ
    6                                                    î          X
    7                                                 ìîåé        ADJ
    8                                               ëó÷øåé      PROPN
    9                                              ïîäðóãå      PROPN
    10                                                   .      PUNCT
    11                                                  Å¸       VERB
    12                                               çîâóò      PROPN
    13                                                Âåðà      PROPN
    14                                                   ,      PUNCT
    15                                  è_îíà_î÷åíü_äîáðàÿ nounphrase
    16                                                   .      PUNCT
    17                                        Îíà_ñðåäíåãî nounphrase
    18                                               ðîñòà       VERB
    19                                                   è        DET
    20                    ó_íå¸_êîðîòêèå_êàøòàíîâûå_âîëîñû nounphrase
    21                                                   .      PUNCT
    22                                   Âåðà_ëþáèò_ìóçûêó nounphrase
    23                                                   ,      PUNCT
    24                                                   è          X
    25                                               ÷àñòî       VERB
    26                                                  ìû nounphrase
    27                                                äà¸ì nounphrase
    28                                               íîâûå nounphrase
    29                                               ïåñíè      PROPN
    30                                                äðóã        ADP
    31                                             ê_äðóãó nounphrase
    32                                                   ,      PUNCT
    33                          íî_÷àùå_îíà_ìíå_äà¸ò_ïåñíè nounphrase
    34                                                   ,      PUNCT
    35                                ïîòîìó_÷òî_îíà_çíàåò nounphrase
    36                                              áîëüøå nounphrase
    37                                                ìåíÿ      PROPN
    38                                            î_ìóçûêå nounphrase
    39                                                   .      PUNCT
    40                                              Ó_Âåðû nounphrase
    41                                                åñòü       VERB
    42                                   ñïîðòèâíàÿ_ôèãóðà nounphrase
    43                                                   ,      PUNCT
    44                                              ïîòîìó       VERB
    45                                             ÷òî_îíà nounphrase
    46                                              èãðàåò       NOUN
    47                                            â_õîêêåé nounphrase
    48                                                   .      PUNCT
    49                                                 Îíà nounphrase
    50                                             ñèëüíàÿ       VERB
    51                              è_unclear_ëþáèò_áåãàòü nounphrase
    52                                                   .      PUNCT
    53             Ìû_ïîçíàêîìèëèñü_â_øêîëå_ïÿòü_ëåò_íàçàä nounphrase
    54                                                   ,      PUNCT
    55                                           êîãäà_íàì nounphrase
    56                                        ÷åòûðíàäöàòü       VERB
    57                                            ëåò_áûëè nounphrase
    58                                                   .      PUNCT
    59 Ìû_áûëè_â_áèáëèîòåêå,_è_ÿ_÷èòàëà_ðîìàí_Äîñòîåâñêîãî nounphrase
    60                                                   .      PUNCT
    61                      Îíà_âèäåë_ðîìàí_è_ñêàçàëà,_÷òî nounphrase
    62                                                  åé        ADP
    63                                            íðàâèòñÿ nounphrase
    64                                             ðóññêàÿ       VERB
    65                                          ëèòåðàòóðà      PROPN
    66                                                   .      PUNCT
    67                                Ìû_ãîâîðèëè_äâà_÷àñà nounphrase
    68                                                   ,      PUNCT
    69                                                  äî        ADP
    70                                            òîãî_êàê nounphrase
    71                                                   ,      PUNCT
    72                                áèáëèîòåêà_çàêðûëàñü nounphrase
    73                                                   .      PUNCT
    74                          Ìû_ñòàëè_ëó÷øèìè_ïîäðóãàìè nounphrase
    75                                                   ,      PUNCT
    76                                                   è          X
    77                                              âñåãäà       VERB
    78                             ìû_÷èòàåì_ðîìàíû_âìåñòå nounphrase
    79                                                   .      PUNCT
    80                       Êàê_çîâóò_òâîþ_ëó÷øóþ_ïîäðóãó nounphrase
    81                                                   ?      PUNCT
    82                                    Âû_ñ_íåé_ó÷èòåñü nounphrase
    83                                      â_óíèâåðñèòåòå nounphrase
    84                                                   ?      PUNCT
    85                                                 Ãäå      PROPN
    86                                                  âû nounphrase
    87                                       ïîçíàêîìèëèñü       VERB
    88                                                äðóã        ADP
    89                                            ñ_äðóãîì nounphrase
    90                                                   ?      PUNCT
    91                            unclear_Êàê_îíà_âûãëÿäèò nounphrase
    92                                                   ?      PUNCT
    93                     Îíà_çàíèìàåòñÿ_êàêèì-òî_ñïîðòîì nounphrase
    94                                                   ?      PUNCT

``` r
#Integration with quanted
ntoken(parsed_text1) #That is more than number of words. I guess it includes all the punctuation
```

    text1 
      174 

``` r
ntype(parsed_text1)
```

    text1 
      114 

``` r
#How to select only nouns
spacy_parse(text1, pos = TRUE) %>%
    as.tokens(include_pos = "pos") %>%
    tokens_select(pattern = c("*/NOUN")) #It shows everything as a noun. It is crazy
```

    Tokens consisting of 1 document.
    text1 :
     [1] "Ëàðà/NOUN"     "õî÷ó/NOUN"     "òåáå/NOUN"     "ñðåäíåãî/NOUN"
     [5] "ëþáèò/NOUN"    "äà¸ì/NOUN"     "íîâûå/NOUN"    "äðóãó/NOUN"   
     [9] "÷àùå/NOUN"     "îíà/NOUN"      "èãðàåò/NOUN"   "â/NOUN"       
    [ ... and 25 more ]

Let me try the udpipe package.

``` r
#install.packages("udpipe")
library(udpipe)
#dl <- udpipe_download_model(language = "russian")
#The website says that I need to load the model https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
udmodel_rus <- udpipe_load_model(file = "additional_documents/russian-gsd-ud-2.5-191206.udpipe")

y <- udpipe_annotate(udmodel_rus, x = text1_df$text)
y <- as.data.frame(y)
y #I think I like this ouput. Let's continue
```

        doc_id paragraph_id sentence_id
    1     doc1            1           1
    2     doc1            1           1
    3     doc1            1           1
    4     doc1            1           2
    5     doc1            1           2
    6     doc1            1           2
    7     doc1            1           2
    8     doc1            1           2
    9     doc1            1           2
    10    doc1            1           2
    11    doc1            1           2
    12    doc1            1           2
    13    doc1            1           3
    14    doc1            1           3
    15    doc1            1           3
    16    doc1            1           3
    17    doc1            1           3
    18    doc1            1           3
    19    doc1            1           3
    20    doc1            1           3
    21    doc1            1           3
    22    doc1            1           4
    23    doc1            1           4
    24    doc1            1           4
    25    doc1            1           4
    26    doc1            1           4
    27    doc1            1           4
    28    doc1            1           4
    29    doc1            1           4
    30    doc1            1           4
    31    doc1            1           4
    32    doc1            1           5
    33    doc1            1           5
    34    doc1            1           5
    35    doc1            1           5
    36    doc1            1           5
    37    doc1            1           5
    38    doc1            1           5
    39    doc1            1           5
    40    doc1            1           5
    41    doc1            1           5
    42    doc1            1           5
    43    doc1            1           5
    44    doc1            1           5
    45    doc1            1           5
    46    doc1            1           5
    47    doc1            1           5
    48    doc1            1           5
    49    doc1            1           5
    50    doc1            1           5
    51    doc1            1           5
    52    doc1            1           5
    53    doc1            1           5
    54    doc1            1           5
    55    doc1            1           5
    56    doc1            1           5
    57    doc1            1           5
    58    doc1            1           5
    59    doc1            1           5
    60    doc1            1           5
    61    doc1            1           5
    62    doc1            1           6
    63    doc1            1           6
    64    doc1            1           6
    65    doc1            1           6
    66    doc1            1           6
    67    doc1            1           6
    68    doc1            1           6
    69    doc1            1           6
    70    doc1            1           6
    71    doc1            1           6
    72    doc1            1           6
    73    doc1            1           6
    74    doc1            1           6
    75    doc1            1           7
    76    doc1            1           7
    77    doc1            1           7
    78    doc1            1           7
    79    doc1            1           7
    80    doc1            1           7
    81    doc1            1           8
    82    doc1            1           8
    83    doc1            1           8
    84    doc1            1           8
    85    doc1            1           8
    86    doc1            1           8
    87    doc1            1           8
    88    doc1            1           8
    89    doc1            1           8
    90    doc1            1           8
    91    doc1            1           8
    92    doc1            1           8
    93    doc1            1           8
    94    doc1            1           8
    95    doc1            1           9
    96    doc1            1           9
    97    doc1            1           9
    98    doc1            1           9
    99    doc1            1           9
    100   doc1            1           9
    101   doc1            1           9
    102   doc1            1           9
    103   doc1            1           9
    104   doc1            1           9
    105   doc1            1           9
    106   doc1            1          10
    107   doc1            1          10
    108   doc1            1          10
    109   doc1            1          10
    110   doc1            1          10
    111   doc1            1          10
    112   doc1            1          10
    113   doc1            1          10
    114   doc1            1          10
    115   doc1            1          10
    116   doc1            1          10
    117   doc1            1          10
    118   doc1            1          11
    119   doc1            1          11
    120   doc1            1          11
    121   doc1            1          11
    122   doc1            1          11
    123   doc1            1          11
    124   doc1            1          11
    125   doc1            1          11
    126   doc1            1          11
    127   doc1            1          11
    128   doc1            1          11
    129   doc1            1          11
    130   doc1            1          12
    131   doc1            1          12
    132   doc1            1          12
    133   doc1            1          12
    134   doc1            1          12
    135   doc1            1          12
    136   doc1            1          12
    137   doc1            1          12
    138   doc1            1          12
    139   doc1            1          12
    140   doc1            1          12
    141   doc1            1          12
    142   doc1            1          13
    143   doc1            1          13
    144   doc1            1          13
    145   doc1            1          13
    146   doc1            1          13
    147   doc1            1          13
    148   doc1            1          14
    149   doc1            1          14
    150   doc1            1          14
    151   doc1            1          14
    152   doc1            1          14
    153   doc1            1          14
    154   doc1            1          14
    155   doc1            1          15
    156   doc1            1          15
    157   doc1            1          15
    158   doc1            1          15
    159   doc1            1          15
    160   doc1            1          15
    161   doc1            1          15
    162   doc1            1          16
    163   doc1            1          16
    164   doc1            1          16
    165   doc1            1          16
    166   doc1            1          17
    167   doc1            1          17
    168   doc1            1          17
    169   doc1            1          17
    170   doc1            1          17
                                                                                                                                   sentence
    1                                                                                                                         Äîðîãàÿ Ëàðà!
    2                                                                                                                         Äîðîãàÿ Ëàðà!
    3                                                                                                                         Äîðîãàÿ Ëàðà!
    4                                                                                       ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    5                                                                                       ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    6                                                                                       ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    7                                                                                       ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    8                                                                                       ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    9                                                                                       ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    10                                                                                      ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    11                                                                                      ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    12                                                                                      ß õî÷ó òåáå ðàññêàçûâàòü î ìîåé ëó÷øåé ïîäðóãå.
    13                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    14                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    15                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    16                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    17                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    18                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    19                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    20                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    21                                                                                                   Å¸ çîâóò Âåðà, è îíà î÷åíü äîáðàÿ.
    22                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    23                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    24                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    25                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    26                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    27                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    28                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    29                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    30                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    31                                                                               Îíà ñðåäíåãî ðîñòà è ó íå¸ êîðîòêèå êàøòàíîâûå âîëîñû.
    32  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    33  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    34  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    35  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    36  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    37  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    38  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    39  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    40  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    41  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    42  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    43  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    44  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    45  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    46  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    47  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    48  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    49  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    50  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    51  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    52  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    53  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    54  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    55  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    56  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    57  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    58  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    59  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    60  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    61  Âåðà ëþáèò ìóçûêó, è ÷àñòî ìû äà¸ì íîâûå ïåñíè äðóã ê äðóãó, íî ÷àùå îíà ìíå äà¸ò ïåñíè, ïîòîìó ÷òî îíà çíàåò áîëüøå ìåíÿ î ìóçûêå.
    62                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    63                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    64                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    65                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    66                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    67                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    68                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    69                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    70                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    71                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    72                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    73                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    74                                                                       Ó Âåðû åñòü ñïîðòèâíàÿ ôèãóðà, ïîòîìó ÷òî îíà èãðàåò â õîêêåé.
    75                                                                                                          Îíà ñèëüíàÿ è ëþáèò áåãàòü.
    76                                                                                                          Îíà ñèëüíàÿ è ëþáèò áåãàòü.
    77                                                                                                          Îíà ñèëüíàÿ è ëþáèò áåãàòü.
    78                                                                                                          Îíà ñèëüíàÿ è ëþáèò áåãàòü.
    79                                                                                                          Îíà ñèëüíàÿ è ëþáèò áåãàòü.
    80                                                                                                          Îíà ñèëüíàÿ è ëþáèò áåãàòü.
    81                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    82                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    83                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    84                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    85                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    86                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    87                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    88                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    89                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    90                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    91                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    92                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    93                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    94                                                            Ìû ïîçíàêîìèëèñü â øêîëå ïÿòü ëåò íàçàä, êîãäà íàì ÷åòûðíàäöàòü ëåò áûëè.
    95                                                                                 Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    96                                                                                 Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    97                                                                                 Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    98                                                                                 Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    99                                                                                 Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    100                                                                                Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    101                                                                                Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    102                                                                                Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    103                                                                                Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    104                                                                                Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    105                                                                                Ìû áûëè â áèáëèîòåêå, è ÿ ÷èòàëà ðîìàí Äîñòîåâñêîãî.
    106                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    107                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    108                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    109                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    110                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    111                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    112                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    113                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    114                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    115                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    116                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    117                                                                      Îíà âèäåë ðîìàí è ñêàçàëà, ÷òî åé íðàâèòñÿ ðóññêàÿ ëèòåðàòóðà.
    118                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    119                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    120                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    121                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    122                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    123                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    124                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    125                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    126                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    127                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    128                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    129                                                                            Ìû ãîâîðèëè äâà ÷àñà, äî òîãî êàê, áèáëèîòåêà çàêðûëàñü.
    130                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    131                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    132                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    133                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    134                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    135                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    136                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    137                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    138                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    139                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    140                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    141                                                                       Ìû ñòàëè ëó÷øèìè ïîäðóãàìè, è âñåãäà ìû ÷èòàåì ðîìàíû âìåñòå.
    142                                                                                                      Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?
    143                                                                                                      Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?
    144                                                                                                      Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?
    145                                                                                                      Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?
    146                                                                                                      Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?
    147                                                                                                      Êàê çîâóò òâîþ ëó÷øóþ ïîäðóãó?
    148                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    149                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    150                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    151                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    152                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    153                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    154                                                                                                    Âû ñ íåé ó÷èòåñü â óíèâåðñèòåòå?
    155                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    156                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    157                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    158                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    159                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    160                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    161                                                                                                 Ãäå âû ïîçíàêîìèëèñü äðóã ñ äðóãîì?
    162                                                                                                                   Êàê îíà âûãëÿäèò?
    163                                                                                                                   Êàê îíà âûãëÿäèò?
    164                                                                                                                   Êàê îíà âûãëÿäèò?
    165                                                                                                                   Êàê îíà âûãëÿäèò?
    166                                                                                                    Îíà çàíèìàåòñÿ êàêèì-òî ñïîðòîì?
    167                                                                                                    Îíà çàíèìàåòñÿ êàêèì-òî ñïîðòîì?
    168                                                                                                    Îíà çàíèìàåòñÿ êàêèì-òî ñïîðòîì?
    169                                                                                                    Îíà çàíèìàåòñÿ êàêèì-òî ñïîðòîì?
    170                                                                                                    Îíà çàíèìàåòñÿ êàêèì-òî ñïîðòîì?
        token_id         token         lemma  upos xpos
    1          1       Äîðîãàÿ       Äîðîãîé   ADJ  JJL
    2          2          Ëàðà          Ëàðà  NOUN   NN
    3          3             !             ! PUNCT    .
    4          1             ß             ÿ  PRON  PRP
    5          2          õî÷ó          õî÷ó   ADV   RB
    6          3          òåáå            òû  PRON  PRP
    7          4  ðàññêàçûâàòü  ðàññêàçûâàòü  VERB   VB
    8          5             î             î   ADP   IN
    9          6          ìîåé           ìîé   DET PRP$
    10         7        ëó÷øåé        ëó÷øèé   ADJ  JJS
    11         8       ïîäðóãå        ïîäðóã  NOUN   NN
    12         9             .             . PUNCT    .
    13         1            Å¸           îíà  PRON  PRP
    14         2         çîâóò          çîòü  VERB  VBC
    15         3          Âåðà          Âåðà PROPN  NNP
    16         4             ,             , PUNCT    ,
    17         5             è             è CCONJ   CC
    18         6           îíà           îíà  PRON  PRP
    19         7         î÷åíü         î÷åíü   ADV   RB
    20         8        äîáðàÿ        äîáðûé   ADJ  JJL
    21         9             .             . PUNCT    .
    22         1           Îíà           îíà  PRON  PRP
    23         2      ñðåäíåãî       ñðåäíèé   ADJ  JJL
    24         3         ðîñòà          ðîñò  NOUN   NN
    25         4             è             è CCONJ   CC
    26         5             ó             ó   ADP   IN
    27         6           íå¸           îíà  PRON  PRP
    28         7      êîðîòêèå      êîðîòêèé   ADJ  JJL
    29         8    êàøòàíîâûå    êàøòàíîâûé   ADJ  JJL
    30         9        âîëîñû         âîëîñ  NOUN   NN
    31        10             .             . PUNCT    .
    32         1          Âåðà          âåðà PROPN  NNP
    33         2         ëþáèò        ëþáèòü  VERB  VBC
    34         3        ìóçûêó        ìóçûêà  NOUN   NN
    35         4             ,             , PUNCT    ,
    36         5             è             è CCONJ   CC
    37         6         ÷àñòî         ÷àñòî   ADV   RB
    38         7            ìû            ìû  PRON  PRP
    39         8          äà¸ì          äàåì   ADV   RB
    40         9         íîâûå         íîâûé   ADJ  JJL
    41        10         ïåñíè         ïåñíÿ  NOUN   NN
    42        11          äðóã          äðóã  NOUN   NN
    43        12             ê             ê   ADP   IN
    44        13         äðóãó          äðóã  NOUN   NN
    45        14             ,             , PUNCT    ,
    46        15            íî            íî CCONJ   CC
    47        16          ÷àùå         ÷àñòî   ADV  RBR
    48        17           îíà           îíà  PRON  PRP
    49        18           ìíå             ÿ  PRON  PRP
    50        19          äà¸ò        äàâàòü  VERB  VBC
    51        20         ïåñíè         ïåñíÿ  NOUN   NN
    52        21             ,             , PUNCT    ,
    53        22        ïîòîìó        ïîòîìó SCONJ   IN
    54        23           ÷òî           ÷òî SCONJ   IN
    55        24           îíà           îíà  PRON  PRP
    56        25         çíàåò         çíàòü  VERB  VBC
    57        26        áîëüøå        áîëüøå   ADV  RBR
    58        27          ìåíÿ             ÿ  PRON  PRP
    59        28             î             î   ADP   IN
    60        29        ìóçûêå        ìóçûêà  NOUN   NN
    61        30             .             . PUNCT    .
    62         1             Ó             ó   ADP   IN
    63         2          Âåðû          âåðà PROPN  NNP
    64         3          åñòü          áûòü  VERB  VBC
    65         4    ñïîðòèâíàÿ    ñïîðòèâíûé   ADJ  JJL
    66         5        ôèãóðà        ôèãóðà  NOUN   NN
    67         6             ,             , PUNCT    ,
    68         7        ïîòîìó        ïîòîìó SCONJ   IN
    69         8           ÷òî           ÷òî SCONJ   IN
    70         9           îíà           îíà  PRON  PRP
    71        10        èãðàåò        èãðàòü  VERB  VBC
    72        11             â             â   ADP   IN
    73        12        õîêêåé         õîêêü   ADJ  JJL
    74        13             .             . PUNCT    .
    75         1           Îíà           îíà  PRON  PRP
    76         2       ñèëüíàÿ       ñèëüíûé   ADJ  JJL
    77         3             è             è CCONJ   CC
    78         4         ëþáèò        ëþáèòü  VERB VBNH
    79         5        áåãàòü        áåãàòü  VERB   VB
    80         6             .             . PUNCT    .
    81         1            Ìû            ìû  PRON  PRP
    82         2 ïîçíàêîìèëèñü ïîçíàêîìèòüñÿ  VERB  VBC
    83         3             â             â   ADP   IN
    84         4         øêîëå         øêîëà  NOUN   NN
    85         5          ïÿòü          ïÿòü   NUM   CD
    86         6           ëåò           ãîä  NOUN   NN
    87         7         íàçàä         íàçàä   ADV   RB
    88         8             ,             , PUNCT    ,
    89         9         êîãäà         êîãäà   ADV  WRB
    90        10           íàì            ìû  PRON  PRP
    91        11  ÷åòûðíàäöàòü  ÷åòûðíàäöàòü   NUM   CD
    92        12           ëåò           ãîä  NOUN   NN
    93        13          áûëè          áûòü   AUX  VBC
    94        14             .             . PUNCT    .
    95         1            Ìû            ìû  PRON  PRP
    96         2          áûëè          áûòü   AUX  VBC
    97         3             â             â   ADP   IN
    98         4    áèáëèîòåêå    áèáëèîòåêà  NOUN   NN
    99         5             ,             , PUNCT    ,
    100        6             è             è CCONJ   CC
    101        7             ÿ             ÿ  PRON  PRP
    102        8        ÷èòàëà        ÷èòàòü  VERB  VBC
    103        9         ðîìàí         ðîìàí  NOUN   NN
    104       10  Äîñòîåâñêîãî   Äîñòîåâñêèé   ADJ  JJL
    105       11             .             . PUNCT    .
    106        1           Îíà           îíà  PRON  PRP
    107        2         âèäåë        âèäåòü  VERB  VBC
    108        3         ðîìàí         ðîìàí  NOUN   NN
    109        4             è             è CCONJ   CC
    110        5       ñêàçàëà       ñêàçàòü  VERB  VBC
    111        6             ,             , PUNCT    ,
    112        7           ÷òî           ÷òî SCONJ   IN
    113        8            åé           îíà  PRON  PRP
    114        9      íðàâèòñÿ     íðàâèòüñÿ  VERB  VBC
    115       10       ðóññêàÿ       ðóññêèé   ADJ  JJL
    116       11    ëèòåðàòóðà    ëèòåðàòóðà  NOUN   NN
    117       12             .             . PUNCT    .
    118        1            Ìû            ìû  PRON  PRP
    119        2      ãîâîðèëè      ãîâîðèòü  VERB  VBC
    120        3           äâà           äâà   NUM   CD
    121        4          ÷àñà           ÷àñ  NOUN   NN
    122        5             ,             , PUNCT    ,
    123        6            äî            äî   ADP   IN
    124        7          òîãî            òî  PRON   DT
    125        8           êàê           êàê SCONJ   IN
    126        9             ,             , PUNCT    ,
    127       10    áèáëèîòåêà    áèáëèîòåêà  NOUN   NN
    128       11     çàêðûëàñü     çàêðûòüñÿ  VERB  VBC
    129       12             .             . PUNCT    .
    130        1            Ìû            ìû  PRON  PRP
    131        2         ñòàëè         ñòàòü  VERB  VBC
    132        3       ëó÷øèìè        ëó÷øèé   ADJ  JJS
    133        4     ïîäðóãàìè        ïîäðóã  NOUN   NN
    134        5             ,             , PUNCT    ,
    135        6             è             è CCONJ   CC
    136        7        âñåãäà        âñåãäà   ADV   RB
    137        8            ìû            ìû  PRON  PRP
    138        9        ÷èòàåì        ÷èòàòü  VERB  VBC
    139       10        ðîìàíû         ðîìàí  NOUN   NN
    140       11        âìåñòå        âìåñòå   ADV   RB
    141       12             .             . PUNCT    .
    142        1           Êàê           êàê SCONJ   IN
    143        2         çîâóò         çîâóò  VERB  VBC
    144        3          òâîþ          òâîé   DET PRP$
    145        4        ëó÷øóþ        ëó÷øèé   ADJ  JJL
    146        5       ïîäðóãó        ïîäðóã  NOUN   NN
    147        6             ?             ? PUNCT    .
    148        1            Âû            âû  PRON  PRP
    149        2             ñ             ñ   ADP   IN
    150        3           íåé           îíà  PRON  PRP
    151        4       ó÷èòåñü       ó÷èòüñÿ  VERB  VBC
    152        5             â             â   ADP   IN
    153        6  óíèâåðñèòåòå   óíèâåðñèòåò  NOUN   NN
    154        7             ?             ? PUNCT    .
    155        1           Ãäå           ãäå SCONJ   IN
    156        2            âû            âû  PRON  PRP
    157        3 ïîçíàêîìèëèñü ïîçíàêîìèòüñÿ  VERB  VBC
    158        4          äðóã          äðóã  NOUN   NN
    159        5             ñ             ñ   ADP   IN
    160        6        äðóãîì          äðóã  NOUN   NN
    161        7             ?             ? PUNCT    .
    162        1           Êàê           êàê   ADP   IN
    163        2           îíà           îíà  PRON  PRP
    164        3      âûãëÿäèò     âûãëÿäåòü  VERB  VBC
    165        4             ?             ? PUNCT    .
    166        1           Îíà           îíà  PRON  PRP
    167        2    çàíèìàåòñÿ    çàíèìàòüñÿ  VERB  VBC
    168        3      êàêèì-òî      êàêèì-òî   DET   DT
    169        4       ñïîðòîì         ñïîðò  NOUN   NN
    170        5             ?             ? PUNCT    .
                                                                                                              feats
    1                                                                    Case=Nom|Degree=Pos|Gender=Fem|Number=Sing
    2                                                                  Animacy=Inan|Case=Nom|Gender=Fem|Number=Sing
    3                                                                                                          <NA>
    4                                                                                 Case=Nom|Number=Sing|Person=1
    5                                                                                                    Degree=Pos
    6                                                                                 Case=Dat|Number=Sing|Person=2
    7                                                                             Aspect=Imp|VerbForm=Inf|Voice=Act
    8                                                                                                          <NA>
    9                                                                               Case=Loc|Gender=Fem|Number=Sing
    10                                                                   Case=Gen|Degree=Pos|Gender=Fem|Number=Sing
    11                                                                 Animacy=Inan|Case=Dat|Gender=Fem|Number=Sing
    12                                                                                                         <NA>
    13                                                                     Case=Acc|Gender=Fem|Number=Sing|Person=3
    14                                   Aspect=Imp|Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    15                                                                Animacy=Anim|Case=Gen|Gender=Masc|Number=Sing
    16                                                                                                         <NA>
    17                                                                                                         <NA>
    18                                                                     Case=Nom|Gender=Fem|Number=Sing|Person=3
    19                                                                                                   Degree=Pos
    20                                                                   Case=Nom|Degree=Pos|Gender=Fem|Number=Sing
    21                                                                                                         <NA>
    22                                                                     Case=Nom|Gender=Fem|Number=Sing|Person=3
    23                                                                  Case=Gen|Degree=Pos|Gender=Masc|Number=Sing
    24                                                                Animacy=Inan|Case=Gen|Gender=Masc|Number=Sing
    25                                                                                                         <NA>
    26                                                                                                         <NA>
    27                                                                     Case=Gen|Gender=Fem|Number=Sing|Person=3
    28                                                                              Case=Nom|Degree=Pos|Number=Plur
    29                                                                              Case=Nom|Degree=Pos|Number=Plur
    30                                                                Animacy=Inan|Case=Nom|Gender=Masc|Number=Plur
    31                                                                                                         <NA>
    32                                                                 Animacy=Anim|Case=Nom|Gender=Fem|Number=Sing
    33                                   Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    34                                                                 Animacy=Inan|Case=Acc|Gender=Fem|Number=Sing
    35                                                                                                         <NA>
    36                                                                                                         <NA>
    37                                                                                                   Degree=Pos
    38                                                                                Case=Nom|Number=Plur|Person=1
    39                                                                                                   Degree=Pos
    40                                                                 Animacy=Inan|Case=Acc|Degree=Pos|Number=Plur
    41                                                                 Animacy=Inan|Case=Acc|Gender=Fem|Number=Plur
    42                                                                Animacy=Anim|Case=Nom|Gender=Masc|Number=Sing
    43                                                                                                         <NA>
    44                                                                Animacy=Anim|Case=Dat|Gender=Masc|Number=Sing
    45                                                                                                         <NA>
    46                                                                                                         <NA>
    47                                                                                                   Degree=Cmp
    48                                                                     Case=Nom|Gender=Fem|Number=Sing|Person=3
    49                                                                                Case=Dat|Number=Sing|Person=1
    50                                   Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    51                                                                 Animacy=Inan|Case=Acc|Gender=Fem|Number=Plur
    52                                                                                                         <NA>
    53                                                                                                         <NA>
    54                                                                                                         <NA>
    55                                                                     Case=Nom|Gender=Fem|Number=Sing|Person=3
    56                                   Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    57                                                                                                   Degree=Cmp
    58                                                                                Case=Gen|Number=Sing|Person=1
    59                                                                                                         <NA>
    60                                                                 Animacy=Anim|Case=Loc|Gender=Fem|Number=Sing
    61                                                                                                         <NA>
    62                                                                                                         <NA>
    63                                                                Animacy=Anim|Case=Gen|Gender=Masc|Number=Sing
    64                                   Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    65                                                                   Case=Nom|Degree=Pos|Gender=Fem|Number=Sing
    66                                                                 Animacy=Inan|Case=Nom|Gender=Fem|Number=Sing
    67                                                                                                         <NA>
    68                                                                                                         <NA>
    69                                                                                                         <NA>
    70                                                                     Case=Nom|Gender=Fem|Number=Sing|Person=3
    71                                   Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    72                                                                                                         <NA>
    73                                                                   Case=Loc|Degree=Pos|Gender=Fem|Number=Sing
    74                                                                                                         <NA>
    75                                                                     Case=Nom|Gender=Fem|Number=Sing|Person=3
    76                                                                   Case=Nom|Degree=Pos|Gender=Fem|Number=Sing
    77                                                                                                         <NA>
    78  Animacy=Anim|Aspect=Perf|Case=Nom|Gender=Masc|Number=Sing|Tense=Past|Variant=Short|VerbForm=Part|Voice=Pass
    79                                                                            Aspect=Imp|VerbForm=Inf|Voice=Act
    80                                                                                                         <NA>
    81                                                                                Case=Nom|Number=Plur|Person=1
    82                                           Aspect=Perf|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin|Voice=Mid
    83                                                                                                         <NA>
    84                                                                 Animacy=Inan|Case=Loc|Gender=Fem|Number=Sing
    85                                                                                        Case=Acc|NumType=Card
    86                                                                Animacy=Inan|Case=Gen|Gender=Masc|Number=Plur
    87                                                                                                   Degree=Pos
    88                                                                                                         <NA>
    89                                                                                                         <NA>
    90                                                                                Case=Dat|Number=Plur|Person=1
    91                                                                                        Case=Acc|NumType=Card
    92                                                                Animacy=Inan|Case=Gen|Gender=Masc|Number=Plur
    93                                                      Aspect=Imp|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin
    94                                                                                                         <NA>
    95                                                                                Case=Nom|Number=Plur|Person=1
    96                                                      Aspect=Imp|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin
    97                                                                                                         <NA>
    98                                                                 Animacy=Inan|Case=Loc|Gender=Fem|Number=Sing
    99                                                                                                         <NA>
    100                                                                                                        <NA>
    101                                                                               Case=Nom|Number=Sing|Person=1
    102                               Aspect=Perf|Gender=Fem|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin|Voice=Act
    103                                                               Animacy=Inan|Case=Acc|Gender=Masc|Number=Sing
    104                                                                 Case=Gen|Degree=Pos|Gender=Masc|Number=Sing
    105                                                                                                        <NA>
    106                                                                    Case=Nom|Gender=Fem|Number=Sing|Person=3
    107                               Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin|Voice=Act
    108                                                               Animacy=Inan|Case=Acc|Gender=Masc|Number=Sing
    109                                                                                                        <NA>
    110                               Aspect=Perf|Gender=Fem|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin|Voice=Act
    111                                                                                                        <NA>
    112                                                                                                        <NA>
    113                                                                    Case=Dat|Gender=Fem|Number=Sing|Person=3
    114                                  Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Mid
    115                                                                  Case=Nom|Degree=Pos|Gender=Fem|Number=Sing
    116                                                                Animacy=Inan|Case=Nom|Gender=Fem|Number=Sing
    117                                                                                                        <NA>
    118                                                                               Case=Nom|Number=Plur|Person=1
    119                                           Aspect=Imp|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin|Voice=Act
    120                                                              Animacy=Inan|Case=Nom|Gender=Masc|NumType=Card
    121                                                               Animacy=Inan|Case=Gen|Gender=Masc|Number=Sing
    122                                                                                                        <NA>
    123                                                                                                        <NA>
    124                                                               Animacy=Inan|Case=Gen|Gender=Neut|Number=Sing
    125                                                                                                        <NA>
    126                                                                                                        <NA>
    127                                                                Animacy=Inan|Case=Nom|Gender=Fem|Number=Sing
    128                               Aspect=Perf|Gender=Fem|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin|Voice=Mid
    129                                                                                                        <NA>
    130                                                                               Case=Nom|Number=Plur|Person=1
    131                                          Aspect=Perf|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin|Voice=Act
    132                                                                             Case=Ins|Degree=Pos|Number=Plur
    133                                                               Animacy=Inan|Case=Ins|Gender=Masc|Number=Plur
    134                                                                                                        <NA>
    135                                                                                                        <NA>
    136                                                                                                  Degree=Pos
    137                                                                               Case=Nom|Number=Plur|Person=1
    138                                  Aspect=Imp|Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin|Voice=Act
    139                                                               Animacy=Inan|Case=Acc|Gender=Masc|Number=Plur
    140                                                                                                  Degree=Pos
    141                                                                                                        <NA>
    142                                                                                                        <NA>
    143                                  Aspect=Imp|Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    144                                                                             Case=Acc|Gender=Fem|Number=Sing
    145                                                                  Case=Acc|Degree=Pos|Gender=Fem|Number=Sing
    146                                                                Animacy=Inan|Case=Acc|Gender=Fem|Number=Sing
    147                                                                                                        <NA>
    148                                                                               Case=Nom|Number=Plur|Person=2
    149                                                                                                        <NA>
    150                                                                    Case=Loc|Gender=Fem|Number=Sing|Person=3
    151                                             Aspect=Imp|Mood=Imp|Number=Plur|Person=2|VerbForm=Fin|Voice=Mid
    152                                                                                                        <NA>
    153                                                               Animacy=Inan|Case=Loc|Gender=Masc|Number=Sing
    154                                                                                                        <NA>
    155                                                                                                        <NA>
    156                                                                               Case=Nom|Number=Plur|Person=2
    157                                          Aspect=Perf|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin|Voice=Mid
    158                                                               Animacy=Anim|Case=Nom|Gender=Masc|Number=Sing
    159                                                                                                        <NA>
    160                                                               Animacy=Anim|Case=Ins|Gender=Masc|Number=Sing
    161                                                                                                        <NA>
    162                                                                                                        <NA>
    163                                                                    Case=Nom|Gender=Fem|Number=Sing|Person=3
    164                                  Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act
    165                                                                                                        <NA>
    166                                                                    Case=Nom|Gender=Fem|Number=Sing|Person=3
    167                                  Aspect=Imp|Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin|Voice=Mid
    168                                                                            Case=Nom|Gender=Masc|Number=Sing
    169                                                               Animacy=Inan|Case=Ins|Gender=Masc|Number=Sing
    170                                                                                                        <NA>
        head_token_id    dep_rel deps            misc
    1               2       amod <NA>            <NA>
    2               0       root <NA>   SpaceAfter=No
    3               2      punct <NA>            <NA>
    4               4      nsubj <NA>            <NA>
    5               4     advmod <NA>            <NA>
    6               4       iobj <NA>            <NA>
    7               0       root <NA>            <NA>
    8               8       case <NA>            <NA>
    9               8        det <NA>            <NA>
    10              8       amod <NA>            <NA>
    11              4        obl <NA>   SpaceAfter=No
    12              4      punct <NA>            <NA>
    13              2        obj <NA>            <NA>
    14              0       root <NA>            <NA>
    15              2        obj <NA>   SpaceAfter=No
    16              8      punct <NA>            <NA>
    17              8         cc <NA>            <NA>
    18              8      nsubj <NA>            <NA>
    19              8     advmod <NA>            <NA>
    20              2       conj <NA>   SpaceAfter=No
    21              2      punct <NA>            <NA>
    22              3      nsubj <NA>            <NA>
    23              3       amod <NA>            <NA>
    24              0       root <NA>            <NA>
    25              9         cc <NA>            <NA>
    26              6       case <NA>            <NA>
    27              9       nmod <NA>            <NA>
    28              9       amod <NA>            <NA>
    29              9       amod <NA>            <NA>
    30              3       conj <NA>   SpaceAfter=No
    31              3      punct <NA>            <NA>
    32              2      nsubj <NA>            <NA>
    33              0       root <NA>            <NA>
    34              2        obj <NA>   SpaceAfter=No
    35              7      punct <NA>            <NA>
    36              7         cc <NA>            <NA>
    37              7     advmod <NA>            <NA>
    38              2       conj <NA>            <NA>
    39              9     advmod <NA>            <NA>
    40             10       amod <NA>            <NA>
    41              2        obj <NA>            <NA>
    42             10       nmod <NA>            <NA>
    43             11      fixed <NA>            <NA>
    44             11      fixed <NA>   SpaceAfter=No
    45             19      punct <NA>            <NA>
    46             19         cc <NA>            <NA>
    47             19     advmod <NA>            <NA>
    48             19      nsubj <NA>            <NA>
    49             19       iobj <NA>            <NA>
    50              2       conj <NA>            <NA>
    51             19        obj <NA>   SpaceAfter=No
    52             25      punct <NA>            <NA>
    53             25       mark <NA>            <NA>
    54             22      fixed <NA>            <NA>
    55             25      nsubj <NA>            <NA>
    56             19      advcl <NA>            <NA>
    57             27     advmod <NA>            <NA>
    58             25        obj <NA>            <NA>
    59             29       case <NA>            <NA>
    60             27       nmod <NA>   SpaceAfter=No
    61              2      punct <NA>            <NA>
    62              2       case <NA>            <NA>
    63              3        obl <NA>            <NA>
    64              0       root <NA>            <NA>
    65              5       amod <NA>            <NA>
    66              3      nsubj <NA>   SpaceAfter=No
    67             10      punct <NA>            <NA>
    68             10       mark <NA>            <NA>
    69              7      fixed <NA>            <NA>
    70             10      nsubj <NA>            <NA>
    71              3      advcl <NA>            <NA>
    72             12       case <NA>            <NA>
    73             10        obl <NA>   SpaceAfter=No
    74              3      punct <NA>            <NA>
    75              2      nsubj <NA>            <NA>
    76              0       root <NA>            <NA>
    77              4         cc <NA>            <NA>
    78              2       conj <NA>            <NA>
    79              2      xcomp <NA>   SpaceAfter=No
    80              2      punct <NA>            <NA>
    81              2      nsubj <NA>            <NA>
    82              0       root <NA>            <NA>
    83              4       case <NA>            <NA>
    84              2        obl <NA>            <NA>
    85              6 nummod:gov <NA>            <NA>
    86              2        obl <NA>            <NA>
    87              2     advmod <NA>   SpaceAfter=No
    88             12      punct <NA>            <NA>
    89             12     advmod <NA>            <NA>
    90             12        det <NA>            <NA>
    91             12 nummod:gov <NA>            <NA>
    92              2        obl <NA>            <NA>
    93             12      nsubj <NA>   SpaceAfter=No
    94              2      punct <NA>            <NA>
    95              4      nsubj <NA>            <NA>
    96              4        cop <NA>            <NA>
    97              4       case <NA>            <NA>
    98              0       root <NA>   SpaceAfter=No
    99              8      punct <NA>            <NA>
    100             8         cc <NA>            <NA>
    101             8      nsubj <NA>            <NA>
    102             4       conj <NA>            <NA>
    103             8        obj <NA>            <NA>
    104             9       amod <NA>   SpaceAfter=No
    105             4      punct <NA>            <NA>
    106             2      nsubj <NA>            <NA>
    107             0       root <NA>            <NA>
    108             2        obj <NA>            <NA>
    109             5         cc <NA>            <NA>
    110             2       conj <NA>   SpaceAfter=No
    111             9      punct <NA>            <NA>
    112             9       mark <NA>            <NA>
    113             9       iobj <NA>            <NA>
    114             5      ccomp <NA>            <NA>
    115            11       amod <NA>            <NA>
    116             9      nsubj <NA>   SpaceAfter=No
    117             2      punct <NA>            <NA>
    118             2      nsubj <NA>            <NA>
    119             0       root <NA>            <NA>
    120             4 nummod:gov <NA>            <NA>
    121             2        obj <NA>   SpaceAfter=No
    122            11      punct <NA>            <NA>
    123             7       case <NA>            <NA>
    124            11        obl <NA>            <NA>
    125            11       mark <NA>   SpaceAfter=No
    126            11      punct <NA>            <NA>
    127            11      nsubj <NA>            <NA>
    128             4  acl:relcl <NA>   SpaceAfter=No
    129             2      punct <NA>            <NA>
    130             2      nsubj <NA>            <NA>
    131             0       root <NA>            <NA>
    132             4       amod <NA>            <NA>
    133             2        obj <NA>   SpaceAfter=No
    134             9      punct <NA>            <NA>
    135             9         cc <NA>            <NA>
    136             9     advmod <NA>            <NA>
    137             9      nsubj <NA>            <NA>
    138             2       conj <NA>            <NA>
    139             9        obj <NA>            <NA>
    140            10     advmod <NA>   SpaceAfter=No
    141             2      punct <NA>            <NA>
    142             2       mark <NA>            <NA>
    143             0       root <NA>            <NA>
    144             5        det <NA>            <NA>
    145             5       amod <NA>            <NA>
    146             2        obj <NA>   SpaceAfter=No
    147             2      punct <NA>            <NA>
    148             0       root <NA>            <NA>
    149             3       case <NA>            <NA>
    150             4        obl <NA>            <NA>
    151             1        acl <NA>            <NA>
    152             6       case <NA>            <NA>
    153             4        obl <NA>   SpaceAfter=No
    154             4      punct <NA>            <NA>
    155             3       mark <NA>            <NA>
    156             3      nsubj <NA>            <NA>
    157             0       root <NA>            <NA>
    158             3        obl <NA>            <NA>
    159             4      fixed <NA>            <NA>
    160             4      fixed <NA>   SpaceAfter=No
    161             3      punct <NA>            <NA>
    162             2       case <NA>            <NA>
    163             3        obl <NA>            <NA>
    164             0       root <NA>   SpaceAfter=No
    165             3      punct <NA>            <NA>
    166             2      nsubj <NA>            <NA>
    167             0       root <NA>            <NA>
    168             4        det <NA>            <NA>
    169             2        obj <NA>   SpaceAfter=No
    170             2      punct <NA> SpacesAfter=\\n

``` r
#install.packages('rsyntax') #From https://www.rdocumentation.org/packages/rsyntax/versions/0.1.2
library(rsyntax)
```


    Attaching package: 'rsyntax'

    The following objects are masked from 'package:tidyr':

        chop, fill

    The following object is masked from 'package:ggplot2':

        annotate

``` r
tokens = as_tokenindex(y)
plot_tree(tokens, token, lemma, upos) #I have a plot of the first sentence and root and an adjective modifier!
```

    Document: doc1
    Sentence: 1

``` r
tquery(upos = c("VERB", "PROPN"))
```

      n                      upos=(VERB,PROPN)

``` r
tq = tquery(upos = 'VERB', 
            children(relation = 'nsubj'))
direct = tquery(label = 'verb', upos = 'VERB', 
                children(label = 'subject', relation = 'nsubj'),
                children(label = 'object', relation = 'obj'))
tokens = annotate_tqueries(tokens, 'clause', direct)

tokens[,c('doc_id','sentence','token','clause','clause_fill')] #This seems to work out but I do not know how to move from here to the number of clauses in a text
```

         doc_id sentence      token  clause clause_fill
      1:   doc1        1    Äîðîãàÿ    <NA>          NA
      2:   doc1        1       Ëàðà    <NA>          NA
      3:   doc1        1          !    <NA>          NA
      4:   doc1        2          ß    <NA>          NA
      5:   doc1        2       õî÷ó    <NA>          NA
     ---                                               
    166:   doc1       17        Îíà subject           0
    167:   doc1       17 çàíèìàåòñÿ    verb           0
    168:   doc1       17   êàêèì-òî  object           1
    169:   doc1       17    ñïîðòîì  object           0
    170:   doc1       17          ?    verb           1

Can I find all simple clauses? Is it even necessary? But let me try

``` r
#simple = tquery(label = )
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
     [1] rsyntax_0.1.2        udpipe_0.8.6         koRpus.lang.en_0.1-4
     [4] koRpus.lang.ru_0.1-2 koRpus_0.13-8        sylly_0.1-6         
     [7] spacyr_1.2.1         corpus_0.10.2        tidytext_0.3.2      
    [10] htmlwidgets_1.5.4    quanteda_3.1.0       forcats_0.5.1       
    [13] stringr_1.4.0        dplyr_1.0.7          purrr_0.3.4         
    [16] readr_2.0.1          tidyr_1.1.3          tibble_3.1.4        
    [19] ggplot2_3.3.5        tidyverse_1.3.1     

    loaded via a namespace (and not attached):
     [1] sylly.ru_0.1-2     httr_1.4.2         jsonlite_1.7.2     modelr_0.1.8      
     [5] RcppParallel_5.1.4 assertthat_0.2.1   cellranger_1.1.0   yaml_2.2.1        
     [9] pillar_1.6.2       backports_1.2.1    lattice_0.20-44    reticulate_1.22   
    [13] glue_1.4.2         digest_0.6.27      rvest_1.0.1        colorspace_2.0-2  
    [17] htmltools_0.5.2    Matrix_1.3-4       pkgconfig_2.0.3    broom_0.7.9       
    [21] haven_2.4.3        scales_1.1.1       tzdb_0.1.2         generics_0.1.0    
    [25] ellipsis_0.3.2     withr_2.4.2        cli_3.0.1          magrittr_2.0.1    
    [29] crayon_1.4.1       readxl_1.3.1       evaluate_0.14      stopwords_2.2     
    [33] tokenizers_0.2.1   janeaustenr_0.1.5  fs_1.5.0           fansi_0.5.0       
    [37] SnowballC_0.7.0    xml2_1.3.2         tools_4.1.1        data.table_1.14.0 
    [41] hms_1.1.0          lifecycle_1.0.0    munsell_0.5.0      reprex_2.0.1      
    [45] compiler_4.1.1     rlang_0.4.11       grid_4.1.1         rstudioapi_0.13   
    [49] rappdirs_0.3.3     igraph_1.2.8       base64enc_0.1-3    rmarkdown_2.10    
    [53] gtable_0.3.0       DBI_1.1.1          R6_2.5.1           sylly.en_0.1-3    
    [57] lubridate_1.7.10   knitr_1.33         fastmap_1.1.0      utf8_1.2.2        
    [61] fastmatch_1.1-3    stringi_1.7.4      Rcpp_1.0.7         png_0.1-7         
    [65] vctrs_0.3.8        dbplyr_2.1.1       tidyselect_1.1.1   xfun_0.25         
