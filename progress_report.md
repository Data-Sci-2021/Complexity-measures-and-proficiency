# Progress report # 3 (11/19/2021)
1. The most important part of my progress was figuring out how to make sure that the gibberish output is not gibberish. Dan helped me with that - Sys.setlocale("LC_CTYPE", "Russian"). Overall, I always had to take additional steps to download models for the Russian language with many packages because everything is by default for English texts. 
2. I was making stupid mistakes a lot of the time. Most of my functions would refuse to work because I would leave show() in the end of my commands.
3. I think I read and understand package descriptions better. I was able to figure out myself several times why one or another function is not working. 
4. I was able to calculate three lexical complexity measures out of four (see Draft_2_Soyan). In order to calculate one of the lexical complexity measures, I created five non-lexical words .txt files. 
5. I was able to calculate onle one syntactic complexity measure. I found that spacyr does not tag pos for Russian texts correctly. The udpipe package does a better job, but I still don't know how to move from there to calculating the number of clauses. 

## Sharing plan
I cannot share the corpus since it does not belong to me. I have access to this corpus only as an RA. I can manipulate data from the corpus but I cannot make the texts publicly available.

## My license
I can make the codes publicly accessible but I cannot make the corpus accessible. I do not think that my codes as of now are useful. Still, I want them to be reusable by other people, but only for non-commercial purposes. Therefore I chose this Attribution-NonCommercial-ShareAlike version of licensing.

# Progress report # 2 (10/26/2021)
1. I decided to work with one text at this moment. Once I figure out how to measure complexity measures in one text, I think I can venture to work with the whole corpus.
2. I figured out how to load a text in Russian in r (using the *readtext* package) but there were problems with setting the directory. It would work one time and then when I go back the next day, it would stop working.
3. I figured out how to remove unnecessary words from the text (online search).
3. I figured out how to measure the number of sentences in r (using the *quanteda* package).
4. I figured out how to measure the number of tokens in r (using the *quanteda* package).
5. I measured my first complexity measure - mean sentence length - in one text. 
6. Now I am thinking how to measure T-units in a text.
7. I couldn't knit the document. I get this message: 
Quitting from lines 17-18 (Draft_1_Soyan.Rmd) 
Error in setwd(dir) : cannot change working directory
Calls: <Anonymous> ... process_group.inline -> call_inline -> in_dir -> setwd
Execution halted

## Sharing plan
I do not think I can share the texts in the corpus. The corpus does not belong to me. I am just an RA in this project and it would not be okay if I start sharing the corpus on my own.

# Progress report # 1 (10/11/2021)
So far I have my data ready. I need to start choosing complexity measures for extraction and figuring out the codes for them.  