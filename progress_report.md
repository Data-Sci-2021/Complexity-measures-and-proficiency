# Progress report # 1 (10/11/2021)
So far I have my data ready. I need to start choosing complexity measures for extraction and figuring out the codes for them.  

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