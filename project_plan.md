Complexity measures and proficiency

*A brief summary*:

The goal of this project is to extract syntactic and lexical complexity from a corpus of L2 Russian texts and to compare these measures against proficiency levels. Clustering analysis may help to identify the complexity measures typical for specific proficiency levels.

*The data portion*:

My data consists of two pieces. The first piece of data is an excel sheet indicating the ACTFL writing proficiency test score of each L2 learner before and after completing an intensive 8-week course in Russian in the summer of 2019. The second piece of data is a corpus of **standardized** texts written by 133 L2 Russian learners. Standardization of texts involves the following measures aimed to make it easier to extract complexity measures:

Change:
1.	Correct the spelling of misspelled words
Example: маленкий - маленький, друзями - друзьями
2.	Correct the gender of a noun in Nom case
Example: большой проблем - большой проблема, душ - душа
3.	Capitalize words which have to be capitalized and vice versa 
Example: техас - Техас, Американский – американский
4.	Correct words that have to be written together or separately
Example: на пример - например, ни когда – никогда BUT тоже самое - то же самое 
5.	Funny cases:
Гуля по городу, я заметил простату Миддлбери changed to Гуляя по городу, я заметил простоту Миддлбери
Я училась в летной школе changed to Я училась  летней школе 

Leave as is:
6.	Do not correct the endings
Example: большой проблема
7.	Do not correct grammatical issues
8.	Do not correct wrong usage of words
волосы - голоса
9.	If the word is mostly unrecognizable, leave as is
пиридивосты


*The analysis portion*:

I will develop codes to extract at least four complexity measures, more if I figure out how to do it. when I learn to perform a clustering analysis, I will calculate which complexity measures are characteristic of each proficiency level. 