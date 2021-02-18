# Descriptive practice


#1. Write two sentences. Save each as a seperate object in R. 

sent1 = "Doris enjoyed tapping her nails on the table to annoy everyone."
sent2 = "He realized there had been several deaths on this road, but his concern rose when he saw the exact number."


require(quanteda)


#2. Combine them into a corpus

txt <- c(sent1, sent2)

corpus_txt<-corpus(txt)
corpus_txt

#3. Make this corpus into a dfm with all pre-processing options at their defaults.

dfm_txt<-dfm(corpus_txt)

#4. Now save a second dfm, this time with stopwords removed.
?dfm

dfm_txt2<-dfm(corpus_txt, remove = stopwords())

#5. Calculate the TTR for each of these dfms (use textstat_lexdiv). Which is higher?

textstat_lexdiv(dfm_txt)
textstat_lexdiv(dfm_txt2)

dfm_txt2

#6. Calculate the Manhattan distance between the two sentences you've constructed (by hand!)

dist(dfm_txt, method= "manhattan")





