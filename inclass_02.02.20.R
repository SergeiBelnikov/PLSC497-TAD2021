## Presidential Inaugural Address Corpus
presDfm <- dfm(data_corpus_inaugural, remove = stopwords("english"))
# compute some document similarities
textstat_simil(presDfm,presDfm[ "1985-Reagan",], margin = "documents")

textstat_simil(presDfm, presDfm[c("2009-Obama", "2013-Obama"),] , margin = "documents", method = "cosine")