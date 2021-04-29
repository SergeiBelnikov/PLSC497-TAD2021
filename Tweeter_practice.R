##### R Task

## load rtweet package
library(rtweet)
library(dplyr)
library(quanteda)

## Pick your favorite celebrity who has a Twitter account. 

# Justin Bieber

## find the most recent tweet the celebrites liked

most_liked <- get_favorites("justinbieber", n = 1)

##Download their 500 most recent tweets. 
timeline <- get_timelines("justinbieber", n = 500)
#Calculate which one got the most ``likes"

# Remove retweets
cleaned_retweets <- timeline[timeline$is_retweet==FALSE, ] 
# Remove replies
cleaned <- subset(cleaned_tweets, is.na(cleaned_retweets$reply_to_status_id))
# find most popular tweets
top <- cleaned %>% arrange(-favorite_count)

### Create a DFM from the text of these tweets
corpus <- corpus(top)
dfm <- dfm(corpus, stopwords(language="en"), verbose=TRUE)

### After removing stopwords, what word did the celebrity tweet most often?
bieber_wordcloud <- textplot_wordcloud(dfm, rotation=0, min_size=.75, max_size=3, max_words=50)