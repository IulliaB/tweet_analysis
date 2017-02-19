library("dplyr")
library("tidyr")
library("rtweet")
library("tm")
library("wordcloud")

## Set the following word
keyword = "environment"

## selects only the hash tags from a given tweet text
select_hash_tags <- function(text) {
  words = strsplit(text, split = " ")[[1]]
  selected_words = sapply(words, function(x) startsWith(x, "#"))
  words[selected_words]
}

## retrieve 1000 tweets
rt <- search_tweets(keyword, n = 1000, token = twitter_token, include_rts = F)
rt_df = tbl_df(rt)

## select only tweet text
text = rt_df$text

## select only hash tags from all tweets
hash_tags = unlist(sapply(text, select_hash_tags))

## cut the hash symbol from the beginning of the word
hash_tags = unlist(sapply(hash_tags, function(x) substring(x, 2)))

## create corpus of words
corpus = Corpus(DataframeSource(tbl_df(hash_tags)))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, removeWords, keyword)

wordcloud(corpus, 
          scale=c(5,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))
