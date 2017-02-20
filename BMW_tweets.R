library("dplyr")
library("tidyr")
library("rtweet")
library("wordcloud")
library("RColorBrewer")
library("tm")


## bmw tweets
bmw_tweets <- get_timeline("BMWGroup", n=1000)

## selects only the hash tags from a given tweet text
select_hash_tags <- function(text) {
  words = strsplit(text, split = " ")[[1]]
  selected_words = sapply(words, function(x) startsWith(x, "#"))
  words[selected_words]
}

## extracts tweets that have hastags
bmw_text <- bmw_tweets[grep("#", bmw_tweets$text), ]

## select only hash tags from all tweets
hash_tags = unlist(sapply(bmw_text$text, select_hash_tags))

## Construct the lexical Corpus and the Term Document Matrix

# create a corpus
bmw_corpus = Corpus(VectorSource(hash_tags))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(bmw_corpus,
                         control = list(removePunctuation = F,
                                        removeNumbers = TRUE, tolower = TRUE))


## Obtain words and their frequencies

# define tdm as matrix
m = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


##Plot the wordcloud

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
#png("Bmw_tweets.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#dev.off()