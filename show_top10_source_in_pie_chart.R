library("dplyr")
library("tidyr")
library("rtweet")

rt <- search_tweets("data science", n = 1000, token = twitter_token)
rt_df = tbl_df(rt)
rt_df$source = as.factor(rt_df$source)
pie(sort(table(rt_df$source), decreasing = T)[1:10])

