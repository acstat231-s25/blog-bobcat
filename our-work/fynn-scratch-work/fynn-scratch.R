# for extracting reddit posts
library(RedditExtractoR)
library(pushshiftR)

# for text analysis
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(ggthemes)
library(textdata)
library(lubridate)





# INITIAL CODE TO GET THE REDDIT DATA, NOW SAVED IN DATA FOLDER
# amherst_posts_raw <- find_thread_urls(
#   subreddit = "amherstcollege", sort_by = "new", period = "day")
# 
# umass_posts_raw <- find_thread_urls(
#   subreddit = "umass", sort_by = "new", period = "day")
# 
# williams_posts_raw <- find_thread_urls(
#   subreddit = 'WilliamsCollege', sort_by = "new", period = 'day')
# 
# save(amherst_posts_raw,
#      williams_posts_raw,
#      umass_posts_raw,
#      file = './data/college_posts_raw.Rdata')

load('./data/college_posts_raw.Rdata')

# since they all have the same cols we can stack into one big dataset of posts
all_posts <- rbind(amherst_posts_raw, umass_posts_raw, williams_posts_raw) |>
  # combining title and text into one string column
  mutate(content = paste0(title, ' ', text)) |>
  select(content, date_utc, comments, subreddit)




amherst_posts <- all_posts |>
  filter(subreddit == 'amherstcollege')

umass_posts <- all_posts |>
  filter(subreddit == 'umass')

williams_posts <- all_posts |>
  filter(subreddit == 'WilliamsCollege')

# WHAT ARE THE MOST IMPORTANT WORDS TO EACH SUBREDDIT? -TF-IDF
word_freq_by_subr  <- all_posts |>
  # get all tokens from the content
  unnest_tokens(output = word, input = content) |>
  # group by subreddit
  group_by(subreddit) |>
  # gets occurences of each word within each subreddit
  count(word)

subr_tfidf <- word_freq_by_subr |>
  # gets tf, idf, and tf-idf all in one
  bind_tf_idf(term = word, document = subreddit, n = n)

subr_top10_tfidf <- subr_tfidf |>
  # arrange in descending order to get highest tf-dfs 
  arrange(desc(tf_idf)) |>
  group_by(subreddit) |>
  # slices the top 10 from each subreddit
  slice(1:10) 

# visualize
subr_top10_tfidf |>
  ggplot(aes(x = fct_reorder(word, tf_idf), y = tf_idf, fill = as.factor(tf_idf))) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~subreddit, ncol=2, scales ="free") +
  labs(x = NULL, 
       y = "TF-IDF",
       title = "Top 10 words by Tf-Idf for Each Subreddit")


# example wrangling:

# get all word-level tokens
amherst_words <- amherst_posts |>
  unnest_tokens(output = word, input=content)

# get word frequencies and sort in descending order
amherst_word_freqs <- amherst_words |>
  count(word)

# now with word, date created, subreddit, frequency, & comment count
amherst_words <- amherst_words |>
  left_join(amherst_word_freqs, by='word')
  

  



