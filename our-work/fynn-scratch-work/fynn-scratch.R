# for extracting reddit posts
library(RedditExtractoR)

# for text analysis
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(ggthemes)
library(textdata)
library(lubridate)
library(viridis)

# interactive experimentation
library(DT)


# ===============================================================================
# Scraping the sub-reddits 
# ===============================================================================


# INITIAL CODE TO SCRAPE THE REDDIT DATA USING REDDITEXTRACTOR, 
# NOW SAVED IN DATA FOLDER
# amherst_posts_raw <- find_thread_urls(
#   subreddit = "amherstcollege", sort_by = "new", period = "day")
# 
# middlebury_posts_raw <- find_thread_urls(
#   subreddit = 'middlebury', sort_by = 'new', period = 'day')
# 
# williams_posts_raw <- find_thread_urls(
#   subreddit = 'WilliamsCollege', sort_by = "new", period = 'day')
# 
# save(amherst_posts_raw,
#      williams_posts_raw,
#      middlebury_posts_raw,
#      file = './data/college_posts_raw.Rdata')


# ===============================================================================
# WRANGLING
# ===============================================================================

# raw data set including all 3 colleges
load('./data/college_posts_raw.Rdata')

# since they all have the same cols we can stack into one big dataset of posts
all_posts <- rbind(amherst_posts_raw, middlebury_posts_raw, williams_posts_raw) |>
# combining title and text into one string column
 mutate(content = paste0(title, ' ', text)) |>
 select(content, date_utc, comments, subreddit)

# renaming each subreddit to human-readable names
all_posts <- all_posts |>
 mutate(subreddit = recode(subreddit,
                           "amherstcollege" = "Amherst College",
                           "middlebury" = "Middlebury College",
                           "WilliamsCollege" = "Williams College"),
        # change dates to date format
        date_utc = as.Date(date_utc)) |>
 filter(date_utc > "2020-01-01")

# Replace unicode \031s with actual apostrophes
all_posts$content <- all_posts$content |>
 gsub("\031", "'", x = _, fixed = TRUE) |>
 tolower() |>
 gsub('https?://\\S+|www\\.\\S+', '', x=_)
 


# save the polished data set
save(all_posts,
   file = './data/all_college_posts.Rdata')


# Final data set we will use + stop words data set

load('./data/all_college_posts.Rdata')
data(stop_words)

# Filter the "all college posts" data set to get posts specific to each college
amherst_posts <- all_posts |>
  filter(subreddit == 'Amherst College') 

middlebury_posts <- all_posts |>
  filter(subreddit == 'Middlebury College')

williams_posts <- all_posts |>
  filter(subreddit == 'Williams College')

# Add "x200b" to stop words (a unicode zero width space character)
stop_words <- bind_rows(
  stop_words, tibble(word = "x200b", lexicon = "custom")
)

# ===============================================================================
# WHAT ARE THE MOST IMPORTANT WORDS/BIGRAMS TO EACH SUBREDDIT? -TF-IDF
# ===============================================================================

# token frequencies
word_freqs  <- all_posts |>
  # get all tokens from the content
  unnest_tokens(output = word, input = content) |>
  # remove stop words
  anti_join(stop_words, by="word") |>
  # group by subreddit
  group_by(subreddit) |>
  # gets occurences of each word within each subreddit
  count(word)

bigram_freqs <- all_posts |>
  unnest_tokens(output = bigram, input = content, token="ngrams", n=2) |>
  separate(bigram, into = c("w1","w2"), sep = " ") |>
  filter(!w1 %in% stop_words$word,
         !w2 %in% stop_words$word) |>
  # 3. Re-unite into a single bigram string
  unite(bigram, w1, w2, sep = " ") |>
  count(subreddit, bigram, sort = TRUE)

# tfidf calculation
word_tfidf <- word_freqs |>
  # gets tf, idf, and tf-idf all in one
  bind_tf_idf(term = word, document = subreddit, n = n) 

bigram_tfidf <- bigram_freqs |>
  bind_tf_idf(term = bigram, document = subreddit, n = n)

top_word_tfidf <- word_tfidf |>
  # arrange in descending order to get highest tf-dfs 
  group_by(subreddit) |>
  arrange(desc(tf_idf)) |>
  # slices the top 10 from each subreddit
  slice(1:10) 

top_bigram_tfidf <- bigram_tfidf |>
  # arrange in descending order to get highest tf-dfs 
  group_by(subreddit) |>
  arrange(desc(tf_idf)) |>
  # slices the top 10 from each subreddit
  slice(1:10) 

# visualize
plot_word_tfidf <- top_word_tfidf |>
  ggplot(aes(x = reorder_within(word, tf_idf, subreddit), y = tf_idf, fill = tf_idf)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, 
       y = "TF-IDF",
       title = "Top 10 words by Tf-Idf for Each Subreddit") +
  scale_fill_viridis('magma')

plot_bigram_tfidf <- top_bigram_tfidf |>
  ggplot(aes(x = reorder_within(bigram, tf_idf, subreddit), y = tf_idf, fill = tf_idf)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, 
       y = "TF-IDF",
       title = "Top 10 Bigrams by Tf-Idf for Each Subreddit") +
  scale_fill_viridis('magma')

save(top_word_tfidf, file='./data/top_word_tfidf.Rdata')
save(top_bigram_tfidf, file='./data/top_bigram_tfidf.Rdata')

# ===============================================================================
# Sentiment Analysis
# ===============================================================================


afinn_lexicon <- get_sentiments('afinn')

# two different methods to get the total sentiment for each subreddit,
# I like having two to not only confirm, but to see the data in a two different
# ways
all_word_sentiments <- all_posts |>
  # get all tokens from the content
  unnest_tokens(output = word, input = content) |>
  # group by subreddit
  group_by(subreddit) |>
  # gets occurences of each word within each subreddit
  count(word) |>
  # get the sentiments for each word
  left_join(afinn_lexicon, by="word") |>
  group_by(subreddit, word) |>
  summarize(
    num_words = n,
    value = value,
    sentiment = sum(value, na.rm = TRUE) * n,
    .groups = "drop"
  )

# total sentiment scores for each subreddit
subreddit_sentiments <- all_word_sentiments |>
  group_by(subreddit) |>
  summarize(
    score = sum(sentiment)
  )

# table visualization

# helpful method that calcs afinn lex sentiment for a given string
get_sentiment <- function(content) {
  # convert char vec to table
  tbl_sentiment <- tibble(content) |>
    # unnest the tokens
    unnest_tokens(output = word, input = content) |>
    # join with sentiments
    left_join(afinn_lexicon, by="word")
  
  # returns the sum of the value col which representing the total sentiment
  # score for the string (char vec)
  sum(tbl_sentiment$value, na.rm = TRUE)
  
}

# I like this one, we could have it as a interactive table in the blog
# adds a sentiment column to every post in our dataset
sentiment_posts <- all_posts |>
  mutate(sentiment = map_dbl(content, get_sentiment))

# table comparing total summed score for each subreddit
subreddit_sentiment <- sentiment_posts |>
  group_by(subreddit)|>
  summarize(
    total_sent = sum(sentiment)
  )

# save total sentiments
save(subreddit_sentiment, file='./data/subreddit_sents.Rdata')


# ===============================================================================
# OVER TIME ANALYSIS
# ===============================================================================
# plot of all three colleges' posts with sentiment over time!

library(ggiraph)

gg_point <- ggplot(data = sentiment_posts) +
  geom_point_interactive(aes(x = date_utc, 
                             y = sentiment, 
                             tooltip = sentiment,
                             color = comments)) + 
  facet_wrap(~subreddit, scales = "free", ncol=1) +
  labs(
    x = 'Date',
    y = 'Total Sentiment Score',
    color = 'Comments'
  ) +
  theme_minimal() 

girafe(ggobj = gg_point)


# interactive table
library(stringi)
rownames(sentiment_posts) <- NULL

sentiment_posts_clean <- sentiment_posts |>
  # convert factors → character
  mutate(across(where(is.factor), as.character)) |>
  # force every character vector into valid UTF-8
  mutate(across(where(is.character),
                ~ stri_enc_toutf8(.x)))

save(sentiment_posts_clean, file='./data/posts_with_sentiment.Rdata')
datatable(sentiment_posts_clean)








  


  

  



