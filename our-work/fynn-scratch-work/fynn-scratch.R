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

# WHAT ARE THE MOST IMPORTANT WORDS TO EACH SUBREDDIT? -TF-IDF
word_freq_by_subr  <- all_posts |>
  # get all tokens from the content
  unnest_tokens(output = word, input = content) |>
  # remove stop words
  anti_join(stop_words, by="word") |>
  # group by subreddit
  group_by(subreddit) |>
  # gets occurences of each word within each subreddit
  count(word)

subr_tfidf <- word_freq_by_subr |>
  # gets tf, idf, and tf-idf all in one
  bind_tf_idf(term = word, document = subreddit, n = n) 

subr_top10_tfidf <- subr_tfidf |>
  # arrange in descending order to get highest tf-dfs 
  group_by(subreddit) |>
  arrange(desc(tf_idf)) |>
  # slices the top 10 from each subreddit
  slice(1:10) 

# visualize
subr_top10_tfidf |>
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

# ===============================================================================
# Sentiment Analysis
# ===============================================================================


# get all word-level tokens
amherst_words <- amherst_posts |>
  unnest_tokens(output = word, input=content)

# get word frequencies and sort in descending order
amherst_word_freqs <- amherst_words |>
  count(word)

# now with word, date created, subreddit, frequency, & comment count
amherst_words <- amherst_words |>
  left_join(amherst_word_freqs, by='word')


  

  



