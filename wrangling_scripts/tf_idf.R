# ===============================================================================
# WHAT ARE THE MOST IMPORTANT WORDS/BIGRAMS TO EACH SUBREDDIT? -TF-IDF
# ===============================================================================

load('.././raw_data/all_college_posts.Rdata')
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

save(top_word_tfidf, file='../data/top_word_tfidf.Rdata')
save(top_bigram_tfidf, file='../data/top_bigram_tfidf.Rdata')
