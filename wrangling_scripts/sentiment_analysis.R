# ===============================================================================
# Sentiment Analysis
# ===============================================================================

load('.././raw_data/all_college_posts.Rdata')

afinn_lexicon <- get_sentiments('afinn')

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

# I like this one, we could have it as a interactive table in reference section
# adds a sentiment column to every post in our dataset
sentiment_posts <- all_posts |>
  mutate(sentiment = map_dbl(content, get_sentiment)) 

# Make a new data set of average sentiment in each subreddit by MONTH
sentiment_posts_monthly <- sentiment_posts |>
  mutate(month = floor_date(date_utc, unit = "month")) |> # round each date down to the first of the month
  # so that we can average using month as a unit
  group_by(subreddit, month) |> 
  # calculating average sentiment and comments in each month
  # making sure to round to 2 decimal points
  summarize(avg_sentiment = round(mean(sentiment), 2),
            avg_comments = round(mean(comments), 2))

# table comparing total summed score for each subreddit
subreddit_sentiment <- sentiment_posts |>
  group_by(subreddit)|>
  summarize(
    total_sent = sum(sentiment),
  )

# save total sentiments
save(subreddit_sentiment, file='.././data/subreddit_sents.Rdata')

# Calculating college-application related posts
# define a vector of application related keywords

application_keywords <- c(
  "waitlist",  "waitlisted", "waitlisting",
  "accept",    "accepted",   "acceptance",
  "apply",     "applied",    "applying", 
  "application", "admission", "admissions",
  "defer",     "deferred",   "deferral",
  "reject",    "rejected",   "rejection",
  "enroll",    "enrolled",   "enrollment",  
  "matriculate", "matriculated", "stats"
)

# calculate # of keyword occurrences for each subreddit
keyword_counts <- all_posts |>
  mutate(post_id = row_number()) |> # dummy counter so we can join later
  # get all tokens from the content
  unnest_tokens(word, content) |> # one row per word
  filter(word %in% application_keywords) |> # keep only admission terms
  group_by(post_id) |>            
  summarise(keywords = n())

# adding dummy counter to original data set
all_posts <- all_posts |>
  mutate(post_id = row_number())

# joining counts back to orignal data set
keyword_posts <- keyword_counts |>
  right_join(all_posts, by = "post_id") |>
  # fill all posts with no keywords with 0
  mutate(keywords = replace_na(keywords, 0)) |>
  select(content, date_utc, comments, subreddit,
         year, month_name, month_num,
         keywords)

keyword_posts_monthly <- keyword_posts |>
  mutate(month = floor_date(date_utc, unit = "month")) |> # round each date down to the first of the month
  # so that we can average using month as a unit
  group_by(subreddit, month) |> 
  # calculating average sentiment and comments in each month
  # making sure to round to 2 decimal points
  summarize(avg_keywords = round(mean(keywords), 2))

# ===============================================================================
# OVER TIME ANALYSIS
# ===============================================================================
# plot of all three colleges' posts with sentiment over time!

library(ggiraph)

# Over time analysis for ALL posts per subreddit
sentiment_posts_daily <- sentiment_posts |>
  group_by(subreddit, date_utc) |>
  summarize(
    avg_sentiment = mean(sentiment),
    avg_comments = mean(comments)
  )

gg_point <- ggplot(data = avg_sent_per_day_per_subreddit) +
  geom_line_interactive(aes(x = date_utc, 
                            y = avg_sentiment, 
                            tooltip = avg_sentiment,
                            color = avg_comments)) + 
  facet_wrap(~subreddit, scales = "free", ncol=1) +
  labs(
    x = 'Date',
    y = 'Avg Daily Sentiment Score',
    color = 'Avg Comments'
  ) +
  theme_minimal() 

girafe(ggobj = gg_point)

# Over time analysis for MONTHLY sentiment average per subreddit
gg_point_sentiment_monthly <- ggplot(data = sentiment_posts_monthly) +
  geom_line_interactive(aes(x = month, 
                            y = avg_sentiment, 
                            tooltip = avg_sentiment,
                            color = avg_comments)) + 
  facet_wrap(~subreddit, scales = "free", ncol=1) +
  labs(
    x = 'Date',
    y = 'Monthly Average Sentiment Score',
    color = 'Average Comments'
  ) +
  theme_minimal() 

girafe(ggobj = gg_point_sentiment_monthly)

# Over time analysis for MONTHLY comment average per subreddit
gg_point_comments_monthly <- ggplot(data = sentiment_posts_monthly) +
  geom_line_interactive(aes(x = month, 
                            y = avg_comments, 
                            tooltip = avg_comments,
                            color = avg_sentiment)) + 
  facet_wrap(~subreddit, scales = "fixed", ncol=1) +
  labs(
    x = 'Date',
    y = 'Monthly Average Comments',
    color = 'Average Sentiment'
  ) +
  theme_minimal() 

girafe(ggobj = gg_point_comments_monthly)

# Over time analysis for MONTHLY admissions keywords per subreddit

gg_point_keywords_monthly <- ggplot(data = keyword_posts_monthly) +
  geom_line_interactive(aes(x = month, 
                            y = avg_keywords, 
                            tooltip = avg_keywords)) + 
  facet_wrap(~subreddit, scales = "fixed", ncol=1) +
  labs(
    x = 'Date',
    y = 'Monthly Average Keywords per Post'
  ) +
  theme_minimal() 

girafe(ggobj = gg_point_keywords_monthly)

# interactive table
library(stringi)
rownames(sentiment_posts) <- NULL

sentiment_posts_clean <- sentiment_posts |>
  # convert factors → character
  mutate(across(where(is.factor), as.character)) |>
  # force every character vector into valid UTF-8
  mutate(across(where(is.character),
                ~ stri_enc_toutf8(.x)))

save(sentiment_posts_clean, file='.././data/posts_with_sentiment.Rdata')
datatable(sentiment_posts_clean)

