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
# General Sentiment Analysis
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

# add sentiment score to every post
sentiment_posts <- all_posts |>
  mutate(sentiment = map_dbl(content, get_sentiment)) 

# force every character into valid UTF-8, needed for datatable display
library(stringi)
rownames(sentiment_posts) <- NULL

sentiment_posts <- sentiment_posts |>
  # convert factors → character
  mutate(across(where(is.factor), as.character)) |>
  # force every character vector into valid UTF-8
  mutate(across(where(is.character),
              ~ stri_enc_toutf8(.x)))

# table comparing total summed score/comments for each subreddit
subreddit_summaries <- sentiment_posts |>
  group_by(subreddit)|>
  summarize(
    total_posts = n(),
    total_sent = sum(sentiment),
    total_comments = sum(comments)
  )

# save to publishable data folder
save(subreddit_summaries, file='.././data/subreddit_summaries.Rdata')
save(sentiment_posts, file='.././data/sentiment_posts.Rdata')

# ===============================================================================
# Over time analysis
# ===============================================================================

# just in case you clear environment
load('.././data/sentiment_posts.Rdata')

# Make a new data set of average sentiment in each subreddit by MONTH
sentiment_posts_monthly <- sentiment_posts |>
  mutate(month = floor_date(date_utc, unit = "month")) |> # round each date down to the first of the month
  # so that we can average using month as a unit
  group_by(subreddit, month) |> 
  # calculating average sentiment and comments in each month
  # making sure to round to 2 decimal points
  summarize(avg_sentiment = round(mean(sentiment), 2),
            avg_comments = round(mean(comments), 2))

# Daily avg sentiment & comments
sentiment_posts_daily <- sentiment_posts |>
  group_by(subreddit, date_utc) |>
  summarize(
    avg_sentiment = round(mean(sentiment), 2),
    avg_comments = round(mean(comments), 2)
  )

# save to publishable data folder
save(sentiment_posts_monthly, file='.././data/sentiment_posts_monthly.Rdata')
save(sentiment_posts_daily, file='.././data/sentiment_posts_daily.Rdata')

# ===============================================================================
# Sentiment Analysis visualizations (to put in index.qmd)
# ===============================================================================

library(ggiraph)

# load in necessary data files
load('.././data/sentiment_posts_monthly.Rdata')
load('.././data/sentiment_posts_daily.Rdata')

gg_daily <- ggplot(data = sentiment_posts_daily) +
  geom_line_interactive(aes(x = date_utc, 
                            y = avg_sentiment, 
                            tooltip = avg_sentiment,
                            color = avg_comments)) + 
  facet_wrap(~subreddit, scales = "free", ncol=1) +
  labs(
    x = 'Date',
    y = 'Daily Average Sentiment Score',
    color = 'Average Comments'
  ) +
  theme_minimal() 

girafe(ggobj = gg_daily)

# Over time analysis for MONTHLY sentiment average per subreddit
gg_sentiment_monthly <- ggplot(data = sentiment_posts_monthly) +
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

girafe(ggobj = gg_sentiment_monthly)

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

load('.././data/keyword_posts_monthly.Rdata')
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

datatable(sentiment_posts_clean)







