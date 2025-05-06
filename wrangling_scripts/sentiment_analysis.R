# for text analysis
library(tidytext)
library(tidyverse)
library(textdata)
library(lubridate)
library(viridis)
library(DT)
library(car)
library(broom)
library(rstatix)
library(stringi)

# ===============================================================================
# General Sentiment Analysis, comparing mean sentiment and comment count 
# ===============================================================================
# raw data file with all college posts
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
    avg_post_sent = round(mean(sentiment), 2),
    med_sent = round(median(sentiment), 2),
    avg_comments_per_post = round(mean(comments), 2),
    med_com = round(median(comments), 2)
    
)

 
# testing sentiment equal variance
sent_var_results <- leveneTest(sentiment ~ subreddit, data = sentiment_posts)

# testing engagement equal variance
com_var_results <- leveneTest(comments ~ subreddit, data = sentiment_posts)

# saving kruskal wallis results to be loaded into sentiment_analysis.qmd
kruskal_sent <- tidy(kruskal.test(sentiment ~ subreddit, data = sentiment_posts))
kruskal_com <- tidy(kruskal.test(comments ~ subreddit, data = sentiment_posts))

# saving dunn results to be loaded into sentiment_analysis.qmd
dunn_sent <- dunn_test(sentiment  ~ subreddit, data=sentiment_posts,  
                       p.adjust.method = "bonferroni")

dunn_com <-  dunn_test(comments ~ subreddit, data=sentiment_posts,
                      p.adjust.method = 'bonferroni') 



save(kruskal_sent, kruskal_com, file='../data/kruskal_results.Rdata')
save(dunn_sent, dunn_com, file='../data/dunn_results.Rdata')

# save the subreddit summary table
save(subreddit_summaries, file='.././data/subreddit_summaries.Rdata')
# save the wrangled sentiment post datset
save(sentiment_posts, file='.././data/sentiment_posts.Rdata')

# ===============================================================================
# Over time analysis
# ===============================================================================

# just in case you clear environment
load('.././data/sentiment_posts.Rdata')

# Make a new data set of average sentiment and comments in each subreddit by QUARTER
sentiment_posts_quarterly <- sentiment_posts |>
mutate(quarter = floor_date(date_utc, unit = "quarter")) |> # round each date down to the first of the month
  # so that we can average using quarter as a unit
  group_by(subreddit, quarter) |> 
  # calculating average sentiment and comments in each quarter
  # making sure to round to 2 decimal points
  summarize(avg_sentiment = round(mean(sentiment), 2),
            avg_comments = round(mean(comments), 2),
            total_comments = round(sum(comments), 2))

# save to publishable data folder
save(sentiment_posts_quarterly, file='.././data/sentiment_posts_quarterly.Rdata')
cor(sentiment_posts$sentiment, sentiment_posts$comments)

# ===============================================================================
#  testing Analysis visualizations (to put in time series analysis)
# ===============================================================================

library(ggiraph)

# load in necessary data files
load('.././data/sentiment_posts_quarterly.Rdata')

# Over time analysis for QUARTERLY sentiment average per subreddit
gg_sentiment_quarterly <- ggplot(data = sentiment_posts_quarterly) +
  geom_line_interactive(aes(x = quarter, 
                            y = avg_sentiment, 
                            tooltip = avg_sentiment,
                            color = avg_comments)) + 
  facet_wrap(~subreddit, scales = "free", ncol=1) +
  labs(
    x = 'Date',
    y = 'Quarterly Average Sentiment Score',
    color = 'Average Comments'
  ) +
  theme_minimal() 

girafe(ggobj = gg_sentiment_quarterly)


# Over time analysis for QUARTERLY comment average per subreddit
gg_point_comments_quarterly <- ggplot(data = sentiment_posts_quarterly) +
  geom_line_interactive(aes(x = quarter, 
                            y = avg_comments, 
                            tooltip = avg_comments,
                            color = avg_sentiment)) + 
  facet_wrap(~subreddit, scales = "fixed", ncol=1) +
  labs(
    x = 'Date',
    y = 'Quarterly Average Comments',
    color = 'Average Sentiment'
  ) +
  theme_minimal() 

girafe(ggobj = gg_point_comments_quarterly)










