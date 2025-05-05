# for text analysis
library(tidytext)
library(tidyverse)
library(textdata)
library(lubridate)
library(viridis)
library(DT)

# ===============================================================================
# General Sentiment Analysis, comparing mean sentiment and comment count 
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
    total_comments = sum(comments),
    avg_post_sent = round(mean(sentiment), 2),
    avg_comments_per_post = round(mean(comments), 2)
    
)

library(car)
library(broom)
library(rstatix)
# sentiment equal variance
sent_var_results <- tidy(leveneTest(sentiment ~ subreddit, data = sentiment_posts))

# engagement equal variance
com_var_results <- tidy(leveneTest(comments ~ subreddit, data = sentiment_posts))


# now lets see if these differences are significant using WELCH's ANOVA


welch_sent <- tidy(oneway.test(sentiment ~ subreddit, 
                               data = sentiment_posts, var.equal = FALSE))

welch_com <- tidy(oneway.test(comments ~ subreddit, 
                              data = sentiment_posts, var.equal = FALSE))

sent_howell <- games_howell_test(sentiment ~ subreddit, data=sentiment_posts)
com_howell <- games_howell_test(comments ~ subreddit, data=sentiment_posts)


# save to publishable data folder
save(sent_var_results, com_var_results,
     file='.././data/levene_results.Rdata')

save(welch_sent, welch_com, file = '.././data/welch_results.Rdata')
save(sent_howell, com_howell, file = '.././data/ghowell_results.Rdata')

save(subreddit_summaries, file='.././data/subreddit_summaries.Rdata')

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
#  Analysis visualizations (to put in time series analysis)
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





# ```{r}
# #| label: tbl-summary
# #| tbl-cap: "ANOVA report: subreddit sentiment & engagement"
# #| tbl-subcap:
#   #|   - "Summary statistics for cross subreddit comparison"
#   #|   - "ANOVA: average sentiment across subreddit"
# #|   - "ANOVA: average comments across subreddit"
# #| layout: [[1], [4, -1, 4]] 
# 
# 
# load('./data/subreddit_summaries.Rdata')
# load('./data/aov_tukey_results.Rdata')
# 
# subreddit_summaries |>
#   kable(digits=2, 
#         col.names = c('Subreddit', 'Post Count','Score (tot)', 
#                       'Comments (tot)', 'Score (avg)', 'Comments (avg)')) |>
#   row_spec(1, background = "#b7a5d3", color = 'white') |>
#   row_spec(2, background = "#37538C", color='white') |>
#   row_spec(3, background = "#FFBE0A", color = 'white')
# 
# aov_sent_res |> 
#   kable(col.names = c('Factor', 'df', 'SSQ','MSQ', 'F', 'p'), digits=2)
# 
# aov_comments_res |> 
#   kable(col.names = c('Factor', 'df', 'SSQ','MSQ', 'F', 'p'), digits=2) 
# ```
# 





