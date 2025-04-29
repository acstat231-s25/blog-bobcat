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
# Application keywords
# ===============================================================================

load('.././raw_data/all_college_posts.Rdata')
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

save(keyword_posts_monthly, file='.././data/keyword_posts_monthly.Rdata')




