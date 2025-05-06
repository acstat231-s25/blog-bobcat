# for text analysis
library(tidytext)
library(tidyverse)
library(textdata)
library(lubridate)

# ===============================================================================
# Application keywords
# ===============================================================================

load('.././raw_data/all_college_posts.Rdata')
# Calculating college-application related posts
# define a vector of application related keywords

application_keywords <- c(
  "waitlist",  "waitlisted", "waitlisting", "wl",
  "accept",    "accepted",   "acceptance",
  "apply",     "applied",    "applying", 
  "application", "admission", "admissions",
  "defer",     "deferred",   "deferral",
  "reject",    "rejected",   "rejection",
  "enroll",    "enrolled",   "enrollment",  
  "ed", "early decision", "rd", "regular decision", "sat" 
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

# quarterly summary data set
keyword_posts_quarterly <- keyword_posts |>
  mutate(quarter = floor_date(date_utc, unit = "quarter")) |>   # round down to first date of the quarter
  group_by(subreddit, quarter) |>
  summarise( # getting average statistics
    avg_keywords = round(mean(keywords), 2),
    total_keywords = round(sum(keywords), 2))     # average for the quarter

save(keyword_posts_quarterly, file='.././data/keyword_posts_quarterly.Rdata')
