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

setwd('wrangling_scripts')
getwd()
# INITIAL CODE TO SCRAPE THE REDDIT DATA USING REDDITEXTRACTOR, 

amherst_posts_raw <- find_thread_urls(
  subreddit = "amherstcollege", sort_by = "new", period = "day")

middlebury_posts_raw <- find_thread_urls(
  subreddit = 'middlebury', sort_by = 'new', period = 'day')

williams_posts_raw <- find_thread_urls(
  subreddit = 'WilliamsCollege', sort_by = "new", period = 'day')

save(amherst_posts_raw,
     williams_posts_raw,
     middlebury_posts_raw,
     file = '.././raw_data/college_posts_raw.Rdata')