# ===============================================================================
# WRANGLING
# ===============================================================================

# raw data set including all 3 colleges
load('.././raw_data/college_posts_raw.Rdata')

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
  filter(date_utc > "2020-01-01") |>
  mutate(
    year  = year(date_utc),                     
    month_name = month(date_utc, label = TRUE),    
    month_num = month(date_utc)                  
  )

# Replace unicode \031s with actual apostrophes
all_posts$content <- all_posts$content |>
  gsub("\031", "'", x = _, fixed = TRUE) |>
  tolower() |>
  gsub('https?://\\S+|www\\.\\S+', '', x=_)



# save the polished data set
save(all_posts,
     file = '.././raw_data/all_college_posts.Rdata')