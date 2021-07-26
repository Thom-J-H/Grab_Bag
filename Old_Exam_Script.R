
# Script for Old Exam Note ------------------------------------------------


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(visdat)



# Data --------------------------------------------------------------------
# Source
dat_url <- "https://education.rstudio.com/blog/2020/02/instructor-certification-exams/ranking.csv"
# Downloaded into dir data

ranking <- read_csv(here::here("data", "ranking.csv"))

ranking %>% vis_dat()  # two vars, rank should be a factor
ranking %>% glimpse()
ranking$rank %>% unique() # "positive", "negative", "indifferent", "wtf"  

# We need to count each level, and pivot_wider



# Data Tranformation ------------------------------------------------------

# levels to vars (names), counts to value
# in this case, replace_na  with 0

rank_wide <- ranking %>%
  group_by(item) %>%
  count(rank) %>%
  pivot_wider(names_from = rank, values_from = n) %>%
  ungroup() %>%
  replace_na(list(positive = 0, 
                  negative = 0, 
                  indifferent = 0, 
                  wtf = 0) )

# Check it
rank_wide %>% head() %>% knitr::kable()


## Stage two -- convert to percentages

rank_wide <- rank_wide %>%
  group_by(item) %>%
  mutate(tot = sum(positive, 
                   negative, 
                   indifferent, 
                   wtf, 
                   na.rm = TRUE)) %>%
  mutate_at(vars(-item, -tot), 
            .funs = list(~ round(./tot, digits = 3)))

# Check it
rank_wide %>% head() %>% knitr::kable()


# Plot time ---------------------------------------------------------------

# Reproduce and fix
## Remove alpha from legend
## Remove line from legend size
## Fix default labels & add title
## Scale axes as percentage
## Scale color


rank_wide %>%
  ggplot( aes(x = negative, 
              y = positive,  
              color = positive, 
              size = tot)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm" , show.legend = FALSE) +
  labs(size = "Number\nof replies", 
       color ="Positivity" ,
       x = "Negative", 
       y = "Positive",
       title = "Survey Responses: Positive ~ Negative",
       caption = "Data: rankings.csv") +
  scale_color_viridis_c() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  guides(size = guide_legend(order = 1))

## Compare with original

img_url <- "https://education.rstudio.com/blog/2020/02/instructor-certification-exams/figs/ranking-scatterplot-1.png"