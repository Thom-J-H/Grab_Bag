library(tidyverse)
library(here)
library(visdat)
library(glue)



# Import data -------------------------------------------------------------


students <- read.csv("https://userpage.fu-berlin.de/soga/200/2010_data_sets/students.csv") 

students <- students %>% 
  tibble()

students %>% glimpse()

students %>% vis_dat()

stu_height <- students %>% select(gender, height)



# Summary stats -----------------------------------------------------------


dist_stats <- stu_height  %>% 
  group_by(gender) %>% 
  summarize(avg_height = mean(height), 
            sd_height = sd(height),
            min_height = min(height),
            max_height = max(height))

dist_stats 

female_avg <- dist_stats$avg_height[1] 
female_sd <- dist_stats$sd_height[1]

male_avg <- dist_stats$avg_height[2] 
male_sd <- dist_stats$sd_height[1]

female_min <- dist_stats$min_height[1] 
male_min <- dist_stats$min_height[2] 

female_max <- dist_stats$max_height[1]
male_max <- dist_stats$max_height[2]

## create the standard deviation lines
f_sd_one_plus <- female_avg + female_sd  # 	0.00646
f_sd_one_minus <- female_avg - female_sd # 0.0313 end

f_sd_two_plus <- female_avg + (2 * female_sd)
f_sd_two_minus <- female_avg - (2 * female_sd)

m_sd_one_plus <- male_avg + male_sd 
m_sd_one_minus <- male_avg - male_sd

m_sd_two_plus <- male_avg + (2 * male_sd)
m_sd_two_minus <- male_avg - (2 * male_sd)


# create the main plot

plot_heights <- stu_height %>%
  ggplot( aes(x = height,  fill = gender) ) +
  geom_density(alpha = 0.4) +
  labs(x = "Height in CM", y = "Density", 
       fill = "Gender", title = "College Student Height: Normally Distributed",
       subtitle = "8239 observations. Female: 4110.  Male: 4129.",
       caption = "Data Source: Freie Universität Berlin") +
  geom_vline(xintercept = male_avg,  lty = 3) +
  geom_vline(xintercept = female_avg,  lty = 3) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic() +
  theme(legend.position = c(.9, .9))


plot_heights  ## Main plot to start


# Build for custom functions ---------------------------------------------------------------


female_height <- stu_height  %>% 
  filter(gender == "Female") 

female_height <- female_height %>% 
  mutate(zed = scale(female_height$height, center = TRUE) )

male_height <- stu_height  %>% 
  filter(gender == "Male")  

male_height <- male_height %>%
  mutate(zed = scale(male_height$height, center = TRUE) )

fh_plot <- female_height %>%
  ggplot( aes(x = height ) ) +
  geom_density() # Just for the data

norm_fh <- female_height %>%
  ggplot( aes(x = zed ) ) +
  geom_density(fill = "red", alpha = 0.3) +
  labs(title = "Normalized Female Heights",
       subtitle = "Data centered and scaled",
       x = "Standard Deviation",
       y = "Density",
       caption = "Data Source: Freie Universität Berlin")

norm_fh 
 
mh_plot <- male_height %>%
  ggplot( aes(x = height ) ) +
  geom_density()  #  Just for the data
  

norm_mh <- male_height %>%
  ggplot( aes(x = zed) ) +
  geom_density( fill = "blue", alpha = 0.3)  +
  labs(title = "Normalized Male Heights",
       subtitle = "Data centered and scaled",
       x = "Standard Deviation",
       y = "Density",
       caption = "Data Source: Freie Universität Berlin")

norm_mh 

fh_area <- ggplot_build(fh_plot)$data[[1]] # grab data

mh_area <-  ggplot_build(mh_plot)$data[[1]] # grab data



# Custom functions --------------------------------------------------------


## female under
female_show_height_under <- function(test_height){
  
  prob_h <- pnorm(test_height, female_avg, female_sd) %>% 
    round(3)
  
  zed <- female_height %>% 
    filter(height == test_height) %>% 
    pull(zed) %>% unique() %>% round(3)
  
  if(test_height < 136) warning ("Choose a value between 136 & 192")
  if(test_height > 192 ) warning ("Choose a value between 136 & 192")

  female_height %>% 
  ggplot( aes(x = height, y = ..density.. ) ) +
  geom_density( alpha = 0.4, fill = "grey") +
  geom_vline(xintercept = test_height, lty = 3, color = "red") +
  geom_vline(xintercept = female_avg, lty = 5, color = "grey") +
  geom_area(data = subset(fh_area, x < test_height ) , 
            aes(x = x, y = y), 
            fill = "red",
            alpha = 0.3) +
  theme_classic() +
  labs(title = "Female Students: Height Under", 
       x = "Height in CM", y = "Density",
       subtitle = paste("Red area <", {test_height}, "CM", " | ",   "Probability space:", {prob_h}, " | ",  "Zed:", {zed}  ),
       caption = "Data Source: Freie Universität Berlin")
}
#

## female over

female_show_height_over <- function(test_height){
  
  prob_h <- pnorm(test_height, female_avg, female_sd, 
                  lower.tail = FALSE) %>% 
    round(3)
  
  zed <- female_height %>% 
    filter(height == test_height) %>% 
    pull(zed) %>% unique() %>% round(3)
  
  if(test_height < 136) warning ("Choose a value between 136 & 192")
  if(test_height > 192 ) warning ("Choose a value between 136 & 192")
  
  female_height %>% 
    ggplot( aes(x = height, y = ..density.. ) ) +
    geom_density( alpha = 0.4, fill = "grey") +
    geom_vline(xintercept = test_height, lty = 3, color = "red") +
    geom_vline(xintercept = female_avg, lty = 5, color = "grey") +
    geom_area(data = subset(fh_area, x > test_height ) , 
              aes(x = x, y = y), 
              fill = "red", 
              alpha = 0.3) +
    theme_classic() +
    labs(title = "Female Students: Height Over", x = "Height in CM", y = "Density",
         subtitle = paste("Red area >", {test_height}, "CM", " | ", "Probability space:", {prob_h}, " | ",  "Zed:", {zed}  ),
         caption = "Data Source: Freie Universität Berlin")
}
#

# male under
male_show_height_under <- function(test_height){
  
  prob_h <- pnorm(test_height, male_avg, male_sd) %>% 
    round(3)
  
  zed <- male_height %>% 
    filter(height == test_height) %>% 
    pull(zed) %>% unique() %>% round(3)
  
  if(test_height < 145) warning ("Choose a value between 145 & 201")
  if(test_height > 201 ) warning ("Choose a value between 145 & 201")
  
  male_height %>% 
    ggplot( aes(x = height, y = ..density.. ) ) +
    geom_density( alpha = 0.4, fill = "grey") +
    geom_vline(xintercept = test_height, lty = 3, color = "blue") +
    geom_vline(xintercept = male_avg, lty = 5, color = "grey") +
    geom_area(data = subset(mh_area, x < test_height ) , 
              aes(x = x, y = y), 
              fill = "blue",
              alpha = 0.3) +
    theme_classic() +
    labs(title = "Male Students: Height Under", x = "Height in CM", y = "Density",
         subtitle = paste("Blue area <", {test_height}, "CM", " | ",   "Probability space:", {prob_h}, " | ",  "Zed:", {zed}   ),
         caption = "Source: Freie Universität Berlin")
}
#

## male over
male_show_height_over <- function(test_height){
  
  prob_h <- pnorm(test_height, male_avg, male_sd, lower.tail = FALSE) %>% 
    round(3)
  
  zed <- male_height %>% 
    filter(height == test_height) %>% 
    pull(zed) %>% unique() %>% round(3)
  
  if(test_height < 145) warning ("Choose a value between 145 & 201")
  if(test_height > 201 ) warning ("Choose a value between 145 & 201")
  
  male_height %>% 
    ggplot( aes(x = height, y = ..density..  ) ) +
    geom_density( alpha = 0.4, fill = "grey") +
    geom_vline(xintercept = test_height, lty = 3, color = "blue") +
    geom_vline(xintercept = male_avg, lty = 5, color = "grey") +
    geom_area(data = subset(mh_area, x > test_height ) , 
              aes(x = x, y = y), 
              fill = "blue", 
              alpha = 0.3) +
    theme_classic() +
    labs(title = "Male Students: Height Over", x = "Height in CM", y = "Density",
         subtitle = paste("Blue area >", {test_height}, "CM", " | ", "Probability space:", {prob_h}, " | ",  "Zed:", {zed}  ),
         caption = "Data Source: Freie Universität Berlin")
}




# Test --------------------------------------------------------------------


# Choose a whole number value between 136 & 192
female_show_height_under(male_min)

female_show_height_over(male_min)

# Choose a whole number  value between 145 & 201
male_show_height_under(female_max) 

male_show_height_over(female_max) 


test_height <- 170

# Choose a whole number value between 136 & 192
female_show_height_under(test_height)
female_show_height_over(test_height)

# Choose a whole number  value between 145 & 201
male_show_height_under(test_height) 
male_show_height_over(test_height) 

# Save --------------------------------------------------------------------

save.image(file = here::here("data", "tidy_data", "student_height.RData"))




height_quantiles <- seq(0.1, 0.9, by = 0.1)
qnorm(height_quantiles, male_avg, male_sd)





# test --------------------------------------------------------------------


test_sd <- sd(stu_height$height)
test_avg <- mean(stu_height$height)

height_norm <- rnorm(10000, test_avg, test_sd )

height_norm <- height_norm %>% enframe(value = "height", name = NULL)

x_ticks <- seq(120, 220, by = 10)

three_dist <- height_norm %>%
  ggplot(aes(x = height)) +
  stat_function(fun = dnorm, 
                args = list(mean = female_avg, sd = female_sd), 
                color = "red") +
  stat_function(fun = dnorm,  
                args = list(mean = male_avg, sd = male_sd),
                color = "blue") +
  stat_function(fun = dnorm,  
                args = list(mean = mean(height_norm$height), 
                            sd = sd(height_norm$height)),
                color = "black") +
  scale_x_continuous(breaks = x_ticks) +
  theme_classic() +
  labs(title = "Student Height Distribution",
       subtitle = "Black: All; Red: Female; Blue: Male",
       y = "Density", x = "Height in CM",
       caption = "Data Source: Freie Universität Berlin")

three_dist 



quick_table <- stu_height %>% 
  summarize(avg_height = mean(height),
            sd_height = sd(height),
            min_height = min(height),
            max_height = max(height)) %>%
  mutate(gender = "All") %>%
  select(gender, everything()) %>%
  bind_rows(dist_stats)


quick_table
