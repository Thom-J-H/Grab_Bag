
# Script for Redwood custom graph -----------------------------------------

library(tidyverse)



# Data source -------------------------------------------------------------

# "Distribution Of Tree Height In An Old Growth Redwood Forest" (2015 July 14), 
# Mark Edward Graham (http://verylargeandtallredwoods.com/?tag=redwood-tree-height-distribution). 


redwood_zed <- c(4.5,  4.0,  3.0,  2.0,  1.0 , 0.0, -1.0, -2.0, -3.0, -4.0, -4.5)
redwood_hfeet <- c(383, 368, 338, 308, 278, 248, 218, 188, 158, 128, 113)
redwood_hmeters <- redwood_hfeet * 0.3048 # my conversion
redwood_stats <- tibble(redwood_zed , redwood_hfeet, redwood_hmeters) 



#  plot data using rnorm --------------------------------------------------


redwood_avg <- 75.5904 # redwood_stats 
redwood_sd <- 9.144 # redwood_stats 
pop_size <-  100000

tree_plot <- rnorm(n = pop_size, 
                   mean = redwood_avg , 
                   sd = redwood_sd ) %>%
  enframe(value = "m_height", 
          name = NULL) # save as data_frame

tree_plot <- tree_plot %>% 
  mutate(zed = scale(tree_plot$m_height, center = TRUE) ) # add for comparison



# Boundaries ZED ----------------------------------------------------------

# make some boundaries 
zed_three <- redwood_avg + (3 * redwood_sd )
zed_three_neg <- redwood_avg - (3 * redwood_sd )

zed_two <- redwood_avg + (2 * redwood_sd )
zed_two_neg <- redwood_avg - (2 * redwood_sd )

zed_one <- redwood_avg + redwood_sd
zed_one_neg  <- redwood_avg - redwood_sd

bundle_zed <- c(zed_three, zed_three_neg, zed_two, 
                zed_two_neg, zed_one, zed_one_neg   )


# Build and pull ----------------------------------------------------------

# please do not change
first_try <- tree_plot %>% 
  ggplot( aes(x = m_height)) + 
  geom_density(fill = "gray", alpha = 0.2) + 
  geom_vline(xintercept = redwood_avg, lty = 3, color = "blue" ) +
  theme_classic() +
  geom_vline(xintercept = bundle_zed, lty = 3, color = "blue")

tree_area <- ggplot_build(first_try)$data[[1]] # grab plot build info



# Add ZED markers and shading ---------------------------------------------


# please do not change
second_try <- first_try +
  geom_area(data = subset(tree_area,  x > zed_three_neg & x < zed_two_neg) , 
            aes(x = x, y = y), fill = "#39FF14", alpha = 0.7) +
  geom_area(data = subset(tree_area,  x < zed_three & x > zed_two) , 
            aes(x = x, y = y), fill = "#39FF14", alpha = 0.7) +
  geom_area(data = subset(tree_area,  x > zed_two_neg & x < zed_one_neg) , 
            aes(x = x, y = y), fill = "#03AC13", alpha = 0.7) +
  geom_area(data = subset(tree_area,  x < zed_two & x > zed_one) , 
            aes(x = x, y = y), fill = "#03AC13", alpha = 0.7) +
  geom_area(data = subset(tree_area,  x < zed_one & x > redwood_avg ) , 
            aes(x = x, y = y), fill = "#0B6623", alpha = 0.7) + 
  geom_area(data = subset(tree_area,  x > zed_one_neg & x < redwood_avg ) , 
            aes(x = x, y = y), fill = "#0B6623", alpha = 0.7) +
  geom_area(data = subset(tree_area,  x < zed_three_neg  ) , 
            aes(x = x, y = y), fill = "yellow", alpha = 0.9)  +
  geom_area(data = subset(tree_area,  x > zed_three  ) , 
            aes(x = x, y = y), fill = "yellow", alpha = 0.9) 


# Add labels and print ----------------------------------------------------


x_ticks <- seq(30, 120, by = 15)
second_try +
  labs(x = "Height in Meters", y = "Density",
       title = "Old Growth Redwood Trees: Height Distribution",
       subtitle = "Shaded areas (& dashed lines) indicate SD regions",
       caption = "Data source: Mark Edward Graham (2015)") +
  scale_x_continuous(breaks = x_ticks )