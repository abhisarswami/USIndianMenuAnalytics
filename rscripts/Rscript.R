library(readxl)
library(tidyverse)
library(ggplot2)
indian_food_cleaned_0_1 <- read_excel("C:/Abhisar/Training/Google Analytics Certificate Program/Capstone Project/indian_food_cleaned_0.1.xlsx")
View(indian_food_cleaned_0_1)

top_5_course_meal_state <- function(course_type) {

  # selection for main course
  indian_food_course <- indian_food_cleaned_0_1 %>% 
    filter(course == course_type) %>% 
    group_by(region, state, course) %>% 
    summarize(cousine_count = n())
  
  indian_food_course_mutated_dividend <- indian_food_course %>%  mutate(tot_eff_cousine_count_pre_dividend = sum(subset(indian_food_course, course == course_type)$cousine_count)) %>% 
    mutate(percent_cousine_pre = (cousine_count / tot_eff_cousine_count_pre_dividend) * 100) %>% 
    mutate(demographic_dividend = case_when(
      (region == 'West') ~ 1.5,
      (region == 'South') ~ 1.25,
      (region == 'North') ~ 1.1,
      TRUE ~ 1.0
    ))
  
  indian_food_course_mutated_eff_count <- indian_food_course_mutated_dividend %>% 
    mutate(effective_cousine_count = 
              (cousine_count * demographic_dividend))
  
  indian_food_course_mutated_tot_eff_count <- indian_food_course_mutated_eff_count %>% 
    mutate(tot_eff_cousine_count_post_dividend = sum(indian_food_course_mutated_eff_count$effective_cousine_count)) %>% 
    arrange(desc(effective_cousine_count)) %>% 
    mutate(percent_cousine_post = as.integer((effective_cousine_count / tot_eff_cousine_count_post_dividend) * 100)) %>% 
    arrange(desc(effective_cousine_count)) %>% 
    head(5)
  
  
  ggplot(indian_food_course_mutated_tot_eff_count, aes(x = "", y = percent_cousine_post, fill = fct_inorder(state))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    guides(fill = guide_legend(title = "State")) +
    geom_text(aes(label = indian_food_course_mutated_tot_eff_count$percent_cousine_post),
              position = position_stack(vjust = 0.5)) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 15), 
          panel.background = element_rect(fill = "white")) +
    labs(x = "", y = "", title = paste("Top 5 states with ", course_type ," choices \n (along with Demographic Dividend)"),
         fill = "Colour Choices")
}

top_5_course_meal_state("main course")
top_5_course_meal_state("snack")
top_5_course_meal_state("dessert")
