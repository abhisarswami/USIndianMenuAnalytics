library(readxl)
library(tidyverse)
library(ggplot2)
indian_food_cleaned_0_1 <- read_excel("C:/Abhisar/Training/Google Analytics Certificate Program/Capstone Project/indian_food_cleaned_0.1.xlsx")

dividend_adjusted_course_by_state <- function(course_type) {
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
             (cousine_count * demographic_dividend)) %>% 
    arrange(desc(effective_cousine_count)) %>% 
    head(5)
  
  return(indian_food_course_mutated_eff_count)
}

plot_cousine_top_5_removing_outlier <- function(course_meal, outlier) {
  result_data <- dividend_adjusted_course_by_state(course_meal)
  topt_5 <- indian_food_cleaned_0_1 %>% 
    filter(course == course_meal, !(name %in% outlier)) %>% 
    filter(state %in% result_data$state) %>% 
    arrange(desc(state))
  # plot
  return(topt_5)
}

plot_graph <- function(data,course_meal) {
  x <- ggplot(data) + 
    geom_point(mapping=aes(x=name, y=cook_time, group = 1,color=state)) +
    facet_wrap(~state) +
    geom_hline(yintercept=45, linetype="dashed", 
               color = "red", size=1) +
    theme(axis.text.x =  element_blank(),  
          panel.grid.minor = element_blank()) +  
    xlab("Cousine") + ylab("Cook Time") + ggtitle(paste(str_to_sentence(course_meal), " by cooking time"))
  print(x)
}

top_5 <- plot_cousine_top_5_removing_outlier("dessert", c("Shrikhand")) 
graphical_presentation <- plot_graph(top_5, "dessert")
write.csv(top_5, "C:/Abhisar/Training/Google Analytics Certificate Program/Capstone Project/final_dessert.csv")
top_5 <- plot_cousine_top_5_removing_outlier("snack", c(""))
graphical_presentation <- plot_graph(top_5, "snack")
write.csv(top_5, "C:/Abhisar/Training/Google Analytics Certificate Program/Capstone Project/final_snack.csv")
top_5 <- plot_cousine_top_5_removing_outlier("main course", c(""))
graphical_presentation <- plot_graph(top_5, "main course")
write.csv(top_5, "C:/Abhisar/Training/Google Analytics Certificate Program/Capstone Project/final_main.csv")





