
# Setup ####
library(shiny)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
load("results_data.RDATA")

# Functions for ordering bars within facet in ggplot.
# From: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}


#
# Sever
function(input, output) {
  
  #
  # tf-idf A: title
  output$tfidfA_title <- renderText({
    
    "Most Distinctive Terms"
    
  })
  
  # tf-idf A: barplot
  output$tfidfA_BP <- renderPlot({
    
    tf_idf %>%
      filter(`Course Title` %in% input$tfidfA_course, 
             n >= 2) %>%
      group_by(Code) %>%
      top_n(9, tf_idf) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(word, tf_idf, Code), tf_idf)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      scale_x_reordered() +
      facet_wrap(~ Title_short, scales = "free") +
      coord_flip() +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # tf-idf A: word cloud
  output$tfidfA_WC <- renderPlot({
    
    tf_idf %>%
      filter(`Course Title` %in% input$tfidfA_course, 
             n >= 2) %>%
      group_by(Code) %>%
      top_n(9, tf_idf) %>%
      mutate(tf_idf_norm = tf_idf / sum(tf_idf), # normalization for better results in wordcloud
             angle = 10 * sample(-2:2, n(), replace = TRUE, prob = c(1,1,4,1,1))) %>%
      ungroup %>%
      
      ggplot(aes(size = order(tf_idf_norm^0.7, tf_idf_norm), label = word,
                 angle = angle, color = tf_idf_norm^0.7)) +
      geom_text_wordcloud_area(area_corr_power = 1,
                               eccentricity    = 1,
                               rm_outside      = T) +
      scale_radius(range = c(3, 7), limits = c(0, NA)) +
      scale_color_gradient(low = "red", high = "blue") +
      facet_wrap(~ `Course Title`) +
      theme(panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  #
  # tf-idf B: title
  output$tfidfB_title <- renderText({
    
    "Most Distinctive Terms"
    
  })
  
  # tf-idf B: barplot
  output$tfidfB_BP <- renderPlot({
    
    tf_idf_cluster %>%
      filter(n >= 5,
             Cluster %in% input$tfidfB_cluster) %>%
      group_by(Cluster) %>%
      top_n(9, tf_idf) %>%
      mutate(tf_idf_norm = tf_idf / sum(tf_idf), 
             angle = 10 * sample(-2:2, n(), replace = T, prob = c(1,1,4,1,1))) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(word, tf_idf, Cluster), tf_idf)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      scale_x_reordered() +
      facet_wrap(~ Cluster, scales = "free") +
      coord_flip() +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # tf-idf: word cloud
  output$tfidfB_WC <- renderPlot({
    
    tf_idf_cluster %>%
      filter(n >= 5,
             Cluster %in% input$tfidfB_cluster) %>%
      group_by(Cluster) %>%
      top_n(9, tf_idf) %>%
      mutate(tf_idf_norm = tf_idf / sum(tf_idf), # normalization for better results in wordcloud
             angle = 10 * sample(-2:2, n(), replace = T, prob = c(1,1,4,1,1))) %>%
      ungroup %>%
      
      ggplot(aes(size = order(tf_idf_norm^0.7, tf_idf_norm), label = word, angle = angle, color = tf_idf_norm^0.7)) +
      geom_text_wordcloud_area(area_corr_power = 1,
                               eccentricity    = 1,
                               rm_outside      = T) +
      scale_radius(range = c(3, 6), limits = c(0, NA)) +
      scale_color_gradient(low = "red", high = "blue") +
      facet_wrap(~ Cluster) +
      theme(panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  #
  # Topic emergence: title
  output$Emergence_title <- renderText({
    
    paste("Emerging (blue) and Declining (red) Terms Between", input$emergence_year_old, "and", input$emergence_year_recent)
    
  })
  
  # Topic emergence: barplot
  output$Emergence_BP <- renderPlot({
    
    d_description_stem %>%
      filter(`Calendar Year` %in% c(input$emergence_year_old, input$emergence_year_recent)) %>%
      count(`Calendar Year`, `word`) %>%
      spread(key = `Calendar Year`, value = n, fill = 0) %>%
      rename(old = input$emergence_year_old, new = input$emergence_year_recent) %>%
      mutate(n         = old + new,
             `Log Ratio` = log( ((new+1) / (sum(new)+1)) /
                                  ((old+1) / (sum(old)+1)) ),
             Trend     = case_when(`Log Ratio`<0 ~ "Declining",
                                   `Log Ratio`>0 ~ "Emerging")) %>%
      filter(n > 15) %>%
      top_n(50, abs(`Log Ratio`)) %>%
      
      ggplot(aes(reorder(word, `Log Ratio`), `Log Ratio`, fill = Trend)) +
      geom_col() +
      coord_flip() +
      labs(x = "Terms", y = "Log Ratio") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Topic emergence: word cloud
  output$Emergence_WC <- renderPlot({
    
    d_description_stem %>%
      filter(`Calendar Year` %in% c(input$emergence_year_old, input$emergence_year_recent)) %>%
      count(`Calendar Year`, `word`) %>%
      spread(key = `Calendar Year`, value = n, fill = 0) %>%
      rename(old = input$emergence_year_old, new = input$emergence_year_recent) %>%
      mutate(n         = old + new,
             `Log Ratio` = log( ((new+1) / (sum(new)+1)) /
                                  ((old+1) / (sum(old)+1)) ),
             Trend     = case_when(`Log Ratio`<0 ~ "Declining",
                                   `Log Ratio`>0 ~ "Emerging")) %>%
      filter(n > 15) %>%
      top_n(50, abs(`Log Ratio`)) %>%
      mutate(angle = 10 * sample(-2:2, n(), replace = T, prob = c(1,1,4,1,1))) %>%
      
      ggplot(aes(size = abs(`Log Ratio`), label = word, angle = angle, color = Trend, angle_group = `Log Ratio` < 0)) +
      geom_text_wordcloud_area(area_corr_power = 1,
                               eccentricity    = 1,
                               rm_outside      = T) +
      scale_radius(range = c(3, 14), limits = c(0, NA)) +
      theme(panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  #
  # Modeling A: beta
  output$modelingA_beta_title <- renderText({
    
    "Main Terms of each Topic"
    
  })
  output$modelingA_beta_plot  <- renderPlot({
    
    model <- get(input$modelingA_ntopic)
    
    tidy(model, matrix = "beta") %>%
      mutate(topic = paste("Topic", topic)) %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup %>%
      
      ggplot(aes(x = reorder_within(term, by = beta, within = topic), y = beta, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ topic, scales = "free") +
      scale_x_reordered() +
      labs(x = "Terms", y = "Beta distribution") +
      coord_flip() +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling A: gamma1
  output$modelingA_gamma1_title <- renderText({
    
    "Main Courses of each Topic"
    
  })
  output$modelingA_gamma1_plot  <- renderPlot({
    
    model   <- get(input$modelingA_ntopic)
    
    tidy(model, matrix = "gamma") %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      mutate(topic = paste("Topic", topic)) %>%
      filter(`Course Title` %in% input$modelingA_course,
             gamma > 0.05) %>%
      rename(facet = Title_short) %>%
      
      ggplot(aes(reorder_within(facet, by = gamma, within = topic), y = gamma, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ topic, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma Distribution") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling A: gamma2
  output$modelingA_gamma2_title <- renderText({
    
    "Main Topics per Courses"
    
  })
  output$modelingA_gamma2_plot  <- renderPlot({
    
    model   <- get(input$modelingA_ntopic)
    
    tidy(model, matrix = "gamma") %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      mutate(topic = paste("Topic", topic)) %>%
      filter(`Course Title` %in% input$modelingA_course,
             gamma > 0.05) %>%
      rename(facet = Title_short) %>%
      
      ggplot(aes(reorder_within(topic, by = gamma, within = facet), y = gamma, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ facet, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  #
  # Modeling B: beta
  output$modelingB_beta_title <- renderText({
    
    "Main Terms of each Topic"
    
  })
  output$modelingB_beta_plot  <- renderPlot({
    
    model <- get(input$modelingB_ntopic)
    
    tidy(model, matrix = "beta") %>%
      mutate(topic = paste("Topic", topic)) %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup %>%
      
      ggplot(aes(x = reorder_within(term, by = beta, within = topic), y = beta, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ topic, scales = "free") +
      scale_x_reordered() +
      labs(x = "Terms", y = "Beta distribution") +
      coord_flip() +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling B: gamma1
  output$modelingB_gamma1_title <- renderText({
    
    "Main Clusters of each Topic"
    
  })
  output$modelingB_gamma1_plot  <- renderPlot({
    
    model   <- get(input$modelingB_ntopic)
    
    tidy(model, matrix = "gamma") %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      mutate(topic = paste("Topic", topic)) %>%
      filter(Cluster %in% input$modelingB_cluster) %>%
      rename(facet = Cluster) %>%
      group_by(facet, topic) %>%
      summarise(gamma = sum(gamma)) %>%
      filter(gamma > 0.05) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(facet, by = gamma, within = topic), y = gamma, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ topic, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma Distribution") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling B: gamma2
  output$modelingB_gamma2_title <- renderText({
    
    "Main Topics per Cluster"
    
  })
  output$modelingB_gamma2_plot  <- renderPlot({
    
    model   <- get(input$modelingA_ntopic)
    
    tidy(model, matrix = "gamma") %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      mutate(topic = paste("Topic", topic)) %>%
      filter(Cluster %in% input$modelingB_cluster) %>%
      rename(facet = Cluster) %>%
      group_by(facet, topic) %>%
      summarise(gamma = sum(gamma)) %>%
      filter(gamma > 0.05) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(topic, by = gamma, within = facet), y = gamma, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ facet, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  #
  # Modeling C: beta
  output$modelingC_beta_title <- renderText({
    
    "Main Terms of each Topic"
    
  })
  output$modelingC_beta_plot  <- renderPlot({
    
    table_topic <- tibble(`Topic Name` = c("Arts", "Psycho/policy", "(Int.) Law",
                                           "Development", "Culture", "Qual. Res.",
                                           "Engineering", "Biology", "Society",
                                           "Foreign Policy", "Reserach", "Skills"),
                          topic_number = 1 : 12)
    
    tidy(LDA_12, matrix = "beta") %>%
      left_join(table_topic, by = c("topic" = "topic_number")) %>%
      group_by(`Topic Name`) %>%
      top_n(10, beta) %>%
      ungroup %>%
      
      ggplot(aes(x = reorder_within(term, by = beta, within = `Topic Name`), y = beta, fill = `Topic Name`)) +
      geom_col(show.legend = F) +
      facet_wrap(~ `Topic Name`, scales = "free") +
      scale_x_reordered() +
      labs(x = "Terms", y = "Beta distribution") +
      coord_flip() +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling C: gamma1
  output$modelingC_gamma1_title <- renderText({
    
    "Main Clusters of each Topic"
    
  })
  output$modelingC_gamma1_plot  <- renderPlot({
    
    table_topic <- tibble(`Topic Name` = c("Arts", "Psycho/policy", "(Int.) Law",
                                           "Development", "Culture", "Qual. Res.",
                                           "Engineering", "Biology", "Society",
                                           "Foreign Policy", "Reserach", "Skills"),
                          topic_number = 1 : 12)
    
    tidy(LDA_12, matrix = "gamma") %>%
      left_join(table_topic, by = c("topic" = "topic_number")) %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      rename(facet = Cluster) %>%
      group_by(facet, `Topic Name`) %>%
      summarise(gamma = sum(gamma)) %>%
      filter(gamma > 0.05) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(facet, by = gamma, within = `Topic Name`), y = gamma, fill = `Topic Name`)) +
      geom_col(show.legend = F) +
      facet_wrap(~ `Topic Name`, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma Distribution") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling C: gamma2
  output$modelingC_gamma2_title <- renderText({
    
    "Main Topics per Cluster"
    
  })
  output$modelingC_gamma2_plot  <- renderPlot({
    
    table_topic <- tibble(`Topic Name` = c("Arts", "Psycho/policy", "(Int.) Law",
                                           "Development", "Culture", "Qual. Res.",
                                           "Engineering", "Biology", "Society",
                                           "Foreign Policy", "Reserach", "Skills"),
                          topic_number = 1 : 12)
    
    tidy(LDA_12, matrix = "gamma") %>%
      left_join(table_topic, by = c("topic" = "topic_number")) %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      rename(facet = Cluster) %>%
      group_by(facet, `Topic Name`) %>%
      summarise(gamma = sum(gamma)) %>%
      filter(gamma > 0.05,
             ! is.na(facet)) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(`Topic Name`, by = gamma, within = facet), y = gamma, fill = `Topic Name`)) +
      geom_col(show.legend = F) +
      facet_wrap(~ facet, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
}