
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
# Sever ####
function(input, output) {
  
  #
  # tf-idf A: title
  output$tfidfA_title <- renderText({
    
    "Key Words"
    
  })
  
  # tf-idf A: word cloud
  output$tfidfA_WC <- renderPlot({
    
    tf_idf %>%
      
      # keep selected courses
      filter(
        `Course Title` %in% input$tfidfA_course,
        n >= 2
        ) %>%
      
      # take top 9 terms per course
      group_by(
        Code
        ) %>%
      top_n(
        n = 9, 
        tf_idf
        ) %>%
      arrange(
        desc(tf_idf)
      ) %>%
      
      # normalize tf_idf (better results in word cloud); provide angle
      mutate(
        tf_idf_norm = tf_idf / sum(tf_idf),
        angle = 10 * sample( 
          x       = -2 : 2, 
          size    = n(),
          replace = TRUE,
          prob    = c(1, 1, 4, 1, 1)
          )
        ) %>%
      ungroup %>%
      
      # plot
      ggplot(
        aes(
          size  = tf_idf_norm,
          label = word,
          color = tf_idf_norm^0.5,
          angle = angle
          )
        ) +
      
      geom_text_wordcloud_area(
        area_corr_power = 1,
        eccentricity    = 1,
        rm_outside      = TRUE
        ) +
      
      scale_radius(
        range  = c(3.5, 14),
        limits = c(0, NA)
        ) +
      
      scale_color_gradient(
        low = "blue", 
        high = "red"
        ) +
      
      facet_wrap(
        ~ Title_short
        ) +
      
      theme(
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)
        )
    
  })
  
  
  #
  # tf-idf B: title
  output$tfidfB_title <- renderText({
    
    "Key Words"
    
  })
  
  # tf-idf: word cloud
  output$tfidfB_WC <- renderPlot({
    
    tf_idf_cluster %>%
      
      # keep selected cluster
      filter(
        Cluster %in% input$tfidfB_cluster,
        n >= 5
        ) %>%
      
      # take top 9 terms per cluster
      group_by(
        Cluster
        ) %>%
      top_n(
        n = 9, 
        tf_idf
        ) %>%
      arrange(
        desc(tf_idf)
        ) %>%
      
      # normalize tf_idf (better results in word cloud); provide angle
      mutate(
        tf_idf_norm = tf_idf / sum(tf_idf),
        angle = 10 * sample(
          x       = - 2 : 2,
          size    = n(),
          replace = TRUE,
          prob    = c(1, 1, 4, 1, 1)
          )
        ) %>%
      ungroup %>%
      
      # plot
      ggplot(
        aes(
          size = tf_idf_norm,
          label = word,
          angle = angle,
          color = tf_idf_norm^0.5
          )
        ) +
      
      geom_text_wordcloud_area(
        area_corr_power = 1,
        eccentricity    = 1,
        rm_outside      = TRUE
        ) +
      
      scale_radius(
        range  = c(3.5, 14),
        limits = c(0, NA)
        ) +
      
      scale_color_gradient(
        low = "blue", 
        high = "red"
        ) +
      
      facet_wrap(
        ~ Cluster
        ) +
      
      theme(
        panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5)
        )
    
  })
  
  
  #
  # Topic emergence: title
  output$Emergence_title <- renderText({
    
    paste(
      "Emerging (blue) and Declining (red) Terms Between",
      input$emergence_year_old,
      "and", 
      input$emergence_year_recent
      )
    
  })
  
  # Topic emergence: word cloud
  output$Emergence_WC <- renderPlot({
    
    d_description %>%
      
      # keep selected years
      filter(
        `Calendar Year` %in% c(
          input$emergence_year_old, 
          input$emergence_year_recent
          )
        ) %>%
      
      # frequency per year
      count(
        `Calendar Year`, 
        `word`
        ) %>%
      
      # spread along year
      spread(
        key   = `Calendar Year`,
        value = n, 
        fill  = 0
        ) %>%
      
      # rename variables for convenience
      rename(
        old = input$emergence_year_old, 
        new = input$emergence_year_recent
        ) %>%
      
      # log odds ratio (and other variables)
      mutate(
        
        log_odds_ratio = log( ((new + 1) / (sum(new) + 1)) /
                                ((old + 1) / (sum(old) + 1)) ),
        
        Trend          = case_when(
          log_odds_ratio < 0 ~ "Declining",
          log_odds_ratio > 0 ~ "Emerging"
          ),
        
        angle = 10 * sample(
          x       = - 2 : 2,
          size    = n(),
          replace = TRUE,
          prob    = c(1, 1, 4, 1, 1)
          )
        
        ) %>%
      
      # take top 50 words (absolute value)
      filter(
        old + new > 15
        ) %>%
      top_n(
        n = 50,
        wt = abs(log_odds_ratio)
        ) %>%
      arrange(
        abs(log_odds_ratio)
      ) %>%
      
      # Plot
      ggplot(
        aes(
          size        = abs(log_odds_ratio),
          label       = word,
          angle       = angle, 
          color       = Trend,
          angle_group = log_odds_ratio < 0
          )
        ) +
      
      geom_text_wordcloud_area(
        area_corr_power = 1,
        eccentricity    = 1.25,
        rm_outside      = TRUE
        ) +
      
      scale_radius(
        range  = c(5, 20), 
        limits = c(0, NA)
        ) +
      
      theme(
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)
        )
    
  })
  
  
  #
  # Modeling A: beta
  output$modelingA_beta_title <- renderText({
    
    "Key Words of Each Topic"
    
  })
  output$modelingA_beta_plot  <- renderPlot({
    
    get(input$modelingA_ntopic)$beta %>%
      
      # take top 10 terms
      mutate(
        topic = factor(topic)
        ) %>%
      group_by(
        topic
        ) %>%
      top_n(
        n  = 10,
        wt = beta
        ) %>%
      ungroup %>%
      
      ggplot(
        aes(
          x = reorder_within(
            x      = term,
            by     = beta,
            within = topic
            ),
          y    = beta,
          fill = topic
          )
        ) +
      
      geom_col(
        show.legend = FALSE
        ) +
      
      facet_wrap(
        ~ topic, 
        scales = "free"
        ) +
      scale_x_reordered() +
      
      labs(
        x = NULL, 
        y = "Beta distribution"
        ) +
      coord_flip() +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5)
        )
    
  })
  
  # Modeling A: gamma1
  output$modelingA_gamma1_title <- renderText({
    
    "Main Courses of Each Topic"
    
  })
  output$modelingA_gamma1_plot  <- renderPlot({
    
    get(input$modelingA_ntopic)$gamma %>%
      mutate(topic = factor(topic)) %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      filter(`Course Title` %in% input$modelingA_course,
             gamma > 1e-3) %>%
      group_by(topic) %>%
      top_n(10, gamma) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(Title_short, by = gamma, within = topic), y = gamma, fill = topic)) +
      geom_col(show.legend = FALSE) +
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
    
    get(input$modelingA_ntopic)$gamma %>%
      mutate(topic = factor(topic)) %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      filter(`Course Title` %in% input$modelingA_course,
             gamma > 1e-3) %>%
      group_by(document) %>%
      top_n(5, gamma) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(topic, by = gamma, within = document), y = gamma, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ Title_short, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma Distribution") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  #
  # Modeling B: beta
  output$modelingB_beta_title <- renderText({
    
    "Key Terms of Each Topic"
    
  })
  output$modelingB_beta_plot  <- renderPlot({
    
    get(input$modelingB_ntopic)$beta %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup %>%
      mutate(topic = factor(topic)) %>%
      
      ggplot(aes(x = reorder_within(term, by = beta, within = topic), y = beta, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ topic, scales = "free") +
      scale_x_reordered() +
      labs(x = NULL, y = "Beta distribution") +
      coord_flip() +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Modeling B: gamma1
  output$modelingB_gamma1_title <- renderText({
    
    "Main Clusters of Each Topic"
    
  })
  output$modelingB_gamma1_plot  <- renderPlot({
    
    get(input$modelingB_ntopic)$gamma %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      filter(Cluster %in% input$modelingB_cluster) %>%
      group_by(Cluster, topic) %>%
      summarise(gamma = sum(gamma)) %>%
      filter(gamma > 1e-3) %>%
      group_by(topic) %>%
      top_n(10, gamma) %>%
      ungroup %>%
      mutate(topic = factor(topic)) %>%
      
      ggplot(aes(reorder_within(Cluster, by = gamma, within = topic), y = gamma, fill = topic)) +
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
    
    get(input$modelingB_ntopic)$gamma %>%
      mutate(topic = factor(topic)) %>%
      left_join(d_course, by = c("document" = "Code")) %>%
      filter(Cluster %in% input$modelingB_cluster) %>%
      group_by(Cluster, topic) %>%
        summarise(gamma = sum(gamma)) %>%
        filter(gamma > 1e-2) %>%
      group_by(Cluster) %>%
        top_n(5, gamma) %>%
      ungroup %>%
      
      ggplot(aes(reorder_within(topic, by = gamma, within = Cluster), y = gamma, fill = topic)) +
      geom_col(show.legend = F) +
      facet_wrap(~ Cluster, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = NULL, y = "Gamma Distribution") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
}