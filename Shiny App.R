
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


# Define UI for app ####
ui <- navbarPage(
  
  # App title
  "Results",
  
  # Panel 1 for tf-idf ####
  navbarMenu(
    
    # Panel title
    title = "Important Terms",
    
    # Subpanel 1.1: tf-idf - course
    tabPanel(
      
      # SubPanel title
      title = "Course",
      
      # Sidebar layout
      sidebarLayout(
        
        # Sidebar panel for input
        sidebarPanel(
          
          # Input: button for Courses
          selectizeInput(
            inputId = "tfidfA_course",
            label = "Courses (max 9)",
            choice = sort(unique(tf_idf$`Course Title`)),
            selected = unique(tf_idf$`Course Title`)[168 : 172],
            multiple = TRUE,
            options = list(maxItems = 9)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "tfidfA_title"), align = "center"),
          
          # Output: Barplot
          plotOutput(outputId = "tfidfA_BP"),
          
          # Output: Word cloud
          plotOutput(outputId = "tfidfA_WC")
          
        )
        
      )
      
    ),
    
    # Subpanel 1.2 tf-idf - Cluster
    tabPanel(
      
      # Panel title
      title = "Cluster",
      
      # Sidebar layout
      sidebarLayout(
        
        # Sidebar panel for input
        sidebarPanel(
          
          # Input: button for Courses
          selectizeInput(
            inputId = "tfidfB_cluster",
            label = "Cluster (max 9)",
            choice = sort(unique(tf_idf$Cluster)),
            selected = unique(tf_idf$Cluster)[1 : 5],
            multiple = TRUE,
            options = list(maxItems = 9)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "tfidfB_title"), align = "center"),
          
          # Output: Barplot
          plotOutput(outputId = "tfidfB_BP"),
          
          # Output: Word cloud
          plotOutput(outputId = "tfidfB_WC")
          
        )
        
      )
      
    )
    
  ),
  
  # Panel 2 for term emergence ####
  tabPanel(
    
    # Panel title
    title = "Term Emergence",
    
    fluidPage(
      
      # Sidebar layout
      sidebarLayout(
        
        # Sidebar panel for input
        sidebarPanel(
          
          # Input: Buttons for the years
          radioButtons(inputId = "emergence_year_old",
                       label = "Year (old)",
                       choices = c("2014-2015", "2015-2016", "2017-2018", "2018-2019"),
                       selected = "2014-2015"),
          
          radioButtons(inputId = "emergence_year_recent",
                       label = "Year (recent)",
                       choices = c("2014-2015", "2015-2016", "2017-2018", "2018-2019"),
                       selected = "2018-2019")
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "Emergence_title"), align = "center"),
          
          # Output: Barplot
          plotOutput(outputId = "Emergence_BP"),
          
          # Output: Word Cloud
          plotOutput(outputId = "Emergence_WC")
          
        )
        
      )
      
    )
    
  ),
  
  # Panel 3 for topic modeling ----
  navbarMenu(
    
    # Panel title
    title = "Topic Modeling",
    
    # Panel 3.1: topic modeling - course
    tabPanel(
      
      # Panel title
      title = "Course",
      
      # Sidebar layout
      sidebarLayout(
        
        # Sidebar panel for input
        sidebarPanel(
          
          # Input: button for number of topics
          radioButtons(
            inputId = "modelingA_ntopic",
            label = "Number of Topics",
            choices = c("5"  = "LDA_5",
                        "12" = "LDA_12",
                        "17" = "LDA_17",
                        "25" = "LDA_25"),
            select = "LDA_12"
          ),
          
          # Input: button for Courses
          selectizeInput(
            inputId = "modelingA_course",
            label = "Courses (max 9)",
            choice = sort(unique(tf_idf$`Course Title`)),
            selected = c(unique(tf_idf$`Course Title`)[168 : 172], "Computer Science", "Optimization", "Philosophy of Language"),
            multiple = TRUE,
            options = list(maxItems = 9)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Barplot
          h4(textOutput(outputId = "modelingA_beta_title"), align = "center"),
          plotOutput(outputId = "modelingA_beta_plot"),
          
          # Output: Word cloud
          h4(textOutput(outputId = "modelingA_gamma1_title"), align = "center"),
          plotOutput(outputId = "modelingA_gamma1_plot"),
          
          # Output: Word cloud
          h4(textOutput(outputId = "modelingA_gamma2_title"), align = "center"),
          plotOutput(outputId = "modelingA_gamma2_plot")
          
        )
        
      )
      
    ),
    
    # Panel 3B: topic modeling - cluster
    tabPanel(
      
      # Panel title
      title = "Cluster",
      
      # Sidebar layout
      sidebarLayout(
        
        # Sidebar panel for input
        sidebarPanel(
          
          # Input: button for number of topics
          radioButtons(
            inputId = "modelingB_ntopic",
            label = "Number of Topics",
            choices = c("5"  = "LDA_5",
                        "12" = "LDA_12",
                        "17" = "LDA_17",
                        "25" = "LDA_25"),
            select = "LDA_12"
          ),
          
          # Input: button for Courses
          selectizeInput(
            inputId = "modelingB_cluster",
            label = "Cluster (max 9)",
            choice = sort(unique(d_course$Cluster[!is.na(d_course$Cluster)])),
            selected = unique(d_course$Cluster[!is.na(d_course$Cluster)])[1:5],
            multiple = TRUE,
            options = list(maxItems = 9)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Barplot
          h4(textOutput(outputId = "modelingB_beta_title"), align = "center"),
          plotOutput(outputId = "modelingB_beta_plot"),
          
          # Output: Word cloud
          h4(textOutput(outputId = "modelingB_gamma1_title"), align = "center"),
          plotOutput(outputId = "modelingB_gamma1_plot"),
          
          # Output: Word cloud
          h4(textOutput(outputId = "modelingB_gamma2_title"), align = "center"),
          plotOutput(outputId = "modelingB_gamma2_plot")
          
        )
        
      )
      
    ),
    
    
    # Panel 3C: topic modeling - label
    tabPanel(
      
      # Panel title
      title = "Labeled Topics",
      
      # Sidebar layout
      sidebarLayout(
        
        sidebarPanel = NULL,
        
        # Main panel (output)
        mainPanel = mainPanel(
          
          # Output: Barplot
          h4(textOutput(outputId = "modelingC_beta_title"), align = "center"),
          plotOutput(outputId = "modelingC_beta_plot"),
          
          # Output: Word cloud
          h4(textOutput(outputId = "modelingC_gamma1_title"), align = "center"),
          plotOutput(outputId = "modelingC_gamma1_plot"),
          
          # Output: Word cloud
          h4(textOutput(outputId = "modelingC_gamma2_title"), align = "center"),
          plotOutput(outputId = "modelingC_gamma2_plot")
          
        )
        
      )
      
    )
    
  )
  
)


# Define server for app ####
server <- function(input, output) {
  
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

# Create Shiny app ####
shinyApp(ui = ui, server = server)