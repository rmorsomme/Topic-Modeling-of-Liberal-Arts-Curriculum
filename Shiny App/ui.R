
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
# UI
navbarPage(
  
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
                       choices = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"),
                       selected = "2014-2015"),
          
          radioButtons(inputId = "emergence_year_recent",
                       label = "Year (recent)",
                       choices = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"),
                       selected = "2018-2019")
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "Emergence_title"), align = "center"),
          
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
            choices = c("5 (with labels)"    = "LDA_5" ,
                        "20 (without labels)" = "LDA_20"),
            select = "LDA_5"
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
            choices = c("5 (with labels)"    = "LDA_5" ,
                        "20 (without labels)" = "LDA_20"),
            select = "LDA_5"
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
      
    )
    
  )
  
)