
# Setup ####
library(shiny)
library(tidyverse)
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
# Default Course (9)
course_default <- d_course %>%
  filter(
    Code %in% c(
      "HUM1010", "SSC2043", "SSC2006", "HUM2022",
      "SCI2002", "SSC3057", "SSC3033", "SKI3002", "SCI2036",
      "HUM3043", "SSC3032", "SSC2004", "COR1003", "HUM2018"
      )
    ) %>%
  pull(
    `Course Title`
    )



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
            label = "Courses (max 16)",
            choice = sort(unique(tf_idf$`Course Title`)),
            selected = course_default,
            multiple = TRUE,
            options = list(maxItems = 16)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "tfidfA_title"), align = "center"),
          
          # Output: Word cloud
          plotOutput(outputId = "tfidfA_WC", height = "600px")
          
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
            label = "Cluster (max 16)",
            choice = sort(unique(tf_idf$Cluster)),
            selected = unique(tf_idf$Cluster)[1 : 10],
            multiple = TRUE,
            options = list(maxItems = 16)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "tfidfB_title"), align = "center"),
          
          # Output: Word cloud
          plotOutput(outputId = "tfidfB_WC", height = "600px")
          
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
          radioButtons(
            inputId = "emergence_year_old",
            label = "Year (old)",
            choices = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"),
            selected = "2014-2015"
            ),
          
          radioButtons(
            inputId = "emergence_year_recent",
            label = "Year (recent)",
            choices = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"),
            selected = "2018-2019"
            )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Title of plots
          h3(textOutput(outputId = "Emergence_title"), align = "center"),
          
          # Output: Word Cloud
          plotOutput(outputId = "Emergence_WC", height = "600px")
          
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
            choices = c("5 (labeled topics)"     = "LDA_5" ,
                        "25 (unlabeled topics)" = "LDA_25"),
            select = "LDA_5"
          ),
          
          # Input: button for Courses
          selectizeInput(
            inputId = "modelingA_course",
            label = "Courses (max 16)",
            choice = sort(unique(tf_idf$`Course Title`)),
            selected = course_default,
            multiple = TRUE,
            options = list(maxItems = 16)
          )
          
        ),
        
        # Main panel (output)
        mainPanel(
          
          # Output: Tabset
          tabsetPanel(
            
            type = "tabs",
                      
            tabPanel(
              title = "Key Words of Each Topic", 
              h4(textOutput(outputId = "modelingA_beta_title"), align = "center"),
              plotOutput(outputId = "modelingA_beta_plot", height = "600px")
              ),
            
            tabPanel(
              title = "Main Courses of Each Topic", 
              h4(textOutput(outputId = "modelingA_gamma1_title"), align = "center"),
              plotOutput(outputId = "modelingA_gamma1_plot", height = "600px")
              ),
            
            tabPanel(
              title = "Main Topics per Courses",
              h4(textOutput(outputId = "modelingA_gamma2_title"), align = "center"),
              plotOutput(outputId = "modelingA_gamma2_plot", height = "600px")
              )
            
            )
          
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
            choices = c("5 (labeled topics)"    = "LDA_5" ,
                        "25 (unlabeled topics)" = "LDA_25"),
            select = "LDA_5"
          ),
          
          # Input: button for Courses
          selectizeInput(
            inputId = "modelingB_cluster",
            label = "Cluster (max 16)",
            choice = sort(unique(d_course$Cluster[!is.na(d_course$Cluster)])),
            selected = unique(d_course$Cluster[!is.na(d_course$Cluster)])[1 : 16],
            multiple = TRUE,
            options = list(maxItems = 16)
          )
          
        ),
        
        
        # Main panel (output)
        mainPanel(
          
          # Output: Tabset
          tabsetPanel(
            
            type = "tabs",
            
            tabPanel(
              title = "Key Words of Each Topic", 
              h4(textOutput(outputId = "modelingB_beta_title"), align = "center"),
              plotOutput(outputId = "modelingB_beta_plot", height = "600px")
            ),
            
            tabPanel(
              title = "Main Clusters of Each Topic", 
              h4(textOutput(outputId = "modelingB_gamma1_title"), align = "center"),
              plotOutput(outputId = "modelingB_gamma1_plot", height = "600px")
            ),
            
            tabPanel(
              title = "Main Topics per Clusters",
              h4(textOutput(outputId = "modelingB_gamma2_title"), align = "center"),
              plotOutput(outputId = "modelingB_gamma2_plot", height = "600px")
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)