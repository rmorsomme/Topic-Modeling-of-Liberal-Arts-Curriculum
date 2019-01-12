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