####Bump chart function####

create_ranking_plot <- function(ranking_df, metric, submetric) {
  
  d <- ranking_df %>%
    filter(str_detect(category, metric)) %>% 
    mutate(top_state = ifelse(ranking <= 10 & 
                                category == submetric, "Top 10", "Others")) %>% 
    mutate(category_wrapped = str_wrap(category, width = 10))
  
  # Set colors based on the metric
  line_color <- switch(tolower(metric),
                       "disbursement" = "#FF5733",
                       "fatalities" = "#C70039",
                       "pavement roughness" = "purple") 
  
  # Get the rightmost category
  rightmost_category <- max(d$category_wrapped)
  
  # Create a ggbump chart highlighting top 10 states
  p <- d %>%
    ggplot(aes(x = category_wrapped, y = ranking, group = state, 
               color = top_state, size = top_state,
               text = paste("State: ", state, "<br>Category: ", category, "<br>Ranking: ", ranking))) +
    geom_bump() +
    geom_point(aes(color = top_state, size = top_state)) +
    geom_text(data = d %>% filter(category == submetric & 
                                    top_state == "Top 10"), 
              aes(x = rightmost_category,label = state), 
              hjust = 0, vjust = 0.5, size = 1.5, nudge_x = 0.3, color = line_color) +
    scale_color_manual(values = c("Top 10" = line_color, "Others" = "gray")) +
    scale_size_manual(values = c("Top 10" = 0.5, "Others" = 0.1)) +
    theme_minimal() +
    labs(title = paste("Rankings by", str_to_title(metric)),
         x = "",
         y = "") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, hjust = 1)
    )
  
  # Convert the ggplot2 plot to an interactive plot using plotly
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  # Customize the tooltip appearance
  interactive_plot <- interactive_plot %>%
    layout(hoverlabel = list(
      bgcolor = "white",
      font = list(size = 10)
    ))
  
  # Display the interactive plot
  return(interactive_plot)
}

#Testing
metric <- "Fatalities"
submetric <- "Rural Fatalities Score"
create_ranking_plot(ranking_df, metric, submetric)


####No submetric ranking####

point_color = c("Overall Score" = "#008E75", 
                "Bridges" = "#900C3F", 
                "Congestion Hours" = "#CA3311")

# Define the function to generate plots for each metric
no_submetric_ranking_plots <- function(ranking_df, metrics) {
  
  for (metric in metrics) {
    p <- ranking_df %>% 
      filter(category == metric) %>% 
      ggplot(aes(x = ranking, y = reorder(state, ranking))) +
      geom_col(fill = "gray", width = 0.1) +
      geom_point(size = 3, color = point_color[metric]) +
      theme_minimal() +
      labs(title = paste("Rankings by", metric),
           x = "Ranking",
           y = "")+
      theme(axis.text.y = element_text(size = 6))
    
    return(p)
    
  }
}

# List of metrics to loop through
metrics <- c("Bridges")
#, "Bridges", "Congestion Hours"
# Generate the plots
no_submetric_ranking_plots(ranking_df, metrics)

