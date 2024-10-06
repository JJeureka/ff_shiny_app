get_all_years <- function(league_input, start_year, end_year) {
  
  total_dataset <- data.frame()
  
  for (year in start_year:end_year) {
    
    year_data <- display_data_function(league_input, year)
    
    total_dataset <- dplyr::bind_rows(total_dataset, year_data)  # FIXED: Now adds to total_dataset correctly
  }
  

  print(total_dataset) 
  
  summary_table <- total_dataset %>%
    group_by(displayName) %>%
    summarise(
      firstName = first(firstName),  # Keep only the first occurrence of firstName cuz ppl changed their name
      avg_points_against = round(mean(pointsAgainst), 2),
      avg_points_for = round(mean(pointsFor), 2)
    ) %>%
    arrange(desc(avg_points_for))

  

  summary_table <- summary_table %>%
    rename(
      `Team Name` = firstName,
      `Points Against` = avg_points_against,
      `Points For` = avg_points_for
    )
  

  summary_table <- kable(summary_table, 
                         caption = "Points For and Against", 
                         align = c("l", "c", "c")) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = TRUE,  # Set to TRUE to make the table wider
      position = "center"
    ) %>%
    column_spec(1, width = "15em") %>%
    column_spec(2, width = "15em") %>%
    column_spec(3, width = "15em") %>%
    row_spec(0, bold = TRUE, color = "white", background = "#00008B", extra_css = "padding: 20px;")
  
  return(as.character(summary_table))
}

display_data_function <- function(league_input, year) {
  
  url_output <- paste0("https://lm-api-reads.fantasy.espn.com/apis/v3/games/FFL/leagueHistory/", league_input,
                       "?view=mMatchupScore&view=mStatus&view=mSettings&view=mTeam&view=modular&view=mNav&seasonId=", year)
  
  all_crew_data <- jsonlite::fromJSON(url_output)
  
  members_crew_data <- all_crew_data$members[[1]]
  teams_crew_data <- all_crew_data$teams[[1]]
  
  result <- dplyr::inner_join(members_crew_data, teams_crew_data, by = c("id" = "primaryOwner"))
  
  result_unnested <- result %>%
    tidyr::unnest(cols = c(record)) %>%
    tidyr::unnest(cols = c(overall))
  
  final_crew_data <- result_unnested %>%
    dplyr::select(displayName, firstName, points, pointsFor, pointsAgainst, id, owners)
  
  return(final_crew_data) 
}
