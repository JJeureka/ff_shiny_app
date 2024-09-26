display_data_function <- function(league_input, year_input) {
  
  url_output <- paste0("https://lm-api-reads.fantasy.espn.com/apis/v3/games/FFL/leagueHistory/",league_input,"?view=mMatchupScore&view=mStatus&view=mSettings&view=mTeam&view=modular&view=mNav&seasonId=",year_input)
  
  
  all_crew_data <- fromJSON(url_output)
  
  
  #pulls just the members list from the entire data set
  members_crew_data <- all_crew_data$members
  
  #this pulls the data from out of the list - this is good to go
  members_crew_data_1 <- members_crew_data[[1]]
  
  teams_crew_data <- all_crew_data$teams
  
  #this pulls the data frame out of the list - this still has nested data so we will need to unnest after joining
  teams_crew_data_1 <- teams_crew_data[[1]]
  
  result <- inner_join(members_crew_data_1, teams_crew_data_1, by = c("id" = "primaryOwner"))
  
  result_unnested_record <- result %>%
    unnest(cols = c(record))
  
  result_unnested_overall <- result_unnested_record %>%
    unnest(cols = c(overall))
  
  final_crew_data <- result_unnested_overall %>%
    select(displayName, firstName, points, pointsFor, pointsAgainst, id, owners)
  
  summary_table <- final_crew_data %>%
    group_by(firstName, displayName) %>%
    summarise(
      avg_points_against = round(mean(pointsAgainst), 2),
      avg_points_for = round(mean(pointsFor), 2)
    ) %>%
    arrange(desc(avg_points_for))
  
  # Rename columns for readability
  summary_table <- summary_table %>%
    rename(
      `Team Name` = firstName,
      `Points Against` = avg_points_against,
      `Points For` = avg_points_for
    )
  
  # Display the table with kable and kableExtra with custom width
  summary_table <- kable(summary_table, 
                         caption = "Points For and Against", 
                         align = c("l", "c", "c")) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = TRUE, # Set to TRUE to make the table wider
      position = "center"
    ) %>%
    column_spec(1, width = "15em") %>%   
    column_spec(2, width = "15em") %>%  
    column_spec(3, width = "15em") %>%  # 
    row_spec(0, bold = TRUE, color = "white", background = "#00008B", extra_css = "padding: 20px;")
  
  return(as.character(summary_table))
}