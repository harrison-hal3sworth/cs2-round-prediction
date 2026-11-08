library(tidyverse)
library(stringr)
library(stringi)
library(data.table)

first_round_kill <- function(rounds, kills) {
  res <- kills |>
          filter(!is.na(attacker_side), attacker_side != "") |>
          select(
              file_id,
              map_name,
              round_num,
              attacker_side,
              tick) |>
          group_by(file_id, map_name, round_num) |>
          slice_min(tick, n = 1, with_ties = FALSE) |>
          ungroup() |> 
          select(-tick) |> 
          rename(first_kill_by = attacker_side) |>
          inner_join(rounds, by = c("file_id", "map_name", "round_num"))
  
  return(res)
}

is_pistol_round <- function(data) {
  data |> 
    mutate(
      pistol_round = if_else(
        round_num %in% c(1, 13),
        TRUE,
        FALSE
        )
      )
}

remove_overtime <- function(data) {
  output <- data |>
    filter(round_num <= 24)

  return(output)
}

get_team_names <- function(file_name) {
  
  maps <- c(
    "dust2", "mirage", "inferno", "nuke",
    "ancient", "anubis", "overpass", "train",
    "vertigo", "cache"
  )
  
  file_name <- basename(file_name)
  
  # Extract team names
  team_1_name <- sub("-vs-.*$", "", file_name)
  team_2_name <- sub("^.*-vs-", "", file_name)
  
  # Remove extension
  team_2_name <- sub("\\.dem$", "", team_2_name)
  
  # Remove map or match suffix
  if (grepl("-m[0-9]+", team_2_name)) {
    
    team_2_name <- sub("-m[0-9]+.*$", "", team_2_name)
    
  } else {
    
    map_pattern <- paste0(
      "-(",
      paste(maps, collapse = "|"),
      ")$"
    )
    
    team_2_name <- sub(map_pattern, "", team_2_name)
  }
  
  # Standardize team names
  team_1_name <- tolower(team_1_name)
  team_2_name <- tolower(team_2_name)
  
  # Correct known spelling inconsistencies
  team_1_name <- sub("^familcao$", "famalicao", team_1_name)
  team_2_name <- sub("^familcao$", "famalicao", team_2_name)
  
  return(list(
    team_1_name = team_1_name,
    team_2_name = team_2_name
  ))
}

plot_team_game_distribution <- function(sample_kills) {
  
  team_games <- sample_kills %>%
    distinct(file_name) %>%
    mutate(
      team_1 = map_chr(file_name, ~ get_team_names(.x)$team_1_name),
      team_2 = map_chr(file_name, ~ get_team_names(.x)$team_2_name)
    ) %>%
    select(team_1, team_2) %>%
    pivot_longer(
      cols = everything(),
      values_to = "team"
    ) %>%
    count(team, name = "games") %>%
    arrange(desc(games))
  
  # Plot distribution
  p <- ggplot(team_games, aes(
    x = reorder(team, games),
    y = games
  )) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Distribution of Games by CS2 Team",
      x = "Team",
      y = "Number of Games"
    ) +
    theme_minimal()
  
  return(list(
    data = team_games,
    plot = p
  ))
}
 
create_player_team_matrix <- function(kills) {
  
  # Extract teams for each unique match
  matches <- kills %>%
    distinct(file_name) %>%
    mutate(
      team_1 = purrr::map_chr(file_name, ~ get_team_names(.x)$team_1_name),
      team_2 = purrr::map_chr(file_name, ~ get_team_names(.x)$team_2_name)
    )
  
  # Collect unique player appearances
  players <- kills %>%
    mutate(across(ends_with("steamid"), as.character)) %>%
    select(file_name, attacker_steamid, victim_steamid, assister_steamid) %>%
    pivot_longer(
      cols = -file_name,
      values_to = "steamid"
    ) %>%
    mutate(steamid = trimws(steamid)) %>%
    filter(
      !is.na(steamid),
      steamid != "",
      steamid != "0"
    ) %>%
    distinct(file_name, steamid)
  
  # Associate each player with both teams in each match
  player_matches <- players %>%
    inner_join(matches, by = "file_name") %>%
    pivot_longer(
      cols = c(team_1, team_2),
      values_to = "team"
    ) %>%
    filter(!is.na(team), team != "") %>%
    distinct(file_name, steamid, team)
  
  # Calculate team proportions
  team_counts <- player_matches %>%
    count(steamid, team, name = "matches")
  
  player_totals <- players %>%
    count(steamid, name = "total_matches")
  
  team_proportions <- team_counts %>%
    left_join(player_totals, by = "steamid") %>%
    mutate(proportion = matches / total_matches)
  
  # Classify players and record why
  player_classification <- team_proportions %>%
    group_by(steamid) %>%
    summarise(
      total_matches = first(total_matches),
      best_proportion = max(proportion),
      n_top_teams = sum(proportion == max(proportion)),
      
      team_name = {
        exact_ones <- team[proportion == 1]
        best_teams <- team[proportion == max(proportion)]
        
        if (length(exact_ones) == 1) {
          exact_ones
        } else if (length(exact_ones) > 1) {
          "not enough games"
        } else if (length(best_teams) == 1) {
          best_teams
        } else {
          "not enough games"
        }
      },
      
      classification_reason = {
        exact_ones <- team[proportion == 1]
        best_teams <- team[proportion == max(proportion)]
        
        if (length(exact_ones) == 1) {
          "One team appears in every match"
        } else if (length(exact_ones) > 1) {
          "Multiple teams appear in every match"
        } else if (length(best_teams) == 1) {
          "Unique highest proportion"
        } else {
          "Tie for highest proportion"
        }
      },
      
      .groups = "drop"
    )
  
  # Create wide team-proportion matrix
  team_matrix <- team_proportions %>%
    select(steamid, team, proportion) %>%
    pivot_wider(
      names_from = team,
      values_from = proportion,
      values_fill = 0
    )
  
  # Add classification information
  team_matrix %>%
    left_join(player_classification, by = "steamid")
}

identify_team_1_role <- function(sample_kills) {
  
  # Infer player teams
  player_teams <- create_player_team_matrix(sample_kills) %>%
    select(steamid, team_name) %>%
    filter(
      !is.na(team_name),
      team_name != "not enough games"
    )
  
  # Extract player-side observations
  player_sides <- sample_kills %>%
    mutate(across(ends_with("steamid"), as.character)) %>%
    select(
      file_name,
      round_num,
      attacker_steamid, attacker_side,
      victim_steamid, victim_side,
      assister_steamid, assister_side
    ) %>%
    pivot_longer(
      cols = -c(file_name, round_num),
      names_to = c("player_type", ".value"),
      names_pattern = "(attacker|victim|assister)_(steamid|side)"
    ) %>%
    filter(
      !is.na(steamid),
      steamid != "",
      steamid != "0",
      !is.na(side)
    ) %>%
    left_join(player_teams, by = "steamid") %>%
    filter(!is.na(team_name))
  
  # Identify each team's starting side using rounds 1-12
  starting_sides <- player_sides %>%
    filter(round_num <= 12) %>%
    count(file_name, team_name, side, name = "observations") %>%
    group_by(file_name, team_name) %>%
    slice_max(observations, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(file_name, team_name, starting_side = side)
  
  # Extract team names from filenames
  matches <- sample_kills %>%
    distinct(file_name) %>%
    mutate(
      team_1 = purrr::map_chr(
        file_name,
        ~ get_team_names(.x)$team_1_name
      ),
      team_2 = purrr::map_chr(
        file_name,
        ~ get_team_names(.x)$team_2_name
      )
    )
  
  # Determine Team 1's starting side
  team_1_starting_side <- matches %>%
    left_join(
      starting_sides,
      by = c("file_name", "team_1" = "team_name")
    ) %>%
    select(file_name, team_1_starting_side = starting_side)
  
  # Assign Team 1's role, switching at round 13
  sample_kills %>%
    left_join(matches, by = "file_name") %>%
    left_join(team_1_starting_side, by = "file_name") %>%
    mutate(
      team_1_role = case_when(
        is.na(team_1_starting_side) ~ NA_character_,
        
        round_num <= 12 ~ tolower(team_1_starting_side),
        
        round_num >= 13 &
          tolower(team_1_starting_side) == "terrorist" ~ "counter-terrorist",
        
        round_num >= 13 &
          tolower(team_1_starting_side) == "counter-terrorist" ~ "terrorist"
      )
    ) %>%
    select(-team_1_starting_side)
}

clean_data <- function(rounds, kills, rankings) {
  
  # Remove incomplete games
  incomplete_files <- rounds |>
    filter(stringr::str_starts(file_name, "incomplete.dem")) |>
    pull(file_name) |>
    unique()
  
  rounds <- rounds |>
    filter(!str_starts(file_name, "incomplete.dem"))
  
  kills <- kills |>
    filter(!str_starts(file_name, "incomplete.dem"))
  
  # Fix map names
  maps <- c(
    "dust2", "mirage", "inferno", "nuke",
    "ancient", "anubis", "overpass", "train",
    "vertigo", "cache"
  )
  
  rounds <- rounds |>
    mutate(
      map_name = case_when(
        map_name %in% maps ~ map_name,
        str_detect(map_name, str_c(maps, collapse = "|")) ~
          str_extract(map_name, str_c(maps, collapse = "|")),
        TRUE ~ map_name
      )
    )
  
  # Other manual fixes for map names
  rounds <- rounds |>
    mutate(
      map_name = recode(
        map_name,
        "dust" = "dust2",
        "dus2" = "dust2",
        "dust-p1" = "dust2"
      )
    )
  
  # Identify team 1's role (CT/T)
  kills <- identify_team_1_role(kills) |>
    select(file_name, round_num, team_1, team_2, team_1_role) |>
    unique()
  rounds <- rounds |>
    left_join(kills, by = c("file_name", "round_num")) |>
    filter(!is.na(team_1) & !is.na(team_2))
  
  # Extract team elo
  rounds <- get_historical_team_elo(rounds, rankings)
  
  # If a team has no ranking or HLTV score of 0, don't include
  rounds <- rounds |> 
    filter(!is.na(team_1_points) & !is.na(team_2_points)) |>
    filter(team_1_points != 0 & team_2_points != 0)
  
  # Don't include overtime games
  rounds <- remove_overtime(rounds)
  
  # ct_purchase_cost and t_purchase_cost being NA means no one on the team bought new equip
  rounds <- rounds |>
    mutate(
      ct_purchase_cost = if_else(is.na(ct_purchase_cost), 0, ct_purchase_cost),
      t_purchase_cost = if_else(is.na(t_purchase_cost), 0, t_purchase_cost),
      purchase_cost_gap_ct = ct_purchase_cost - t_purchase_cost
    )
  
  
  # If a match has a round that's missing t_equip, ct_equip then remove
  # If a match has a round that's missing team 1 role, remove
  bad_matches <- rounds |> group_by(file_name) |>
    summarise(
      na_equip = sum(is.na(equip_gap_ct)),
      na_team_role = sum(is.na(team_1_role))
      ) |>
    filter(na_equip > 0 | na_team_role >0) |>
    select(file_name)
  
  rounds <- rounds |> anti_join(bad_matches, by = "file_name")
  
  
  # Find pistol rounds
  rounds <- is_pistol_round(rounds)
  
  # Calculate log diff equip
  rounds <- rounds |>
    mutate(log_diff_equip_gap_ct = log((ct_equip + 1) / (t_equip + 1)))
  
  # Denote whether team_1 wins or loses
  # Denote team_1 equip gap (standard and log)
  
  rounds <- rounds |> 
    mutate(
      team_1_win = if_else(winner == team_1_role, TRUE, FALSE),
      team_1_equip_gap = if_else(team_1_role == "counter-terrorist", equip_gap_ct, -equip_gap_ct),
      team_1_log_diff_equip_gap = if_else(team_1_role == "counter-terrorist", equip_gap_ct, -equip_gap_ct)
    )

  
  return(rounds)
}


get_historical_team_elo <- function(rounds, historical_elo) {
  
  # Prepare historical ELO data
  elo <- historical_elo %>%
    transmute(
      date = as.Date(date),
      team = str_to_lower(
        stri_trans_general(str_trim(team), "Latin-ASCII")
      ) %>%
        str_replace_all("\\s+", "-"),
      points
    ) %>%
    distinct(date, team, .keep_all = TRUE)
  
  # Prepare round data
  result <- rounds %>%
    mutate(
      date = as.Date(date),
      team_1 = str_to_lower(
        stri_trans_general(str_trim(team_1), "Latin-ASCII")
      ) %>%
        str_replace_all("\\s+", "-"),
      team_2 = str_to_lower(
        stri_trans_general(str_trim(team_2), "Latin-ASCII")
      ) %>%
        str_replace_all("\\s+", "-"),
      row_id = row_number()
    )
  
  # Create lookup tables
  team_1_lookup <- result %>%
    select(row_id, date, team = team_1) %>%
    as.data.table()
  
  team_2_lookup <- result %>%
    select(row_id, date, team = team_2) %>%
    as.data.table()
  
  # Prepare historical data for rolling joins
  elo_dt <- as.data.table(elo)
  setkey(elo_dt, team, date)
  
  # Retrieve latest available ratings
  team_1_points <- elo_dt[
    team_1_lookup,
    on = .(team, date),
    roll = Inf,
    x.points
  ]
  
  team_2_points <- elo_dt[
    team_2_lookup,
    on = .(team, date),
    roll = Inf,
    x.points
  ]
  
  # Return results
  result %>%
    mutate(
      team_1_points = team_1_points,
      team_2_points = team_2_points
    ) %>%
    select(-row_id)
}

plot_equipment_by_first_winner <- function(sample_rounds, first_winner) {
  
  plot_data <- sample_rounds |>
    group_by(file_id, map_name) |>
    filter(first(winner[round_num == 1]) == first_winner) |>
    ungroup() |>
    filter(round_num >= 1, round_num <= 12) |>
    pivot_longer(
      cols = c(ct_equip, t_equip),
      names_to = "side",
      values_to = "cash"
    )
  
  mean_data <- plot_data |>
    group_by(round_num, side) |>
    summarise(
      n = sum(!is.na(cash)),
      mean_cash = mean(cash, na.rm = TRUE),
      sd_cash = sd(cash, na.rm = TRUE),
      se = sd_cash / sqrt(n),
      t_critical = qt(0.975, df = n - 1),
      lower = mean_cash - t_critical * se,
      upper = mean_cash + t_critical * se,
      .groups = "drop"
    )
  
  ggplot() +
    geom_line(
      data = plot_data,
      aes(
        x = round_num,
        y = cash,
        color = side,
        group = interaction(file_id, map_name, side)
      ),
      alpha = 0.10,
      linewidth = 0.4
    ) +
    geom_errorbar(
      data = mean_data,
      aes(
        x = round_num,
        ymin = lower,
        ymax = upper,
        color = side
      ),
      width = 0.15,
      linewidth = 0.8
    ) +
    geom_line(
      data = mean_data,
      aes(x = round_num, y = mean_cash, color = side),
      linewidth = 1.4
    ) +
    geom_point(
      data = mean_data,
      aes(x = round_num, y = mean_cash, color = side),
      size = 2
    ) +
    scale_color_manual(
      values = c(ct_equip = "blue", t_equip = "red"),
      labels = c(ct_equip = "CT", t_equip = "T"),
      name = "Side"
    ) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
      title = paste(
        "Equipment Value When",
        ifelse(first_winner == "counter-terrorist", "CT", "T"),
        "Wins Round 1"
      ),
      subtitle = "Individual matches in faint lines; bold lines show means with 95% confidence intervals",
      x = "Round",
      y = "Mean Team Equipment Value ($)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )
}
