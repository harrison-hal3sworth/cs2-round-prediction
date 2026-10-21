library(tidyverse)


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

remove_drawed_rounds <- function(data) {
  output <- data |>
    filter(reason != "draw") |>
    group_by(file_id, map_name, file_name) |>
    arrange(round_num, .by_group = TRUE) |>
    mutate(round_num = row_number()) |>
    ungroup()
  
  return(output)
}

remove_overtime <- function(data) {
  output <- data |>
    filter(round_num > 24)

  return(output)
}

clean_data <- function(rounds, kills) {
  
}