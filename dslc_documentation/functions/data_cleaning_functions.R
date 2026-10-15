library(tidyverse)

first_round_kill <- function(rounds, kills) {
  res <- kills |> select(
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
