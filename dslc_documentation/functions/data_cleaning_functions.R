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

remove_overtime <- function(data) {
  output <- data |>
    filter(round_num <= 24)

  return(output)
}

clean_data <- function(rounds, kills) {
  
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
