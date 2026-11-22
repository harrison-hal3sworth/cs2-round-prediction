library(tidyverse)
library(scales)
library(yardstick)

train_logit_model <- function(formula, train_data) {
  model <- glm(
    formula,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  train_accuracy <- model_prediction_accuracy(model, train_data)
  
  return(list(model = model, train_accuracy = train_accuracy))
}

model_prediction_accuracy <- function(model, data) {
  data$model_prob_team_1_win <- predict(
    model,
    newdata = data,
    type = "response"
  )$predictions
  
  data$model_predicted_team_1_win <- ifelse(
    data$model_prob_team_1_win >= 0.5,
    TRUE,
    FALSE
  )
  
  data <- data |> 
    mutate(model_correct = ifelse(team_1_win == model_predicted_team_1_win, TRUE, FALSE))
  
  sum(data$model_correct) / nrow(data)
}

test_logit_model <- function(formula, train_data, test_data) {
  model <- train_logit_model(formula, train_data)$model
  
  test_accuracy <- model_prediction_accuracy(model, test_data)
  
  return(list(model = model, test_accuracy = test_accuracy))
}


required_equip_gap <- function(
    model,
    map_name,
    team_1_side,
    team_1_points_gap,
    equip_range = seq(-100000, 100000, by = 10)
) {
  
  # Create prediction data
  new_data <- data.frame(
    map_and_team_1_side = paste0(map_name, "_", team_1_side),
    team_1_equip_gap = equip_range,
    team_1_points_gap = team_1_points_gap
  )
  
  # Predict win probabilities
  new_data$win_probability <- predict(
    model,
    newdata = new_data,
    type = "response"
  )
  
  # Plot
  ggplot(new_data, aes(
    x = team_1_equip_gap,
    y = win_probability
  )) +
    geom_line(linewidth = 1) +
    geom_hline(
      yintercept = 0.60,
      linetype = "dashed",
      color = "red"
    ) +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1)
    ) +
    labs(
      title = "Round Win Probability by Equipment Advantage",
      subtitle = paste(
        map_name,
        team_1_side,
        "| Skill difference:", team_1_points_gap
      ),
      x = "Team 1 Equipment Advantage (log scale)",
      y = "Predicted Probability of Winning"
    ) +
    theme_minimal()
}



