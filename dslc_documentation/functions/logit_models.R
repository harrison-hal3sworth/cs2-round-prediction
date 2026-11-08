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
  )
  
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

test_logit_model(
  team_1_win ~ 
    map_name:team_1_role +
    team_1_log_diff_equip_gap +
    I(log(team_1_points / team_2_points)), 
  train_data, 
  test_data)