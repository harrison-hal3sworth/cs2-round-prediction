
library(shiny)
library(ggplot2)

# --------------------------------------------------
# Load saved objects
# --------------------------------------------------

lr <- readRDS("models/logistic_model.rds")
bootstrap_lookup <- readRDS("models/bootstrap_lookup.rds")
model_metadata <- readRDS("data/model_metadata.rds")

# Use the factor levels stored in the fitted model
map_side_levels <- lr$xlevels$map_and_team_1_side

map_names <- unique(
  sub("_(counter-terrorist|terrorist)$", "", map_side_levels)
)

# Equipment-gap range observed in the model's training data
equip_range <- range(lr$model$team_1_equip_gap, na.rm = TRUE)

# Bootstrap lookup must contain these columns
required_columns <- c("target", "lower", "median", "upper")

if (!all(required_columns %in% names(bootstrap_lookup))) {
  stop("bootstrap_lookup is missing required columns.")
}

bootstrap_lookup <- bootstrap_lookup[
  order(bootstrap_lookup$target),
]

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(
  
  titlePanel("CS2 Round Win Probability Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Matchup"),
      
      numericInput(
        "team_points",
        "Your team's HLTV points",
        value = 1000,
        min = 1
      ),
      
      numericInput(
        "opponent_points",
        "Opponent's HLTV points",
        value = 1000,
        min = 1
      ),
      
      selectInput(
        "map",
        "Map",
        choices = map_names
      ),
      
      radioButtons(
        "side",
        "Starting side",
        choices = c("CT", "T"),
        selected = "CT",
        inline = TRUE
      ),
      
      hr(),
      
      h4("Target probability"),
      
      sliderInput(
        "target",
        "Desired round-win probability",
        min = 5,
        max = 95,
        value = 60,
        step = 1,
        post = "%"
      ),
      
      helpText(
        "The graph shows your team's estimated probability of winning an individual round."
      )
    ),
    
    mainPanel(
      
      plotOutput(
        "probability_plot",
        height = "550px"
      ),
      
      hr(),
      
      h4("Equipment advantage required"),
      
      textOutput("target_description"),
      
      hr(),
      
      h4("Model details"),
      
      textOutput("model_description")
    )
  )
)

# --------------------------------------------------
# Server
# --------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------
  # Generate predictions across equipment ratios
  # ----------------------------------------------
  
  prediction_data <- reactive({
    
    req(
      input$team_points > 0,
      input$opponent_points > 0
    )
    
    # Your team is treated as team 1
    team_1_points_gap <- log(
      input$team_points / input$opponent_points
    )
    
    # Construct the map/side factor
    side_label <- if (input$side == "CT") {
      "counter-terrorist"
    } else {
      "terrorist"
    }
    
    map_side <- paste(
      input$map,
      side_label,
      sep = "_"
    )
    
    validate(
      need(
        map_side %in% map_side_levels,
        "This map/side combination is not available in the model."
      )
    )
    
    # Equipment ratio on the x-axis is:
    # your team's equipment / opponent's equipment
    #
    # The model expects CT equipment / T equipment.
    #
    # Therefore:
    # If your team is CT: model gap = log(x)
    # If your team is T:  model gap = -log(x)
    
    ratio_limits <- exp(equip_range)

    
    equipment_ratios <- exp(
      seq(
        log(ratio_limits[1]),
        log(ratio_limits[2]),
        length.out = 201
      )
    )
    
    model_equip_gap <- log(equipment_ratios)
    
    new_data <- data.frame(
      
      map_and_team_1_side = factor(
        rep(map_side, length(equipment_ratios)),
        levels = map_side_levels
      ),
      
      team_1_equip_gap = model_equip_gap,
      
      team_1_points_gap = team_1_points_gap
    )
    
    # Model predictions
    probabilities <- predict(
      lr,
      newdata = new_data,
      type = "response"
    )
    
    # Interpolate the bootstrap lookup table
    lower <- approx(
      x = bootstrap_lookup$target,
      y = bootstrap_lookup$lower,
      xout = probabilities,
      rule = 2
    )$y
    
    median <- approx(
      x = bootstrap_lookup$target,
      y = bootstrap_lookup$median,
      xout = probabilities,
      rule = 2
    )$y
    
    upper <- approx(
      x = bootstrap_lookup$target,
      y = bootstrap_lookup$upper,
      xout = probabilities,
      rule = 2
    )$y
    
    curve <- data.frame(
      equipment_ratio = equipment_ratios,
      probability = probabilities,
      lower = lower,
      median = median,
      upper = upper
    )
    
    list(
      curve = curve,
      ratio_limits = ratio_limits
    )
  })
  
  # ----------------------------------------------
  # Find equipment ratio needed for target
  # ----------------------------------------------
  
  target_ratio <- reactive({
    
    curve <- prediction_data()$curve
    
    target <- input$target / 100
    
    probability_range <- range(curve$probability)
    
    # Check whether the target is achievable
    if (
      target < probability_range[1] ||
      target > probability_range[2]
    ) {
      return(NA_real_)
    }
    
    # Find the closest predicted probability
    index <- which.min(
      abs(curve$probability - target)
    )
    
    curve$equipment_ratio[index]
  })
  
  # ----------------------------------------------
  # Plot
  # ----------------------------------------------
  
  output$probability_plot <- renderPlot({
    
    results <- prediction_data()
    curve <- results$curve
    
    target <- input$target / 100
    required_ratio <- target_ratio()
    
    p <- ggplot(
      curve,
      aes(x = equipment_ratio, y = probability)
    ) +
      
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = "steelblue",
        alpha = 0.20
      ) +
      
      geom_line(
        color = "steelblue4",
        linewidth = 1.2
      ) +
      
      geom_hline(
        yintercept = target,
        color = "red",
        linetype = "dashed",
        linewidth = 0.8
      ) +
      
      scale_x_log10(
        breaks = scales::breaks_log(n = 7)
      ) +
      
      scale_y_continuous(
        limits = c(0, 1),
        labels = function(x) paste0(round(100 * x), "%")
      ) +
      
      labs(
        title = paste(
          input$map,
          "- Starting",
          input$side
        ),
        
        subtitle = paste(
          "Your HLTV points:",
          input$team_points,
          "| Opponent HLTV points:",
          input$opponent_points
        ),
        
        x = "Your equipment value / Opponent equipment value",
        
        y = "Probability of winning the round",
        
        caption = paste(
          "Blue ribbon: bootstrap interval for comparable historical predictions.",
          "Red dashed line: target win probability."
        )
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "gray40")
      )
    
    if (!is.na(required_ratio)) {
      
      p <- p +
        geom_vline(
          xintercept = required_ratio,
          color = "darkgreen",
          linetype = "dashed",
          linewidth = 0.8
        )
    }
    
    p
  })
  
  # ----------------------------------------------
  # Target description
  # ----------------------------------------------
  
  output$target_description <- renderText({
    
    required_ratio <- target_ratio()
    
    if (is.na(required_ratio)) {
      
      return(
        paste(
          "A",
          input$target,
          "% win probability is not reached within the equipment-ratio",
          "range observed in the model's training data."
        )
      )
    }
    
    paste0(
      "To reach approximately ",
      input$target,
      "% probability of winning a round as ",
      input$side,
      " on ",
      input$map,
      ", your team needs an equipment ratio of approximately ",
      round(required_ratio, 1),
      ":1 relative to the opponent."
    )
  })
  
  # ----------------------------------------------
  # Model description
  # ----------------------------------------------
  
  output$model_description <- renderText({
    
    paste(
      "HLTV points are transformed using log(your points / opponent points).",
      "Equipment is transformed using log(CT equipment / T equipment).",
      "The model estimates the probability that your team wins an individual round.",
      "Bootstrap intervals are based on the precomputed validation lookup table."
    )
  })
}

# --------------------------------------------------
# Run app
# --------------------------------------------------

shinyApp(ui = ui, server = server)
