library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(sets)
library(readr)

# Load dataset
lottery <- read_csv("649.csv")

# Probability functions
combinations <- function(n, k) {
  return(choose(n, k))
}

one_ticket_probability <- function(nums) {
  total <- combinations(49, 6)
  prob <- (1 / total) * 100
  return(sprintf("🎰 Your chance of winning: %1.9f%%", prob))
}

# Process historical lottery numbers
historical_lots <- pmap(
  list(
    lottery$`NUMBER DRAWN 1`,
    lottery$`NUMBER DRAWN 2`,
    lottery$`NUMBER DRAWN 3`,
    lottery$`NUMBER DRAWN 4`,
    lottery$`NUMBER DRAWN 5`,
    lottery$`NUMBER DRAWN 6`
  ),
  function(a, b, c, d, e, f) { c(a, b, c, d, e, f) }
)

check_historical_occurrences <- function(lot) {
  historical_matches <- map(historical_lots, ~setequal(.x, lot))
  num_past_matches <- sum(unlist(historical_matches))
  return(paste("🔍 Your numbers have appeared", num_past_matches, "times in history."))
}

# UI
ui <- fluidPage(
  theme = shinytheme("darkly"), # Stylish dark theme
  titlePanel("🎲 Lottery Probability Calculator"),
  
  tags$head(
    # Include GSAP.js for animations
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/gsap/3.12.2/gsap.min.js"),
    
    # Custom CSS for animated balls
    tags$style(HTML("
      .lottery-balls {
        display: flex;
        justify-content: center;
        gap: 10px;
        margin-top: 20px;
      }
      .ball {
        width: 50px;
        height: 50px;
        background: radial-gradient(circle, #ffcc00 0%, #ff9900 100%);
        color: black;
        font-weight: bold;
        font-size: 20px;
        text-align: center;
        line-height: 50px;
        border-radius: 50%;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.3);
        display: inline-block;
        opacity: 0;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Pick Your Lucky Numbers 🎟️"),
      
      div(class="lottery-balls",
          span(id="ball1", class="ball", "?"),
          span(id="ball2", class="ball", "?"),
          span(id="ball3", class="ball", "?"),
          span(id="ball4", class="ball", "?"),
          span(id="ball5", class="ball", "?"),
          span(id="ball6", class="ball", "?")
      ),
      
      br(),
      actionButton("spin", "🎰 Spin the Wheel!", class = "btn btn-warning btn-lg"),
      br(), br(),
      
      pickerInput("num1", "Number 1:", choices = 1:49, selected = 1),
      pickerInput("num2", "Number 2:", choices = 1:49, selected = 2),
      pickerInput("num3", "Number 3:", choices = 1:49, selected = 3),
      pickerInput("num4", "Number 4:", choices = 1:49, selected = 4),
      pickerInput("num5", "Number 5:", choices = 1:49, selected = 5),
      pickerInput("num6", "Number 6:", choices = 1:49, selected = 6),
      
      actionButton("check", "🔮 Check Probability", class = "btn btn-primary btn-lg"),
      br(), br()
    ),
    
    mainPanel(
      h3("Results 🎯"),
      div(class="card text-white bg-success mb-3", 
          div(class="card-header", "Winning Probability"),
          div(class="card-body",
              verbatimTextOutput("probability_result")
          )
      ),
      
      div(class="card text-white bg-info mb-3", 
          div(class="card-header", "Historical Occurrences"),
          div(class="card-body",
              verbatimTextOutput("history_result")
          )
      )
    )
  ),
  
  # JavaScript for animations
  tags$script(HTML("
    function animateLotteryBalls(numbers) {
      let balls = ['ball1', 'ball2', 'ball3', 'ball4', 'ball5', 'ball6'];
      
      gsap.fromTo('.ball', {opacity: 0, scale: 0}, 
                  {opacity: 1, scale: 1, duration: 1, stagger: 0.2, ease: 'elastic.out(1, 0.5)'});

      setTimeout(() => {
        for (let i = 0; i < 6; i++) {
          document.getElementById(balls[i]).innerText = numbers[i];
        }
      }, 1200);
    }
  "))
)

# Server
server <- function(input, output, session) {
  observeEvent(input$spin, {
    # Generate 6 unique random numbers
    rand_nums <- sample(1:49, 6)
    
    # Animate lottery balls with generated numbers
    session$sendCustomMessage(type = "animateBalls", message = rand_nums)
    
    # Update the selected numbers in input fields
    updatePickerInput(session, "num1", selected = rand_nums[1])
    updatePickerInput(session, "num2", selected = rand_nums[2])
    updatePickerInput(session, "num3", selected = rand_nums[3])
    updatePickerInput(session, "num4", selected = rand_nums[4])
    updatePickerInput(session, "num5", selected = rand_nums[5])
    updatePickerInput(session, "num6", selected = rand_nums[6])
  })
  
  observeEvent(input$check, {
    user_numbers <- c(input$num1, input$num2, input$num3, input$num4, input$num5, input$num6)
    
    if (length(unique(user_numbers)) != 6) {
      output$probability_result <- renderText("❌ Invalid input! Please select 6 unique numbers.")
      output$history_result <- renderText("")
    } else {
      output$probability_result <- renderText(one_ticket_probability(user_numbers))
      output$history_result <- renderText(check_historical_occurrences(user_numbers))
    }
  })
}

# Run the app
shinyApp(ui, server)
