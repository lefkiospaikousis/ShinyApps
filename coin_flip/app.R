#
# This app simjlates the result of throwing dice a number of times
# You can select the number times you thrpiw the dice and the number of dice you use

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Flip a coin simulator"),
  
  p("This app simulates the result of flipping a coin a number of times."),
  p("You may select the number of", code("flips"),  "and/or the", code('fairness'), "of the coin and view the distribution of the results in the graph"),
  p("Once you have selected these, the simulation runs automatically or just hit the", code("Rerun"), "button to rerun the simulation using the same selections",
    "to see how the distribution of results changes"),
  p("The code for the app can be found in my", a(href="https://github.com/lefkiospaikousis/ShinyApps", "Github page")),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("flips",
                  "Number of flips:",
                  min = 1,
                  max = 1000,
                  value = 10
      ),
      sliderInput("prob",
                  "Is it a fair coin?\nSelect a probability of e.g.heads",
                  min = 0.05,
                  max = 0.95,
                  value = 0.5
      ),
      # numericInput("dice", 
      #              "Number of dice", value = 2, min=1, max=5, step = 1
      # ),
      actionButton("run", "Rerun"),
      width = 3
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("results_table")
      , width = 6
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  results <- reactive({
    
    input$run
    
    result <- sample(c("head", "tail")
                     , input$flips
                     , replace = TRUE
                     , prob = c(input$prob, 1-input$prob)
                     ) 
    
    table_r <- 
      result %>% 
      tibble::enframe("flip", "result") %>% 
      count(result) %>% 
      mutate(pct = n/ sum(n),
             pct = scales::percent(pct)) 
    
    
  })
  
  output$results_table <- renderTable({
    
    results()
  })
  
  
  output$distPlot <- renderPlot({

    validate(
      need(input$flips, "Please select the number of dice")
    )
  
  results() %>% 
    mutate(result = factor(result, levels = c("head", "tail"))) %>%
    ggplot(aes(result, n))+
    coord_flip()+
    geom_col(width = 0.5)+
    geom_text(aes(label = n), colour = "white", hjust = +1.5)+
    scale_x_discrete(drop = FALSE)+
    labs(y = "Frequency of result"
         , x = ""
    )+
    theme_light(base_size = 16)
  })
}

shinyApp(ui = ui, server = server)
