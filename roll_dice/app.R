#
# This app simjlates the result of throwing dice a number of times
# You can select the number times you thrpiw the dice and the number of dice you use

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(

    titlePanel("Roll your dice"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("rolls",
                        "Number of rolls:",
                        min = 1,
                        max = 1000,
                        value = 30
                        ),
            numericInput("dice", 
                         "Number of dice", value = 2, min=1, max=5, step = 1
                         ),
            actionButton("run", "Rerun"),
            width = 3
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
           #,tableOutput("results_table")
           , width = 6
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    results <- reactive({
        
        input$run
        
        result <- 
            replicate(
                input$rolls, sample(1:6, input$dice, replace = TRUE)
                )
        
        sums <- colSums(result) 
        
        table(sums) %>% as_tibble()
        
    })
        
    
    output$results_table <- renderTable({
        
        results()
    })
    
    
    output$distPlot <- renderPlot({
        
        validate(
            need(input$dice, "Please select the number of dice")
        )
        
        results() %>% 
            mutate(sums = factor(sums, levels = input$dice:(6*input$dice))) %>% 
            ggplot(aes(sums, n))+
            geom_col(width = 0.8)+
            geom_text(aes(label = n), colour = "white", vjust = +1)+
            scale_x_discrete(drop = FALSE)+
            labs(y = "Frequency of result"
                 , x = "Result"
            )+
            theme_light(base_size = 16)+
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            )
        
    })
}

shinyApp(ui = ui, server = server)
