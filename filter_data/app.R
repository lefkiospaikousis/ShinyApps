#
# This is a Shiny web application where you can filter your data 
# in two ways
# A) Render a filter UI
# B) Enble the user to write a filter expression in the dplyr way 
#
# For A) the code draws from this post
# https://towardsdatascience.com/utilizing-quosures-to-create-ultra-flexible-filtering-controls-in-r-shiny-f3e5dc461399
#
# For B) it draaws from these resources:
# '''so far no particulr resource..
#

library(shiny)
library(tidyverse)

filter1_by <- function(df, fcol1, fv1) {
    
    filter_var1 <- dplyr::quo(fcol1)
    df %>% filter_at(vars(!!filter_var1), all_vars(. == fv1))
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Testing dplyr filtering interactively"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file", NULL, accept = c(".csv", ".tsv")),
            uiOutput("var_names"),
            uiOutput("filter1choice"),
            fluidRow(
                #p("Write your expression"),
                column(5,
                       textInput("expression", "Write your expression"),
                       ),
                column(3,
                       actionButton("doFilter", "Filter!")
                       )
                
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            #tableOutput("new_table"),
            DT::dataTableOutput("data_tbl"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- reactive({
        
        req(input$file)
        
        ext <- tools::file_ext(input$file$name)
        
        switch(ext,
               csv = vroom::vroom(input$file$datapath, delim = ",") %>%
                   mutate_if(is.numeric, ~round(., 2)) %>%
                   drop_na()
               ,
               tsv = vroom::vroom(input$file$datapath, delim = "\t") ,
               
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
        
    })
    
    output$var_names <- renderUI({
        list(
            h3("Select variables to filter"),
            selectInput(
                "filter1",
                "Start typing or use dropDown", size = 5, selectize = FALSE,
                c("",names(data())), selected = "")
        )
        })
    
    choicevec1 <- reactive({
        
        req(input$filter1)
        unique(data()[[input$filter1]])
    })
    
    output$filter1choice <- renderUI(
        if(is.numeric(data()[[input$filter1]])){
            shiny::sliderInput("filter1val", "Select a range", 
                                min = min(data()[[input$filter1]]),
                                max = max(data()[[input$filter1]]),
                                value = c(min,max)
            )
            
        } else {
            selectizeInput("filter1val", "Select filter 1 condition:", choices = choicevec1(), multiple = TRUE)
            
        }
    )
    
    output$data_tbl <- DT::renderDataTable({
        
        filtered_data()
    })
    
    filtered_data <- eventReactive(input$doFilter, {
        
        req(data())
        req(input$expression)
        
        validate(
            need(
                tryCatch( 
                    
                    expr = {
                        
                        condition <- rlang::parse_expr(input$expression)
                        res <- data() %>% filter(!!condition)
                        return(res)
                    },
                    error =  function(e){
                        
                        return(NULL)
                    },
                    warning = function(w){
                        return(NULL)
                    }
                    
                )
                , message = "Wrong expression! Try again")
        )
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        data() %>% 
            ggplot(aes(sepal.length, sepal.width))+
            geom_point(aes(colour = variety))
    
        
    })
    
    # filtered_2 <- reactive({
    #     
    #     req(data())
    #     req(input$filter1)
    #     
    # need two filers? one for range and one for valuess???
    # based on the two filtering optons above???
    # Slider input and select a valiue?
    #     data() %>% 
    #         filter(between(
    #             input$filter1
    #         ))
    #     
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
