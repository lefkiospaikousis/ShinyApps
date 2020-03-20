#
# This is a Shiny web application. You can run the application by clicking

library(shiny)
library(ggplot2)
library(dplyr)


# Now lets use a module where we can reuse the handler
# The Modules need to be before the UI and Server functions??


# The module UI function
plotDownloadButton_UI <- function(id, buttonLabel = "Download figure") {
    
    # this will create the button
    # I will be able to use this function in the UI of the app
    # like this: `downloadButton_UI("blabla")` and it will create a button to download
    ns <- NS(id)
    tagList(
        downloadButton(ns("download_button"), label = buttonLabel)
    )
}

# The module server function
plotDownloadButton <- function(input, output, session, plot_name, the_plot) {
    
    # this will create the download handler
    # Remember that the handler's output$_name_ has to be the same as the `downloadButton`'s id
    # in this case its download_plot
    output$download_button <- downloadHandler(
        
        filename = function() {
            paste("plot-",plot_name,".png", sep="")
        },
        
        # See here that i wrapped the `the_plot` with `()`
        content = function(file) {
            ggsave(file, plot = the_plot())
        }
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How to save ggplots and use Shiny modules"),
    
    fluidRow(
        
        column(2,
               p("Description here"),
               br(),
               p("This is an app I created to teach myself about shiny modules"),
               p("What I wanted to do was first learn how to download a ggplot object to incorporate
                 a ", code("download"), " button in a Clustering app I am creating. 
                 This could be accomplished by using 2 pieces of information.a) A Download button that is 
                 associated with b) a Download hander fuction to handle the call to download a plot.
                 However, I had a bunch of plots I need to desing a download process for. 
                 Enter Shiny Modules:"),
               br(),
               p("With Shiny modules, the idea is to write up ONE module that creates a button and an associated 
               handler, and use this module to render a button to each plot I had"),
               p("As an added benefit of this `function` style module, I can declare the button label
                 and file name to be used when saving (see the button labels"),
               br(),
               p("The first plot (Histogram) is using a separate button and handler, while the other two
               use the modularised technique"),
               p("See the code in my", 
                 a(href = "https://github.com/lefkiospaikousis/ShinyApps/tree/master/download_figures", "github page"))
              ),
        
        column(10,
               p("A bunch of plots here"),
               
               mainPanel(
                   h4("This is the IRIS dataset"),
                   
                   fluidRow(
                       sliderInput("bins",
                                   "Number of bins:",
                                   min = 1,
                                   max = 50,
                                   value = 30)
                   ),
                   fluidRow(
                       column(4,
                              plotOutput("distPlot"),
                              p("This button has its own ui and output$handler"),
                              downloadButton("downloadPlot", "Download as png" ),
                       ),
                       column(4,
                              plotOutput("scatterPlot"),
                              p("This button is created by the module"),
                              plotDownloadButton_UI("scatter", buttonLabel = "download this figure")
                       ),
                       column(4,
                              plotOutput("boxPlot"),
                              p("This button is created by the module"),
                              plotDownloadButton_UI("boxplot")
                       )
                   )
                   
               )
               )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # need to have the plot in a reactive context
    # cannot save the ggplot (using ggsave)if in the `renderPlot` expression
    
    # a scatter plot
    scatterPlot <- reactive({
        iris %>% 
            ggplot(aes(Sepal.Length, Sepal.Width))+
            geom_point()
    })
    
    output$scatterPlot <- renderPlot({scatterPlot()})
    
    # a histogram
    distPlot <- reactive({
        iris %>% 
            ggplot(aes(Sepal.Length))+
            geom_histogram(bins = input$bins)
    })
    
    output$distPlot <- renderPlot({distPlot()})
    
    # a box plot
    boxPlot <- reactive({
        iris %>% 
            ggplot(aes(Species, Sepal.Length))+
            geom_boxplot()
    })
    
    output$boxPlot <- renderPlot({boxPlot()})
    
    
    # The downlod handler.  It has the same`id` name as the button in the UI
    output$downloadPlot <- downloadHandler(
        filename = function(){paste("my_plot",'.png',sep='')},
        
        content = function(file){
            ggsave(file,plot=distPlot())
        }
    )
    
    # Here I call the module and I pass 2 (extra) arguments - (other than module and id)
    # -`plot_name` to be used in the file when saving the plot
    # - `the_plot` The plot to save. NOTE! Use the name of reactive plot WITHOUT the ()
    # I pass the `()` in the module server function in the downloadHandler. see top of shiny app
    callModule(plotDownloadButton, id = "scatter", plot_name = "scatter", the_plot = scatterPlot)
    callModule(plotDownloadButton, id = "boxplot", plot_name = "boxplot", the_plot = boxPlot)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
