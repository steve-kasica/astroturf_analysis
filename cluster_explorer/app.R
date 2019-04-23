#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

n <- 1000
random_seed <- 42

if (basename(getwd()) == 'astroturf_analysis') {
    source ('./util.R')
    data_dir <- file.path('./data', paste(n, random_seed, sep='_'))
} else {
    source('../util.R')
    data_dir <- file.path('../data', paste(n, random_seed, sep='_'))
}

clusters <- getClusters(data_dir)
methods <- str_replace_all(clusters, '^clusters_([^\\.]+)\\.csv', '\\1')

# Define UI for application
ui <- fluidPage(
    titlePanel('Cluster Explorer'),
    fluidRow(
        column(3, selectInput("cl_method", "Cluster Method", methods)),
        column(9, plotOutput('bar'))
    ),
    fluidRow(
        column(12, DTOutput('tbl'))
    )
)

# Define server logic
server <- function(input, output) {
    data <- reactive({
        out <- getClusterResults(data_dir)
        if (input$cl_method != '') {
            out <- out[out$method == input$cl_method,]
        }
        return(out[c('text_data', 'cluster_id')])
    })
    
    output$tbl <- renderDT(data(), filter = 'top', options = list(
        lengthChange = FALSE, 
        pageLength = 5))
    
    output$bar <- renderPlot({
        ggplot(data(), aes(cluster_id)) +
            geom_bar() + 
            ggtitle(paste0('Cluster Frequency (', input$cl_method, ')'))
    })
    
    output$groupedBar <- renderPlot({
        rand <-calcRand(data())
        ggplot(rand, aes(fill=method, y=rand, x=vec)) + 
            geom_bar(position='dodge', stat='identity') + 
            ggtitle('Rand Index By Vectorization')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

