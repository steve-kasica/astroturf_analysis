#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

n <- 1000
seed <- 42
data_dir <- file.path('../data', paste(n, seed, sep='_'))

getMethods <- function(dir) {
    methods <- list.files(dir)
    methods <- clusters[grepl('^clusters_', clusters)]
    methods <- str_replace_all(clusters, '^clusters_([^\\.]+)\\.csv', '\\1')
    return(methods)
}

methods <- getMethods(data_dir)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel('Exploring Clusters'),
    fluidRow(
        column(3, selectInput("cl_method", "Cluster Method", methods))
    ),
    fluidRow(
        column(6, plotOutput('bar'))
    ),
    fluidRow(
        column(12, DTOutput('tbl'))
    )
))