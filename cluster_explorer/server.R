#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)

getData <- function(n, seed) {
    data_dir <- file.path(getwd(), '../data', paste0(n, '_', seed))
    clusters <- list.files(data_dir)
    clusters <- clusters[grepl('^clusters_', clusters)]
    comments <- read.csv(file.path(data_dir, 'comments_sample.csv')) %>%
        select(-dupe_count)

    comments <- lapply(clusters, function(fn) {
        cl <- read.csv(file.path(data_dir, fn)) %>% select(docid, cluster)
        key <- sub('clusters_(.*).csv', '\\1', fn)
        colnames(cl) <- c('docid', key)
        return(cl)
    }) %>% 
        reduce(inner_join, by='docid') %>% inner_join(comments, by='docid') %>%
        gather(key='cluster_method', value='cluster_id', -docid, -text_data)
    
    comments$cluster_id <- as.factor(comments$cluster_id)
    
    return(comments)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    sample <- 1000
    random_seed <- 42
    
    data <- reactive({
        data <- getData(sample, random_seed)
        if (input$cl_method != '') {
            data <- data[data$cluster_method == input$cl_method,]
        }
        return(data[c('text_data', 'cluster_id')])
    })
    
    output$tbl <- renderDT(data(), filter = 'top', options = list(
        lengthChange = FALSE, 
        pageLength = 5))
    
    output$bar <- renderPlot({
        ggplot(data(), aes(cluster_id)) +
            geom_bar() + 
            ggtitle(paste0('Cluster Frequency (', input$cl_method, ')'))
    })
    
})
