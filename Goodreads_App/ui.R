library(tidyverse)
library(shiny)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Goodreads Data"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Pages Read by Genre",
               plotlyOutput("output1"),
               plotlyOutput("output2"),
               plotlyOutput("output3")
      ),
      tabPanel("Individual Book Data",
               dataTableOutput('booktable')
      ),
      tabPanel("Leaderboard",
               fluidRow(width = 12,
                        column(4,
                               #Add an icon or some formatting here
                               dataTableOutput('topAuthors')),
                        column(3, offset = 2,
                               dataTableOutput('topGenre_books')),
                        column(3,
                               dataTableOutput('topGenre_pages')))
      )
    )
  )
)
)
