library(tidyverse)
library(shiny)
library(shinyBS)
library(shinyjs)
library(plotly)
library(DT)
library(zoo)


# Start the server
shinyServer(function(input, output) {
  
  data <- readRDS("goodreads_05112019.rds")
  
  pages_genre_by_month <- data %>%
    mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
    mutate(year_read = year(date_read)) %>%
    mutate(month_year_read = as.yearmon(date_read)) %>%
    filter(year_read >2015) %>%
    filter(!is.na(Number.of.Pages)) %>%
    group_by(Genre, month_year_read) %>%
    ungroup()
  
  
  output$output1 <- renderPlotly({
    plot_ly(pages_genre_by_month, x = ~month_year_read, y = ~Number.of.Pages, type = 'bar', color = ~Genre) %>%
      # add_trace(y = ~Number.of.Pages, name = 'Number of Pages') %>%
      layout(yaxis = list(title = 'Total Pages'), barmode = 'stack')
  })
  
  
  
  
  ### Pages by race ###
  
  pages_race_by_month <- data %>%
    mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
    mutate(year_read = year(date_read)) %>%
    mutate(month_read = month(date_read)) %>%
    mutate(month_year_read = as.yearmon(date_read)) %>%
    filter(year_read >2015) %>%
    filter(!is.na(Number.of.Pages)) %>%
    group_by(Race, month_read, year_read)
  
  output$output2 <- renderPlotly({
    plot_ly(pages_race_by_month, x = ~month_year_read, y = ~Number.of.Pages, type = 'bar', color = ~Race) %>%
      # add_trace(y = ~Number.of.Pages, name = 'Number of Pages') %>%
      layout(yaxis = list(title = 'Total Pages'), barmode = 'stack')
  })
  
  
  ### Pages by Gender
  pages_gender_by_month <- data %>%
    mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
    mutate(year_read = year(date_read)) %>%
    mutate(month_read = month(date_read)) %>%
    mutate(month_year_read = as.yearmon(date_read)) %>%
    filter(year_read >2015) %>%
    filter(!is.na(Number.of.Pages)) %>%
    group_by(Gender, month_read, year_read)
  
  output$output3 <- renderPlotly({
    plot_ly(pages_gender_by_month, x = ~month_year_read, y = ~Number.of.Pages, type = 'bar', color = ~Gender) %>%
      # add_trace(y = ~Number.of.Pages, name = 'Number of Pages') %>%
      layout(yaxis = list(title = 'Total Pages'), barmode = 'stack')
  })
  
  # ## Pages by LGBTQ ##
  # pages_race_by_lgbtq <- d3 %>%
  #   mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
  #   mutate(year_read = year(date_read)) %>%
  #   mutate(month_read = month(date_read)) %>%
  #   filter(year_read >2015) %>%
  #   filter(!is.na(Number.of.Pages)) %>%
  #   group_by(LGBTQ, month_read, year_read)
  #
  # ggplot(pages_genre_by_month, aes(x=month_read, y = Number.of.Pages))+
  #   geom_bar(stat = "identity", aes(fill = LGBTQ)) +
  #   facet_wrap(~year_read)
  
  table_data <- data %>%
    filter(Exclusive.Shelf == "read") %>%
    mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
    mutate(year_read = year(date_read)) %>%
    mutate(month_read = month(date_read)) %>%
    filter(year_read >2015) %>%
    select(-Additional.Authors, -Exclusive.Shelf, -Date.Added, -date_read, -date_added, -month_read)
  
  output$booktable <- renderDataTable(table_data)
  
  
  topstats <- genTopStats(data)
  
  output$topAuthors <- renderDataTable(topstats[[1]])
  output$topGenre_books <- renderDataTable(topstats[[2]])
  output$topGenre_pages <- renderDataTable(topstats[[3]])
  
  
  
  
  
})
