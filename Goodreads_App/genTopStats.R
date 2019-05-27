genTopStats <- function(data){
  
  topAuthors<- data %>%
    select(Author, Race, Gender, LGBTQ) %>%
    add_count(Author) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    unique() %>%
    top_n(5, n)
  
  topGenre_books <- data %>%
    select(Genre) %>%
    add_count(Genre) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    unique() %>%
    top_n(5, n)
  
  topGenre_pages <- data %>%
    select(Genre, Number.of.Pages) %>%
    group_by(Genre) %>%
    summarize(total_pages = sum(Number.of.Pages)) %>%
    ungroup() %>%
    arrange(desc(total_pages)) %>%
    top_n(5, total_pages)
  
  return(list(topAuthors, topGenre_books, topGenre_pages))
}
