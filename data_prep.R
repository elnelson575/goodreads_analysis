library(tidyverse)
library(shiny)

data <- read.csv('goodreads_library_export.csv')

d2 <- data %>%
  select(Title, Author, Additional.Authors, My.Rating, Average.Rating, Publisher, Number.of.Pages,
         Original.Publication.Year, Date.Read, Date.Added, Exclusive.Shelf)

write.csv(d2, "to_add_genre.csv")

d3 <- read.csv('added_genre.csv') %>%
  select(-X)

saveRDS(d3, "goodreads_04152019.rds")

pages_genre_total <- d3 %>%
  group_by(Genre) %>%
  summarize(total_pages = sum(Number.of.Pages, na.rm = TRUE))

pages_genre_by_year <- d3 %>%
  mutate(date_read = mdy(Date.Read)) %>%
  mutate(year_read = year(date_read)) %>%
  group_by(Genre, year_read) %>%
  summarize(total_pages = sum(Number.of.Pages, na.rm = TRUE))

pages_genre_by_year_no_na <- pages_genre_by_year %>% filter(!is.na(year_read))

ggplot(pages_genre_by_year_no_na, aes(x = Genre, y = total_pages)) +
  geom_bar(stat="identity", aes(color = Genre)) +
  facet_wrap(~year_read)

pages_genre_by_year_no_na2 <- d3 %>%
  mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
  mutate(year_read = year(date_read)) %>%
  mutate(month_read = month(date_read)) %>%
  filter(year_read >2015) %>%
  filter(!is.na(Number.of.Pages)) %>%
  group_by(Genre, year_read) %>% 
  arrange(month_read) %>%
  mutate(cs = cumsum(Number.of.Pages))

ggplot(pages_genre_by_year_no_na2, aes(x = month_read, y = cs)) +
  geom_line(aes(color = Genre))+
  geom_point(aes(color = Genre)) +
  facet_grid(~year_read)

pages_genre_by_month <- d3 %>%
  mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
  mutate(year_read = year(date_read)) %>%
  mutate(month_read = month(date_read)) %>%
  filter(year_read >2015) %>%
  filter(!is.na(Number.of.Pages)) %>%
  group_by(month_read, year_read) %>% 
  arrange(month_read) %>%
  mutate(cs = cumsum(Number.of.Pages))

ggplot(pages_genre_by_month, aes(x=month_read, y = cs))+
  geom_bar(stat = "identity")+
  facet_grid(~year_read)

pages_genre_by_month <- d3 %>%
  mutate(date_read = mdy(Date.Read), date_added = mdy(Date.Added)) %>%
  mutate(year_read = year(date_read)) %>%
  mutate(month_read = month(date_read)) %>%
  filter(year_read >2015) %>%
  filter(!is.na(Number.of.Pages)) %>%
  group_by(Genre, month_read, year_read)

ggplot(pages_genre_by_month, aes(x=month_read, y = Number.of.Pages))+
  geom_bar(stat = "identity", aes(fill = Genre)) +
  facet_wrap(~year_read)
