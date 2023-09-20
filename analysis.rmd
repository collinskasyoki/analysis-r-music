---
title: "R Notebook"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r}
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(viridis)
```


```{r}
music <- read.csv("~/Software/musics/output.csv")
count(music)
```


```{r}
head(music)
```


```{r}
cbind(lapply(lapply(music, is.na), sum))
```


```{r}
music <- subset(music, select=-c(lyrics, id, album_id, artist_id))
head(music)
```


```{r}
top_10 <- music %>% count(year) %>% arrange(desc(n)) %>% top_n(10)
filtered <- music %>% filter(year %in% top_10$year) %>% mutate(Years = factor(year, levels=top_10$year))
ggplot(filtered, aes(x=Years)) + 
  geom_bar() + coord_flip() +
  labs(x = "Year", y = "Count", title = "Top 10 Years by Song Count")
```


```{r}
top_10_art <- music %>% count(artist_name) %>% arrange(desc(n)) %>% top_n(10)
filtered_art <- music %>% filter(artist_name %in% top_10_art$artist_name) %>% mutate(Artists = factor(artist_name, levels=top_10_art$artist_name))

ggplot(filtered_art, aes(x=Artists), fill=Artists) + 
  geom_bar() + coord_flip() + 
  labs(x = "Artist Name", y = "Count", title = "Top 10 Artists by Song Count")
```


```{r}
album_lengths <- music %>%
  group_by(album) %>% 
  summarize(length_minutes = sum(length)/1000/60) %>% 
  arrange(desc(length_minutes)) %>% 
  top_n(10)

album_lengths$album <- factor(album_lengths$album, levels = album_lengths$album)

ggplot(album_lengths, aes(x = album, y = length_minutes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Album Name", y = "Length (Minutes)", title = "Top 10 Album Lengths")
```


```{r}
corpus <- Corpus(VectorSource(music$album))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
term_doc_matrix <- TermDocumentMatrix(corpus)
word_frequency <- rowSums(as.matrix(term_doc_matrix))
wordcloud(words = names(word_frequency), freq = word_frequency, min.freq = 40)
```


```{r}
corpus <- Corpus(VectorSource(music$title))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
term_doc_matrix <- TermDocumentMatrix(corpus)
word_frequency <-rowSums(as.matrix(term_doc_matrix))
wordcloud(words = names(word_frequency), freq = word_frequency, min.freq = 50)
```

