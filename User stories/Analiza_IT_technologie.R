


#' ---
#' title: "Asocjacje"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)





#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)



#' # Dane tekstowe
# Dane tekstowe ----

# Ustaw Working Directory!
# Załaduj dokumenty z folderu
# docs <- DirSource("textfolder2")
# W razie potrzeby dostosuj ścieżkę
# np.: docs <- DirSource("C:/User/Documents/textfolder2")


# Utwórz korpus dokumentów tekstowych

# Gdy tekst znajduje się w jednym pliku csv:
data <- read.csv("pracuj_pl_IT.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
corpus <- VCorpus(VectorSource(data$technologies_expected))


# Korpus
# inspect(corpus)


# Korpus - zawartość przykładowego elementu
corpus[[1]]
corpus[[1]][[1]]
# corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

corpus <- tm_map(corpus,toSpace, "\\[")
corpus <- tm_map(corpus,toSpace, "\\]")
corpus <- tm_map(corpus,toSpace, "\\'")


# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus[[1]][[1]]

corpus <- tm_map(corpus, removeNumbers)
corpus[[1]][[1]]

corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]][[1]]

#corpus <- tm_map(corpus, removePunctuation)
#corpus[[1]][[1]]

corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]]

# usunięcie ewt. zbędnych nazw własnych
corpus <- tm_map(corpus, removeWords, c("flight", "lot"))

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]]



# Decyzja dotycząca korpusu ----
# do dalszej analizy użyj:
#
# - corpus (oryginalny, bez stemmingu)
#




#' # Tokenizacja
# Tokenizacja ----



# Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)



#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 10))



#' # 4. Inżynieria cech w modelu Bag of Words:
#' # Reprezentacja słów i dokumentów w przestrzeni wektorowej
# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



#' # Asocjacje - znajdowanie współwystępujących słów
# Asocjacje - znajdowanie współwystępujących słów ----



# Funkcja findAssoc() w pakiecie tm służy do:
# - znajdowania słów najbardziej skorelowanych z danym terminem w macierzy TDM/DTM
# - wykorzystuje korelację Pearsona między wektorami słów
# - jej działanie nie opiera się na algorytmach machine learning


# Samodzielnie wytypuj słowa (terminy), 
# które chcesz zbadać pod kątem asocjacji

#class(tdm)
#"sql" %in% Terms(tdm)
#grep("sql", Terms(tdm), value = TRUE)  # powinno zwrócić np. "sql"


#' # Wizualizacja asocjacji
# Wizualizacja asocjacji ----


# Wytypowane słowo i próg asocjacji
wykres_lizakowy <- function(target_word){

#target_word <- "sap"
cor_limit <- 0.2


# Oblicz asocjacje dla tego słowa
associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)


# Ramka danych
assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)



# Wykres lizakowy z natężeniem
# na podstawie wartości korelacji score:
ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_color_gradient(low = "#a6bddb", high = "#08306b") +
  scale_x_continuous(
    limits = c(0, max(assoc_df$score) + 0.1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "Słowo",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )


}

print(head(tdm_df, 10))
top_words <- head(tdm_df, 10)$word
for(top_word in top_words){
  print(wykres_lizakowy(top_word))
}


#' # Dodanie średnich zarobkowych
# Zakładamy, że wszystkie płace są podane w złotówkach i pomijamy ewentualne grosze. 
#Podane wydełki spłaszczamy średnią arytmetyczną. Pokażemy średnie płac związane z danym językiem.

#Najpierw podstawowe czyszczenie stringów
earnings_dirty <- data$earning_amount
earnings <- gsub("zł", "", earnings_dirty)
earnings <- gsub("\\s", "", earnings)
earnings <- gsub(",\\d{2}", "", earnings)
earnings <- gsub(",00", "", earnings)
has_dash <- grepl("–", earnings)

#Analiza widełek. Rozdielamy płace zapisane z "–" i obliczamy z dwóch wartości średnią.
earnings_parts <- strsplit(earnings[has_dash], "–")
earnings_avg <- sapply(earnings_parts, function(x) mean(as.integer(x)))
earnings[has_dash] <- earnings_avg
earnings <- as.numeric(earnings)
hourly <- earnings < 1000
hourly[is.na(hourly)] <- FALSE
earnings[hourly] <- earnings[hourly]*168

data$earnings <- earnings

#Ilość ofert pracy z podanymi zarobkami
print(length(earnings[!is.na(earnings)]))

#Odsetek ofert pracy z podanymi zarobkami 
print(length(earnings[!is.na(earnings)])/(length(earnings)))

#Analizujemy dla naszych zwycięzców
top_words <- head(tdm_df, 10)

#Spłaszczamy korpus byśmy mogli sprawdzać po tecg %in% words
flat_corpus <- lapply(corpus, function(x) as.character(x[[1]]))
str(flat_corpus[1:2])
flat_corpus[1]

split_corpus <- lapply(flat_corpus, function(x) {
  words <- unlist(strsplit(x, ","))
  trimws(words)
})

#Podajemy średnią zarobkową wszystkich prac zawierających daną technologię
earnings_for_tech <- function(split_corpus, tech){
  is_tech <- sapply(split_corpus, function(words) tech %in% words)
  filtered_earnings <- earnings[is_tech]
  filtered_earnings <- filtered_earnings[!is.na(filtered_earnings)]
  return(mean(filtered_earnings))
}
earnings_for_top <- earnings_for_tech(split_corpus, "sql")

#Podajemy wynik dla topowej dziesiątki
head(tdm_df, 10)
top_words <- head(tdm_df, 20)$word
results <- sapply(top_words, function(tech) earnings_for_tech(split_corpus, tech))
print(results)

df <- data.frame(
  tech = names(results),
  mean_earnings = as.numeric(results),
  stringsAsFactors = FALSE
)

#Sortowanie
df <- df %>%
  filter(!is.na(mean_earnings)) %>%
  arrange(mean_earnings) %>%
  mutate(tech = factor(tech, levels = tech))

#Wykres
ggplot(df, aes(x = tech, y = mean_earnings)) +
  geom_col(fill = "#4C9F70") +
  coord_flip() +
  labs(
    title = "Średnie zarobki dla wybranych technologii",
    x = "Technologia",
    y = "Średnie zarobki (PLN/miesiąc)"
  ) +
  theme_minimal(base_size = 13)


