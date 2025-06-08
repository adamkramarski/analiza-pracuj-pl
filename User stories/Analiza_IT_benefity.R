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
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(rmarkdown)




#wczytaj plik excel lub csv
#data <- read_csv("ścieżka pliku")
data <- read_excel("C:/Users/Julek/Desktop/IT.xlsx")


#' # Najczęstsze benefity
#wyondrembnianie kolumny z benefitami, rozdzielanie i czyszczenie, tworzenie tabeli
benefity <- data$benefit_titles
benefity_df <- data.frame(benefity)
benefity_czyste <- str_remove_all(benefity_df$benefity, "\\[|\\]")
benefity_czyste_df <- data.frame(benefity_czyste)
benefity_lista <- str_split(benefity_czyste_df$benefity, ",")
benefity_lista_czyste <- lapply(benefity_lista, function(x) {
  x <- str_trim(x)           # usuń nadmiarowe spacje
  x <- str_remove_all(x, "'")  # usuń pojedyncze cudzysłowy
  x[x != ""]                 # usuń puste elementy (jeśli są)
})
wszystkie_benefity <- unlist(benefity_lista_czyste)


# Zamiana tabeli na ramke danych
tabela_benefitow <- table(wszystkie_benefity)
benefity_czestosc_df <- as.data.frame(tabela_benefitow)


# tworzenie chmury "słów" benefitów 
wordcloud(words = benefity_czestosc_df$wszystkie_benefity, freq = benefity_czestosc_df$Freq, min.freq = 50, 
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE,
          scale = c(2, 0.8))




#' # Najczęstsze technologie

#wyondrembnianie kolumny z progmramami, rozdzielanie i czyszczenie, tworzenie tabeli
programy <- data$technologies_expected
programy_df <- data.frame(programy)
programy_czyste <- str_remove_all(programy_df$programy, "\\[|\\]")
programy_czyste_df <- data.frame(programy_czyste)
programy_lista <- str_split(programy_czyste_df$programy, ",")
programy_lista_czyste <- lapply(programy_lista, function(x) {
  x <- str_trim(x)           # usuń nadmiarowe spacje
  x <- str_remove_all(x, "'")  # usuń pojedyncze cudzysłowy
  x[x != ""]                 # usuń puste elementy (jeśli są)
})
wszystkie_programy <- unlist(programy_lista_czyste)


# Zamiana tabeli na ramke danych
tabela_programy <- table(wszystkie_programy)
programy_czestosc_df <- as.data.frame(tabela_programy)


# tworzenie chmury "słów" programów technologi
wordcloud(words = programy_czestosc_df$wszystkie_programy, freq = programy_czestosc_df$Freq, min.freq = 30, 
          colors = brewer.pal(8, "Dark2"),
          scale = c(3, 0.8))

#' # Przetwarzanie i manipulacja danymi 
#patrzenie na asocjacje miedzy programami a benefitami
data_bp <- data[c("technologies_expected","benefit_titles")]

#połączenie kolumn i oczysczenie

data_bp$tekst_polaczony <- paste(data$benefit_titles, data$technologies_expected, sep = "")
data_bp$tekst_polaczony <- gsub(", ", ",", data_bp$tekst_polaczony)
data_bp$tekst_polaczony <- gsub(" ", "_", data_bp$tekst_polaczony)
data_bp$tekst_polaczony <- str_remove_all(data_bp$tekst_polaczony, "\\[|\\]")
data_bp$tekst_polaczony <- gsub("''", "' '", data_bp$tekst_polaczony)
data_bp$tekst_polaczony <- gsub(",", " ", data_bp$tekst_polaczony)
data_bp$tekst_polaczony <- gsub("'", "", data_bp$tekst_polaczony)


#przyda sie później do usunięcia z szukania korelacji innych programów
data_bp$technologies_expected <- tolower(data_bp$technologies_expected)
data_bp$technologies_expected <- gsub(", ", ",", data_bp$technologies_expected)
data_bp$technologies_expected <- str_remove_all(data_bp$technologies_expected, "\\[|\\]")
data_bp$technologies_expected <- gsub(" ", "_", data_bp$technologies_expected)
data_bp$technologies_expected <- gsub("'", "", data_bp$technologies_expected)
data_bp$technologies_expected <- gsub(",", " ", data_bp$technologies_expected)
lista_slow <- strsplit(data_bp$technologies_expected, split = " ")
wszystkie_slowka <- unlist(lista_slow)
wszystkie_slowka <- trimws(tolower(wszystkie_slowka))
unikalne_slowka <- unique(wszystkie_slowka[wszystkie_slowka != ""])


#tworzenie korpusu
corpus <- VCorpus(VectorSource(data_bp$tekst_polaczony))

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, stripWhitespace)


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


#wytypowane najczęściej pojawiające się programy oraz jedne z najpopularniejszych


#findAssocs(tdm,"sql",0.1)
findAssocs(tdm,"python",0.15)
#findAssocs(tdm,"java",0.1)
#findAssocs(tdm,"javascript",0.1)
#findAssocs(tdm,"git",0.1)
#findAssocs(tdm,"docker",0.1)
#findAssocs(tdm,"c++",0.1)
#findAssocs(tdm,"css",0.1)



#' # Wizualizacja asocjacji
# Wizualizacja asocjacji ----


# Wytypowane słowo i próg asocjacji
target_word <- "python"
cor_limit <- 0.1


# Oblicz asocjacje dla tego słowa
associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)


# Ramka danych
assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)
#usń inne programy gdyż interesuje nas jedynie korelacja między konkretnym programem a benefitami
assoc_df <- assoc_df[!(assoc_df$word %in% unikalne_slowka), ]


# Wykres lizakowy (lollipop chart)
# stosowany w raportach biznesowych i dashboardach:
ggplot(assoc_df, aes(x = score, y = reorder(word, score))) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a6bddb", size = 1.2) +
  geom_point(color = "#0570b0", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1), expand = expansion(mult = c(0, 0.2))) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "benefity"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
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
    y = "benefity",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )



