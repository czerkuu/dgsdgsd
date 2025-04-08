# Załadowanie niezbędnych bibliotek
library(dplyr)
library(ggplot2)
library(tidyr)

# Wczytanie danych
data <- read.csv("Global YouTube Statistics.csv", stringsAsFactors = FALSE)

# 1. Przygotowanie danych
# Usunięcie wierszy z brakującymi wartościami w kolumnach 'uploads', 'category' i 'Country'
clean_data <- data %>%
  filter(!is.na(uploads), !is.na(category), !is.na(Country), uploads > 0)

# 2. Analiza średniej liczby uploadów wg kategorii
category_analysis <- clean_data %>%
  group_by(category) %>%
  summarise(
    mean_uploads = mean(uploads, na.rm = TRUE),
    median_uploads = median(uploads, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(mean_uploads))

# 3. Analiza średniej liczby uploadów wg krajów (ograniczenie do krajów z co najmniej 10 kanałami)
country_analysis <- clean_data %>%
  group_by(Country) %>%
  summarise(
    mean_uploads = mean(uploads, na.rm = TRUE),
    median_uploads = median(uploads, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count >= 10) %>%
  arrange(desc(mean_uploads))

# 4. Wizualizacja dla kategorii
# Wykres średniej liczby uploadów wg kategorii (top 10)
ggplot(category_analysis %>% top_n(10, mean_uploads), aes(x = reorder(category, mean_uploads), y = mean_uploads)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 kategorii z najwyższą średnią liczbą uploadów",
    x = "Kategoria",
    y = "Średnia liczba uploadów"
  ) +
  theme_minimal()

# 5. Wizualizacja dla krajów
# Wykres średniej liczby uploadów wg krajów (top 10)
ggplot(country_analysis %>% top_n(10, mean_uploads), aes(x = reorder(Country, mean_uploads), y = mean_uploads)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(
    title = "Top 10 krajów z najwyższą średnią liczbą uploadów",
    x = "Kraj",
    y = "Średnia liczba uploadów"
  ) +
  theme_minimal()

# 6. Dodatkowa analiza: rozkład uploadów wg kategorii (boxplot)
ggplot(clean_data %>% filter(category %in% head(category_analysis$category, 5)), aes(x = reorder(category, uploads, FUN = median), y = uploads)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_log10() +  # Skala logarytmiczna dla lepszej czytelności
  labs(
    title = "Rozkład liczby uploadów wg kategorii (top 5)",
    x = "Kategoria",
    y = "Liczba uploadów (skala logarytmiczna)"
  ) +
  theme_minimal()