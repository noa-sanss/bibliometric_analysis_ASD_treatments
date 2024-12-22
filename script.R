

# Note: This is only a portion of the full code.


# Package Installation -----------------------------------------------------
install.packages("stringr")
install.packages("treemap")
install.packages("treemapify")
install.packages("sf")
install.packages("maps")
install.packages("viridis")

library(viridis)
library(stringr)
library(ggplot2)
library(dplyr) 
library(gridExtra)
library(treemap)
library(treemapify)
library(RColorBrewer)

# Data Management --------------------------------------------------------------
data <- read.csv2("C:/Users/PC/Documents/M1/Analyse bibliométrique/data.csv")
data <- data[data$Times.Cited..WoS.Core >= 11 & data$Document.Type == "Article", ] 

# JOURNAL INFLUENCE -----------------------------------------------------------

## Based on the Number of Publications ----------------------------------------

# Calculating the number of publications per journal
num_publications <- data %>%
  group_by(Source.Title) %>%
  summarise(n_pubs = n()) %>%
  arrange(desc(n_pubs))

# Top 10 journals with the highest number of publications
top_publications <- num_publications %>% slice_max(n_pubs, n = 10)
ggplot(top_publications, aes(x = reorder(Source.Title, n_pubs), y = n_pubs)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Publications by Journal", x = "Journal", y = "Number of Publications") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) 

## Based on the Average Number of Citations ------------------------------------

# Calculating the average number of citations per journal
avg_citations <- data %>%
  group_by(Source.Title) %>%
  summarise(avg_cit = mean(Times.Cited..WoS.Core))

# Top 15 journals with the highest average number of citations
top_citations <- avg_citations %>% slice_max(avg_cit, n = 15)
ggplot(top_citations, aes(x = reorder(Source.Title, avg_cit), y = avg_cit)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Number of Citations by Journal", x = "Journal", y = "Average Number of Citations") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) 

## Comparing TOP Rankings ------------------------------------------------------ 

common_top <- intersect(top_publications$Source.Title, top_citations$Source.Title)

# Adding columns to indicate if a journal appears in both top rankings
top_publications <- top_publications %>%
  mutate(Highlight = ifelse(Source.Title %in% common_top, "Common", "Unique"))

top_citations <- top_citations %>%
  mutate(Highlight = ifelse(Source.Title %in% common_top, "Common", "Unique"))

# Graphical Representation (top publications)
plot_publications <- ggplot(top_publications, aes(x = reorder(Source.Title, n_pubs), y = n_pubs, fill = Highlight)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Common" = "#e74c3c", "Unique" = "grey")) +
  coord_flip() +
  labs(title = "Top 10 Journals (Publications)", x = "Journal", y = "Number of Publications", fill = "Category") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50))

# Graphical Representation (top citations)
plot_citations <- ggplot(top_citations, aes(x = reorder(Source.Title, avg_cit), y = avg_cit, fill = Highlight)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Common" = "#e74c3c", "Unique" = "grey")) +
  coord_flip() +
  labs(title = "Top 15 Journals (Average Citations)", x = "Journal", y = "Average Number of Citations per Publication on the Topic", fill = "Category") +
  theme_minimal()

# Combining the plots
grid.arrange(plot_publications, plot_citations, ncol = 1)

print(common_top)

# COUNTRY INFLUENCE -----------------------------------------------------------

## Treemap Representation ------------------------------------------------------

data$country <- stringr::str_extract(data$Reprint.Addresses, "[^,\\s]+(?=\\.$)")

publications_by_country <- data %>%
  group_by(country) %>%
  summarise(num_country = n()) %>%
  arrange(desc(num_country))

# Generate a dynamic palette for 38 countries
palette <- viridis(38, option = "D")

# Graphical Representation (distribution of publications by country)
ggplot(publications_by_country, aes(area = num_country, fill = country, label = paste(country, "\n", num_country))) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre", size = 10) +
  scale_fill_manual(values = palette) +
  labs(title = "Distribution of Publications by Country") +
  theme_minimal()
