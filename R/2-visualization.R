library(tidyverse)
library(plotly)
library(gifski)
library(gganimate)
library(ggplot2)

data <- readRDS("../data/data.rds")

# 2.1 Histograma

histogram <- data %>%
  ggplot(aes(x = data$avgWage)) +
  labs(title = "Vidutinio atlyginimo histograma",
       x = "Vidutinis atlyginimas",
       y = "Dažnis") +
  geom_histogram(bins = 100) +
  theme_classic()

ggsave("../img/pav1.png", histogram, width = 10, height = 5)

# 2.2 Top 5 metinio atlyginimo įmonės

top5 <- data %>%
  group_by(name) %>%
  summarise(dsk = sum(avgWage)) %>%
  arrange(desc(dsk)) %>%
  head(5)

p <- data %>%
  filter(name %in% top5$name) %>%
  mutate(month = ym(month)) %>%
  ggplot(aes(x = month, y = avgWage, color = name)) +
  labs(title = "Atlyginimo dinamika",
       x = "Mėnesis",
       y = "Vidutinis atlyginimas") +
  geom_line() +
  theme_classic() +
  theme(legend.title = element_blank()) 


ggsave("../img/pav2.png", p, width = 10, height = 5)

# 2.2 Vizualizacija su animacija 
p_animate <- p +
  transition_reveal(month)

p_animated <- animate(p_animate, nframes = 50, duration = 5, width = 1600, height = 1000)
p_animated
anim_save("../img/animated_plot.gif", animation = p_animated)

# 2.3. Top 5 daugiausiai apdraustų darbuotojų turinčios įmonės

top_insured <- data %>%
  group_by(name) %>%
  summarise(numInsured = max(numInsured)) %>%
  arrange(desc(numInsured)) %>%
  head(5)

pp <- ggplot(top_insured, aes(reorder(name, -numInsured), numInsured, fill=name)) +
  geom_bar(stat="identity") +
  labs(title = "Daugiausiai apdraustųjų įmonės",
       x = "Įmonės pavadinimas",
       y = "Apdraustųjų skaičius") +
  theme_classic() +
  theme(legend.title = element_blank()) 

ggsave("../img/pav3.png", pp, width = 15, height = 10)
  

