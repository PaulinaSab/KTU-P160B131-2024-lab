library(tidyverse)

cat("Darbinė direktorija:", getwd())

download.file("https://atvira.sodra.lt/imones/downloads/2023/monthly-2023.csv.zip", "../data/temp")
unzip("../data/temp",  exdir = "../data/")
readLines("../data/monthly-2023.csv", 2)
data <- read_delim("../data/monthly-2023.csv", delim = ";")
names(data) <- str_extract(names(data), "(?<=\\().*(?=\\))")
data %>%
  filter(`ecoActCode` == '620100') %>%
  saveRDS("../data/data.rds")
file.remove("../data/temp")
file.remove("../data/monthly-2023.csv")
