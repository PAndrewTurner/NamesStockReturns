library(haven)
library(stringdist)
library(dplyr)
library(ggplot2)

# Loading data
mret <- read_sas("mret7018.sas7bdat")
names_data <- read.csv("Names.csv")

data <- mret %>%
  group_by(COMNAM) %>%
  summarise(AVG_RET = mean(RET, na.rm = TRUE)) %>%
  # Rename columns
  rename(Name = COMNAM, Returns = AVG_RET)

data <- na.omit(data)

# top 20 most and least popular names
distinct_names <- data$Name
distinct_names <- na.omit(distinct_names)

popular_names <- head(names_data$name, 20)
unpopular_names <- tail(names_data$name, 20)

calculate_similarity <- function(name1, name2) {
  dist <- stringdist::stringdistmatrix(name1, name2, method = "jw")
  similarity_score <- 1 - dist
  return(similarity_score)
}

cor_dataset <- data.frame("Comp_Name" = character(), "Sim_Score" = numeric(), "Returns" = numeric())

for (x in distinct_names) {
  for (y in popular_names) {
    similarity_score <- calculate_similarity(y, x)
    ret_values <- data$Returns[data$Name == x]
  }
  print(x)
  print(similarity_score)
  cor_dataset <- rbind(cor_dataset, data.frame("Comp_Name" = x, "Sim_Score" = similarity_score, "Returns" = ret_values))
}


unp_cor_dataset <- data.frame("Comp_Name" = character(), "Sim_Score" = numeric(), "Returns" = numeric())

for (x in distinct_names) {
  for (y in unpopular_names) {
    similarity_score <- calculate_similarity(y, x)
    ret_values <- data$Returns[data$Name == x]
  }
  print(x)
  print(similarity_score)
  unp_cor_dataset <- rbind(unp_cor_dataset, data.frame("Comp_Name" = x, "Sim_Score" = similarity_score, "Returns" = ret_values))
}


pop_cor_value <- cor(cor_dataset$Sim_Score, cor_dataset$Returns)
unp_cor_value <- cor(unp_cor_dataset$Sim_Score, unp_cor_dataset$Returns)

plot(cor_dataset$Sim_Score, cor_dataset$Returns)
plot(unp_cor_dataset$Sim_Score, unp_cor_dataset$Returns)