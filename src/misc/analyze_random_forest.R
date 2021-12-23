# Finds out the number of leaves in trees of random forest in each category: accepted and deferred

library(tidyverse)

get_leave_counts_helper <- function(k, f) { 
  v <- as.character(na.omit(randomForest::getTree(f, k=k, labelVar = TRUE)$prediction)) # Drop non-leave nodes
  #print(v)
  #v <-  table(v) %>% as.numeric()
  #print(v)
  t <- as.data.frame(table(v, dnn="name")) %>% 
     as_tibble() %>% 
     pivot_wider(names_from=name, values_from = Freq)
  return(t)
}

number_of_tree <- 500

get_leave_counts <- function(filename) {
  fit <- readRDS(filename)
  df <- map_dfr(1:number_of_trees, get_leave_counts_helper, fit$finalModel)
  return(df)
}

cat("Leave count statistics for males\n")
male <- get_leave_counts("/tmp/rf-fit-male.rds")
print(summary(male))

cat("Leave count statistics for females\n")
female <- get_leave_counts("/tmp/rf-fit-female.rds")
print(summary(female))
