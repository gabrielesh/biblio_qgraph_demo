# Cocitaion analysis with the Biblionetwork package ----

## Load Packages -----

install.packages(c("biblionetwork", "igraph", "qgraph"))

library(biblionetwork)
library(tidyverse)
library(igraph)
library(qgraph)


## Explore Sample Data ----
# Sample data in the biblionetwork package called Ref_stagflation
# which includes articles and books explaining the 1970s US stagflation
View(Ref_stagflation)

# biblionetwork analysis
cocit <- biblio_cocitation(Ref_stagflation, 
                           source = "Citing_ItemID_Ref",
                           ref = "ItemID_Ref",
                           normalized_weight_only = FALSE,
                           weight_threshold = 5)

cocit <- cocit %>% 
  mutate(bucket = case_when(nb_shared_citations >= 1 
                            & nb_shared_citations < 5 ~ "1-4",
                            nb_shared_citations >= 5 
                            & nb_shared_citations < 10 ~ "5-10",
                            nb_shared_citations >= 10 
                            & nb_shared_citations < 15 ~ "10-14",
                            nb_shared_citations >= 15 
                            & nb_shared_citations < 20 ~ "15-19",
                            nb_shared_citations >= 20 
                            & nb_shared_citations < 25 ~ "20-24",
                            nb_shared_citations >= 25
                            & nb_shared_citations < 30 ~ "25-30"))

write_csv(cocit, "results/cocitation.csv")

cocit_summary <- cocit %>% 
  group_by(bucket) %>% 
  summarise(count = n()) %>% 
  arrange(count)

write_csv(cocit_summary, "results/cocitation_summary.csv")

cocit_for_graph <- cocit %>% 
  select(from, to, weight)

## Visualize using qgraph ----

qgraph(cocit_for_graph, directed = FALSE)


