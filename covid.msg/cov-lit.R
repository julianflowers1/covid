## cov lit

### pubmed

library(myScrapers)
library(aRxiv)
library(europepmc)
#install.packages('dplyr')
#remotes::install_github("tidyverse/dplyr")
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

### search

library(tidyverse)
library(devtools)
devtools::install_github("julianflowers/myScrapers")
library(myScrapers)
library(europepmc)
#install.packages("spacyr")
library(spacyr)

spacy_initialize()

### covid mortality rates from the literature

  covid_mr <- googlesearchR("scholar:covid-19 mortality rate")
links_text <- covid_mr %>%
  .[3:40] %>%
  purrr::map(., as.character()) %>%
  enframe() %>%
  unnest("value") %>%
  mutate(text = map(value, possibly(get_page_text, otherwise = NA_real_))) %>%
  chop("text")

View(links_text)

search <- '(coronavirus OR COVID*) mortality'
n <- 188
end <- 2020
start = 2019
ncbi_key = "bd86b3e3500c581cbc6ee0896f551bae0408"
set.seed <- 42

test1 <- pubmedAbstractR(search = search, n = n, end = end, ncbi_key = ncbi_key, start = start)

test2 <- test1$abstracts %>%
  rename(absText = abstract, pubYear = year, pmid = DOI)

corp <- create_abstract_corpus(test2)
clust <- create_abstract_cluster(corp$corpus, minPts = 5)
clust

labels <- create_cluster_labels(corp$corpus, clust$clustering)
labels$labels

labelled <- labels$results %>%
  left_join(test2, by = c("pmid.value" = "pmid"))

filtered <- labelled %>%
  filter(cluster == 0) %>%
  .[c(73, 80, 82, 83, 90, 93), ]

filtered$title

## corpus method

library(quanteda)


corp1 <- quanteda::corpus(labelled$absText)
docvars(corp1, "title") <- labelled$title
docvars(corp1, "label") <- labelled$clus_names
df <- docvars(corp1) %>%
  mutate(docname = paste0("text", row_number()))
out <- kwic(corp1, pattern = "mortality|death", valuetype = "regex", window = 10) %>%
  data.frame() %>%
  left_join(df) %>%
  select(docname, title, label, everything())
  View(out)

parsed <- spacy_parse(labelled$absText,  nounphrase = TRUE)
ent <-entity_consolidate(parsed)
nouns <- nounphrase_consolidate(parsed) %>%
  filter(pos == "nounphrase")

nouns %>%
  View()

labels$labels

head(corp)

test <- europepmc::epmc_search(search)
head(test$title)


search <- "(coronavirus OR covid19 OR SARS2) (surveillance[tw] OR epidemiology[mh] OR prevention and control[mh])"
n <- 1737
key <- Sys.getenv("ncbi_key")
end <- 2020
start <- 2019
set.seed(42)
### download abstracts

s1 <- pubmedAbstractR(search = search, n = n, ncbi_key = key, end = end, start = start)

s1 <- s1$abstracts %>%
  rename(absText = abstract, pmid = DOI, pubYear = year ) %>%
  mutate(absText = paste(title, absText))

install_github("ropensci/aRxiv")
library(aRxiv)
s2 <- arxiv_search("coronavirus OR au:Gelman", limit = 100)
s2

### classify

s1_corp <- create_abstract_corpus(s1)
s1_clust <- create_abstract_cluster(s1_corp$corpus, minPts = 7, perplexity = 10)

s1_clust$cluster_count

### label

s1_labels <- create_cluster_labels(s1_corp$corpus, s1_clust$clustering, top_n = 4)

s1_labels$labels

### cluster plot

p <- s1_labels$results %>%
  ggplot(aes(X1, X2, colour = factor(cluster), group = clus_names)) +
  geom_point(show.legend = FALSE)

a <- p +
  ggrepel::geom_text_repel(data = s1_labels$plot, aes(medX, medY, label = clus_names), shape = "X", colour = "black") +
  theme_void() +
  viridis::scale_color_viridis(discrete = TRUE)

a 

s1_labels$results %>%
  count(clus_names, sort = TRUE) %>%
  ggplot(aes(clus_names, n)) +
  geom_col() +
  coord_flip()

  
data_filt <- s1_labels$results %>%
  filter(cluster %in% c(0, 5)) %>%
  left_join(s1, by = c("pmid.value" = "pmid")) %>%
  rename(pmid = pmid.value)



## trial extract values

### method1 - kwic


data_filt_corpus <- quanteda::corpus(data_filt$absText)

numbers <- quanteda::kwic(data_filt_corpus, pattern = "\\d", valuetype = "regex", window = 10) %>%
  data.frame()

numbers %>%
  View()


### method2 ner in spacyr

library(spacyr)

np <- spacy_extract_nounphrases(data_filt$title)

ner <- spacy_extract_entity(data_filt$absText) %>%
  filter(ent_type %in% c("DATE", "CARDINAL", "ORDINAL"))

ner_title <- spacy_extract_entity(data_filt$title)

head(ner, 20)

np %>%
  View()

ner %>%
  View()

corp1 <- create_abstract_corpus(data_filt) 







clus1 <- corp1$corpus %>%
  #rename(pmid = pmid.value) %>%
  create_abstract_cluster(minPts = 5)

clus1$cluster_count

corp_filt <- rename(corp_filt, pmid = pmid.value)

clus1_labels <- create_cluster_labels(corpus = corp1$corpus, clus1$clustering)

clus1_labels$results %>%
  filter(cluster == 0) %>%
  left_join(s1, by = c("pmid.value" = "pmid")) %>%
  DT::datatable()


