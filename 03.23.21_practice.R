# Set up workspace
rm(list = ls())

setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")

# Loading packages
#install.packages("factoextra")

library(quanteda)
library(factoextra)
library(dplyr)

## 1 PCA

# 1.1  function in base R for PCA:
# see: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
?prcomp # uses the singular value decomposition approach: examines the covariances/correlations between individuals

# Remember to center your data! (default = TRUE) -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!



# 1.2 Example
data("data_corpus_inaugural")
inaugural <- corpus_subset(data_corpus_inaugural, Year > "1900-01-01")

inaugural_dfm <- dfm(inaugural, 
                     stem = T, 
                     remove_punct = T, 
                     remove = stopwords("english")
)


inaugural_mat <- convert(inaugural_dfm, to = "matrix") # convert to matrix

# run pca
inaugural_pca <- prcomp(inaugural_mat, center = TRUE, scale = TRUE)

# Loadings for each variable: columns contain the eigenvectors
inaugural_pca$rotation[1:10, 1:5]
dim(inaugural_pca$rotation)

# Q: can we interpret the dimensions?
# top loadings on PC1
pc_loadings <- inaugural_pca$rotation


##### Exercise ######

# Question 1: Update the code so that it calculates the terms with the 
#top loadings on PC2. What is the theme of this dimension?

# token loadings
N <- 5
Y <- 10
pc2_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,2])) %>% arrange(-loading)
pc2_loading$loading <- scale(pc2_loading$loading, center = TRUE)
pc2_loading <- rbind(top_n(pc2_loading, N, loading),top_n(pc2_loading, -Y, loading))
pc2_loading <- transform(pc2_loading, token = factor(token, levels = unique(token)))

# plot top tokens according to absolute loading values
ggplot(pc2_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc2_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))

## Actually all negative terms are close to each other, next after the "time" will be "fellow" term.
## Overall, positive themes of dimension are ... I do not know, something about warfare, inspiration and sperm. It too hard to interpret as I had to reduce N to 5.
## Because otherwise, the top_n(N) will show first 120 terms as terms from 6 to 120 had the same loading values.
## Negative terms are also almost impossible to interpret, except from "white","fellow","struggle", "world" other terms are not meaningful on theirown.

# Question 2: Who are the 5 people Obama's inaugural address is most
#close to in 2013?
##"2009-Obama"     "1997-Clinton"   "1993-Clinton"   "2001-Bush"      "2021-Biden.txt"

#What about Trump in 2017?

## "1945-Roosevelt" "1993-Clinton"   "1969-Nixon"     "2021-Biden.txt" "1997-Clinton"

library(text2vec)

# function computes cosine similarity between query and all documents and returns N most similar
nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # query is always the nearest neighbor hence dropped
}

# apply to document retrieval
nearest_neighbors(query = "2013-Obama", low_dim_space = inaugural_pca$x, N = 5, norm = "l2")
nearest_neighbors(query = "2017-Trump", low_dim_space = inaugural_pca$x, N = 5, norm = "l2")


