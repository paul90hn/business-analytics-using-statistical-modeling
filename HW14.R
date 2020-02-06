setwd("C:/Users/USER/Documents/BASM")


library("psych")

#### question 1 ----

#  a.	Report your earlier findings from applying the "eigenvalue > 1" 
#     and screeplot criteria to the security dataset.

data <- read.csv("security_questions.csv",stringsAsFactors = FALSE)
pca <- prcomp(data, scale. = TRUE)

eigen(cor(data))$values
screeplot(pca, type = "line")



#  b.	Perform a parallel analysis to find out how many principal 
#     components have higher eigenvalues than their counterparts in random datasets  
#     of the same dimensions as the security dataset.

#noise <- data.frame(replicate(18, rnorm(405)))
#eigen(cor(noise))$values
 
sim_noise <- function(n, p) {
  noise <- data.frame(replicate(p, rnorm(n)))
  return( eigen(cor(noise))$values)
}

set.seed(42)
evalues_noise <- replicate(100, sim_noise(405, 18))
evalues_mean <- apply(evalues_noise, 1, mean)

screeplot(pca, type= "lines")
lines(evalues_mean, type = "b")
abline( h=1, lty= "dotted")

#### Question 2 ----

#Earlier, we examined the eigenvectors of the security dataset. This time, let's examine 
#loadings of our principal components (use the principal() method from the psych package)

fca <- principal(data, nfactors = 3, rotate = "none", scores = TRUE)
fca


#   a)	Looking at the loadings of the first 3 principal components, to which components does 
#       each item seem to belong?

sum(fca$loadings[, "PC1"]^2)  #SSloading PC1
sum(fca$loadings[, "PC2"]^2)
sum(fca$loadings[, "PC3"]^2)


#### question 3 ----

#To improve interpretability of loadings, let's rotate the our principal component
#axes to get rotated components (extract and rotate only three principal components)

# a)	Individually, does each rotated component explain the same, or different, amount 
#     of variance than the three principal components?

principal(data, nfactors = 3, rotate = "none", scores = TRUE)

prcomp(data)
