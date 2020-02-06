setwd("C:/Users/USER/Documents/BASM")

library("car")

cars <- na.omit(read.table("auto-data.txt", header=FALSE, na.strings = "?"))
colnames(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")

cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), log(horsepower),
                                  log(weight), log(acceleration), model_year, origin))
colnames(cars_log) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin")




### a.	Create a new data.frame of the four log-transformed independent variables with multicollinearity -----
     #1.	Give this smaller data frame an appropriate name (think what they jointly mean)

fuel_efficiency <- with( cars_log, data.frame(cylinders, displacement, horsepower, weight))

    #2.	Check the correlation table of these four variables to confirm they are indeed collinear

cor(fuel_efficiency)

#### b.	Let's analyze the principal components of the four collinear variables ----
    #1.	How many principal components are needed to summarize these four variables? 
    #   (use the eigenvalues and scree plot criteria we discussed in class).
    
eigen(cor(fuel_efficiency))

pc <- prcomp(fuel_efficiency)
screeplot(pc)


    #2.	How much variance of the four variables is explained by their first principal component? 
    #   (a summary of the pca reports it, but try computing this from the eigenvalues alone)



eigen(cor(fuel_efficiency))$values[1] / sum(eigen(cor(fuel_efficiency))$values)





    #3.	Looking at the values and valence (positive/negative) of the first principal component's eigenvector, 
    #    what would you call the information captured by this component? (i.e., think what the first principal component means)


###  c)	Let's reduce the four collinear variables into one new variable! -----
   #   1.	Store the scores of the first principal component as a new column of cars_log
  #          cars_log$new_column_name <- ...scores of the PC...


cars_log$PC1 <- pc$x[,1]

biplot(pc)

  #   2.	Name this column appropriately based on the meaning of this first principal component.


colnames(cars_log)[9] <- "VarDirection"

#####    d.	Let's revisit our regression analysis on cars_log:  ----
#       (HINT: to compare variables across models, it helps to conduct fully standardized regression)


#  1.	Regress mpg over weight, acceleration, model_year and origin

scale_cars_log <- with(cars_log, data.frame(scale(mpg), scale(cylinders), scale(displacement), scale(horsepower),
                                            scale(weight), scale(acceleration), model_year, origin, scale(VarDirection)))
colnames(scale_cars_log) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                               "acceleration", "model_year", "origin", "VarDirection")




mpg_regression <- lm(mpg ~ weight + acceleration + model_year + factor(origin), data = scale_cars_log)
summary(mpg_regression)




#  2.	Repeat the regression, but replace weight with the factor scores of the 1st principal 
     #component of our collinear independent variables

mpg_regression_pc1 <- lm(mpg ~ VarDirection + acceleration + model_year + factor(origin), data = scale_cars_log)
summary(mpg_regression_pc1)


#  3.	Use VIF scores to check whether the either regression suffers from multicollinearity

vif(mpg_regression)
vif(mpg_regression_pc1)

# 4.	(ungraded) Comparing the two regressions, how has the story changed?

#### ----

