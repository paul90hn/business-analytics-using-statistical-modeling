Question 2) We're going to take a look back at the heady days of car manufacturing, when American, 
Japanese, and European cars competed to rule the world. Take a look at a data set (auto-data.txt).
We are interested in explaining what kind of cars have higher fuel efficiency (measured by mpg).

mpg:           miles-per-gallon (dependent variable)
cylinders:     cylinders in engine
displacement:  size of engine
horsepower:    power of engine
weight:        weight of car
acceleration:  acceleration ability of car
model_year:    year model was released
origin:        place car was designed (1: USA, 2: Europe, 3: Japan)
car_name:      make and model names

This data set has some missing values ('?' in data set), and it lacks a header row with variable names:
  
setwd("C:/Users/USER/Documents/BASM")
  
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")


Let's first try exploring this data and problem:
1)  Visualize the data in any way you feel relevant (report only relevant/interesting ones)


boxplot(mpg ~ origin, auto, horizontal= TRUE)


2)Report a correlation table of all variables, rounding to two decimal places
(in the cor(...) function, set use="pairwise.complete.obs" to handle missing values)

cor(auto[,-9], use="pairwise.complete.obs" )


3)From the visualizations and correlations, which variables seem to relate to mpg?
4)Which relationships might not be linear? (don't worry about linearity for rest of this HW)
5)Are any of the independent variables highly correlated (r > 0.7) with others?

Let's try an ordinary linear regression, where mpg is dependent upon all other suitable variables 
(Note: origin is categorical with three levels, so use factor(origin) in lm(...)  to split it into 
  two dummy variables) 


    regression <- lm(mpg ~ cylinders + displacement+ horsepower+ weight+ acceleration
                   + model_year + factor(origin), data = auto[,-9])
  
  round(summary(regression))

1) Which factors have a 'significant' effect on mpg at 1% significance?
2) Looking at the coefficients, is it possible to determine which independent variables are the
most effective at increasing mpg? If so, which ones, and if not, why not? (hint: units!)

Let's try to resolve some of the issues with our regression model above.
1)Create fully standardized regression results: are these values easier to interpret?
(note: consider if you should standardize origin)

auto_std <- cbind(data.frame(scale(auto[,-9:-8]), auto$origin))

regression_std <- lm(mpg ~ cylinders + displacement+ horsepower+ weight+ acceleration
                 + model_year + factor(auto.origin), data = auto_std)
summary(regression_std)

2)Regress mpg over each nonsignificant independent variable, individually.
Which ones are significant if we regress mpg over them individually?
2)Plot the density of the residuals: are they normally distributed and centered around zero?
(hint: get the residuals of a linear model, e.g. regr <- lm(...), using regr$residuals
 


plot(density(as.numeric(regression_std$residuals)), main= "Residuals")










regression_displacement <- lm(mpg ~  displacement, data = auto[,-9])
summary(regression_displacement)

regression_weight <- lm(mpg ~ weight, data = auto[,-9])
summary(regression_weight)

regression_model_year <- lm(mpg ~  model_year, data = auto[,-9])
summary(regression_model_year)

regression_origin <- lm(mpg ~ factor(origin), data = auto[,-9])
summary(regression_origin)




















