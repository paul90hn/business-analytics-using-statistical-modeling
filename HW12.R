

setwd("C:/Users/USER/Documents/BASM")


  
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")

cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), log(horsepower),
                                  log(weight), log(acceleration), model_year, origin))


light_cars <- subset(cars_log, log.weight. < mean(log.weight.))
heavy_cars <- subset(cars_log, log.weight. > mean(log.weight.))


weight_colors <- c("blue", "red")
with( cars_log, plot( log.acceleration., log.mpg., col=weight_colors[log.weight.], pch=log.weight.))
points( light_cars$log.acceleration., light_cars$log.mpg., col= weight_colors[1])
points(heavy_cars$log.acceleration., heavy_cars$log.mpg., col= weight_colors[2])

light_cars_lm <- lm( log.mpg. ~ log.acceleration., data = light_cars )
abline(light_cars_lm, lty= "dashed", col= weight_colors[1])
heavy_cars_lm <- lm( log.mpg. ~ log.acceleration., data = heavy_cars )
abline(heavy_cars_lm, lty= "dotted", col= weight_colors[2])



light_cars_reg <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin ,data = light_cars)
summary(light_cars_reg)

heavy_cars_reg <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin ,data = heavy_cars)
summary(heavy_cars_reg)


#### QUESTION 2 ----



no_interaction_reg <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin ,data = cars_log)
summary(no_interaction_reg)



interaction_reg <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin + log.weight.*log.acceleration. ,data = cars_log)
summary(interaction_reg)

3.	Report a regression with a mean-centered interaction term.


mpg_mc <- scale(cars_log$log.mpg. , center = TRUE, scale = FALSE)
weight_mc <-  scale(cars_log$log.weight., center = TRUE, scale = FALSE)
acceleration_mc <-  scale(cars_log$log.acceleration., center = TRUE, scale = FALSE)

mc_interaction_reg <- lm(mpg_mc ~ weight_mc + acceleration_mc + cars_log$model_year + cars_log$origin + weight_mc*acceleration_mc)
summary(mc_interaction_reg)


3.	Report a regression with an orthogonalized interaction term


weight_x_acceleration <- cars_log$log.weight. * cars_log$log.acceleration.
weight_accelerationn_regr <- lm(weight_x_acceleration ~ cars_log$log.weight. + cars_log$log.acceleration.)

cor(weight_accelerationn_regr$residuals, cars_log$log.weight.)

cor(weight_accelerationn_regr$residuals, cars_log$log.acceleration.)


summary(with(cars_log, lm( log.mpg. ~ log.weight. + log.acceleration. + weight_accelerationn_regr$residuals)))


#### question 3 ----



###	Regress log.mpg. over log.cylinders. and all control variables 
###(does cylinders have a significant direct effect on mpg when weight is not considered?)
### Acceleration, model_year, and origin are kept as control variables (see gray variables in diagram).


mediation_reg  <- lm(log.mpg. ~ log.cylinders. + log.acceleration. + model_year + origin ,data = cars_log)
summary(mediation_reg)

### Regress log.weight. over log.cylinders. only  (does cylinders have a significant direct effect on weight itself)

weight_cylinders_reg  <- lm(log.weight. ~ log.cylinders. ,data = cars_log)
summary(weight_cylinders_reg)



### Regress log.mpg. over log.weight. and all control variables  (does weight have a direct effect on mpg?)

b_mediation_reg  <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin ,data = cars_log)
summary(b_mediation_reg)



###	Regress log.mpg. on log.weight., log.cylinders., and all control variables  
## (does cylinders have a significant direct effect on mpg when weight is also considered?) 
## If the coefficient of cylinders in step (iv) is not significant, then we have "full mediation"

c_mediation_reg  <- lm(log.mpg. ~ log.weight. + log.cylinders. + log.acceleration. + model_year + origin ,data = cars_log)
summary(c_mediation_reg)






#	Regress log.weight. over log.cylinders. only  (does cylinders have a significant direct effect on weight itself)

# Regress log.mpg. over log.weight. and all control variables  (does weight have a direct effect on mpg?)

weight_cylinders_reg$coefficients[2] * b_mediation_reg$coefficients[2]

boot_mediation <- function(model1, model2, dataset) {
  boot_index <- sample(1:nrow(dataset), replace=TRUE)
  data_boot <- dataset[boot_index, ]
  regr1 <- lm(model1, data_boot)
  regr2 <- lm(model2, data_boot)
  return(regr1$coefficients[2] * regr2$coefficients[2])
}
set.seed(42)
cylindersxmpg <- replicate(2000, boot_mediation(weight_cylinders_reg, b_mediation_reg, cars_log))
quantile(cylindersxmpg, probs=c(0.025, 0.975))


plot(density(cylindersxmpg))
abline(v=quantile(cylindersxmpg, probs=c(0.025, 0.975)))



#### ----