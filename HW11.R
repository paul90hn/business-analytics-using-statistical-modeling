setwd("C:/Users/USER/Documents/BASM")


auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")

cars_log <- with(auto, data.frame(log(mpg), log(cylinders), log(displacement),
                  log(horsepower), log(weight), log(acceleration), model_year, origin))
colnames(cars_log) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin")


#### question 1 ----
log_regression <- lm(mpg ~ cylinders + displacement+ horsepower+ weight+acceleration
                 + model_year + factor(origin), data = cars_log)

summary(log_regression)

regr_wt <- lm(mpg~ weight, data = auto[,-9])
summary(regr_wt)
regr_wt_log <-  lm(mpg ~ weight , data = cars_log)
summary(regr_wt_log)

plot(mpg ~ weight, data= auto)
abline(regr_wt)

par(mfrow=c(1,1))
plot(mpg ~ weight, data= cars_log, main= "Mpg ~ Weight regression")
abline(regr_wt_log, col= "Blue")




par(mfrow=c(1,1))
plot(cars_log$weight, resid(regr_wt_log), pch = 1)


par(mfrow=c(2,1))
plot(density(regr_wt$residuals), main= "Raw Data Regression")
plot(density(regr_wt_log$residuals), main= "Log transformend Regression")


# Function for single resampled regression line
plot(cars_log$weight, cars_log$mpg, col=NA, pch=19)

boot_regr <- function(model, dataset) {
    boot_index <- sample(1:nrow(dataset), replace=TRUE)
    data_boot <- dataset[boot_index,]
    regr_boot <- lm(model, data= data_boot)
    abline(regr_boot, lwd=1, col= rgb(0.7, 0.7, 0.7, 0.5))
    return( regr_boot$coefficients)
  }
# Bootstratping for confidence interval
coeffs <- replicate(3000, boot_regr(mpg ~ weight, cars_log))
# Plot points and regression line
points(cars_log$weight, cars_log$mpg, col = "blue", pch= 19)
abline(a=mean(coeffs[1,]), b=mean(coeffs[2,]), lwd=2)

# Confidnce interval values
quantile(coeffs[2,], c(0.025, 0.975))

#Plot confidence interval of coefficient
plot(density(coeffs[2,]), xlim=c(-1, 0),
     col="cornflowerblue", lwd=2)
abline(v=quantile(coeffs[2,], c(0.025, 0.975)))


2.	Verify your results with a confidence interval using traditional statistics




weight_log <- cars_log$weight
weight_regr_log <- lm(cars_log$mpg ~ cars_log$weight)
new_weights <- data.frame(weight_log=c(8.5, 8.9))
predict(weight_regr_log, new_weights, interval = "confidence")
predict(weight_regr_log, new_weights, interval = "predict")

weights_seq <- seq(min(cars_log$weight), max(cars_log$weight))
new_weights = data.frame(cars_log$weight, weights_seq)
mpg_conf <- predict(weight_regr_log, new_weights, interval = "confidence")
plot(cars_log$weight, cars_log$mpg)
lines(new_weights$cars_log.weight, mpg_conf[, "lwr"], col= "red")
lines(new_weights$cars_log.weight, mpg_conf[, "upr"], col= "red")





####question 2 ----

regr_weight <- lm(weight ~ cylinders + displacement + horsepower +
                 mpg + acceleration + model_year +
                 factor(origin),  data=cars_log)

plot( cars_log$weight, cars_log$mpg)

r2_weight <- summary(regr_weight)$r.squared 

vif_weight <- 1/ (1- r2_weight)


vif(regr_weight)

regr_weight0 <- lm(weight ~  mpg + acceleration + model_year +
                    factor(origin),  data=cars_log)
vif(regr_weight0)
summary(regr_weight0)
summary(regr_weight)



### question 3 ----

Might the relationship of weight on mpg be different for cars from different origins? 
Let's try visualizing this. First, plot all the weights, using different colors and symbols 
for the three origins:
  
  origin_colors <- c("blue", "darkgreen", "red")
  with(cars_log, plot(cars_log$weight, cars_log$mpg, pch=origin, col=origin_colors[origin]))

  cars_us <- subset(cars_log, origin==1)
  cars_eu <- subset(cars_log, origin==2)
  cars_jp <- subset(cars_log, origin==3)
  wt_regr_us <- lm(mpg ~ weight, data=cars_us)
  wt_regr_eu <- lm(mpg ~ weight, data=cars_eu)
  wt_regr_jp <- lm(mpg ~ weight, data=cars_jp)
 
  abline(wt_regr_us, col=origin_colors[1], lwd=2)
  abline(wt_regr_eu, col=origin_colors[2], lwd=2)
  abline(wt_regr_jp, col=origin_colors[3], lwd=2)
  
    