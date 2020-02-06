setwd("C:/Users/USER/Documents/BASM")


pnorm(-3.7)
x <- rnorm(n=2500000)
sum(x < -3.7, na.rm=TRUE)


data <- read.csv("tires.csv")
lifetime <- data$lifetime_km

#### A

hypothesis_mean <- 90000
sample_mean <- mean(lifetime)
sample_sd <- sd(lifetime)
n <- length(lifetime)
se <- sample_sd / sqrt(n) 


ci <- c((sample_mean-1.96*se), (sample_mean+1.96*se))



test <- t.test(lifetime, mu= hypothesis_mean, conf.level = 0.95 )
test$statistic    
test$statistic < -1.96


#### B

#1
num_boots <- 2000

sample_statistic <- function(stat_function, sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return(stat_function(resample))
}
set.seed(42)
sample_means <- replicate(num_boots,
                          sample_statistic(mean, lifetime))
mean(sample_means)
quantile(sample_means, probs = c(0.025, 0.975))



#2

boot_mean_diffs <- function(sample0, hypothesis_mean) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return( mean(resample) - hypothesis_mean )
}
mean_diffs <- replicate(num_boots,
                        boot_mean_diffs(lifetime, hypothesis_mean))
mean(mean_diffs)
sd(mean_diffs)
diff_ci_95 <- quantile(mean_diffs, probs=c(0.025, 0.975))





#3
boot_t_stat <- function(sample0, hypothesis_mean) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  diff <- mean(resample) - hypothesis_mean
  resample_se <- sd(resample)/sqrt(length(resample))
  return( diff/resample_se )
}
num_boots <- 200
t_boots <- replicate(num_boots, boot_t_stat(lifetime, hypothesis_mean))

mean(t_boots)
diff_ci_95 <- quantile(t_boots, probs=c(0.025, 0.975))

#4
par(mfrow = c(3,1))
plot(density(sample_means), col="blue", lwd=2,
     main="Sample means")
diff_ci_95 <- quantile(sample_means, probs=c(0.025, 0.975))
abline(v=mean(sample_means))
abline(v=diff_ci_95, lty="dashed")


plot(density(mean_diffs), col="blue", xlim= c(-7500, 500), lwd=2,
     main="sampling mean differences with hypothesized mean")
diff_ci_95 <- quantile(mean_diffs, probs=c(0.025, 0.975))
abline(v=mean(mean_diffs))
abline(v=diff_ci_95, lty="dashed")


plot(density(t_boots), col="blue", xlim= c(-10, 1), lwd=2,
     main="T statistic")
diff_ci_95 <- quantile(t_boots, probs=c(0.025, 0.975))
abline(v=mean(t_boots))
abline(v=diff_ci_95, lty="dashed")