setwd("C:/Users/USER/Documents/BASM")


####### EJERCICIO 1 #########################################


#####  A 

bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]

hours <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
plot(density(minday), main="Minute (of the day) of first ever booking",
     col="blue", lwd=2)

mean(minday)
standardError <- sd(minday)/sqrt(length(minday))
conf.Int.95 <- c((mean(minday) - 1.96*standardError), (mean(minday) + 1.96*standardError))


num_bootstraps <- 2000
resamples <- replicate(num_bootstraps, sample(minday, length(minday), replace = TRUE))

plot(density(minday), lwd = 0, main = " Population vs bootstrapped Samples")
plot_resample_density <- function( sample_i) {
  lines(density(sample_i), col =rgb(0.0, 0.4, 0.0, 0.01))
  return(mean(sample_i))
}
sample_means <- apply(resamples, 2, FUN=plot_resample_density)
lines(density(minday))
quantile(sample_means, probs = c(0.025, 0.975))

######## B


pop_median <- median(minday)
pop_median



sample_means_median <- median(sample_means)

plot(density(minday), main = "Confidence Interval of medians")
plot_resample_median <- function(sample_i) {
  abline(v=mean(sample_i), col=rgb(0.0, 0.4, 0.0, 0.01))
}

sample_median <- apply(resamples, 2, FUN=plot_resample_median)
abline(v=sample_means_median, lwd=2)
abline(v=pop_median, lty = "dashed", col="blue")
quantile(sample_means_median, probs = c(0.025, 0.975))




######### EJERCICIO 2 #########

##### A

d1 <- rnorm( 2000, mean = 940, sd= 190)
rnorm_std <- sapply(d1, function(x) x - mean(d1))

plot(density(rnorm_std), main="Normilized Standard distribution", lwd=2)

round(mean(rnorm_std), digits = 4)
sd(rnorm_std)



#### B 

minday_std <- sapply(d1, function(x) x - mean(minday))

plot(density(minday_std), main="Minday Normilized Standard distribution")
abline(v= mean(minday_std), col="blue")
abline(v= mean(minday_std)+sd(minday_std), lty = "dashed", col="blue" )
abline(v= mean(minday_std)-sd(minday_std), lty = "dashed", col="blue" )

mean(minday_std)
sd(minday_std)


par(mfrow=c(2,1))
plot(density(minday), main = "Minday", xlim = c(550, 1450))
abline(v= mean(minday), col="blue")
abline(v= mean(minday)+sd(minday), lty = "dashed", col="blue" )
abline(v= mean(minday)-sd(minday), lty = "dashed", col="blue" )
plot(density(minday_std), main="Minday Normilized Standard distribution")
abline(v= mean(minday_std), col="blue")
abline(v= mean(minday_std)+sd(minday_std), lty = "dashed", col="blue" )
abline(v= mean(minday_std)-sd(minday_std), lty = "dashed", col="blue" )