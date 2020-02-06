setwd("C:/Users/USER/Documents/BASM")

#Question 1)  Let's compare the mean load times of Alentus versus HostMonster using their two samples
#(see class notes on difference of means between two samples; data in page_loads.csv)

#CLAIM: Imagine Alentus claims that their mean load time is actually quite comparable to that of HostMonster,
#if we remove the major outliers from the Alentus load times.

load_times <- read.csv("page_loads.csv", header = TRUE)

alentus <- load_times$Alentus
host_monster <- load_times$HostMonster[!is.na(load_times$HostMonster)]



#Use a boxplot to remove the major outliers of Alentus' load times. Then, use the appropriate form of the t.test
#function to test the difference between the mean of Alentus and the mean of HostMonster load times (assume the samples 
#come from populations with different variances). From the output of t.test:


alentus_boxplot <- boxplot(alentus, horizontal = TRUE)
alentus_boxplot$out

alentus_omit_outliers <- alentus[!(alentus %in% alentus_boxplot$out)]
boxplot(alentus_omit_outliers, horizontal = TRUE)$out

mean(alentus_omit_outliers)
mean(host_monster)
sd(alentus_omit_outliers)
sd(host_monster)


# -What is the null and alternative hypotheses in this case?

        #Null Hypothesis, Alentus withour outliers mean equals Hostmonster's

# -What is the 95% CI of the difference of the two providers' means?

t.test(alentus_omit_outliers, host_monster, var.equal = FALSE)$conf.int

#-Based on the 95% CI, the t-value, and the p-value, would you reject the null hypothesis or not?

test <- t.test(alentus_omit_outliers, y= host_monster, var.equal = FALSE)


plot(density(alentus_omit_outliers), lwd="2", ylim=c(0, 0.7))
abline(v=mean(alentus_omit_outliers))
lines(density(host_monster), lty="dashed")
abline(v= test$conf.int, lty="dashed")






#Let's try this using bootstrapping: Estimate bootstrapped alternative values of t using the same t.test function 
#as above to compare bootstrapped samples of both providers; Estimate bootstrapped null values of t by using the 
#t.test function above to compare bootstrapped values of Alentus against the original Alentus sample; 
#also estimate the difference between means of both bootstrapped samples.
#       -What is the bootstrapped 95% CI of the difference of means?



bootstrap_null_alt <- function(sample0, hyp_mean) {
      resample <- sample(sample0, length(sample0), replace = TRUE)
      resample_se <- sd(resample) / sqrt(length(resample))
      
      t_stat_alt <- (mean(resample) - hyp_mean) / resample_se
      t_stat_null <- (mean(resample)- mean(sample0)) / resample_se
      
      return(c(t_stat_alt, t_stat_null))
}


hyp_mean <- mean(host_monster)

boot_t_stats <- replicate(1000, bootstrap_null_alt(alentus, hyp_mean))
t_alt <- boot_t_stats[1,]
t_null <- boot_t_stats[2,]
CI_95 <- quantile(t_null, probs = c(0.025, 0.975))

plot(density(t_alt),xlim = c(-5,5), lwd = 2, main = "Bootstrapped differences of means")
abline(v=mean(t_alt), lwd= 2)
lines(density(t_null), lty= "dashed")
abline(v=CI_95, lty="dashed")


#       -Plot a distribution of the bootstrapped null t-values and bootstrapped alternative t-values, adding vertical lines 
#       -for the 95% CI of the alternative distribution (adjust x- and y- axis limits accordingly).
#       -Based on these bootstrapped results, should we reject the null hypothesis?


#Question 2) This time, let's test a claim for which we do not know a test statistic
#CLAIM: Alentus claims that, with its major outliers removed, its median load time is
#in fact significantly smaller than the median load time of HostMonster (with 95% confidence)!
  
#  Here, we really don't know what test statistic to use to compare medians of two samples, 
#so let's just bootstrap the confidence interval:
#           -First, confirm that the median load time of Alentus (without outliers) is smaller than for HostMonster.

 median(alentus_omit_outliers)
 median(host_monster)
 median(alentus_omit_outliers) < median(host_monster)


 
 #      -Bootstrap the difference between the median of Alentus (without major outliers) and the median for HostMonster
 #         Also bootstrap the 'null' difference (compare the median of bootstrapped samples of Alentus against the 
 #         median of the original Alentus sample).
 
 
 bootstrap_median <- function(sample0, sample1) {
   resample_alentusOmit <- sample(sample0, length(sample0), replace = TRUE)
   resample_median_alentusOmit <- median(resample_alentusOmit)
   resample_hostMonster <- sample(sample1, length(sample1), replace = TRUE)
   resample_median_hostMonster <- median(resample_hostMonster)
   
   
   median_difference <- resample_median_alentusOmit - resample_median_hostMonster
   null_difference   <- resample_median_alentusOmit - median(alentus)
   
   return(c(median_difference, null_difference))
 }
 
 set.seed(42)
 
 boot_medians <- replicate(1000, bootstrap_median(alentus_omit_outliers, host_monster))
 
 
 alt_differences <- boot_medians[1,]
 null_differences  <-  boot_medians[2,]
 


 
 #      -What is the average difference between medians of the two service providers?
 
 mean(alt_differences)
 
 #      -What is the 95% CI of the difference between the medians of the two service providers?
 #        (consider this time, where the 5% 'rejection zone' of the distribution of differences 
 #        will be: will it be on both sides or only on one side?)
 

  CI_95 <- quantile(null_differences, probs = c(0.05))

 
 #      -Plot the distributions of the bootstrapped alternative and null differences between medians and use a vertical 
 #        dashed lines to show us the 5% 'rejection zone'


 plot(density(alt_differences), lwd = 2, ylim=c(0,3), main = "Mean Differences")
 abline(v=mean(alt_differences))
 lines(density(null_differences ), lty= "dashed")
 abline(v= CI_95 , col="blue", lty="dashed")
 
 #Question 3) Let's take a look back at some data from a marketing survey of mobile users.{
#You may load the data(from Data_0630.txt) using:

 survey <- read.csv("Data_0630.txt", sep="\t", header = TRUE)
 
#Some of the rows are responses by iPhone users, and the others are by Android users.
#In particular, we are interested in responses by iPhone versus Samsung users to a brand identification question:
#"[brand] users share the same values as I do"  - [brand] is the respondent's phone brand (iPhone/Samsung)
#Responses are scored from 1-7 where 1 is "Strongly disagree", 4 is "Neutral", and 7 is "Strongly Agree"

#You may separate the data about brand identification by each set of users:
 
iphone <- survey[survey$X.current_phone.==1,]$X.Brand_Identification.1.
samsung <- survey[survey$X.current_phone.==2,]$X.Brand_Identification.1.

#We find that the means of identification scores between users of the two phone brands are very similar. 
#So we wish to test whether one brand's variance of identification scores is higher than the other brand's variance of identification scores.
 
mean(iphone)
mean(samsung)
 
#  - What is the null and alternative hypotheses in this case? 
#    (Start by identifying which brand has the higher variance)

var(iphone)
var(samsung)

#       Iphone variance is same as Samsung's


#Let's try traditional statistical methods first:
#      - What is the F-statistic of the ratio of variances?


f_stat <- var(iphone) / var(samsung)
f_stat


#      - What is the cut-off value of F, such that we want to reject the 5% most extreme F-values?

var.test(iphone, samsung, alternative = "greater", conf.level = 0.95)
qf(p=0.95, df1 = length(iphone)-1, df2 = length(samsung)-1 )

#         (this is another way of saying we want 95% confidence)
#         Use the qf() function in R to determine the cutoff.
#      - Can we reject the null hypothesi






#Let's try bootstrapping this time:
#  Create bootstrapped values of the F-statistic, for both null and alternative hypotheses.



set.seed(43)
sd_identificationScores_tests <- function(larger_sd_sample, smaller_Sd_sample) {
              resample_larger_sd <- sample(larger_sd_sample, length(larger_sd_sample), replace = TRUE)
              resample_smaller_sd <- sample(smaller_Sd_sample, length(smaller_Sd_sample), replace = TRUE)
              f_alt <- var(resample_larger_sd) / var(resample_smaller_sd)
              f_null <- var(resample_larger_sd) / var(larger_sd_sample)
              return(c(f_alt, f_null))
}

f_stats <- replicate(10000, sd_identificationScores_tests(iphone, samsung))
f_alts <- f_stats[1,]
f_nulls <- f_stats [2,]


#           -What is the 95% cutoff value according to the bootstrapped null values of F?

quantile(f_nulls, probs = 0.95)



#           -What is the median bootstrapped F-value for the alternative hypothesis?

median(f_alts)


#           -Plot a visualization of the null and alternative distributions of the bootstrapped F-statistic, 
#               with vertical lines at the cutoff value of F nulls, and at median F-values for the alternative.

plot(density(f_alts), col = "cornflowerblue", main = "Null and Alt distributions of F", lwd = 2, ylim= c(0,4))
abline(v=median(f_alts), col = "cornflowerblue")
lines(density(f_nulls), col="coral3", lwd=2, lty="dotted")
abline(v=quantile(f_nulls, probs = 0.95), lty="dotted", col="coral3")
      

#           -What do the bootstrap results suggest about the null hypothesis?

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

