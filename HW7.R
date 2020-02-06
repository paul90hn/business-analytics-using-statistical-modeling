setwd("C:/Users/USER/Documents/BASM")

A psychological research paper has published an experiment to see if emotion affects our perception of color 
on different color-axes. Participants viewed one of two videos: either the famous death scene in the Lion King,
or a video of a desktop screensaver - let's call these the sad and neutral conditions, respectively. 

Afterwards, 
participants performed a color discrimination task requiring them to classify colors along the red-green color-axis 
and  blue-yellow color axis. The dependent measures are the accuracy in each of the color conditions (red-green and
blue-yellow). The researchers found some potential difference in the blue-yellow accuracy of sad versus neutral
participants, but not so for red-green accuracy. Let's examine their findings more carefully. You will find the
experiment data in the file study2Data.csv on Canvas.


#sad <- data0[which(data0[, "Emotion_Condition"] == "Sadness"), ]
#neutral <- data0[which(data0[, "Emotion_Condition"] == "Neutral"), ]
#sad_diff <- data.frame(sad$Subject, ( sad$BY_ACC - sad$RG_ACC))
#colnames(sad_diff) <- c("Subject", "Difference")
#neutral_diff <- data.frame(neutral$Subject, ( neutral$BY_ACC - neutral$RG_ACC))
#colnames(neutral_diff) <- c("Subject", "Difference")




a) Visualize the differences between blue-yellow accuracy (BY_ACC) and red-green accuracy (RG_ACC) for both 
the sad and neutral viewers (Emotion_Condition). You are free to choose any visualization method you wish, 
but only report the most useful or interesting visualizations and any first impressions.




data0 <- read.csv("study2data.csv")
data1 <- data.frame(data0$Subject, data0$Emotion_Condition, (data0$BY_ACC-data0$RG_ACC))
colnames(data1) <- c("Subject", "Emotion", "Difference")

boxplot(data1$Difference ~ data1$Emotion, horizontal = TRUE, main = "Difference of perception per Emotion")



##############
Run a t-test (traditional or bootstrapped) to check if their is a significant difference in blue-yellow
accuracy between sad and neutral participants at 95% confidence.

sad <- data0[which(data0[, "Emotion_Condition"] == "Sadness"), ]
neutral <- data0[which(data0[, "Emotion_Condition"] == "Neutral"), ]


their is a significant difference in blue-yellow
accuracy between sad and neutral participants


#hnull <- sad mean = Neutral mean

#sample mean

sad_mean_BY <- mean(sad$BY_ACC)

#hypotezied mean




bootstrap_null_alt <- function(sample0, hyp_mean) {
  resample <- sample(sample0, length(sample0), replace = TRUE)
  resample_se <- sd(resample) / sqrt(length(resample))
  
  t_stat_alt <- (mean(resample) - hyp_mean) / resample_se
  t_stat_null <- (mean(resample)- mean(sample0)) / resample_se
  
  return(c(t_stat_alt, t_stat_null))
}


hyp_mean <- mean(neutral$BY_ACC)

boot_t_stats <- replicate(1000, bootstrap_null_alt(sad$BY_ACC, hyp_mean))
t_alt <- boot_t_stats[1,]
t_null <- boot_t_stats[2,]
CI_95 <- quantile(t_null, probs = c(0.025, 0.975))

plot(density(t_null), lwd = 2, main = "Bootstrapped differences of means", xlim= c(-7,5))
lines(density(t_alt), lty= "dashed", lwd=2, col= "coral2")
abline(v=mean(t_alt),  lty= "dashed", lwd=2, col= "coral2")
abline(v=CI_95, lty="dashed")




Run a t-test (traditional or bootstrapped) to check if their is a significant difference in red-green 
accuracy between sad and neutral participants at 95% confidence.

#hnull <- sad mean = Neutral mean

#sample mean

sad_mean_RG <- mean(sad$RG_ACC)

#hypotezied mean


hyp_mean <- mean(neutral$RG_ACC)

boot_t_stats <- replicate(1000, bootstrap_null_alt(sad$RG_ACC, hyp_mean))
t_alt <- boot_t_stats[1,]
t_null <- boot_t_stats[2,]
CI_95 <- quantile(t_null, probs = c(0.025, 0.975))

plot(density(t_null), lwd = 2, main = "Bootstrapped differences of means", xlim= c(-7,5))
lines(density(t_alt), lty= "dashed", lwd=2, col= "coral2")
abline(v=mean(t_alt),  lty= "dashed", lwd=2, col= "coral2")
abline(v=CI_95, lty="dashed")


(not graded)  Do the above t-tests support a claim that there is an interaction between emotion and
color axis?  (i.e., does people's accuracy of color perception along different color-axes depend on
their emotion? Here, accuracy is an outcome variable, while color-axis and emotion are
idependent factors)

ABove t test support the claim that there is a statistical difference betweed the means of sad and neutral emotions


for (i in 1:length(data2$Subject)){
  if (data2$RG_ACC[i] < (mean(data2$RG_ACC) - sd(data2$RG_ACC))){
    data2$RG_CAT[i] <- "Below"
  }else if (data2$RG_ACC[i] > (mean(data2$RG_ACC) + sd(data2$RG_ACC))){
    data2$RG_CAT[i] <- "Above"
  }else {
    data2$RG_CAT[i] <- "Average" 
  }
}

for (i in 1:length(data2$Subject)){
  if (data2$BY_ACC[i] < (mean(data2$BY_ACC) - sd(data2$BY_ACC))){
    data2$BY_CAT[i] <- "Below" 
  }else if (data2$BY_ACC[i] > (mean(data2$BY_ACC) + sd(data2$BY_ACC))){
    data2$BY_CAT[i] <- "Above"
  }else {
    data2$BY_CAT[i] <- "Average" 
  }
}




Run a factorial design ANOVA where color perception accuracy is determined by emotion (sad vs. neutral),
color-axis (RG vs. BY), and the interaction of emotion and color-axis. Note that you will likely have to 
reshape the data and create new columns -- please ask/discuss/share your data shaping strategy online.
Are any of these three factors (emotion/color-axis/interaction) possibly influencing color perception
accuracy at any meaningful level of confidence?


b_y <- data.frame( "BY ",   data0$Subject, data0$Emotion_Condition, data0$BY_ACC )
colnames(b_y ) <- c("Axis", "Subject", "Emotion", "Accuracy")
r_g <- data.frame( "RG ",   data0$Subject, data0$Emotion_Condition, data0$RG_ACC )
colnames(r_g ) <- c("Axis", "Subject", "Emotion", "Accuracy")
data2 <- rbind.data.frame(b_y, r_g)

factorial_anova <- aov(data2$accuracy ~ data2$AXIS + data2$Emotion_Condition + data2$AXIS:data2$Emotion_Condition, 
                     data=data2)
summary(factorial_anova)





