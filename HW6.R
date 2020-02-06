#[INTEND.0]:   I intend to share the information I saw with others.
#(answered on 7 point scale:  1=strongly disagree;  4=neutral;  7=strongly agree)
#You may find the researcher's data in four CSV files named: pls-media[1-4].csv
#(note: the number in the filename corresponds to the type of media listed above)


#Question 1)  Let's describe and visualize the data:
  
#  a) What are the means of viewers intentions to share (INTEND.0) for each media type? (report four means)


setwd("C:/Users/USER/Documents/BASM")

animation_audio_vid <- read.csv("health-media1.csv")
pictures_audio_vid <- read.csv("health-media2.csv")
pictures_text_web <- read.csv("health-media3.csv")
text_only_web <- read.csv("health-media4.csv")

vid1_intend <- animation_audio_vid$INTEND.0
vid2_intend <- pictures_audio_vid$INTEND.0
web1_intend <- pictures_text_web$INTEND.0
web2_intend <- text_only_web$INTEND.0

mean_vid1 <- mean(vid1_intend)
mean_vid2 <- mean(vid2_intend)
mean_web1 <- mean(web1_intend)
mean_web2 <- mean(web2_intend)

sd(vid1_intend)
sd(vid2_intend)
sd(web1_intend)
sd(web2_intend)

mean_vid1
mean_vid2
mean_web1
mean_web2

mean_of_means <- (mean_vid1 + mean_vid2 + mean_web1 + mean_web2) / 4
mean_of_means

#  b) Visualize the distribution and mean of intention to share, across all four media.
#     (Your choice of data visualization; Try to put them all on the same plot and make it look sensible;
#     Recommendation: conceptualize your visualization on paper, then search online for how to produce it)

par(mfrow= c(4,1))
plot(density(vid1_intend), main = "Animation + Audio")
abline(v=mean_vid1)
abline(v=mean_of_means, col = "blue", lty= "dashed", lwd= 2)
plot(density(vid2_intend), main = "Pictures + Audio")
abline(v=mean_vid2)
abline(v=mean_of_means, col = "blue", lty= "dashed", lwd= 2)
plot(density(web1_intend), main = "Pictures + Text")
abline(v=mean_web1)
abline(v=mean_of_means, col = "blue", lty= "dashed", lwd= 2)
plot(density(web2_intend), main = "Only text")
abline(v=mean_web2)
abline(v=mean_of_means, col = "blue", lty= "dashed", lwd= 2)


library(plotly)
plot_ly(data=vid1_intend, mode = "histogram")

plot(density(vid1_intend), main = "Media type", ylim= c(0, 0.35), xlim = c(-2, 10), col= "chartreuse3", lwd= 3)
lines(density(vid2_intend), col= "chocolate3", lwd= 3)
lines(density(web1_intend), col= "coral2", lwd= 3)
lines(density(web2_intend), col= "azure4", lwd= 3)
abline(v=mean_of_means, col = "darkblue", lty= "dashed", lwd= 2)
legend("topleft", legend= c("Animatio+Audio", "Pictures+Audio", "Picture+Text", "Text Only"), lty=c(1,1,1,1), 
       lwd=c(2.5,2.5,2.5,2.5), col = c("chartreuse3", "chocolate3", "coral2", "azure4"))

legend('topright', legend = c("Vid1 Intend","Vid2 Intend"), lty=c(1,1), lwd=c(2.5,2.5),col=c("chartreuse3", "chocolate3"))


#  c) Based on the visualization, do you feel that the type of media make a difference on intention to share?


#Based on the visualization, Only text has the highest intention to share




#Question 2) Let's try traditional one-way ANOVA:
  
#    a) State the null and alternative hypotheses when comparing INTEND.0 across four groups using ANOVA

#         Hnull,  Mean of the means is not significantly different that the means of the other means


#    b) Model and produce the F-statistic for our test


media1 <- data.frame(type=rep(1, length(vid1_intend)), Intend= vid1_intend)
media2 <- data.frame(type=rep(2, length(vid2_intend)), Intend= vid2_intend)
media3 <- data.frame(type=rep(3, length(web1_intend)), Intend= web1_intend)
media4 <- data.frame(type=rep(4, length(web2_intend)), Intend= web2_intend)

media <- rbind(media1, media2, media3, media4)

anova.test <- oneway.test(media$Intend ~ media$type, var.equal = TRUE)
anova.test$statistic

#    c) What is the appropriate cut-off values of F for 95% and 99% confidence

CI_95 <- qf(p=0.95, df1 = 3, df2 = length(media$Intend)-4)
CI_99 <- qf(p=0.99, df1 = 3, df2 = length(media$Intend)-4)
CI_95
CI_99

#    d) According to the traditional ANOVA, do the four types of media produce the same mean intention to share,

anova.test$statistic < CI_95
anova.test$statistic < CI_99



#    e) at 95% confidence? How about at 99% confidence? Are the classic requirements of one-way ANOVA met? Why or why not?


#The requirements are not met because: 1) not all the populations are normally distributed
#                                      2) The variances are not the same
#                                      3) The response variables are related since the same group was exposed to the four types of media




#Question 3) Let's try bootstrapping ANOVA:
  
#     a) Bootstrap the null values of F and also the actual F-statistic.


boot_anova <- function(t1, t2, t3, t4, treat_nums) {    #Precompute treatment sizes
 
  size1 = length(t1)
  size2 = length(t2)
  size3 = length(t3)
  size4 = length(t4)
  
  #null treatments: resample from mean centered treatments
  null_grp1 = sample(t1 - mean(t1), size1, replace = TRUE)
  null_grp2 = sample(t2 - mean(t2), size2, replace = TRUE)
  null_grp3 = sample(t3 - mean(t3), size3, replace = TRUE)
  null_grp4 = sample(t4 - mean(t4), size4, replace = TRUE)
  null_values <- c(null_grp1, null_grp2, null_grp3, null_grp4)
  
  #Alternative treatments: resample from actual trearments
  alt_grp1 = sample(t1, size1, replace = TRUE)
  alt_grp2 = sample(t2, size2, replace = TRUE)
  alt_grp3 = sample(t3, size3, replace = TRUE)
  alt_grp4 = sample(t4, size4, replace = TRUE)
  alt_values = c(alt_grp1, alt_grp2, alt_grp3, alt_grp4)
  
  return(c(oneway.test(null_values ~ treat_nums, var.equal = TRUE)$statistic, #ANOVA for null f-stats
              oneway.test(alt_values ~ treat_nums, var.equal = TRUE)$statistic))  #ANOVA for alterative f-stats
}
  
  

media_type <- media$type

#bootstrap anova for null and alt 
set.seed(42)
f_values <- replicate(5000, boot_anova(vid1_intend,vid2_intend,web1_intend,web2_intend,media_type))
f_nulls <- f_values[1,]
f_alts <- f_values[2,]

#     b) According to the bootstrapped null values of F, What are the cutoff values for 95% and 99% confidence?

#examine null and alt F-stats

mean(f_nulls)
mean(f_alts)
sd(f_alts)
quantile(f_nulls, 0.95)
quantile(f_nulls, 0.99)



#     c) Show the distribution of bootstrapped null values of F, the 95% and 99% cutoff values of F 
#        (according to the bootstrap), and also the mean actual F-statistic.

#Visualize null and alt distributions of F

par(mfrow= c(1,1))
plot(density(f_nulls), main= "Null Distribution vs Alt mean")
abline(v=quantile(f_nulls, 0.95), lty="dashed", col="coral3")
abline(v=quantile(f_nulls, 0.99), lty="dashed", col="aquamarine2")
abline(v=quantile(f_nulls, 0.99), lty="dashed", col="aquamarine2")
abline(v=mean(f_alts), lwd=2)




#     d) According to the bootstrap, do the four types of media produce the same mean intention to share, 
#        at 95% confidence? How about at 99% confidence?



