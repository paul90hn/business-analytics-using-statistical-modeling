setwd("C:/Users/USER/Documents/BASM")
exams_table<- read.table("exam_results.txt", header = TRUE)

# What is the 5th element in the original list of correct grades?
exams_scores <- c(exams_table$scores)
exams_scores[5]


#What is the fifth lowest grade?

sort(exams_scores)[5]  #sorts scores from lowest to highest, then calls the fifth element (fifth lowest)

#Extract the five lowest grades together
sort(exams_scores)[1:5]

#Get the five highest scores by first sorting exam$scores in decreasing order
sort(exams_scores, decreasing = TRUE)[1:5]

#What is the standard deviation of scores? 
sd(exams_scores)

#Make a new variable called scores_diff, with the difference between each grade and the mean grade
scores_diff <- c(exams_scores - mean((exams_scores)))

#What is the average "difference between each grade and the mean of all grades"?
mean(scores_diff)   # =-24.2713

#Visualize the data as we did in class: histogram, density plot, boxplot+stripchart
hist(exams_scores,main = "Scores")
plot(density(exams_scores))
boxplot(exams_scores, horizontal = TRUE, main = "Scores")
  stripchart(exams_scores, add = TRUE)
   
