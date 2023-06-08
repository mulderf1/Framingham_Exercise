rm(list = ls())
library(gitcreds)
library(unibeCols)
library(usethis)
library(riskCommunicator)
library(tidyverse)
data("framingham")

#Statistics

sum(framingham$GLUCOSE)
sum(is.na(framingham$GLUCOSE))
sum(framingham$GLUCOSE, na.rm = TRUE) / sum(!is.na(framingham$GLUCOSE))
mean_GLUC <- mean(framingham$GLUCOSE, na.rm = TRUE)

#median
sorted_GLU <- sort(framingham$GLUCOSE)
sum(is.na(sorted_GLU))
length(sorted_GLU)
middle_GLU <- (length(sorted_GLU) + 1) / 2
sorted_GLU[middle_GLU]
median_GLU <- median(framingham$GLUCOSE, na.rm =  TRUE) 
quart_GLU <- quantile(framingham$GLUCOSE, probs = c(0.25, 0.5, 0.75),
                      na.rm = TRUE)
sum((framingham$GLUCOSE - mean_GLUC) ^ 2, na.rm =  TRUE) / 
  (sum(!is.na(framingham$GLUCOSE)) - 1)
var(framingham$GLUCOSE, na.rm =  TRUE) 
sqrt(var(framingham$GLUCOSE, na.rm =  TRUE))
sd_GLU <- sd(framingham$GLUCOSE, na.rm =  TRUE) 
min_GLU <- sort(framingham$GLUCOSE)[1]
max_GLU <- sort(framingham$GLUCOSE)[length(framingham$GLUCOSE) - 1]
cbind(min_hr, max_GLU)
max_GLU <- max(framingham$GLUCOSE, na.rm = T)
min_GLU <- min(framingham$GLUCOSE, na.rm = TRUE)
range(framingham$GLUCOSE, na.rm = TRUE)
IQR(framingham$GLUCOSE, na.rm = TRUE) 
summary(framingham$GLUCOSE)

#graph
ggplot(framingham, aes(x = GLUCOSE)) + geom_histogram(binwidth = 5) +
  geom_vline(xintercept = mean_GLUC, colour = "red") +
  geom_vline(xintercept = median_GLU, colour = "blue")

#95% c.i
SE_GLU <- sd_GLU / sqrt(length(which(!is.na(framingham$GLUCOSE))))
zValue <- qnorm(0.975)                        
lowerCI_mean_GLU <- mean_GLUC - zValue * SE_GLU
upperCI_mean_GLU <- mean_GLUC + zValue * SE_GLU
cbind(mean_GLUC, lowerCI_mean_GLU, upperCI_mean_GLU)                               
n <- length(which(!is.na(framingham$GLUCOSE)))
tQuantile <- qt(p = 0.975, df =  n - 1)
lowerCI_mean_GLU <- mean_GLUC - tQuantile * SE_GLU
upperCI_mean_GLU <- mean_GLUC + tQuantile * SE_GLU
cbind(mean_GLUC, lowerCI_mean_GLU, upperCI_mean_GLU)

