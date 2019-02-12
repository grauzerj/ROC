# Clean up
rm(list=ls()) #Clear workspace

# Load Libraries
library(MASS)
library(Hmisc)
library(pROC)
library(glmnet)
library(tidyverse)

# Load Data
ddp.dat <- read.csv("Z:\\Personal Folders\\Jeff\\Data Projects\\DDP Meta\\Data\\DDP_STAT_Final.csv", header=T)

# Create Model Calibration Data Set
ddp.cal <- filter(ddp.dat,split==1)
dim(ddp.cal) # 114 x 190

# Create Model Validation Data Set
ddp.val <- filter(ddp.dat,split==2)
dim(ddp.val) # 57 x 190

n <- nrow(ddp.cal)

# Predictor variables from the calibration data set
predictors <- cbind(ddp.cal$stat_play_turn, ddp.cal$stat_play_doll, ddp.cal$stat_req_bubble, ddp.cal$stat_req_food, 
ddp.cal$stat_dir_balloon, ddp.cal$stat_dir_puppet, ddp.cal$stat_dir_toy_bag, ddp.cal$stat_dir_noisemaker, 
ddp.cal$stat_imi_shake_rattle, ddp.cal$stat_imi_roll_car,  ddp.cal$stat_imi_drum_hands, ddp.cal$stat_imi_hop_dog)


########### Lasso Regression on Calibration Data
# Creates LASSO Regression Models
lasso.12 <- glmnet(predictors,ddp.cal$asd,family= "binomial")
lasso.12$beta[,63] # Model coefficients
lasso.12$a0 # Model intercept

## Use the model to predict likelihood of and ASD Diagnosis
lasso.p <- predict(lasso.12, newx = predictors, type="response")

# Predicted likelihood of ASD on the calibration data set
lasso.63 <- lasso.p[,63]

# Create an "ROC" object using the roc() function
# First argument - Binary variable on whether or not person has the condition
# Second argument - The person's score on the assessment
roc.lasso <- roc(ddp.cal$asd, lasso.63,smooth=F)

# Simple plot of the ROC object
plot(roc.lasso)

# Nicer plot - the usual base plot tools apply
plot(roc(ddp.cal$asd, lasso.63,smooth=T),main="ROC Curve for STAT Calibration Sample - LASSO",col="Purple") #Nicer plot with smoothing
legend("bottomright", legend= c("AUC = ",round(roc.lasso$auc,3)))
# Note that an ROC object can be "smoothed"

# ROC Curve Metrics

# Compute the "optimal" cutoff for your screening method
# and return values for PPV, NPV, sensitivity and specificity
coords(roc.lasso,x="best",best.method="youden",ret=c("threshold","ppv","npv","sensitivity","specificity","accuracy"))

# The AUC (Area Under Curve) of an ROC
roc.lasso$auc

# Compute a confidence interval on your value of AUC
ci.auc(roc.lasso)

# Create an ROC object on the original STAT scoring method
roc.stat <- roc(ddp.cal$asd,ddp.cal$stat_sum_domain,smooth = T)
coords(roc.stat,x="best",best.method="youden",ret=c("threshold","ppv","npv","sensitivity","specificity","accuracy"))
roc.stat

# Plot to compare two ROC Curves
plot(roc(ddp.cal$asd, lasso.63,smooth=T),main="ROC Curve Original vs. LASSO Scoring", col="Purple") #Nicer plot with smoothing
lines(roc(ddp.cal$asd,ddp.cal$stat_sum_domain,smooth=T), col = "Red",lty=5)
points(0.7968464,0.8884540, col = "purple", cex=1.5, pch=5)
points(0.7650748,0.8630137, col = "red", cex=1.5, pch=5)
legend("bottomright", legend= c("LASSO AUC = 0.92","STAT Original AUC = 0.89"),
col=c("purple","red"),lty=c(1,5),cex=1.0,text.font=4,lwd=2,bty="n")

# Obtain a p-value for testing the difference between two ROC Curves
roc.stat <- roc(ddp.cal$asd,ddp.cal$stat_sum_domain,smooth = F)
roc.test(roc.lasso,roc.stat)

