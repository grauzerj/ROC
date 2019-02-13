# pROC Package Walkthrough

# Load Libraries
install.packages("pROC")
library(pROC)

# Load Data for ROC Curves
roc.data <- read.csv("Z:\\Personal Folders\\Jeff\\Data Projects\\DDP Meta\\Data\\ROC.csv", header=T)

# Dataset contains 114 toddlers at risk for autism sprectrum disorder who were given
# the Screening Tool for Autism in Toddlers (STAT)

# Dataset Field Names
names(roc.data)
  # asd = Whether child received a diagnosis of autism spectrum disorder
  # stat_sum_domain = The child's cumulative score on the Screening Tool for Autism in Toddlers
  # log.p = Logistic model probability for predicting autism diagnosis


############## pROC Functionality ##########################################################

# Create an "ROC" object using the roc() function
# First argument - Binary variable on whether or not person has the condition
# Second argument - The person's score on the assessment
roc.lasso <- roc(roc.data$asd, roc.data$log.p,smooth=F)

# Simple plot of the ROC object
plot(roc.lasso)

# Nicer plot - the usual base plot tools apply
plot(roc(roc.data$asd, roc.data$log.p,smooth=T),main="ROC Curve for STAT Calibration Sample - LASSO",col="Purple") #Nicer plot with smoothing
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
roc.stat <- roc(roc.data$asd,roc.data$stat_sum_domain,smooth = T)
coords(roc.stat,x="best",best.method="youden",ret=c("threshold","ppv","npv","sensitivity","specificity","accuracy"))
roc.stat

# Plot to compare two ROC Curves
plot(roc(roc.data$asd, roc.data$log.p,smooth=T),main="ROC Curve Original vs. LASSO Scoring", col="Purple") #Nicer plot with smoothing
lines(roc(roc.data$asd,roc.data$stat_sum_domain,smooth=T), col = "Red",lty=5)
points(0.7968464,0.8884540, col = "purple", cex=1.5, pch=5)
points(0.7513787,0.8395303, col = "red", cex=1.5, pch=5)
legend("bottomright", legend= c("LASSO AUC = 0.92","STAT Original AUC = 0.87"),
col=c("purple","red"),lty=c(1,5),cex=1.0,text.font=4,lwd=2,bty="n")

# Obtain a p-value for testing the difference between two ROC Curves
roc.stat <- roc(roc.data$asd,roc.data$stat_sum_domain,smooth = F)
roc.test(roc.lasso,roc.stat)

