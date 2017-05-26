# Feature selection
# 1. Linear regression
# 2. Forward selection
# 3. Backward selection
# 4. PCA

library(ISLR)
library(pheatmap)
require(leaps)

# Linear regression

# Read data from csv file
testData = read.csv(file.choose())
varList = read.csv(file.choose())

# Pair wise scatter plot
pairs(testData)

# Generate data for each variety
path = "~/self/Study/2016_Fall/Data_Science/Project/Dataset/VAR/"
imgPath = "~/self/Study/2016_Fall/Data_Science/Project/images/"
for(i in c(1:174)){
  write.csv(testData[testData$Variety == varList[i,1], ], paste(c(path, paste(varList[i,1], collapse = ""), ".csv"), collapse = ""))
}
  

# Read each variety
for(i in c(1:174)){
  d = read.csv(paste(c(path, paste(varList[i,1], collapse = ""), ".csv"), collapse = ""))
  
  # Create heatmap
  pheatmap(cor(d[3:11], use="pairwise.complete.obs"), filename = paste(c(imgPath, paste(varList[i,1], collapse = ""), ".png"), collapse = ""))
  
}

# Linear regression 
lm.fit = lm(Yield~., data = testData[2:11])
summary(lm.fit)
summary(lm.fit)$coefficients

# From cor chart it is obvious that every environmental attribute has influence on phenotype

# Forward selection
regsubsets(Yield~.,data = testData[2:11], nvmax = 8)

# Backward selection
regsubsets(Yield~.,data = testData[2:11], method = "backward")





