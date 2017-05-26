# clustering with max values

testData = read.csv(file.choose())

# data normalization
library(clusterSim)
test.feature = testData[,2:11]
env.feature = data.Normalization(test.feature[,2:10], type="n8")

# k-means
set.seed(20)
kmeans.cluster = kmeans(env.feature, 3)

# Data points are so densed that it is very hard to find a cluster
# try some different process
# steps:
# 1. For each variety there has env condition for top most 10 phenotype value
# 2. At first pick all 174 env conditions which has pick phenotype values

path = "~/self/Study/2016_Fall/Data_Science/Project/Dataset/VAR/"

# Read all varieties
varList = read.csv(file.choose())

test.max = NULL

# Read each variety
for(i in c(1:174)){
  d = read.csv(paste(c(path, paste(varList[i,1], collapse = ""), ".csv"), collapse = ""))
  
  maxValue = max(d$Yield)
  
  ifelse(test = is.null(test.max), yes = (test.max = d[d$Yield==maxValue, 2:12]), no = (test.max=rbind(test.max, d[d$Yield==maxValue, 2:12])))
}

summary(test.max)

set.seed(20)
test.max.kmeans = kmeans(test.max[,3:11], 7)
pairs(test.max, col = test.max.kmeans$cluster + 1L)

# Assign cluster information to dataset
test.max.cluster = data.frame(test.max, test.max.kmeans$cluster)

# kmeans is not performing well
# Now try DBSCAN

library(dbscan)

# normalize environmental attributes
test.max.env = data.Normalization(test.max[,3:11], type = "n8")
test.max.dbscan = dbscan(test.max.env, .2, 4)
pairs(test.max.env, col = test.max.dbscan$cluster + 1L)

# Assign cluster information to dataset
test.max.cluster = data.frame(test.max, test.max.dbscan$cluster)

# hierarical clustering
test.max.env = dist(as.matrix(test.max[,3:11]))
test.max.hclust = hclust(test.max.env)
plot(test.max.hclust)

# cut tree into 7 clusters
test.max.hclust.groups <- cutree(test.max.hclust, k=7) 

# draw dendogram with red borders around the 7 clusters 
rect.hclust(test.max.hclust, k=7, border="red")

# Assign cluster information to dataset
test.max.cluster = data.frame(test.max, test.max.hclust.groups)

# Create new data frame with cluster information
clsID = 0
testData.cluster = NULL

for(i in c(1:nrow(test.max.cluster))){
  clsID = test.max.cluster$test.max.hclust.groups[i]
  
  d = testData[(testData$Temperature == test.max.cluster$Temperature[i] & 
                  testData$Precipitation == test.max.cluster$Precipitation[i] & 
                  testData$Solar.Radiation == test.max.cluster$Solar.Radiation[i] &
                  testData$CEC == test.max.cluster$CEC[i] & 
                  testData$Organic.matter == test.max.cluster$Organic.matter[i] & 
                  testData$pH == test.max.cluster$pH[i] & 
                  testData$Clay == test.max.cluster$Clay[i] & 
                  testData$Silt == test.max.cluster$Silt[i] & 
                  testData$Sand == test.max.cluster$Sand[i]
                )
               ,]
  Cluster = rep(clsID, nrow(d))
  
  ifelse(test = is.null(testData.cluster),
         yes = (testData.cluster = data.frame(d, Cluster)),
         no = (testData.cluster = rbind(testData.cluster, data.frame(d, Cluster)))
         )
}

testData.cluster = unique(testData.cluster)
summary(testData.cluster)


