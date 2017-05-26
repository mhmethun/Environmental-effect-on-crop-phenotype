# clustering with avg values

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

test.avg = NULL

# Read each variety
for(i in c(1:174)){
  d = read.csv(paste(c(path, paste(varList[i,1], collapse = ""), ".csv"), collapse = ""))
  
  avgValue = mean(d$Yield)
  
  ifelse(test = is.null(test.avg), yes = (test.avg = d[d$Yield>=avgValue, 2:12]), no = (test.avg=rbind(test.avg, d[d$Yield>=avgValue, 2:12])))
}

test.avg = unique(test.avg)
summary(test.avg)

set.seed(20)
test.avg.kmeans = kmeans(test.avg[,3:11], 4)
#pairs(test.avg, col = test.avg.kmeans$cluster + 1L)

# Assign cluster information to dataset
test.avg.cluster = data.frame(test.avg, Cluster = test.avg.kmeans$cluster)

# kmeans is not performing well
# Now try DBSCAN

library(dbscan)

# normalize environmental attributes
test.avg.env = data.Normalization(test.avg[,3:11], type = "n8")
test.avg.dbscan = dbscan(test.avg.env, .2, 4)
#pairs(test.avg.env, col = test.avg.dbscan$cluster + 1L)

# Assign cluster information to dataset
test.avg.cluster = data.frame(test.avg, Cluster = test.avg.dbscan$cluster)

# hierarical clustering
test.avg.env = dist(as.matrix(test.avg[,3:11]))
test.avg.hclust = hclust(test.avg.env)
#plot(test.avg.hclust)

# cut tree into 7 clusters
test.avg.hclust.groups <- cutree(test.avg.hclust, k=7) 

# draw dendogram with red borders around the 7 clusters 
rect.hclust(test.avg.hclust, k=7, border="red")

# Assign cluster information to dataset
test.avg.cluster = data.frame(test.avg, Cluster=test.avg.hclust.groups)

# Create new data frame with cluster information
clsID = 0
testData.cluster = NULL

for(i in c(1:nrow(test.avg.cluster))){
  clsID = test.avg.cluster$Cluster[i]
  
  d = testData[(testData$Temperature == test.avg.cluster$Temperature[i] & 
                  testData$Precipitation == test.avg.cluster$Precipitation[i] & 
                  testData$Solar.Radiation == test.avg.cluster$Solar.Radiation[i] &
                  testData$CEC == test.avg.cluster$CEC[i] & 
                  testData$Organic.matter == test.avg.cluster$Organic.matter[i] & 
                  testData$pH == test.avg.cluster$pH[i] & 
                  testData$Clay == test.avg.cluster$Clay[i] & 
                  testData$Silt == test.avg.cluster$Silt[i] & 
                  testData$Sand == test.avg.cluster$Sand[i]
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


