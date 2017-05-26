library(plotrix)


# Calculate mean of environmental attributes of each cluster
testData.cluster.env = testData.cluster[,3:12]
testData.cluster.env.average = aggregate(testData.cluster.env[,1:9], by=list(Cluster = testData.cluster.env$Cluster), FUN = mean)
testData.cluster.env.average.compact = data.frame(Cluster=testData.cluster.env.average[,1], Means=rowMeans(testData.cluster.env.average[,-1]))
testData.cluster.count = aggregate(testData.cluster$Variety, by=list(Cluster = testData.refine.cluster$Cluster), FUN = length)

# Simply compute the average of variety for each cluster
testData.cluster.avg = aggregate(testData.cluster$Yield, by=list(Variety = testData.cluster$Variety, Cluster = testData.cluster$Cluster), FUN = mean)

# Reshape the table
# Columns are the cluster IDs
vl = unique(testData.cluster.avg$Variety)
Cluster1 = c(rep(0), nrow(vl))
Cluster2 = Cluster3=Cluster4=Cluster5=Cluster6=Cluster7 = Cluster1
testData.reform.Cluster = data.frame(Variety = unique(testData.cluster.avg$Variety), Cluster1,Cluster2,Cluster3,Cluster4,Cluster5,Cluster6,Cluster7)
View(testData.reform.Cluster)

for(i in c(1:nrow(testData.reform.Cluster))){
  var = testData.reform.Cluster[i,1]
  
  for(j in c(2:ncol(testData.reform.Cluster))){
    
    cls = j-1
    val = -1
    v = testData.cluster.avg[which(testData.cluster.avg$Variety==var & testData.cluster.avg$Cluster==cls),3]
    ifelse(test = (length(v) > 0), yes = (val = v), no = (val=0))
    
    testData.reform.Cluster[i,j] = val
  }
}

testData.reform.Cluster.rowName = data.frame(testData.reform.Cluster, row.names = 1)

library(d3heatmap)

colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
d3heatmap(testData.reform.Cluster.rowName, scale = "column", dendrogram = "none",
          color = colfunc)

row.names(testData.reform.Cluster)=testData.reform.Cluster$Variety
heatmap(data.matrix(testData.reform.Cluster[,2:8]), Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))


# Add count value to testData.cluster.env.average.compact data frame
testData.cluster.env.average.final = data.frame(testData.cluster.env.average.compact, Count=testData.cluster.count$x)

# Treemap plot for cluster
# Color reflects the env.conditions
# Size reflects the individuals
treemap(testData.cluster.env.average.final, #Your data frame object
         index=c("Cluster"),  #A list of your categorical variables
         vSize = "Count",  #This is your quantitative variable
         vColor = "Means", #This is your color variable
         type="index", #Type sets the organization and color scheme of your treemap
         palette = "RdYlBu",  #Select your color palette from the RColorBrewer presets or make your own.
         title="", #Customize your title
         fontsize.title = 1 #Change the font size of the title
)



imgPath = "~/self/Study/2016_Fall/Data_Science/Project/images/viz.max/"

# Pick unique cluster ids
unique.cluster.idList = unique(testData.cluster$Cluster)

for(i in unique.cluster.idList){
  
  # Collect records for each cluster
  d.cls = testData.cluster[testData.cluster$Cluster == i, 1:2]
  
  # Aggregate Yield based on variety
  d.cls.avg = aggregate(d.cls$Yield, by=list(Variety = d.cls$Variety), FUN = mean)
  
  # Sort in descending order
  d.cls.avg = d.cls.avg[order(-d.cls.avg$x), ]
  
  # create file to save
  png(filename=paste(c(imgPath, paste(c("pie_",i), collapse = ""), ".png"), collapse = ""))
  
  colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
  pie3D(d.cls.avg$x[1:10],labels=d.cls.avg$Variety[1:10], explode=0.1,
         main=paste(c("Pie Chart of cluster ", i), collapse = ""), col = colfunc(10))
  
  # Save the file.
  dev.off()
}

warning()




