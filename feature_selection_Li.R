data1 <- read.csv("Experiment newdataset.csv")
View(data1)
names(data1)

temp <- data1[2:12]
View(temp)
names(temp)


#################################

full <- lm(temp$Yield~., data=temp)
null <- lm(temp$Yield~1, data=temp)

#forward selection
step(null, scope=list(lower=null, upper=full), direction="forward")

#backward selection
step(full, direction = "backward")





