data <- read.csv(file.choose())
View(data)

summary(data)
#noted that there are NA's   :3847 appyied to multiple attributes

hist(data$Area)

summary(data$Area)

data[is.na(data$Area),]
#all these data have NA on every attributes...basically

newdata <- data[!is.na(data$Area),]
summary(newdata)  #now newdata gets all those NA experiment data removed

hist(newdata$Area)


#Notice that some data value for PI and Planting.data shown as "."
summary(newdata$PI)

tmp1 <- newdata[newdata$PI == ".",]
View(tmp1)
summary(tmp1)

tmp2 <- newdata[newdata$Planting.date == ".",]
View(tmp2)
summary(tmp2)

finalData <- newdata[newdata$Planting.date != ".",]

