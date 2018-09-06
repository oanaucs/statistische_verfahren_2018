data = read.csv('data/islands.csv')
data$trees <- as.factor(data$trees)

### Korrelationen
library(corrplot)
correlations <- cor(data[,2:12])
corrplot(correlations, method="color")


### Scatter
plot(data)


### density
library(lattice)
library(ggplot2)
library(caret)
library(ellipse)

#copy.data <- data
#copy.data[14][copy.data[14] == 0] <- 'not'
#copy.data[14][copy.data[14] == 1] <- 'present'
x <- data[,2:13]
y <- data[,14]
y <- as.factor(y)
scales <- list(x=list(relation="free"), y=list(relation="free"))
obj <- featurePlot(x=x, y=y, plot="density", scales=scales, auto.key=list(columns=2))
filename <- "plots/density.pdf"
pdf(filename)
print(obj)
dev.off()

########## export plots ##########

###hist
###density
###index
name = names(data)
name = gsub("[^0-9A-Za-z ]","" , name ,ignore.case = TRUE)
for(i in 2:10){
  #png(filename= paste("plots/hist/",name[i], ".png" , sep = ""))
  #hist(data[,i], main = name[i])
  #png(filename= paste("plots/dens/",name[i], ".png" , sep = ""))
  #plot(density(data[,i]), main = name[i])
  #png(filename= paste("plots/index/",name[i], ".png" , sep = ""))
  #plot(sort(data[,i]),type = "p", main = name[i])
  png(filename= paste("plots/boxplots_mf/",name[i], ".png" , sep = ""))
  boxplot(data[,i] ~ data$mf.presence, main = name[i])
}
dev.off()



