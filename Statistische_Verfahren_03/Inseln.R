########   Blattl??use auf Inseln P3 ##########

data = read.csv('../statistische_verfahren_2018/data/islands.csv')
laeuse = data$mf.presence + data$mt.presence
plot(data$no.ramet, laeuse)
plot(data$size, laeuse) # makes sense
plot(data$dist.isl.MF, data$mf.presence)
plot(data$dist.isl.MF, data$mt.presence)
plot(data$no.hab, laeuse) # useable
plot(data$dens.group, laeuse) #useable
plot(data$no.ramet, data$no.group)
# histogramm f??r trees und l??use
plot(data$mean.height, laeuse) # useable
plot(data$dist.group, laeuse) #useable
plot(data$mt.presence, data$mf.presence) # anders plotten


plot(data)

for(i in 1:ncol(data)) {
  d = as.numeric(data[,i])
  hist(d, main=names(data)[i])
}

# run system command and then restart RStudio
# system("defaults write org.R-project.R force.LANG en_US.UTF-8")

# now install package
#install.packages("corrplot")
install.packages("Amelia")
#install.packages("mlbench")

ylibrary(corrplot)
corr_data <- data
corr_data <- mapply(corr_data, FUN=as.numeric)
correlations <- cor(corr_data)
corrplot(correlations, method="circle")


#library(Amelia)
#library(mlbench)
#missmap(data, col=c("blue", "red"), legend=FALSE)

pairs(data, col=data$mf.presence)  

glm.fit <- glm(data$mf.presence ~ data$no.ramet + data$no.ramet + data$dist.land + data$mean.height + data$dist.isl.MF, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mf.presence ~ data$no.ramet + data$dist.land + data$dist.isl.MF + data$size, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mf.presence ~ data$no.ramet + data$dist.land + data$dist.isl.MF + data$size + data$trees, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mf.presence ~ data$no.ramet + data$dist.land + data$dist.isl.MF + data$size + data$dens.group, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mf.presence ~ data$dens.ramet + data$dist.land + data$dist.isl.MF + data$size + data$dens.group, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mf.presence ~ data$dens.ramet:data$no.ramet + data$dist.land + data$dist.isl.MF + data$size + data$dens.group, 
               data = data, family = binomial)
summary(glm.fit)
