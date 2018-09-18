###MF model 
# 
data = read.csv('data/islands.csv')
data$trees <- as.factor(data$trees)
data$mt.presence <- as.factor(data$mt.presence)
data$mf.presence <- as.factor(data$mf.presence)
###
numeric_data <- data
numeric_data
# remove names
colnames(numeric_data)
numeric_data <- numeric_data[,-1]
c(colnames(numeric_data))

# transform to numeric data
numeric_data <- mapply(numeric_data, FUN=as.numeric)
numeric_data <- as.data.frame(numeric_data)


### Ansatz: volles Modell nur aus den Prädiktoren, die >0.1 mit mf.precense korrelieren.
corr.matrix <- correlation_matrix <- cor(numeric_data, method = c("kendall"))
pos <- as.vector(which(corr.matrix[14,1:12] > 0.1))
corr.vals <- data[, pos]
corr.vals <- as.data.frame(corr.vals)
data.corr <- cbind(corr.vals, data$mf.presence)

glm.null <- glm(data.corr$`data$mf.presence` ~ 1, data=data.corr, family=binomial)
glm.full <- glm(data.corr$`data$mf.presence` ~ ., data=data.corr, family=binomial)
#fw
glm.full.fw <- step(object = glm.null, scope=list(lower=glm.null, upper=glm.full), data=data.corr, direction = 'forward')
glm.full.fw.best <- glm(formula = data.corr$`data$mf.presence` ~ no.hab + size, family = binomial, data = data.corr)
summary(glm.full.fw)
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 68.287  on 56  degrees of freedom
#AIC: 74.287
#Number of Fisher Scoring iterations: 5
#bw
glm.full.bw <- step(glm.full , direction = "backward")
glm.full.bw.best <- glm(formula = data.corr$`data$mf.presence` ~ size + no.hab, family = binomial, data = data.corr)
summary(glm.full.bw)
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 68.287  on 56  degrees of freedom
#AIC: 74.287
#Number of Fisher Scoring iterations: 5

#Volles Modell mit korr. Prädiktoren
glm.full.2 <- glm(data.corr$`data$mf.presence`~ data.corr$mean.height:data.corr$size+data.corr$size:data.corr$dist.land+data.corr$size:data.corr$no.hab+data.corr$no.ramet:data.corr$no.hab+data.corr$mean.height+data.corr$size+data.corr$no.ramet+data.corr$dist.land+data.corr$no.hab, data=data.corr, family=binomial)

glm.full.2.fw <- step(object = glm.null, scope=list(lower=glm.null, upper=glm.full.2), data=data.corr, direction = 'forward')
summary(glm.full.2.fw)
glm.full.2.fw.best <- glm(formula = data.corr$`data$mf.presence` ~ data.corr$no.hab + data.corr$size, family = binomial, data = data.corr)
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 68.287  on 56  degrees of freedom
#AIC: 74.287
#Number of Fisher Scoring iterations: 5


#######
#
#
#
######backward:
###no.ramet, no.hab
glm.fw.mf.1 <- glm(mf.presence ~ 1+no.ramet*no.hab,data = data, family = binomial)
step.fw.mf.1 <- step(glm.fw.mf.1 , direction = "backward")
glm.fw.mf.1.best <- glm(formula = mf.presence ~ no.hab, family = binomial, data = data)
summary(step.fw.mf.1)
#    Null deviance: 79.728  on 58  degrees of freedom 
#Residual deviance: 70.340  on 57  degrees of freedom #diff 9,388 / 1
#AIC: 74.34
#Number of Fisher Scoring iterations: 4

###no.ramet, no.hab, size
glm.fw.mf.2 <- glm(mf.presence ~ data$no.ramet*data$no.hab + data$no.ramet*data$size + data$size*data$no.hab,data = data, family = binomial)
step.fw.mf.2 <- step(glm.fw.mf.2 , direction = "backward")
glm.fw.mf.2.best <- glm(formula = mf.presence ~ data$no.ramet + data$no.hab + data$size + 
data$no.ramet:data$no.hab + data$no.ramet:data$size, family = binomial, data = data)
summary(step.fw.mf.2)
#    Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 61.440  on 53  degrees of freedom #diff 18,288 / 5
#AIC: 73.44
#Number of Fisher Scoring iterations: 10

###no.ramet, no.hab, size, no.group
glm.fw.mf.3 <-  glm(data$mf.presence ~ data$no.ramet*data$no.hab + data$no.ramet*data$size + data$size*data$no.hab + data$no.ramet*data$no.group + data$no.hab*data$no.group + data$size*data$no.group, data=data, family=binomial)
step.fw.mf.3 <- step(glm.fw.mf.3 , direction = "backward")
glm.fw.mf.3.best <- glm(formula = data$mf.presence ~ data$no.ramet + data$no.hab + data$size + no.hab + no.group + size + data$no.ramet:data$no.hab + data$no.ramet:data$size + data$no.hab:data$size + no.hab:no.group + no.group:size, family = binomial, data = data)
summary(step.fw.mf.3)
#AIC: 68.473
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 48.473  on 49  degrees of freedom #diff 31,255 / 7
#Number of Fisher Scoring iterations: 11


#forward:
glm.bw.mf.null <-  glm(data$mf.presence ~ 1, data=data, family=binomial)

#1
step.bw.mf.1 <- step(object =glm.fw.mf.1 , scope=list(lower=glm.bw.mf.null, upper=glm.fw.mf.1), data=data, direction = 'forward')
glm.bw.mf.1.best <- glm(formula = data$mf.presence ~ no.hab, family = binomial, data = data)
summary(step.bw.mf.1)
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 70.340  on 57  degrees of freedom
#AIC: 74.34
#Number of Fisher Scoring iterations: 4

#2
step.bw.mf.2 <- step(object =glm.bw.mf.null , scope=list(lower=glm.bw.mf.null, upper=glm.fw.mf.2), data=data, direction = 'forward')
glm.bw.mf.2.best <- glm(formula = data$mf.presence ~ data$no.hab + data$size, family = binomial, data = data)
summary(step.bw.mf.2)
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 68.287  on 56  degrees of freedom
#AIC: 74.287
#Number of Fisher Scoring iterations: 5

#3
step.bw.mf.3 <- step(object =glm.bw.mf.null , scope=list(lower=glm.bw.mf.null, upper=glm.fw.mf.3), data=data, direction = 'forward')
glm.bw.mf.3.best <- glm(formula = data$mf.presence ~ data$no.hab + data$size, family = binomial, data = data)
summary(step.bw.mf.3)
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 68.287  on 56  degrees of freedom
#AIC: 74.287
#Number of Fisher Scoring iterations: 5







