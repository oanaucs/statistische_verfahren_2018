###MF

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
glm.fw.mf.1.best <- glm(formula = mf.presence ~ data$no.ramet + data$no.hab + data$size + 
data$no.ramet:data$no.hab + data$no.ramet:data$size, family = binomial, data = data)
summary(step.fw.mf.2)
#    Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 61.440  on 53  degrees of freedom #diff 18,288 / 5
#AIC: 73.44
#Number of Fisher Scoring iterations: 10

###no.ramet, no.hab, size, no.group
glm.fw.mf.3 <-  glm(data$mf.presence ~ data$no.ramet*data$no.hab + data$no.ramet*data$size + data$size*data$no.hab + data$no.ramet*data$no.group + data$no.hab*data$no.group + data$size*data$no.group, data=data, family=binomial)
step.fw.mf.3 <- step(glm.fw.mf.3 , direction = "backward")
glm.fw.mf.1.best <- glm(formula = data$mf.presence ~ data$no.ramet + data$no.hab + data$size + no.hab + no.group + size + data$no.ramet:data$no.hab + data$no.ramet:data$size + data$no.hab:data$size + no.hab:no.group + no.group:size, family = binomial, data = data)
summary(step.fw.mf.3)
#AIC: 68.473
#Null deviance: 79.728  on 58  degrees of freedom
#Residual deviance: 48.473  on 49  degrees of freedom #diff 31,255 / 7
#Number of Fisher Scoring iterations: 11


#forward:

glm.fit.mf.3 <-  glm(data$mf.presence ~ 1, data=data, family=binomial)
step.model.mf.3 <- step(object =glm.fit.mf.3 , scope=list(lower=glm.fit.mf.3, upper=glm.fit.mf.2), data=data, direction = 'forward')
summary(step.model.mf.3)
