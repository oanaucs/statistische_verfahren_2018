#model 
data = read.csv('data/islands.csv')
data$trees <- as.factor(data$trees)
data$mt.presence <- as.factor(data$mt.presence)
data$mf.presence <- as.factor(data$mf.presence)

##gmulti
library(glmulti)

xr <- colnames(data)[2:13]
#xr <- c('no.group','no.hab', 'no.ramet', 'size', 'no.group')
#sink("glm_output2.txt")
glm.output <-  glmulti(y = 'mt.presence', xr = xr, data = data, family=binomial(link='logit'))
#sink()

print(glm.output)

model.best <-  mt.presence~1+dist.land:no.group+no.hab:size+no.hab:no.ramet+dens.ramet:dis.isl.MT+dens.ramet:dist.isl.MF+dens.ramet:no.ramet+dens.ramet:no.group+dens.ramet:dist.land

#unlink("glm_output2.txt")
library(MASS)
# Fit the full model 
#full.model <- glm(mt.presence ~., data = data)
full.model <- glm(data$mt.presence~1+dist.land+no.group+no.hab+size+no.ramet+dens.ramet+dis.isl.MT+dist.isl.MF, data = data, family = binomial)
summary(full.model)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "backward", trace = FALSE)
#summary(step.model)
step.model$anova
step2.model <- stepAIC(full.model, ~.^2, trace = F)
step2.model$anova


#####
library(caret)
library(e1071)
set.seed(123)
#get rid of island names 
data.split <- data[2:14]
#10-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model (all data)
step.model <- train(mt.presence ~., data = data.split,
                    method = "glmStepAIC"
)
step.model$results

#print output to txt fíle instead of console
sink("train_output.txt")
step.model.2 <- train(mt.presence ~.^2, data = data.split,
                    method = "glmStepAIC"
)
sink()

####
#Step:  AIC=26
#.outcome ~ dis.isl.MT + no.ramet + no.hab + trees1 + `dis.isl.MT:dist.isl.MF` + 
#  `dist.isl.MF:size` + `mean.height:no.ramet` + `mean.height:no.group` + 
#  `mean.height:no.hab` + `size:no.ramet` + `size:no.group` + 
#  `size:dist.land`
###
