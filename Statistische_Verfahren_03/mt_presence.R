data = read.csv('../statistische_verfahren_2018/data/islands.csv')

numeric_data <- data
c(colnames(numeric_data))
# remove mt.presence from data
c(colnames(numeric_data))
# remove names
numeric_data <- numeric_data[,-1]
c(colnames(numeric_data))
# transform to numeric data
numeric_data <- mapply(numeric_data, FUN=as.numeric)
numeric_data <- as.data.frame(numeric_data)

### first approach - forward and/or backward selection

full <- glm(data$mt.presence~., data=numeric_data, family=binomial(link='logit'))
summary(full)

full_bw <- step(object = full, direction = 'backward')

summary(full_bw)

result_backward <- glm(formula = data$mt.presence ~ dist.isl.MF + dist.group + no.ramet + 
                         dist.land + trees + dens.ramet + dens.group, family = binomial(link = "logit"), 
                       data = numeric_data)

summary(result_backward)

result_backward_int <- glm(formula = data$mt.presence ~ dist.isl.MF + dist.group + no.ramet + 
                         dist.land + trees + dens.ramet + dens.group+ trees:no.ramet, family = binomial(link = "logit"), 
                       data = numeric_data)
summary(result_backward_int)

null <- glm(data$mt.presence ~ 1, data=numeric_data, family=binomial(link='logit'))
summary(null)

full_fw <- step(object = null, scope=list(lower=null, upper=full), data=numeric_data, direction = 'forward')
summary(full_fw)

full_both <- step(null, scope = list(upper=full), data=numeric_data, direction="both")
summary(full_both)

full_correlated <- glm(data$mt.presence ~ data$dist.group + data$size + data$no.group + data$no.ramet + data$no.hab + data$trees, data=numeric_data, family=binomial(link='logit'))

summary(full_correlated)

full_correlated <- step(object = full_correlated, direction = 'backward')

summary(full_correlated)

### second approach - glmulti


corr_var_names <- c('no.hab', 'no.group', 'no.ramet', 'trees', 'dist.group')

library(glmulti)

output <- glmulti(y = 'mt.presence', xr = corr_var_names, data = data, family=binomial(link='logit'))
output

simple <- glm(data$mt.presence~data$no.hab + data$no.group + data$no.ramet + data$trees + data$dist.group)
summary(simple)

simple <- glm(data$mt.presence~data$no.hab + 
                data$no.group + 
                data$no.ramet + 
                data$trees + 
                # data$dist.group +
                data$no.hab:data$no.group + 
                #data$no.hab:data$no.ramet + 
                #data$no.hab:data$trees + 
                data$no.ramet:data$trees,
              #data$no.hab:data$dist.group + 
              #data$no.ramet:data$dist.group, 
              #data$trees:data$dist.group, 
              data=data, family=binomial(link='logit'))
summary(simple)

simple.1 <- glm(data$mt.presence~data$no.hab + 
                  data$no.group + 
                  data$no.ramet + 
                  data$trees + 
                  data$dist.group +
                  data$no.hab:data$no.group, data=data, family=binomial(link='logit'))
summary(simple.1)

# AIC 57
simple.2 <- glm(data$mt.presence~data$no.hab + 
                  data$no.group + 
                  data$no.ramet + 
                  data$trees +
                  data$no.hab:data$no.group + 
                  data$no.ramet:data$trees,
                
                data=data, family=binomial(link='logit'))
summary(simple.2)

corr_var_names <- c('no.hab', 'no.group', 'no.ramet', 'trees', 'size')

output2 <- glmulti(y = 'mt.presence', xr = corr_var_names, data = data, family=binomial(link='logit'))
output2



