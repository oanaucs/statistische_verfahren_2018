data = read.csv('../statistische_verfahren_2018/data/islands.csv')

numeric_data <- data
numeric_data <- numeric_data[,-1]
numeric_data <- mapply(numeric_data, FUN=as.numeric)
numeric_data <- as.data.frame(numeric_data)

### first approach - forward and/or backward selection

full <- glm(data$mf.presence~., data=numeric_data, family=binomial(link='logit'))
summary(full)

full_bw <- step(object = full, direction = 'backward')

summary(full_bw)

result_backward <- glm(formula = data$mf.presence ~ dis.isl.MT + no.hab + dens.group, 
    family = binomial(link = "logit"), data = numeric_data)
summary(result_backward)

null <- glm(data$mf.presence ~ 1, data=numeric_data, family=binomial(link='logit'))
summary(null)

full_fw <- step(object = null, scope=list(lower=null, upper=full), data=numeric_data, direction = 'forward')
summary(full_fw)

full_both <- step(null, scope = list(upper=full), data=numeric_data, direction="both")
summary(full_both)

full_correlated <- glm(data$mf.presence ~ data$no.hab + data$no.ramet,data=numeric_data, family=binomial(link='logit'))

summary(full_correlated)

full_correlated <- step(object = full_correlated, direction = 'backward')

summary(full_correlated)


best <- glm(formula = mf.presence ~ dis.isl.MT + no.hab + dens.group + trees,  
           family = binomial(link = "logit"), data = numeric_data)
summary(best)

### second approach - glmulti



library(glmulti)

output <- glmulti(y = 'mf.presence', xr = corr_var_names, data = data, family=binomial(link='logit'))
output

simple <- glm(data$mt.presence~data$no.hab + data$no.group + data$no.ramet + data$trees + data$dist.isl.MF)
summary(simple)

simple <- glm(data$mf.presence~ 1 + data$no.hab + 
                # data$dist.isl.MF + 
                data$no.group +
                data$trees + 
                # data$no.ramet +
                # data$dist.land +
                data$no.hab:data$no.group + 
                data$no.ramet:data$trees,
              data=data, family=binomial(link='logit'))
summary(simple)

corr_var_names <- c('no.hab', 'no.group', 'no.ramet', 'trees', 'size')

output2 <- glmulti(y = 'mf.presence', xr = var_names, data = data, family=binomial(link='logit'))
output2

## different approach - forward and/or backward selection

full <- glm(data$mf.presence~., data=numeric_data, family=binomial(link='logit'))
summary(full)

full_bw <- step(object = full, direction = 'backward')

summary(full_bw)

empty <- glm(data$mf.presence ~ 1, data=numeric_data, family=binomial(link='logit'))
summary(empty)

full_fw <- step(object = empty, scope=list(lower=empty, upper=full), direction = 'forward')

summary(full_fw)
