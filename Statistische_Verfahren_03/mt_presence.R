data = read.csv('../statistische_verfahren_2018/data/islands.csv')

# remove mt.presence, mf.presence and dist.isl.mt from variables
var_names <- c(colnames(data))
var_names
var_names <- var_names[-14]
var_names
var_names <- var_names[-14]
var_names 

numeric_data <- data
numeric_data <- numeric_data[,-1]
numeric_data <- mapply(numeric_data, FUN=as.numeric)
numeric_data

# use xtable package to export tables to latex
library('xtable')

# first, compute correlations to get an idea of the data
correlation_matrix <- cor(numeric_data, method = c("spearman"))
correlation_matrix
print(xtable(correlation_matrix, type = "latex"), file = "./correlation_matrix.tex")

library(corrplot)
corrplot(correlation_matrix)

corrplot(correlation_matrix, method='number')

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
