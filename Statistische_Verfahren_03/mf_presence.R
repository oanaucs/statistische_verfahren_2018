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

corr_var_names <- c('no.hab', 'no.group', 'no.ramet', 'trees', 'dist.isl.MF', 'dist.land')

library(glmulti)

output <- glmulti(y = 'mf.presence', xr = corr_var_names, data = data, family=binomial(link='logit'))
output

simple <- glm(data$mt.presence~data$no.hab + data$no.group + data$no.ramet + data$trees + data$dist.isl.MF)
summary(simple)

simple <- glm(data$mf.presence~ 1 + data$no.hab + 
                # data$dist.isl.MF + 
                data$no.group +
                # data$no.ramet +
                # data$dist.land +
                data$no.hab:data$no.group, 
               # data$no.ramet:data$trees,
              data=data, family=binomial(link='logit'))
summary(simple)

corr_var_names <- c('no.hab', 'no.group', 'no.ramet', 'trees', 'size')

output2 <- glmulti(y = 'mf.presence', xr = var_names, data = data, family=binomial(link='logit'))
output2
