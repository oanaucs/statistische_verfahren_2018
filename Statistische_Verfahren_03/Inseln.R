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

# Check out some possible models for mf.presence
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


# Check out some possible models for mt.presence
pairs(data, col=data$mt.presence)  

glm.fit <- glm(data$mt.presence ~ data$no.ramet + data$dist.land + data$mean.height + data$dis.isl.MT, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mt.presence ~ data$no.ramet:dens.ramet + data$dist.land + data$dis.isl.MT + data$size, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mt.presence ~ data$no.ramet + data$dist.land + data$dis.isl.MT + data$size + data$trees, 
               data = data, family = binomial)
summary(glm.fit)

best.fit <- glm(data$mt.presence ~ data$no.ramet + data$dist.land + data$dis.isl.MT + data$size + data$dens.group:no.group, 
               data = data, family = binomial)
summary(best.fit)

new.fit <- glm(data$mt.presence ~ data$no.ramet:dens.ramet + data$dist.land + data$dis.isl.MT + data$size + data$dens.group:no.group, 
                data = data, family = binomial)
summary(new.fit)

glm.fit <- glm(data$mt.presence ~ data$dens.ramet + data$dist.land + data$dis.isl.MT + data$size + data$dens.group, 
               data = data, family = binomial)
summary(glm.fit)

glm.fit <- glm(data$mt.presence ~ data$dens.ramet:data$no.ramet + data$dist.land + data$dis.isl.MT + data$size + data$dens.group, 
               data = data, family = binomial)
summary(glm.fit)

library(glmulti)

var_names <- c(colnames(data))
var_names
typeof(var_names)
# remove mt.presence
var_names <- var_names[-14]
var_names
var_names <- var_names[-14]
var_names 

vars <- names(data) %in% c("mt.presence", "mf.presence") 
cleaned_data <- data[!vars]

output <- glmulti(y = 'mt.presence', xr = var_names, data = data,  maxit=10)
output

glm.final <- glm(data$mt.presence ~ data$no.hab:data$no.group + data$no.hab:data$dist.land + data$trees:data$dis.isl.MT + data$trees:data$size, 
               data = data, family = binomial(link="logit"))
summary(glm.final)

coeff<- coefficients(glm.final)
summary(glm.final)
fisher_matrix <- summary(glm.final)$cov.scaled
fisher_matrix


library('mvtnorm')
# model approximate distribution
approx_distribution <- rmvnorm(n=1000, mean=coeff, sigma=fisher_matrix)
approx_distribution
hist(approx_distribution[,3])

X <- model.matrix(glm.final)
X

s <- sigma(glm.final)
s

mean <- X %*% coeff
mean

inv_mean <- exp(mean) / (exp(mean) + rep(1, NROW(mean) ))
inv_mean 

sample_size <- 10

plot(residuals(glm.final))



# create loop for generating new pseudo observations
experiments_no <- 2
for (i in 1:experiments_no)
{

  # model distribution of explained variable
  # new_y <- rnorm(n=sample_size, mean = mean, sd = s)
  
  with_replacement <- F
  rows <- c(sample(1:NROW(data), sample_size, replace=with_replacement))
  rows
  new_X <- X[rows,]
  new_X <- cbind(new_X, new_y)
  new_X <- as.data.frame(new_X)
  new_X
  names(new_X)
  names(new_X)[2]<-"no.hab.no.group"
  names(new_X)[3]<-"no.hab.dist.land"
  names(new_X)[4]<-"trees.dis.isl.MT"
  names(new_X)[5]<-"trees.size"
  names(new_X)
  
  mean <- new_X %*% coeff
  mean
  
  inv_mean <- exp(mean) / (exp(mean) + rep(1, NROW(mean) ))
  inv_mean 
  
  new_y <- rbinom(10, 1, mean)
  
  new_y
  
  
  
  current_experiment <- glm(new_X$new_y ~ new_X$no.hab.no.group + new_X$no.hab.dist.land + new_X$trees.dis.isl.MT + new_X$trees.size,
                                       data = new_X, family = binomial(link="logit"))
  summary(new.fit)
}

x <- citation()
toBibtex(x)


