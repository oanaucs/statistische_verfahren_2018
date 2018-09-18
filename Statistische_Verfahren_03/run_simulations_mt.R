data = read.csv('../statistische_verfahren_2018/data/islands.csv')
data<-data[,-1]
colnames(data)
# create our model
glm.model <- glm(data$mt.presence ~ data$dist.isl.MF + data$dist.group + data$no.ramet + 
      data$dist.land + data$trees + data$dens.ramet + data$dens.group + data$trees:data$no.ramet, 
      family = binomial(link = "logit"), 
      data = data)
summary(glm.model)
# save our design matrix
X <- model.matrix(glm.model)
colnames(X)
X

# calculate logits from model
logits <- X %*% coefficients(glm.model)
logits

# calculate probabilities from logits
probabilities <- exp(logits) / (exp(logits) + rep(1, NROW(logits) ))
probabilities 

### run simulations for sample size, for experiment

# just do one experiment first for a sample size of 10
no.experiments <- 1000

# select design matrix rows
no.samples <- 200
with_replacement <- T
rows <- c(sample(1:NROW(data), no.samples, replace=with_replacement))
rows

new_X <- X[rows,]
new_X <- as.data.frame(new_X)
colnames(new_X)

new_X
# rename columns for easier access
colnames(new_X) <- c('intercept', 'dist.isl.MF', 'dist.group', 'no.ramet', 'dist.land', 'trees', 'dens.ramet', 'dens.group', 'int.ramet.trees')
colnames(new_X)


simulations <- vector()

# run the experiment no.experiments times
for (i in 1:no.experiments)
{
  # simulate unobserved data
  new_y <- rbinom(no.samples, 1, probabilities)
  new_y
  
  # append current results to the design matrix
  current_X <- cbind(new_X, new_y)
  current_X
  
  # find model parameters
  # if only 10, algorithm does not converge?
  current_experiment <- glm(current_X$new_y ~ dist.isl.MF + dist.group + no.ramet + 
                              dist.land + trees + dens.ramet + dens.group + trees:no.ramet,
                            data = current_X, family = binomial(link="logit"))
  summary(current_experiment)
 #  print(coefficients(current_experiment))
  
  # print(as.numeric(coefficients(current_experiment)))
  # TODO save parameters
  simulations <- rbind(simulations, as.numeric(coefficients(current_experiment)))
}

# necessary imports
library('matrixcalc')
library('mvtnorm')

# calculate covariance
simulations.num <- simulations
class(simulations.num)
r <- nrow(simulations.num)
c <- ncol(simulations.num)

simulations.num <- as.matrix(simulations.num, nrow=r, ncol=c)
class(simulations.num)
simulations.cov <- cov(simulations.num)

coeff <- coefficients(glm.model)

# model approximate distribution
approx_distribution <- rmvnorm(n=1000, mean=coeff, sigma=simulations.cov)
approx_distribution

max.val <- max(max(approx_distribution[,9]), simulations.num[,9])
min.val <- min(min(approx_distribution[,9]), simulations.num[,9])

i=3
name_approx <- paste('Approximatierte Normalverteilung', colnames(new_X)[i])
plot(hist(approx_distribution[,i]), col=rgb(0,0,1,1/4), main=name_approx)

i=9
name_sim <- paste('Simulierte Verteilung', colnames(new_X)[i])
plot(hist(simulations.num[,i]), col=rgb(1,0,0,1/4), main=name_sim)

library(entropy)
KL.empirical(approx_distribution[,1], simulations.num[,1])

