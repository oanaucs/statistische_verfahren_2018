data = read.csv('../statistische_verfahren_2018/data/islands.csv')
data<-data[,-1]
colnames(data)
# create our model
model <- glm(data$mt.presence ~ data$dist.isl.MF + data$dist.group + data$no.ramet + 
      data$dist.land + data$trees + data$dens.ramet + data$dens.group + data$trees:data$no.ramet, 
      family = binomial(link = "logit"), 
      data = data)
summary(model)
# save our design matrix
X <- model.matrix(model)
colnames(X)

# calculate logits from model
logits <- X %*% coefficients(model)
logits

# calculate probabilities from logits
probabilities <- exp(logits) / (exp(logits) + rep(1, NROW(logits) ))
probabilities 

### run simulations for sample size, for experiment

# just do one experiment first for a sample size of 10
no.experiments <- 100

# select design matrix rows
no.samples <- 30
with_replacement <- F
rows <- c(sample(1:NROW(data), no.samples, replace=with_replacement))
rows

new_X <- X[rows,]
new_X <- as.data.frame(new_X)
colnames(new_X)

# rename columns for easier access
colnames(new_X) <- c('intercept', 'dist.isl.MF', 'dist.group', 'no.ramet', 'dist.land', 'trees', 'dens.ramet', 'dens.group', 'int.ramet.trees')
colnames(new_X)


simulations <- vector()
simulations

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
  as.numeric(coefficients(current_experiment))
  # TODO save parameters
  simulations <- cbind(simulations, as.numeric(coefficients(current_experiment)))
}

simulations

# necessary imports
library('matrixcalc')
library('mvtnorm')

# calculate covariance
simulations.num <- simulations[,-1]
class(simulations.num)
r <- nrow(simulations.num)
r
c <- ncol(simulations.num)
c
simulations.num <- as.matrix(simulations.num, nrow=r, ncol=c)
simulations.num
class(simulations.num)
simulations.cov <- cov(simulations.num)
simulations.cov

sigma <- simulations.cov * matrix.inverse(t(X) %*% X)
# check if matrix is symmetric - condition for distribution
isSymmetric(sigma)

coeff <- coefficients(model)

# model approximate distribution
approx_distribution <- rmvnorm(n=100, mean=coeff, sigma=sigma)
approx_distribution

# visualize histograms
for (i in 1:ncol(approx_distribution))
{
  colname <- colnames(data)[i]
  first <- hist(approx_distribution[,i], main=colname)
  second <- hist(simulations.num[,i], main=colname)
  max.val <- max(max(approx_distribution[,i]), simulations.num[,i])
  min.val <- min(min(approx_distribution[,i]), simulations.num[,i])
  plot(first, col=rgb(0,0,1,1/4), xlim=c(min.val, max.val), main=colname)
  plot(second, col=rgb(1,0,0,1/4), add=T)
}
