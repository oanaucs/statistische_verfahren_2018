data = read.csv('../statistische_verfahren_2018/data/islands.csv')

# create our model
glm.final <- glm(data$mt.presence ~ data$no.hab:data$no.group + data$no.hab:data$dist.land + data$trees:data$dis.isl.MT + data$trees:data$size, 
                 data = data, family = binomial(link="logit"))
summary(glm.final)

# save our design matrix
X <- model.matrix(glm.final)
X

# calculate logits from model
logits <- X %*% coefficients(glm.final)
logits
# calculate probabilities from logits
probabilities <- exp(logits) / (exp(logits) + rep(1, NROW(logits) ))
probabilities 

### run simulations for sample size, for experiment

# just do one experiment first for a sample size of 10
no.experiments <- 1

# select design matrix rows
no.samples <- 10
with_replacement <- F
rows <- c(sample(1:NROW(data), no.samples, replace=with_replacement))
rows

new_X <- X[rows,]
new_X <- as.data.frame(new_X)
new_X

# rename variables
names(new_X)
names(new_X)[2]<-"no.hab.no.group"
names(new_X)[3]<-"no.hab.dist.land"
names(new_X)[4]<-"trees.dis.isl.MT"
names(new_X)[5]<-"trees.size"
names(new_X)

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
  current_experiment <- glm(current_X$new_y ~ current_X$no.hab.no.group + current_X$no.hab.dist.land + current_X$trees.dis.isl.MT + current_X$trees.size,
                            data = current_X, family = binomial(link="logit"))
  summary(current_experiment)
}
