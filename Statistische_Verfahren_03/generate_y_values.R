data = read.csv('../statistische_verfahren_2018/data/islands.csv')

glm.final <- glm(data$mt.presence ~ data$no.hab:data$no.group + data$no.hab:data$dist.land + data$trees:data$dis.isl.MT + data$trees:data$size, 
                 data = data, family = binomial(link="logit"))
summary(glm.final)

fisher_matrix <- summary(glm.final)$cov.scaled
fisher_matrix

### simulate new, unobserved variables
X <- model.matrix(glm.final)
X

logits <- X %*% coefficients(glm.final)
logits
# calculate probabilities from logits
probabilities <- exp(logits) / (exp(logits) + rep(1, NROW(logits) ))
probabilities 

# draw from binomial distribution
new_y <- rbinom(100, 1, probabilities)
new_y
