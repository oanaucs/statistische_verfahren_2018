#####Model####
data = read.csv('data/islands.csv')
data$trees <- as.factor(data$trees)
data$mt.presence <- as.factor(data$mt.presence)
data$mf.presence <- as.factor(data$mf.presence)

# ?glm() ... For binomial and quasibinomial families the response can also be specified as a factor (when the first level denotes failure and all others success) or as a two-column matrix with the columns giving the numbers of successes and failures.??


##### MT #####
#dist.grooup
#no.group
#no.hab
#no.ramet

glm.fit.mt.1 <- glm(mt.presence ~  no.ramet + no.group + dist.group + as.factor(trees), data = data, family = binomial) ##trees
summary(glm.fit.mt.1)
plot(glm.fit.1, which = 1)


glm.fit.mt.2 <- glm(mt.presence ~  no.ramet + dist.group:no.group + as.factor(trees), data = data, family = binomial) ##trees
summary(glm.fit.mt.2)
plot(glm.fit.mt.2, which = 1)

glm.fit.mt.3 <- glm(mt.presence ~  dens.ramet:no.ramet + dist.group:no.group + as.factor(trees), data = data, family = binomial) ##trees #####
#summary(glm.fit)
plot(glm.fit.3, which = 1)

glm.fit.mt.4 <- glm(mt.presence ~  no.ramet:no.group + dist.group + no.hab, data = data, family = binomial) ##trees
#summary(glm.fit)
plot(glm.fit.4, which = 1)

glm.fit.mt.5 <- glm(mt.presence ~  no.ramet:no.group, data = data, family = binomial) ##trees
#summary(glm.fit)
plot(glm.fit.5, which = 1)

glm.fit.mt.6 <- glm(mt.presence ~  no.hab, data = data, family = binomial) ##trees
#summary(glm.fit)
plot(glm.fit.5, which = 1)


#### PLOT ####
plot(mt.presence~size, data = data)
#abline(glm.fit.3, col=2)
points(data$size, predict(glm.fit.mf.3), col = 4)


##### MF #####
#no.hab - Anzahl der unterschiedlichen Habitate
#size - Größe der Insel
#no.group - Anzahl der vorhandenen Gruppen von Rainfarn

glm.fit.mf.1 <- glm(factor(mf.presence) ~  1+size + no.hab + no.group, data = data, family = binomial)
plot(glm.fit.mf.1, which = 1)

glm.fit.mf.2 <- glm(factor(mf.presence) ~ no.hab*size + no.group*size, data = data, family = binomial)

glm.fit.mf.3 <- glm(factor(mf.presence) ~ no.hab*size + no.group, data = data, family = binomial)

glm.fit.mf.4 <- glm(factor(mf.presence) ~ no.hab:size + no.group:size, data = data, family = binomial)

glm.fit.mf.5 <- glm(factor(mf.presence) ~ no.hab:size + size + no.group, data = data, family = binomial)
plot(glm.fit.mf.5, which = 1)

glm.fit.mf.6 <- glm(factor(mf.presence) ~ no.group+size, data = data, family = binomial) 
summary(glm.fit.mf.6)
#plot(glm.fit.mf.6, which = 1)
#plot.glm(glm.fit.mf.6)

glm.fit.mf.7 <- glm(factor(mf.presence) ~ no.group+I(size)+no.hab, data = data, family = binomial)

glm.fit.mf.8 <- glm(factor(mf.presence) ~ size, data = data, family = binomial) 
summary(glm.fit.mf.1)

#### PLOT ####
plot(mf.presence~no.group, data = data)
#abline(glm.fit.3, col=2)
points(data$no.group, predict(glm.fit.mf.6), pch = "+",col = 2)

plot(mf.presence~size, data = data)
points(data$no.group, predict(glm.fit.mf.6), pch = "+",col = 2)
points(data$no.group, predict(glm.fit.mf.7), pch = "#",col = 3)

