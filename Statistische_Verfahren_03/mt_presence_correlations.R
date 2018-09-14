data = read.csv('../statistische_verfahren_2018/data/islands.csv')

numeric_data <- data
numeric_data
# remove names
colnames(numeric_data)
numeric_data <- numeric_data[,-1]
c(colnames(numeric_data))

# transform to numeric data
numeric_data <- mapply(numeric_data, FUN=as.numeric)
numeric_data <- as.data.frame(numeric_data)

# first, compute correlations to get an idea of the data
mt.p.values <- rep(1, ncol(numeric_data))
for (i in 1:ncol(numeric_data))
{
  corr_test <- cor.test(numeric_data[,i], numeric_data$mt.presence, 'two.sided', 'kendall')
  print(corr_test)
  print(colnames(numeric_data)[i])
  mt.p.values[i] <- corr_test$p.value
}

mt.p.values <- as.list(setNames(mt.p.values, colnames(numeric_data)))
mt.p.values

mt.p <- mt.p.values < 0.05
mt.p

corr.matrix <- correlation_matrix <- cor(numeric_data, method = c("kendall"))
corr.matrix

corr_test_dist_group <- cor.test(numeric_data$dist.group, numeric_data$mt.presence, 'two.sided', 'kendall')
corr_test_size <- cor.test(numeric_data$size, numeric_data$mt.presence, 'two.sided', 'kendall')
corr_test_ramet <- cor.test(numeric_data$no.ramet, numeric_data$mt.presence, 'two.sided', 'kendall')
corr_test <- cor.test(numeric_data$mf.presence, numeric_data$mt.presence, 'two.sided', 'kendall')
print(corr_test)

library(corrplot)

corrplot(correlation_matrix, method='number')
