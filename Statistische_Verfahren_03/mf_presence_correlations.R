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

corr.matrix <- correlation_matrix <- cor(numeric_data, method = c("kendall"))

corr_test <- cor.test(numeric_data$dis.isl.MT, numeric_data$mf.presence, 'two.sided', 'kendall')
print(corr_test)

corr_test_ramet <- cor.test(numeric_data$no.ramet, numeric_data$mf.presence, 'two.sided', 'kendall')
corr_test_hab<- cor.test(numeric_data$no.hab, numeric_data$mf.presence, 'two.sided', 'kendall')


corr_test <- cor.test(numeric_data$mf.presence, numeric_data$mt.presence, 'two.sided', 'kendall')
print(corr_test)

library(corrplot)

corrplot(correlation_matrix, method='number')
