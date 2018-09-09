data = read.csv('../statistische_verfahren_2018/data/islands.csv')
data
colnames(data)
numeric_data <- data[,-1]
numeric_data <- mapply(numeric_data, FUN=as.numeric)

boxplot(data$dis.isl.MT, data=data, main="No.group")

names <- colnames(numeric_data)
normality_tests <- rep(0, length(names))

shapiro.test(numeric_data[,1])$p

for (i in 1:length(names))
{
  normality_tests[i] <- shapiro.test(numeric_data[,i])$p > 0.05
  hist(numeric_data[,i])
}

names
# here we obtain some nice lines, but the response variable is not mt.presence..
interaction.plot(data$no.hab, data$mt.presence, data$no.group)
interaction.plot(data$no.hab, data$mf.presence, data$no.group)

interaction.plot(data$trees, data$mt.presence, data$size)
interaction.plot(data$trees, data$mf.presence, data$size)

# does this mean there just isn't any interaction there?
interaction.plot(data$dens.ramet, data$mt.presence, data$dens.group)


interaction.plot(data$trees, data$mf.presence, data$size)

