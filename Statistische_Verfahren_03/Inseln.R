########   Blattläuse auf Inseln P3 ##########

data = read.csv('data/islands.csv')
laeuse = data$mf.presence + data$mt.presence
plot(data$no.ramet, laeuse)
plot(data$size, laeuse) # makes sense
plot(data$dist.isl.MF, data$mf.presence)
plot(data$dist.isl.MF, data$mt.presence)
plot(data$no.hab, laeuse) # useable
plot(data$dens.group, laeuse) #useable
plot(data$no.ramet, data$no.group)
# histogramm für trees und läuse
plot(data$mean.height, laeuse) # useable
plot(data$dist.group, laeuse) #useable
plot(data$mt.presence, data$mf.presence) # anders plotten

