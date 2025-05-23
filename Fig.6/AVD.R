
rm(list = ls())
setwd("E:/sz")

otu <- read.delim('ASV-bac.txt', row.names = 1, sep = '\t', check.names = FALSE)

ai <- abs(otu-apply(otu, 1, mean))/apply(otu, 1, sd)

avd <- colSums(ai)/(1*nrow(otu))

group <- read.delim('group-B.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
group$AVD <- avd
group

write.csv(group,"bac_AVD.csv")
