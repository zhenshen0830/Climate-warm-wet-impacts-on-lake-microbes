setwd("E:/SZ")
rm(list=ls())
pacman::p_load(tidyverse,microeco,aplot,ggsci,reshape)

feature_table <- read.csv('ASV-bac.csv', row.names = 1)
sample_table <- read.csv('metadata-bac.csv', row.names = 1)
tax_table <- read.csv('taxonomy-bac.csv', row.names = 1)

dataset <- microtable$new(sample_table = sample_table,
                          otu_table = feature_table, 
                          tax_table = tax_table)

t2 <- trans_func$new(dataset)
t2$cal_spe_func(prok_database = "FAPROTAX")
t2$res_spe_func[1:5, 1:2]

t2$cal_spe_func_perc(abundance_weighted = FALSE)
t2$res_spe_func_perc[1:5, 1:2]
t2$res_spe_func_perc
write.csv(t2$res_spe_func_perc, "16S_FAPROTAX.csv")
