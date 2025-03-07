library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggalluvial)
library(ggsignif)
library(gghalves)

rm(list = ls())
setwd("D:/Climate warm-wet impacts on lake microbes/Fig.2D")
data <- read.csv('Alpha-bacterial.csv')
observed_ASVs <- data[,c(2,3)]
shannon <- data[,c(2,4)]
faith_PD <- data[,c(2,5)]

p1 <- ggplot(observed_ASVs, aes(x=Type, y=observed_ASVs,  fill = Type))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="A",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="B",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="A",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="B",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="B",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="A",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="B",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="A",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_compare_means(label.x = 0.65, label.y = 680, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  labs(x=NULL,y="observed_ASVs")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p1
wilcox.test(observed_ASVs~ Type, data = observed_ASVs)
ggsave("p1.pdf",device="pdf",width = 5, height = 4)
###################

p2 <- ggplot(shannon, aes(x=Type, y=shannon, fill = Type))+
  geom_half_violin(data = shannon[shannon$Type=="A",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = shannon[shannon$Type=="B",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = shannon[shannon$Type=="A",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = shannon[shannon$Type=="B",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = shannon[shannon$Type=="B",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="A",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="B",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="A",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_compare_means(label.x = 0.65, label.y = 8, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  labs(x=NULL,y="shannon")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p2
wilcox.test(shannon~ Type, data = shannon)
ggsave("p2.pdf",device="pdf",width = 5, height = 4)
###################################

p3 <- ggplot(faith_PD, aes(x=Type, y=faith_PD,  fill = Type))+
  geom_half_violin(data =faith_PD[faith_PD$Type=="A",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = faith_PD[faith_PD$Type=="B",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = faith_PD[faith_PD$Type=="A",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = faith_PD[faith_PD$Type=="B",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = faith_PD[faith_PD$Type=="B",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = faith_PD[faith_PD$Type=="A",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = faith_PD[faith_PD$Type=="B",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = faith_PD[faith_PD$Type=="A",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+

  stat_compare_means(label.x = 0.65, label.y = 95, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,80), breaks = seq(0,75,25))+
  labs(x=NULL,y="faith_PD")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p3
wilcox.test(faith_PD~ Type, data = faith_PD)
ggsave("p3.pdf",device="pdf",width = 5, height = 4)
ggarrange(p1,p2,p3,ncol=3,nrow=1)



########################################################################################
########################################################################################
data <- read.csv('Alpha-eukaryotic.csv')
observed_ASVs <- data[,c(2,3)]
shannon <- data[,c(2,4)]
faith_PD <- data[,c(2,5)]

p11 <- ggplot(observed_ASVs, aes(x=Type, y=observed_ASVs,  fill = Type))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="A",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="B",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="A",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="B",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="B",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="A",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="B",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="A",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_compare_means(label.x = 0.65, label.y = 680, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  labs(x=NULL,y="observed_ASVs")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p11
wilcox.test(observed_ASVs~ Type, data = observed_ASVs)
ggsave("p11.pdf",device="pdf",width = 5, height = 4)
############################

p22 <- ggplot(shannon, aes(x=Type, y=shannon, fill = Type))+
  geom_half_violin(data = shannon[shannon$Type=="A",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = shannon[shannon$Type=="B",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = shannon[shannon$Type=="A",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = shannon[shannon$Type=="B",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = shannon[shannon$Type=="B",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="A",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="B",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="A",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_compare_means(label.x = 0.65, label.y = 8, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  scale_y_continuous(expand = c(0,0), limits = c(2,7.7), breaks = seq(0,7.5,2.5))+
  labs(x=NULL,y="shannon")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p22
wilcox.test(shannon~ Type, data = shannon)
ggsave("p22.pdf",device="pdf",width = 5, height = 4)
#######################

p33 <- ggplot(faith_PD, aes(x=Type, y=faith_PD, fill = Type))+
  geom_half_violin(data =faith_PD[faith_PD$Type=="A",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = faith_PD[faith_PD$Type=="B",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = faith_PD[faith_PD$Type=="A",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = faith_PD[faith_PD$Type=="B",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = faith_PD[faith_PD$Type=="B",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = faith_PD[faith_PD$Type=="A",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = faith_PD[faith_PD$Type=="B",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = faith_PD[faith_PD$Type=="A",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_compare_means(label.x = 0.65, label.y = 95, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,50), breaks = seq(0,50,15))+
  labs(x=NULL,y="faith_PD")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p33
wilcox.test(faith_PD~ Type, data = faith_PD)
ggsave("p33.pdf",device="pdf",width = 5, height = 4)
