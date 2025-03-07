library(tidyverse)
library(ggsankey)
library(ggplot2)
library(ggsci)
library(ggalluvial)
library(reshape2)
library(dplyr)
setwd("D:/Climate warm-wet impacts on lake microbes/Fig.3A")

rm(list=ls()) 

dat <- read.csv("Phylum_bacterial.csv")

dat <- melt(dat,id.vars ="X")
colnames(dat)[3]<- c("Freq")
dat1 <-melt(dat ,id='Freq')
type <-summary(dat1$variable)
dat1$flow<-rep(1:type[1],length(type))
color <- c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#BC80BD","#80B1D3",
           "#D9D9D9","#B3DE69","#FCCDE5","#FDB462","#67AFDC","#E18040")
p <- ggplot(dat1,aes(x=variable,y=Freq,stratum=value,alluvium=flow,fill=value))+
     geom_stratum(width = 0.25,color = NA) +
     geom_text(stat='stratum',aes(label = after_stat(stratum)),size=3)+ 
     geom_flow(width = 0.25)+ 
     scale_fill_manual(values=color)+
     scale_x_discrete(labels=c('Group','s_Group'))+ 
     scale_y_continuous(expand=c(0, 0))+
     labs(x ="",y="")+ 
     theme_classic()+
     theme(legend.position ='none',axis.line = element_line(),
           panel.background =element_blank(),
           axis.line.y= element_blank(),
           axis.line.x= element_blank(),
           axis.text.y=element_blank(),
           axis.ticks = element_blank())
p
ggsave("Phylum_bacterial.pdf",device="pdf",width =5, height = 4)


###################################
###################################
###################################                                        
dat <- read.csv("Phylum_eukaryotic.csv")

dat <- melt(dat,id.vars ="X")
colnames(dat)[3]<- c("Freq")
dat1 <-melt(dat ,id='Freq')
type <-summary(dat1$variable)
dat1$flow<-rep(1:type[1],length(type))
color <- c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#B3DE69","#80B1D3",
           "#FDB462","#FCCDE5","#D9D9D9","#BC80BD","#E18040","#67AFDC")            
p <- ggplot(dat1,aes(x=variable,y=Freq,stratum=value,alluvium=flow,fill=value))+
geom_stratum(width = 0.25,color = NA) +
geom_text(stat='stratum',aes(label = after_stat(stratum)),size=3)+ 
geom_flow(width = 0.25)+ 
scale_fill_manual(values=color)+
scale_x_discrete(labels=c('Group','s_Group'))+ 
scale_y_continuous(expand=c(0, 0))+
labs(x ="",y="")+ 
theme_classic()+
theme(legend.position ='none',axis.line = element_line(),
panel.background =element_blank(),
axis.line.y= element_blank(),
axis.line.x= element_blank(),
axis.text.y=element_blank(),
axis.ticks = element_blank())
p
ggsave("Phylum_eukaryotic.pdf",device="pdf",width =5, height = 4)