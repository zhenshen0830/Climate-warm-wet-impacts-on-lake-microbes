rm(list = ls())
library(lmerTest)
library(readxl)
library(lme4)
library(vegan)
library(ade4)
library(ggplot2)
library(ggeffects)
library(broom)
library(broom.mixed)
library(car)

df <- read.csv('bacteria_physicochemical.csv')
df$WT <- scale(df$WT)   
df$TN <- scale(df$TN)
df$Chl.a <- scale(df$Chl.a) 
df$TDS <- scale(df$TDS)
df$WL <- scale(df$WL) 

head(df)

mod1 <- lmer(shannon~TN+WT+Chl.a+TDS+WL+ (1|Site),data = df)
summary(mod1)

mod2 <- lmer(shannon~TN+WT+Chl.a+TDS+WL+ (0+Type|Site),data = df)
summary(mod2)

mod3 <- lmer(shannon~TN+WT+Chl.a+TDS+WL+ (Type|Site),data = df)
summary(mod3)

mod4 <- lmer(shannon~TN+WT+Chl.a+TDS+WL+ (Type||Site),data = df)
summary(mod4)

anova(mod1, mod2,mod3,mod4)

df_new <- df

library(glmm.hp)
mod<-lmer(shannon ~ TN + WT + Chl.a + TDS + WL + (1 | Site),data=df_new)

summary(mod)
V<-glmm.hp(mod)
V
write.csv(V$hierarchical.partitioning,"shannon Contribution.csv")


p<-summary(mod)
p
p$coefficients
write.csv(p$coefficients,"bacteria_effect size.csv")