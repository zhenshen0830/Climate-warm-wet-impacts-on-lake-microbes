# Load required libraries
library(lavaan)        #For structural equation modeling
library(semPlot)      #For visualizing SEM models

#Set working directory
setwd("E:/Climate warm-wet impacts on lake microbes/Fig.6")
# Read data
SEM <- read.csv("SEM.csv",row.names = 1)

#---Dimensionality Reduction for Environmental Parameters
data1 <- SEM[,c(6:10)]
fa1 <- princomp(data1,cor=T) 
summary(fa1,loadings=T)
pca_water_properties <- predict(fa1)
environment <- data.frame(pca_water_properties[,1])
environment

#---Dimensionality Reduction for Climate Data
data2 <- SEM[,c(2:5)]
fa2 <- princomp(data2,cor=T) 
summary(fa2,loadings=T)
pca_Climate <- predict(fa2)
climate <- data.frame(pca_Climate[,1])
climate

#---Dimensionality Reduction for Diversity Data
data3 <- SEM[,c(11:13)]
fa3 <- princomp(data3,cor=T) 
summary(fa3,loadings=T)
pca_Diversity_index <- predict(fa3)
diversity <- data.frame(pca_Diversity_index[,1])
diversity

#Combine PCA results into final dataset
SEM_plotdata <- cbind(climate,environment,diversity)
colnames(SEM_plotdata) = c("climate","environment","diversity")
SEM_plotdata

#write.csv(SEM_plotdata,'SEM_plotdata.csv', quote = T, row.names= T)
SEM_plotdata <- read.csv("SEM_plotdata.csv",row.names = 1)

#---Structural Equation Modeling (SEM)---
# Define SEM model structure
model1 <- 'environment ~ climate
       diversity ~ environment
       ecological_stability ~ diversity 
       ecological_stability ~ climate
       ecological_stability ~ environment
       functional_structure ~ diversity + ecological_stability
       functional_structure ~~ FRI
       FRI ~ environment + diversity + ecological_stability
       FRI ~ climate' 

Fit <- lavaan::sem(model1, data=  SEM_plotdata)
summary(Fit, rsquare=T, standardized=T,fit.measures=TRUE)

out <- fitMeasures(Fit,c("chisq","df","pvalue","cfi","nfi","ifi","rmsea","EVCI","AIC","BIC"))
out
write.csv(out,file="SEM_model_out.csv",row.names= T) 

out1 <- residuals(Fit, type="cor")
out1
write.csv(out1,file="residuals_out.csv",row.names= T)
#Check modification indices for model improvement
mi <- modificationIndices(Fit)
print(mi)

#Visualize SEM model
semPaths(Fit,"std",residuals=F,
         nCharNodes=4,
         layout="tree2",
         edge.label.cex = 1,
         sizeMan =6)