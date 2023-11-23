matrix<-read.csv('Data.csv',header=T,row.names = 1)
mads<-apply(matrix,1,mad)# mad(x) 
matrix<-matrix[rev(order(mads))[1:1500],]
matrix <- sweep(matrix,1, apply(matrix,1,median,na.rm=T))
library(ConsensusClusterPlus)
title<-tempdir()
matrix<-as.matrix(matrix)
results <- ConsensusClusterPlus(matrix,maxK=10,reps=1000,pItem=0.8,pFeature=1,                             clusterAlg="hc",distance="pearson",seed=1262118388.71279,plot="pdf")
#i=2-10
ki_run1 <- results[[i]][["consensusClass"]]
ki_run2 <- results[[i]][["consensusTree"]]
names(ki_run1[ki_run1==1])
names(ki_run1[ki_run2$order])
write.table(names(ki_run1[ki_run2$order]), "group.txt", sep = "\t")
write.table(ki_run1, "order.txt", sep = "\t")
dev.off()