library(GSVA)
gmtpath <-"c2.cp.v7.4.symbols.gmt" 
datapath <-"Data.csv"
geneSets <- getGmt(gmtpath)
mydata <- read.csv(datapath, header = T, row.names = 1)
mydata <- as.matrix(mydata)
res_es <- gsva(mydata, geneSets, min.sz=10, max.sz=500, verbose=FALSE, parallel.sz=1)
write.csv(res_es,"Data_GSVA.csv") 