##
rm(list=ls())
# setwd('D:/Study/My projects/Statchem-q2/Codes')

summarize = function(err.matrix){
  rbind(err.matrix[1:3,],
        apply(err.matrix[-(1:3),],2,min),
        apply(err.matrix[-(1:3),],2,function(x) quantile(x, .25)),
        apply(err.matrix[-(1:3),],2,median),
        apply(err.matrix[-(1:3),],2,function(x) quantile(x, .75)),
        apply(err.matrix[-(1:3),],2,max),
        apply(err.matrix[-(1:3),],2,mean))
}

load('out_sim_p100.Rda')
summary.list = lapply(out100, summarize)
round(apply(simplify2array(summary.list),1:2,mean),2)
round(apply(simplify2array(summary.list),1:2,sd),3)

load('out_sim_p500.Rda')
summary.list = lapply(out500, summarize)
round(apply(simplify2array(summary.list),1:2,mean),2)
round(apply(simplify2array(summary.list),1:2,sd),3)

load('out_sim_p1000.Rda')
summary.list = lapply(out1000, summarize)
round(apply(simplify2array(summary.list),1:2,mean),2)
round(apply(simplify2array(summary.list),1:2,sd),3)
