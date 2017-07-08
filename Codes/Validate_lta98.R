##
rm(list=ls())
setwd('D:/Study/My projects/Statchem-q2/Codes')

library(glmnet)
library(caret)
load('../Data/lta98.rda')

set.seed(07072017)
y = lta98$Y[-1]
X = as.matrix(with(lta98, cbind(ltaTS,ltaTC,lta3D, ltaQC))[-1,])
n = nrow(X)
p = ncol(X)

## shall use LASSO
## 5-fold cv
cat("5-fold cv\n")
err.mat1 = matrix(0, nrow=5, ncol=2)
folds = createFolds(1:n, k=5)
pb = txtProgressBar(0,5)
for(i in 1:5){
  test = folds[[i]]
  cv.obj1 = cv.glmnet(X[-test,],y[-test])
  err1 = mean((y[test] - predict(cv.obj1, X[test,], s="lambda.min"))^2)
  err.mat1[i,] = c(err1, 1- err1/mean(y[test]^2))
  setTxtProgressBar(pb,i)
}
close(pb)

## LOO cv
cat("LOO cv\n")
err.vec2 = rep(0,n)
pb = txtProgressBar(0,n)
for(rep2 in 1:n){
  cv.obj2 = cv.glmnet(X[-rep2,],y[-rep2])
  err.vec2[rep2] = (y[rep2] - sum(c(1,X[rep2,]) * as.numeric(coef(cv.obj2, s="lambda.min"))))^2
  setTxtProgressBar(pb,rep2)
}
close(pb)

## external validation
# test = sample(1:n, 10, replace=F)
# cv.obj3 = cv.glmnet(X[-test,],y[-test])
# mean((y[test] - predict(cv.obj3, X[test,], s="lambda.min"))^2)

## repeated external validation
cat("repeated external cv\n")
err.mat4 = matrix(0, nrow=100, ncol=2)
pb = txtProgressBar(0,100)
for(rep4 in 1:100){
  test = sample(1:n, 10, replace=F)
  cv.obj4 = cv.glmnet(X[-test,],y[-test])
  err4 = mean((y[test] - predict(cv.obj4, X[test,], s="lambda.min"))^2)
  err.mat4[rep4,] = c(err4, 1- err4/mean(y[test]^2))
  setTxtProgressBar(pb,rep4)
}
close(pb)

## return
out.all = rbind(colMeans(err.mat1),
                c(mean(err.vec2), 1-mean(err.vec2)/mean(y^2)),
                err.mat4)

save(out.all, file="out_lta98.Rda")
