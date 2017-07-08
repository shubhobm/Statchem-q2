##
rm(list=ls())
setwd('D:/Study/My projects/Statchem-q2/Codes')

library(glmnet)
library(caret)

## generate data
n = 100

do.all = function(p){
  set.seed(07072017)
  X = matrix(rnorm(n*p), nrow=n)
  beta = c(rep(1,10), rep(0,p-10))
  e = rnorm(n)
  y = X %*% beta + e
  
  ## actual signal
  err0 = c(mean(e^2), 1-mean(e^2)/mean(y^2))
  
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
  rbind(err0,
        colMeans(err.mat1),
        c(mean(err.vec2), 1-mean(err.vec2)/mean(y^2)),
        err.mat4)
}

out100 = do.all(100)
out500 = do.all(500)
out1000 = do.all(1000)
out.all = list(out100, out500, out1000)
save(out.all, file="out_simulation.Rda")
