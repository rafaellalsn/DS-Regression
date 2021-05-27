rm(list=ls(all=TRUE))

library(ISLR)
library(dplyr) 
library(readxl)

setwd("F:\\RAFAELLA\\Artigo EVASAO")
Dados <- read_excel("TX_EVASAO_FUND.xlsx", sheet=1, col_names=TRUE)

TE = as.numeric(Dados$TE)
IRD = as.numeric(Dados$IRD)
TDI = as.numeric(Dados$TDI)
DSU = as.numeric(Dados$DSU)
ICG = as.numeric(Dados$ICG)
ATU = as.numeric(Dados$ATU)
IED1 = as.numeric(Dados$IED1)
IED2 = as.numeric(Dados$IED2)
IED3 = as.numeric(Dados$IED3)
IED4 = as.numeric(Dados$IED4)
IED5 = as.numeric(Dados$IED5)
IED6 = as.numeric(Dados$IED6)
AFD1 = as.numeric(Dados$AFD1)
AFD2 = as.numeric(Dados$AFD2)
AFD3 = as.numeric(Dados$AFD3)
AFD4 = as.numeric(Dados$AFD4)
AFD5 = as.numeric(Dados$AFD5)

#Matriz de Correlacao
#DadosEvasao = data.frame(IRD,TDI,ICG,DSU,ATU,IED1,IED2,IED3,IED4,IED5,IED6,AFD1,AFD2,AFD3,AFD4,AFD5,TE)
#library(corrplot)
#spearmanT =cor(DadosEvasao, method='spearman')
#library(ggplot2)
DadosEvasao = data.frame(TDI,AFD1,TE) #,IED3,IED2,ICG

#library(tidyverse)
#library(hrbrthemes)
#library(viridis)


library(foreach)
library(iterators)
library(doParallel)
library(tcltk)

set.seed(28)
n <- 100
cl <- makeCluster(2)
registerDoParallel(cl)
time3 <- system.time({
  clusterExport(cl, c("n")) 
  variavel = foreach (i = icount(n), .combine = c, .packages = c('tcltk','caret','e1071','MASS','quantreg','np','ipred','fpc','Metrics')) %dopar% {

  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
  setTkProgressBar(pb, i,sprintf("%d%% done", round(i)))
  Sys.sleep(0.05)
  log2(i)
  
  lista_min = NULL
  lista_max = NULL
  indice_treino = createDataPartition(y=DadosEvasao$TE, p=0.75, list=FALSE)
  treino = DadosEvasao[indice_treino, ]
  treino2 = treino
  teste = DadosEvasao[-indice_treino, ]
  teste2 = teste
  teste2$TE = NULL
  
  lm.fit <- lm(TE~., data = treino)
  kernel.1 <- npregbw(TE~TDI+AFD1, data = treino,ckertype='gaussian', nmulti=1)
  kernel.2 <- npregbw(TE~TDI+AFD1, data = treino,ckertype='uniform', nmulti=1)
  kernel.3 <- npregbw(TE~TDI+AFD1, data = treino,ckertype='epanechnikov', nmulti=1)
  lm.np.fit <- npreg(bws=kernel.1)
  lm.np.fit2 <- npreg(bws=kernel.2)
  lm.np.fit3 <- npreg(bws=kernel.3)
  
  rq.fit.25 <- rq(TE~., data = treino, tau = 0.25)
  rq.fit.5 <- rq(TE~., data = treino, tau = 0.5)
  rq.fit.75 <- rq(TE~., data = treino, tau = 0.75)
  
  quantilica.bw2 <- npcdistbw(TE~TDI+AFD1, data = treino, nmulti=1)
  rqnp.fit.25 <- npqreg(bws = quantilica.bw2, tau=0.25, nmult = 1)
  rqnp.fit.5 <- npqreg(bws = quantilica.bw2, tau=0.5, nmult = 1)
  rqnp.fit.75 <- npqreg(bws = quantilica.bw2, tau=0.75, nmult = 1)
  
  quantilica2.bw2 <- npcdistbw(TE~TDI+AFD1, data = treino, cxkertype='uniform',cykertype='uniform', nmulti=1)
  rqnp2.fit.25 <- npqreg(bws = quantilica2.bw2, tau=0.25, nmulti=1)
  rqnp2.fit.5 <- npqreg(bws = quantilica2.bw2, tau=0.5, nmulti=1)
  rqnp2.fit.75 <- npqreg(bws = quantilica2.bw2, tau=0.75, nmulti=1)
  
  quantilica3.bw2 <- npcdistbw(TE~TDI+AFD1, data = treino,cxkertype='epanechnikov',cykertype='epanechnikov', nmulti=1)
  rqnp3.fit.25 <- npqreg(bws = quantilica3.bw2, tau=0.25,nmulti=1)
  rqnp3.fit.5 <- npqreg(bws = quantilica3.bw2, tau=0.5, nmulti=1)
  rqnp3.fit.75 <- npqreg(bws = quantilica3.bw2, tau=0.75, nmulti=1)
  
  rlm.fit <- rlm(TE~., data = treino, psi = psi.bisquare)
  rlm2.fit <- rlm(TE~., data = treino, psi = psi.hampel)
  rlm3.fit <- rlm(TE~., data = treino, psi = psi.huber)
  rs.fit <- ltsreg(TE~., data = treino)
  
  svml.fit <- svm(TE~., data = treino, kernel = "linear")
  svm.fit <- svm(TE~., data = treino, kernel = "radial")
  svmp.fit <- svm(TE~., data = treino, kernel = "polynomial")
  svms.fit <- svm(TE~., data = treino, kernel = "sigmoid")
  
  library("plot3D")
  library('rgl')
  
  x <- teste$TDI
  y <- teste$AFD1
  z <- teste$TE
  
  fit <- svm(z~x+y,svm='radial')
 
  grid.lines = 26
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  y.pred <- seq(min(y), max(y), length.out = grid.lines)
  xy <- expand.grid( x = x.pred, y = y.pred)
  z.pred <- matrix(predict(fit, newdata = xy), 
                   nrow = grid.lines, ncol = grid.lines)
  
  fitpoints <- predict(fit,newdata = teste2)

  scatter3D(x, y, z, pch = 16, cex = 0.6,
            col = ramp.col (col = c("gray", "black"), n = 50, alpha = 1),
            theta = 50, phi = 10, 
            xlab = "TDI", ylab = "AFD1", zlab = "TE",grid.col='magenta',
             surf = list(x = x.pred, y = y.pred, z = z.pred,  
              facets = NA, fit = fitpoints, col='black'))

  
  scatter3D(x, y, z, pch = 16, cex = 0.6, 
            theta = 50, phi = 1, 
            xlab = "TDI", ylab = "AFD1", zlab = "TE",grid.col='magenta',
            surf = list(x = x.pred, y = y.pred, z = z.pred,  
                        facets = NA, fit = fitpoints,grid.col='black'))
  
  
  ###################################
  
  library(plotly)
  library(reshape2)
  
  x <- treino$TDI
  y <- treino$AFD1
  z <- treino$TE
  
  fit <- lm(treino$TE ~ treino$AFD1 +treino$TDI)
  graph_reso <- 0.05
  
  #Setup Axis
  axis_x <- seq(min(x), max(x), by = graph_reso)
  axis_y <- seq(min(y), max(y), by = graph_reso)
  
  #Sample points
  petal_lm_surface <- expand.grid(TDI = axis_x,AFD1 = axis_y,KEEP.OUT.ATTRS = F)
  petal_lm_surface$TE <- predict.lm(fit, newdata = petal_lm_surface)
  petal_lm_surface <- acast(petal_lm_surface, TDI ~ AFD1, value.var = "TE") #y ~ x
  
  
  prediction.lm <- predict(lm.fit, newdata = teste2)
  prediction.lm.np <- predict(lm.np.fit, newdata = teste2)
  prediction.lm.np2 <- predict(lm.np.fit2, newdata = teste2)
  prediction.lm.np3 <- predict(lm.np.fit3, newdata = teste2)
  prediction.rq.25 <- predict(rq.fit.25, newdata = teste2)
  prediction.rq.5 <- predict(rq.fit.5, newdata = teste2)
  prediction.rq.75 <- predict(rq.fit.75, newdata = teste2)
  prediction.rqnp.25 <- predict(rqnp.fit.25, newdata = teste2)
  prediction.rqnp.5 <- predict(rqnp.fit.5, newdata = teste2)
  prediction.rqnp.75 <- predict(rqnp.fit.75, newdata = teste2)
  prediction2.rqnp.25 <- predict(rqnp2.fit.25, newdata = teste2)
  prediction2.rqnp.5 <- predict(rqnp2.fit.5, newdata = teste2)
  prediction2.rqnp.75 <- predict(rqnp2.fit.75, newdata = teste2)
  prediction3.rqnp.25 <- predict(rqnp3.fit.25, newdata = teste2)
  prediction3.rqnp.5 <- predict(rqnp3.fit.5, newdata = teste2)
  prediction3.rqnp.75 <- predict(rqnp3.fit.75, newdata = teste2)
  prediction.rlm <- predict(rlm.fit, newdata = teste2)
  prediction.rlm2 <- predict(rlm2.fit, newdata = teste2)
  prediction.rlm3 <- predict(rlm3.fit, newdata = teste2)
  prediction.rs <- predict(rs.fit, newdata = teste2)
  prediction.svm <- predict(svm.fit, newdata = teste2)
  prediction.svml <- predict(svml.fit, newdata = teste2)
  prediction.svmp <- predict(svmp.fit, newdata = teste2)
  prediction.svms <- predict(svms.fit, newdata = teste2)
  plot()
  
  plot(teste$TE,prediction.lm.np,xlim=c(0,40),ylim = c(0,40),xlab='actual',ylab='predicted',main = "KR gaussian")
  abline(0,1,col='blue')
  cor(teste$TE,prediction.lm.np)
  
  plot(teste$TE,prediction.svm,xlim=c(0,40),ylim = c(0,10),xlab='actual',ylab='predicted',main = "SVR Radial")
  abline(0,1,col='blue')
  cor(teste$TE,prediction.svm)
  
  plot(teste$TE,prediction.rqnp.5,xlim=c(0,40),ylim = c(0,40),xlab='actual',ylab='predicted',main = "NPRQ Gaussian 0.5")
  abline(0,1,col='blue')
  cor(teste$TE,prediction.rqnp.5)
  
  plot(teste$TE,prediction.lm,xlim=c(0,3),ylim = c(0,3),xlab='actual',ylab='predicted')
  abline(0,1,col='blue')
  cor(teste$TE,prediction.lm)
  
  write.table(mae(prediction.lm, teste$TE), file="LM_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.lm)/teste$TE), file="LM_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.lm, teste$TE), file="LM_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.lm, teste$TE), file="LM_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.lm.np, teste$TE), file="LMNP_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.lm.np)/teste$TE), file="LMNP_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.lm.np, teste$TE), file="LMNP_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.lm.np, teste$TE), file="LMNP_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.lm.np2, teste$TE), file="LMNP2_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.lm.np2)/teste$TE), file="LMNP2_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.lm.np2, teste$TE), file="LMNP2_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.lm.np2, teste$TE), file="LMNP2_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.lm.np3, teste$TE), file="LMNP3_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.lm.np3)/teste$TE), file="LMNP3_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.lm.np3, teste$TE), file="LMNP3_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.lm.np3, teste$TE), file="LMNP3_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rq.25, teste$TE), file="RQ25_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rq.25)/teste$TE), file="RQ25_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rq.25, teste$TE), file="RQ25_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rq.25, teste$TE), file="RQ25_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rq.5, teste$TE), file="RQ5_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rq.5)/teste$TE), file="RQ5_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rq.5, teste$TE), file="RQ5_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rq.5, teste$TE), file="RQ5_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rq.75, teste$TE), file="RQ75_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rq.75)/teste$TE), file="RQ75_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rq.75, teste$TE), file="RQ75_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rq.75, teste$TE), file="RQ75_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rqnp.25, teste$TE), file="RQNP25_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rqnp.25)/teste$TE), file="RQNP25_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rqnp.25, teste$TE), file="RQNP25_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rqnp.25, teste$TE), file="RQNP25_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction2.rqnp.25, teste$TE), file="RQNP25_MAE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction2.rqnp.25)/teste$TE), file="RQNP25_MRE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction2.rqnp.25, teste$TE), file="RQNP25_MSE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction2.rqnp.25, teste$TE), file="RQNP25_RMSE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction3.rqnp.25, teste$TE), file="RQNP25_MAE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction3.rqnp.25)/teste$TE), file="RQNP25_MRE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction3.rqnp.25, teste$TE), file="RQNP25_MSE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction3.rqnp.25, teste$TE), file="RQNP25_RMSE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rqnp.5, teste$TE), file="RQNP5_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rqnp.5)/teste$TE), file="RQNP5_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rqnp.5, teste$TE), file="RQNP5_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rqnp.5, teste$TE), file="RQNP5_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction2.rqnp.5, teste$TE), file="RQNP5_MAE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction2.rqnp.5)/teste$TE), file="RQNP5_MRE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction2.rqnp.5, teste$TE), file="RQNP5_MSE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction2.rqnp.5, teste$TE), file="RQNP5_RMSE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction3.rqnp.5, teste$TE), file="RQNP5_MAE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction3.rqnp.5)/teste$TE), file="RQNP5_MRE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction3.rqnp.5, teste$TE), file="RQNP5_MSE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction3.rqnp.5, teste$TE), file="RQNP5_RMSE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rqnp.75, teste$TE), file="RQNP75_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rqnp.75)/teste$TE), file="RQNP75_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rqnp.75, teste$TE), file="RQNP75_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rqnp.75, teste$TE), file="RQNP75_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction2.rqnp.75, teste$TE), file="RQNP75_MAE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction2.rqnp.75)/teste$TE), file="RQNP75_MRE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction2.rqnp.75, teste$TE), file="RQNP75_MSE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction2.rqnp.75, teste$TE), file="RQNP75_RMSE_2.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction3.rqnp.75, teste$TE), file="RQNP75_MAE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction3.rqnp.75)/teste$TE), file="RQNP75_MRE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction3.rqnp.75, teste$TE), file="RQNP75_MSE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction3.rqnp.75, teste$TE), file="RQNP75_RMSE_3.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rlm, teste$TE), file="RLM_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rlm)/teste$TE), file="RLM_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rlm, teste$TE), file="RLM_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rlm, teste$TE), file="RLM_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rlm2, teste$TE), file="RLM2_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rlm2)/teste$TE), file="RLM2_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rlm2, teste$TE), file="RLM2_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rlm2, teste$TE), file="RLM2_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rlm3, teste$TE), file="RLM3_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rlm3)/teste$TE), file="RLM3_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rlm3, teste$TE), file="RLM3_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rlm3, teste$TE), file="RLM3_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.rs, teste$TE), file="RS_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.rs)/teste$TE), file="RS_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.rs, teste$TE), file="RS_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.rs, teste$TE), file="RS_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.svm, teste$TE), file="SVMR_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.svm)/teste$TE), file="SVMR_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.svm, teste$TE), file="SVMR_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.svm, teste$TE), file="SVMR_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.svml, teste$TE), file="SVML_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.svml)/teste$TE), file="SVML_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.svml, teste$TE), file="SVML_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.svml, teste$TE), file="SVML_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.svmp, teste$TE), file="SVMP_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.svmp)/teste$TE), file="SVMP_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.svmp, teste$TE), file="SVMP_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.svmp, teste$TE), file="SVMP_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
  write.table(mae(prediction.svms, teste$TE), file="SVMS_MAE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mean(abs(teste$TE - prediction.svms)/teste$TE), file="SVMS_MRE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(mse(prediction.svms, teste$TE), file="SVMS_MSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  write.table(rmse(prediction.svms, teste$TE), file="SVMS_RMSE.txt",row.names=TRUE, col.names=FALSE, sep=";",append = TRUE)
  
 # rm( lm.fit,kernel.1,kernel.2, kernel.3 ,lm.np.fit,lm.np.fit2,lm.np.fit3,rq.fit.25 ,rq.fit.5 ,rq.fit.75,quantilica.bw2 ,rqnp.fit.25, rqnp.fit.5,rqnp.fit.75 ,quantilica2.bw2 ,rqnp2.fit.25,rqnp2.fit.5 ,rqnp2.fit.75,quantilica3.bw2, rqnp3.fit.25,rqnp3.fit.5, rqnp3.fit.75,rlm.fit,rlm2.fit,rlm3.fit, rs.fit , svml.fit,svm.fit,svmp.fit,svms.fit,prediction.lm,prediction.lm.np,prediction.lm.np2,prediction.lm.np3,prediction.rq.25,prediction.rq.5,prediction.rq.75,prediction.rqnp.25,prediction.rqnp.5,prediction.rqnp.75, prediction2.rqnp.25,prediction2.rqnp.5, prediction2.rqnp.75,prediction3.rqnp.25, prediction3.rqnp.5,prediction3.rqnp.75,prediction.rlm,prediction.rlm2,prediction.rlm3,prediction.rs,prediction.svm, prediction.svml,prediction.svmp, prediction.svms )
  
 }
})

#hist(svrr_mae)
#hist(svrr_rmse)
#shapiro.test(svrr_mae) 
#shapiro.test(svrr_rmse) 
#hist(lmnpG_mae)
#hist(lmnpG_rmse)
#shapiro.test(lmnpG_mae) 
#shapiro.test(lmnpG_rmse) 
#hist(rqnp5_mae)
#hist(rqnp5_rmse)
#shapiro.test(rqnp5_mae) 
#shapiro.test(rqnp5_rmse) 

#wilcox.test(svrr_mae,rqnp5_mae,paired = TRUE, alternative = "less")
#wilcox.test(lmnpG_rmse,svrr_rmse,paired = TRUE, alternative = "less")

stopCluster(cl)
print(time3)

