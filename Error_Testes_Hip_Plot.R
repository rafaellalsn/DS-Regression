#rm(list=ls(all=TRUE))
#set.seed(28)

#library(ISLR)
#library(dplyr) 
#library(readxl)
#setwd("F:\\RAFAELLA\\Artigo EVASAO")
#Dados <- read_excel("TX_EVASAO_FUND.xlsx", sheet=1, col_names=TRUE)
#library(corrplot)
#library(tidyverse)
#library(hrbrthemes)
#library(viridis)
#library(foreach)
#library(iterators)
#library(doParallel)
#library(tcltk)

setwd("F:\\RAFAELLA\\Artigo EVASAO\\ERRO 100 EXEC FINAL")
ERRO_MAE1 <- read.table("LM_MAE.txt", header =FALSE, sep = ';')
lm_mae = ERRO_MAE1$V2
mean(ERRO_MAE1$V2)
sd(ERRO_MAE1$V2)

ERRO_MRE1 <- read.table("LM_MRE.txt", header =FALSE, sep = ';')
lm_mre = ERRO_MRE1$V2
mean(ERRO_MRE1$V2)
sd(ERRO_MRE1$V2)

ERRO_RMSE1 <- read.table("LM_RMSE.txt", header =FALSE, sep = ';')
lm_rmse = ERRO_RMSE1$V2
mean(ERRO_RMSE1$V2)
sd(ERRO_RMSE1$V2)

#----------------------------------2----------------------------

ERRO_MAE2 <- read.table("RLM_MAE.txt", header =FALSE, sep = ';')
rlm_mae = ERRO_MAE2$V2
mean(ERRO_MAE2$V2)
sd(ERRO_MAE2$V2)

ERRO_MRE2 <- read.table("RLM_MRE.txt", header =FALSE, sep = ';')
rlm_mre = ERRO_MRE2$V2
mean(ERRO_MRE2$V2)
sd(ERRO_MRE2$V2)

ERRO_RMSE2 <- read.table("RLM_RMSE.txt", header =FALSE, sep = ';')
rlm_rmse = ERRO_RMSE2$V2
mean(ERRO_RMSE2$V2)
sd(ERRO_RMSE2$V2)

#---------------------------------3----------------------------

ERRO_MAE3 <- read.table("RLM2_MAE.txt", header =FALSE, sep = ';')
rlm_mae2 = ERRO_MAE3$V2
mean(ERRO_MAE3$V2)
sd(ERRO_MAE3$V2)

ERRO_MRE3 <- read.table("RLM2_MRE.txt", header =FALSE, sep = ';')
rlm_mre2 = ERRO_MRE3$V2
mean(ERRO_MRE3$V2)
sd(ERRO_MRE3$V2)

ERRO_RMSE3 <- read.table("RLM2_RMSE.txt", header =FALSE, sep = ';')
rlm_rmse2 = ERRO_RMSE3$V2
mean(ERRO_RMSE3$V2)
sd(ERRO_RMSE3$V2)

#---------------------------------4----------------------------

ERRO_MAE4 <- read.table("RLM3_MAE.txt", header =FALSE, sep = ';')
rlm_mae3 = ERRO_MAE4$V2
mean(ERRO_MAE4$V2)
sd(ERRO_MAE4$V2)

ERRO_MRE4 <- read.table("RLM3_MRE.txt", header =FALSE, sep = ';')
rlm_mre3 = ERRO_MRE4$V2
mean(ERRO_MRE4$V2)
sd(ERRO_MRE4$V2)

ERRO_RMSE4 <- read.table("RLM3_RMSE.txt", header =FALSE, sep = ';')
rlm_rmse3 = ERRO_RMSE4$V2
mean(ERRO_RMSE4$V2)
sd(ERRO_RMSE4$V2)

#---------------------------------5----------------------------

ERRO_MAE5 <- read.table("RQ25_MAE.txt", header =FALSE, sep = ';')
rq25_mae = ERRO_MAE5$V2
mean(ERRO_MAE5$V2)
sd(ERRO_MAE5$V2)

ERRO_MRE5 <- read.table("RQ25_MRE.txt", header =FALSE, sep = ';')
rq25_mre = ERRO_MRE5$V2
mean(ERRO_MRE5$V2)
sd(ERRO_MRE5$V2)

ERRO_RMSE5 <- read.table("RQ25_RMSE.txt", header =FALSE, sep = ';')
rq25_rmse = ERRO_RMSE5$V2
mean(ERRO_RMSE5$V2)
sd(ERRO_RMSE5$V2)

#---------------------------------6----------------------------

ERRO_MAE6 <- read.table("RQ5_MAE.txt", header =FALSE, sep = ';')
rq5_mae = ERRO_MAE6$V2
mean(ERRO_MAE6$V2)
sd(ERRO_MAE6$V2)

ERRO_MRE6 <- read.table("RQ5_MRE.txt", header =FALSE, sep = ';')
rq5_mre = ERRO_MRE6$V2
mean(ERRO_MRE6$V2)
sd(ERRO_MRE6$V2)

ERRO_RMSE6 <- read.table("RQ5_RMSE.txt", header =FALSE, sep = ';')
rq5_rmse = ERRO_RMSE6$V2
mean(ERRO_RMSE6$V2)
sd(ERRO_RMSE6$V2)

#---------------------------------7----------------------------

ERRO_MAE7 <- read.table("RQ75_MAE.txt", header =FALSE, sep = ';')
rq75_mae = ERRO_MAE7$V2
mean(ERRO_MAE7$V2)
sd(ERRO_MAE7$V2)

ERRO_MRE7 <- read.table("RQ75_MRE.txt", header =FALSE, sep = ';')
rq75_mre = ERRO_MRE7$V2
mean(ERRO_MRE7$V2)
sd(ERRO_MRE7$V2)

ERRO_RMSE7 <- read.table("RQ75_RMSE.txt", header =FALSE, sep = ';')
rq75_rmse = ERRO_RMSE7$V2
mean(ERRO_RMSE7$V2)
sd(ERRO_RMSE7$V2)

#---------------------------------8----------------------------

ERRO_MAE8 <- read.table("LMNP_MAE.txt", header =FALSE, sep = ';')
lmnpG_mae = ERRO_MAE8$V2
mean(ERRO_MAE8$V2)
sd(ERRO_MAE8$V2)

ERRO_MRE8 <- read.table("LMNP_MRE.txt", header =FALSE, sep = ';')
lmnpG_mre = ERRO_MRE8$V2
mean(ERRO_MRE8$V2)
sd(ERRO_MRE8$V2)

ERRO_RMSE8 <- read.table("LMNP_RMSE.txt", header =FALSE, sep = ';')
lmnpG_rmse = ERRO_RMSE8$V2
mean(ERRO_RMSE8$V2)
sd(ERRO_RMSE8$V2)

#---------------------------------9----------------------------

ERRO_MAE9 <- read.table("LMNP2_MAE.txt", header =FALSE, sep = ';')
lmnpU_mae = ERRO_MAE9$V2
mean(ERRO_MAE9$V2)
sd(ERRO_MAE9$V2)

ERRO_MRE9 <- read.table("LMNP2_MRE.txt", header =FALSE, sep = ';')
lmnpU_mre = ERRO_MRE9$V2
mean(ERRO_MRE9$V2)
sd(ERRO_MRE9$V2)

ERRO_RMSE9 <- read.table("LMNP2_RMSE.txt", header =FALSE, sep = ';')
lmnpU_rmse = ERRO_RMSE9$V2
mean(ERRO_RMSE9$V2)
sd(ERRO_RMSE9$V2)

#---------------------------------10----------------------------

ERRO_MAE10 <- read.table("LMNP3_MAE.txt", header =FALSE, sep = ';')
lmnpE_mae = ERRO_MAE10$V2
mean(ERRO_MAE10$V2)
sd(ERRO_MAE10$V2)

ERRO_MRE10 <- read.table("LMNP3_MRE.txt", header =FALSE, sep = ';')
lmnpE_mre = ERRO_MRE10$V2
mean(ERRO_MRE10$V2)
sd(ERRO_MRE10$V2)

ERRO_RMSE10 <- read.table("LMNP3_RMSE.txt", header =FALSE, sep = ';')
lmnpE_rmse = ERRO_RMSE10$V2
mean(ERRO_RMSE10$V2)
sd(ERRO_RMSE10$V2)

#---------------------------------11----------------------------

ERRO_MAE11 <- read.table("RQNP25_MAE.txt", header =FALSE, sep = ';')
rqnp25_mae = ERRO_MAE11$V2
mean(ERRO_MAE11$V2)
sd(ERRO_MAE11$V2)

ERRO_MRE11 <- read.table("RQNP25_MRE.txt", header =FALSE, sep = ';')
rqnp25_mre = ERRO_MRE11$V2
mean(ERRO_MRE11$V2)
sd(ERRO_MRE11$V2)

ERRO_RMSE11 <- read.table("RQNP25_RMSE.txt", header =FALSE, sep = ';')
rqnp25_rmse = ERRO_RMSE11$V2
mean(ERRO_RMSE11$V2)
sd(ERRO_RMSE11$V2)

#---------------------------------12----------------------------

ERRO_MAE12 <- read.table("RQNP5_MAE.txt", header =FALSE, sep = ';')
rqnp5_mae = ERRO_MAE12$V2
mean(ERRO_MAE12$V2)
sd(ERRO_MAE12$V2)

ERRO_MRE12 <- read.table("RQNP5_MRE.txt", header =FALSE, sep = ';')
rqnp5_mre = ERRO_MRE12$V2
mean(ERRO_MRE12$V2)
sd(ERRO_MRE12$V2)

ERRO_RMSE12 <- read.table("RQNP5_RMSE.txt", header =FALSE, sep = ';')
rqnp5_rmse = ERRO_RMSE12$V2
mean(ERRO_RMSE12$V2)
sd(ERRO_RMSE12$V2)

#---------------------------------13----------------------------

ERRO_MAE13 <- read.table("RQNP75_MAE.txt", header =FALSE, sep = ';')
rqnp75_mae = ERRO_MAE13$V2
mean(ERRO_MAE13$V2)
sd(ERRO_MAE13$V2)

ERRO_MRE13 <- read.table("RQNP75_MRE.txt", header =FALSE, sep = ';')
rqnp75_mre = ERRO_MRE13$V2
mean(ERRO_MRE13$V2)
sd(ERRO_MRE13$V2)

ERRO_RMSE13 <- read.table("RQNP75_RMSE.txt", header =FALSE, sep = ';')
rqnp75_rmse = ERRO_RMSE13$V2
mean(ERRO_RMSE13$V2)
sd(ERRO_RMSE13$V2)

#---------------------------------14----------------------------

ERRO_MAE14 <- read.table("RQNP25_MAE_2.txt", header =FALSE, sep = ';')
rqnp25_mae_2 = ERRO_MAE14$V2
mean(ERRO_MAE14$V2)
sd(ERRO_MAE14$V2)

ERRO_MRE14 <- read.table("RQNP25_MRE_2.txt", header =FALSE, sep = ';')
rqnp25_mre_2 = ERRO_MRE14$V2
mean(ERRO_MRE14$V2)
sd(ERRO_MRE14$V2)

ERRO_RMSE14 <- read.table("RQNP25_RMSE_2.txt", header =FALSE, sep = ';')
rqnp25_rmse_2 = ERRO_RMSE14$V2
mean(ERRO_RMSE14$V2)
sd(ERRO_RMSE14$V2)

#---------------------------------15----------------------------

ERRO_MAE15 <- read.table("RQNP5_MAE_2.txt", header =FALSE, sep = ';')
rqnp5_mae_2 = ERRO_MAE15$V2
mean(ERRO_MAE15$V2)
sd(ERRO_MAE15$V2)

ERRO_MRE15 <- read.table("RQNP5_MRE_2.txt", header =FALSE, sep = ';')
rqnp5_mre_2 = ERRO_MRE15$V2
mean(ERRO_MRE15$V2)
sd(ERRO_MRE15$V2)

ERRO_RMSE15 <- read.table("RQNP5_RMSE_2.txt", header =FALSE, sep = ';')
rqnp5_rmse_2 = ERRO_RMSE15$V2
mean(ERRO_RMSE15$V2)
sd(ERRO_RMSE15$V2)

#---------------------------------16----------------------------

ERRO_MAE16 <- read.table("RQNP75_MAE_2.txt", header =FALSE, sep = ';')
rqnp75_mae_2 = ERRO_MAE16$V2
mean(ERRO_MAE16$V2)
sd(ERRO_MAE16$V2)

ERRO_MRE16 <- read.table("RQNP75_MRE_2.txt", header =FALSE, sep = ';')
rqnp75_mre_2 = ERRO_MRE16$V2
mean(ERRO_MRE16$V2)
sd(ERRO_MRE16$V2)

ERRO_RMSE16 <- read.table("RQNP75_RMSE_2.txt", header =FALSE, sep = ';')
rqnp75_rmse_2 = ERRO_RMSE16$V2
mean(ERRO_RMSE16$V2)
sd(ERRO_RMSE16$V2)

#---------------------------------17----------------------------

ERRO_MAE17 <- read.table("RQNP25_MAE_3.txt", header =FALSE, sep = ';')
rqnp25_mae_2 = ERRO_MAE17$V2
mean(ERRO_MAE17$V2)
sd(ERRO_MAE17$V2)

ERRO_MRE17 <- read.table("RQNP25_MRE_3.txt", header =FALSE, sep = ';')
rqnp25_mre_2 = ERRO_MRE17$V2
mean(ERRO_MRE17$V2)
sd(ERRO_MRE17$V2)

ERRO_RMSE17 <- read.table("RQNP25_RMSE_3.txt", header =FALSE, sep = ';')
rqnp25_rmse_2 = ERRO_RMSE17$V2
mean(ERRO_RMSE17$V2)
sd(ERRO_RMSE17$V2)

#---------------------------------18----------------------------

ERRO_MAE18 <- read.table("RQNP5_MAE_3.txt", header =FALSE, sep = ';')
rqnp5_mae_2 = ERRO_MAE18$V2
mean(ERRO_MAE18$V2)
sd(ERRO_MAE18$V2)

ERRO_MRE18 <- read.table("RQNP5_MRE_3.txt", header =FALSE, sep = ';')
rqnp5_mre_2 = ERRO_MRE18$V2
mean(ERRO_MRE18$V2)
sd(ERRO_MRE18$V2)

ERRO_RMSE18 <- read.table("RQNP5_RMSE_3.txt", header =FALSE, sep = ';')
rqnp5_rmse_2 = ERRO_RMSE18$V2
mean(ERRO_RMSE18$V2)
sd(ERRO_RMSE18$V2)

#---------------------------------19----------------------------

ERRO_MAE19 <- read.table("RQNP75_MAE_3.txt", header =FALSE, sep = ';')
rqnp75_mae_2 = ERRO_MAE19$V2
mean(ERRO_MAE19$V2)
sd(ERRO_MAE19$V2)

ERRO_MRE19 <- read.table("RQNP75_MRE_3.txt", header =FALSE, sep = ';')
rqnp75_mre_2 = ERRO_MRE19$V2
mean(ERRO_MRE19$V2)
sd(ERRO_MRE19$V2)

ERRO_RMSE19 <- read.table("RQNP75_RMSE_3.txt", header =FALSE, sep = ';')
rqnp75_rmse_2 = ERRO_RMSE19$V2
mean(ERRO_RMSE19$V2)
sd(ERRO_RMSE19$V2)

#---------------------------------20----------------------------

ERRO_MAE20 <- read.table("SVML_MAE.txt", header =FALSE, sep = ';')
svrl_mae = ERRO_MAE20$V2
mean(ERRO_MAE20$V2)
sd(ERRO_MAE20$V2)

ERRO_MRE20 <- read.table("SVML_MRE.txt", header =FALSE, sep = ';')
svrl_mre = ERRO_MRE20$V2
mean(ERRO_MRE20$V2)
sd(ERRO_MRE20$V2)

ERRO_RMSE20 <- read.table("SVML_RMSE.txt", header =FALSE, sep = ';')
svrl_rmse = ERRO_RMSE20$V2
mean(ERRO_RMSE20$V2)
sd(ERRO_RMSE20$V2)

#---------------------------------21----------------------------

ERRO_MAE21 <- read.table("SVMR_MAE.txt", header =FALSE, sep = ';')
svrr_mae = ERRO_MAE21$V2
mean(ERRO_MAE21$V2)
sd(ERRO_MAE21$V2)

ERRO_MRE21 <- read.table("SVMR_MRE.txt", header =FALSE, sep = ';')
svrr_mre = ERRO_MRE21$V2
mean(ERRO_MRE21$V2)
sd(ERRO_MRE21$V2)

ERRO_RMSE21 <- read.table("SVMR_RMSE.txt", header =FALSE, sep = ';')
svrr_rmse = ERRO_RMSE21$V2
mean(ERRO_RMSE21$V2)
sd(ERRO_RMSE21$V2)

#boxplot(lmnpG_rmse, svrr_rmse, names = c("KR-g","SVR-r"))
#boxplot(rq25_mre,rqnp5_mre, names = c("RQ 0.25","RQNP-g 0.5"))
#---------------------------------22----------------------------

ERRO_MAE22 <- read.table("SVMP_MAE.txt", header =FALSE, sep = ';')
svrp_mae = ERRO_MAE22$V2
mean(ERRO_MAE22$V2)
sd(ERRO_MAE22$V2)

ERRO_MRE22 <- read.table("SVMP_MRE.txt", header =FALSE, sep = ';')
svrp_mre = ERRO_MRE22$V2
mean(ERRO_MRE22$V2)
sd(ERRO_MRE22$V2)

ERRO_RMSE22 <- read.table("SVMP_RMSE.txt", header =FALSE, sep = ';')
svrp_rmse = ERRO_RMSE22$V2
mean(ERRO_RMSE22$V2)
sd(ERRO_RMSE22$V2)

#---------------------------------23----------------------------

ERRO_MAE23 <- read.table("SVMS_MAE.txt", header =FALSE, sep = ';')
svrs_mae = ERRO_MAE23$V2
mean(ERRO_MAE23$V2)
sd(ERRO_MAE23$V2)

ERRO_MRE23 <- read.table("SVMS_MRE.txt", header =FALSE, sep = ';')
svrs_mre = ERRO_MRE23$V2
mean(ERRO_MRE23$V2)
sd(ERRO_MRE23$V2)

ERRO_RMSE23 <- read.table("SVMS_RMSE.txt", header =FALSE, sep = ';')
svrs_rmse = ERRO_RMSE23$V2
mean(ERRO_RMSE23$V2)
sd(ERRO_RMSE23$V2)


#-----------------MAE


RLR_bi = ERRO_MAE2$V2
QR_0.5 =  ERRO_MAE6$V2

par(mfrow=c(1,2))
hist(main="",RLR_bi,breaks = 23,border="dimgray")
hist(main="",QR_0.5,breaks=23,col="dimgray",border="gray")
mtext("Two best linear models ",cex=1.5,font=2, side = 3, line = -2, outer = TRUE)

#---------------------

NPQR_g_0.5 = ERRO_MAE12$V2
SVR_r =  ERRO_MAE21$V2

par(mfrow=c(1,2))
hist(main="",NPQR_g_0.5,breaks = 23,border="dimgray")
hist(main="",SVR_r,breaks=23,col="dimgray",border="gray")
mtext("Two best kernel-based models ",cex=1.5,font=2, side = 3, line = -2, outer = TRUE)



#----------------RMSE

LR = ERRO_RMSE1$V2
RLR_ha =  ERRO_RMSE3$V2

par(mfrow=c(1,2))
hist(main="",LR,breaks = 22,xlim=c(2,4))
hist(main="",RLR_ha,breaks=23,col="dimgray",border="gray",xlim=c(2,4))
mtext("Two best linear models ",cex=1.5,font=2, side = 3, line = -2, outer = TRUE)

#-----------------------

KR_g = ERRO_RMSE8$V2
SVR_r =  ERRO_RMSE21$V2

par(mfrow=c(1,2))
hist(main="", KR_g,breaks = 21,xlim=c(2,4))
hist(main="",SVR_r,breaks=23,col="dimgray",border="gray",xlim=c(2,4))
mtext("Two best kernel-based models ",cex=1.5,font=2, side = 3, line = -2, outer = TRUE)


id = c(1:100)
dados_mae = as.data.frame(cbind(id,ERRO_MAE1$V2,ERRO_MAE2$V2,ERRO_MAE3$V2,ERRO_MAE4$V2,ERRO_MAE5$V2,
                                ERRO_MAE6$V2,ERRO_MAE7$V2))

dados_mae2 = as.data.frame(cbind(id,ERRO_MAE8$V2,ERRO_MAE9$V2,ERRO_MAE10$V2,
                                 ERRO_MAE12$V2,ERRO_MAE15$V2,ERRO_MAE18$V2,
                                ERRO_MAE20$V2,ERRO_MAE21$V2,ERRO_MAE22$V2))

dados_rmse = as.data.frame(cbind(id,ERRO_RMSE1$V2,ERRO_RMSE2$V2,ERRO_RMSE3$V2,ERRO_RMSE4$V2,ERRO_RMSE5$V2,
                                ERRO_RMSE6$V2,ERRO_RMSE7$V2))

dados_rmse2 = as.data.frame(cbind(id,ERRO_RMSE8$V2,ERRO_RMSE9$V2,ERRO_RMSE10$V2,
                                  ERRO_RMSE12$V2,ERRO_RMSE15$V2,ERRO_RMSE18$V2,
                                 ERRO_RMSE20$V2,ERRO_RMSE21$V2,ERRO_RMSE22$V2))
require(dplyr)
require(rstatix)
require(reshape)
require(PMCMRplus)
require(ggplot2)

dados_mae_teste = as.data.frame(cbind(id,ERRO_RMSE8$V2,ERRO_RMSE21$V2))
dados_mae_testel = melt(dados_mae_teste,id='id',measured=c('V2','V3'))
colnames(dados_mae_testel) = c('ID',"Model","Error")
dados_mae_testel = sort_df(dados_mae_testel, vars='ID')
dados_mae_testel$ID <- factor(dados_mae_testel$ID)
ggplot(dados_mae_testel, aes(x=Error))+geom_histogram(aes(color=Model, fill=Model),alpha=0.8,position = 'stack',binwidth = 0.04)+scale_fill_grey()+scale_color_grey(start=0.8, end=0.2)+theme_classic()

dados_mael <- melt(dados_mae, id='id',
                   measured = c("V2","V3","V4","V5","V6","V7","V8"))
dados_mae2l <- melt(dados_mae2, id='id',
                   measured = c("V2","V3","V4","V5","V6","V7","V8"))
dados_rmsel <- melt(dados_rmse, id='id',
                   measured = c("V2","V3","V4","V5","V6","V7","V8","V9"))
dados_rmse2l <- melt(dados_rmse2, id='id',
                    measured = c("V2","V3","V4","V5","V6","V7","V8","V9"))


colnames(dados_mael) = c('ID',"Model","Error")
colnames(dados_mae2l) = c('ID',"Model","Error")
colnames(dados_rmsel) = c('ID',"Model","Error")
colnames(dados_rmse2l) = c('ID',"Model","Error")

dados_mael = sort_df(dados_mael, vars='ID')
dados_mae2l = sort_df(dados_mae2l, vars='ID')
dados_rmsel = sort_df(dados_rmsel, vars='ID')
dados_rmse2l = sort_df(dados_rmse2l, vars='ID')

dados_mael$ID <- factor(dados_mael$ID)
dados_mae2l$ID <- factor(dados_mae2l$ID)
dados_rmsel$ID <- factor(dados_rmsel$ID)
dados_rmse2l$ID <- factor(dados_rmse2l$ID)

friedman.test(Error ~ Model | ID, data = dados_mael)
friedman.test(Error ~ Model | ID, data = dados_mae2l)
friedman.test(Error ~ Model | ID, data = dados_rmsel)
friedman.test(Error ~ Model | ID, data = dados_rmse2l)

#post-hoc

frdAllPairsSiegelTest(dados_mael$Error, dados_mael$Model, dados_mael$ID, p.adjust = "bonferroni")
dados_mael %>% group_by(Model) %>% get_summary_stats(Error, type = "median_iqr")

frdAllPairsSiegelTest(dados_mae2l$Error, dados_mae2l$Model, dados_mae2l$ID, p.adjust = "bonferroni")
dados_mae2l %>% group_by(Model) %>% get_summary_stats(Error, type = "median_iqr")

frdAllPairsSiegelTest(dados_rmse2l$Error, dados_rmse2l$Model, dados_rmse2l$ID, p.adjust = "bonferroni")
dados_rmse2l %>% group_by(Model) %>% get_summary_stats(Error, type = "median_iqr")

frdAllPairsSiegelTest(dados_rmsel$Error, dados_rmsel$Model, dados_rmsel$ID, p.adjust = "bonferroni")
dados_rmsel %>% group_by(Model) %>% get_summary_stats(Error, type = "median_iqr")

boxplot(Error ~ Model, data = dados_mael,cex.axis=0.75, names=c("LR","RLR_bi","RLR_ha","RLR_hu","QR 0.25","QR 0.5","QR 0.75"))
boxplot(Error ~ Model, data = dados_rmsel,cex.axis=0.75, names=c("LR","RLR_bi","RLR_ha","RLR_hu","QR 0.25","QR 0.5","QR 0.75"))
boxplot(Error ~ Model, data = dados_rmse2l,cex.axis=0.75, names=c("KR_g","KR_u","KR_e","NPQR_g 0.5","NPQR_u 0.5","NPQR_e 0.5","SVR_l","SVR_r","SVR_p"))
boxplot(Error ~ Model, data = dados_mae2l,cex.axis=0.75, names=c("KR_g","KR_u","KR_e","NPQR_g 0.5","NPQR_u 0.5","NPQR_e 0.5","SVR_l","SVR_r","SVR_p"))

ggplot(dados_mae_teste, aes(x=Error))+geom_histogram(aes(color=Model, fill=Model),alpha=0.8,position = 'stack',binwidth = 0.04)+theme_classic()
hist(dados_mael$Error[dados_mael$Model=='V8'])




library(ggplot2)
myplot=ggplot() +
  geom_point(aes(x=base.prediction.lm.np$obs, y = base.prediction.lm.np$pred, color = 'npreg gaussian'),size=1) +
  geom_line(x=base.prediction.lm.np$obs, y = base.prediction.lm.np$pred, color = 'npreg gaussian') +
  geom_point(aes(x=base.prediction.svm$obs, y = base.prediction.svm$pred, color = 'svr radial'),size=1) +
#  geom_line(aes(x=teste$TE, y = base.prediction.svm$pred, color = 'svr radial')) +
  scale_color_manual(name = "Modelo",             # legend name
                     values = c("npreg gaussian" = "red",
                                "svr radial" = "black" ))+
  xlab('x = Real') +ylab('y = Predito')
myplot+theme_bw()


library(ggplot2)
myplot=ggplot(data=base.prediction.lm.np, aes(x=obs, y = pred) ) +
  geom_point(aes(x=base.prediction.lm.np$obs, y = base.prediction.lm.np$pred, color = 'npreg gaussian'),size=1) +geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(name = "Modelo",             # legend name
                     values = c("npreg gaussian" = "black"))+
  xlab('x = Real') +ylab('y = Predito')
myplot+theme_bw()


library(ggplot2)
myplot=ggplot(data=base.prediction.svm, aes(x=obs, y = pred) ) +
  geom_point(aes(x=base.prediction.svm$obs, y = base.prediction.svm$pred, color = 'svm radial'),size=1) +
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(name = "Modelo",             # legend name
                     values = c("svm radial" = "black"))+
  xlab('x = Real') +ylab('y = Predito')
myplot+theme_bw()


plot(x=base.prediction.lm.np$obs, y = base.prediction.lm.np$pred, ylab="Predito",xlab="Real")
abline(lm( base.prediction.lm.np$pred ~ base.prediction.lm.np$obs, data = base.prediction.lm.np),col='red')
plot(x=base.prediction.svm$obs, y = base.prediction.svm$pred,ylab="Predito",xlab="Real")
abline(lm( base.prediction.svm$pred ~ base.prediction.svm$obs, data = base.prediction.svm),col='blue')

