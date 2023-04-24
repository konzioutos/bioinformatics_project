#R Project 2020-21
# Konstantinos Zioutos csd3946

library('abc')
library('readr')
library(bayestestR)
library(dplyr)
library(ggplot2)

rm(list=ls())

#kanw ena matrix sti morfi wste na exoun stiles

getDataSet=function(file_dataset){
  dataset=NULL
  
  d1 <- as.matrix(file_dataset)
  d2 <- strsplit(d1, "") 
  d3 <- unlist(d2) 
  d3 <- as.numeric(d3) 
  dataset <- matrix(d3, nrow=dim(d1)[1], byrow=T)
}

#vriskoume to k

get_k_statistic=function(dataset){
  sum_diff=0;
  
  #arxika sygkrinoume kathe grammi me tis ypoloipes 
  
  compare = t(combn(nrow(dataset),2,FUN=function(x)dataset[x[1],]==dataset[x[2],]))
  rownames(compare) = combn(nrow(dataset),2,FUN=function(x)paste0("seq",x[1],"_seq",x[2]))
  
  #kai sth synexeia opou einai diaforetikes afksanw to arthroisma twn synolikwn diaforetikwn
  
  for(i in 1:nrow(dataset)){
    for(j in 1:ncol(dataset)){
      if(compare[i,j]==FALSE){
        sum_diff=sum_diff+1;
      }
    }
  }
  
  k=sum_diff/50 #kai diairoume me tis grammes
}

get_w_statistic=function(dataset){
  w=ncol(dataset)/(sum(1/1:nrow(dataset))) #vriskoume to w efarmozontas ton typo
}


#diavazoume ta dedomena kai ta apothikeuoume

observation<- read.table('ms_obs_final.txt', header = FALSE,colClasses = 'character')

pars<- read.table('pars_final.txt', header = FALSE)

sims_read <-read.table('sims.txt', header = FALSE,colClasses = 'character' )

#kanoume ton matrix twn observation sti morfi wste na exei stiles

obs=getDataSet(observation)

#vriskw ta k,w gia ta observation

k_obs=get_k_statistic(obs)

w_obs=get_w_statistic(obs)

obs_stats=matrix(c(k_obs,w_obs),nrow=1,ncol=2) #ftiaxnoume ton pinaka me ta stats gia to abc


sumstats=matrix(0,nrow=10000,ncol=2)
list=0
row=1
r=1
for (i in seq(1,nrow(sims_read), by=50)) { #twra edw xeirizomaste antistoixa to arxeio me tis posomoiwseis,kai to kanoume ana 50 diladi an kathe dataset
  list[row:50]<-(sims_read[i:(i+49),]) #gia kathe 50ada ftiaxnw ena matrix wste na vrw ta statistics
  
  sims=getDataSet(list)
  
  #vriskw ta k,w gia kathe dataset apo ta simulations
  
  k_sims=get_k_statistic(sims)
  
  w_sims=get_w_statistic(sims)
  
  #edw kanw ton pinaka duo stilwn poy exei ola ta k,w kai tha xreiastoun sto abc
  
  sumstats[r,1]=k_sims
  sumstats[r,2]=w_sims
  
  row=1
  r=r+1
}

#efarmozw to abc

myabc=abc(target=obs_stats,param=pars,sumstat=sumstats,method="loclinear",hcorr=TRUE,tol=0.1)
summary(myabc)

#b erwthma

ci_hdi <- ci(myabc$adj.values[,1], method = "HDI",ci=0.95)
ci_hdi

#c erwthma

d.prior = density(pars[,1]) #prior katanomi
d.pos1 <- density( myabc$unadj.values[,1]) #posterior prin
d.pos = density( myabc$adj.values[,1]) #posterior meta

plot(d.prior$x, d.prior$y, ylim=c(0, max(d.prior$y, d.pos1$y, d.pos$y)), type='l', col='orange') #ta sxediazw me plots
points(d.pos1$x, d.pos1$y,  type='l', col='blue')
points(d.pos$x, d.pos$y,  type='l', col='red')


abline(v = mean(myabc$adj.values),col='green') #sxediazw me kathetes grammes ta apotelesmata apo ta proigoumena erwthmata
abline(v=c(ci_hdi[2],ci_hdi[3]),col='black')


