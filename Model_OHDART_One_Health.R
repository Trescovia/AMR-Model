###################################################################################
################################  MODEL 1 AMR #####################################
################################# Ross Booton ######################################
################################## JULY 2020  ####################################
###################################################################################

#from Knight paper
#Both acquisition rates (via transmission and de novo emergence) were dependent on 
#exposure to antibiotics, as antibiotic use clears sensitive bacterial carriage, 
#predisposing a host to colonisation with the (new) ARB. Linking transmission directly 
#to antibiotic exposure captures this impact of selection on both the source of 
#transmission (antibiotic exposure increases the ARB load) and the receiver 
#(antibiotic exposure increases the chance of successful ARB transmission)

library(deSolve) #Load the package deSolve
library(ggplot2) #Load the package ggplot2
library(ODEsensitivity)
library(FME)
library(tidyr)
library(data.table)
library(yarrr)
require(lhs)
require(IMIS)
require(dplyr)
require(gridExtra)

setwd("C:/Users/tresc/Desktop/Booton testing")
  
################################  AMRmodel #####################################
#state <- c(H=(1/70000000),A=0,E=0)
state <- c(H=(1/1000),A=(1/1000),E=(1/1000))
epid.start<- 2004
epid.duration <- 50
vectTime <- c(0,1:epid.duration)
int.time <- 2020 
eval.time <- 20
end.time <- int.time  + eval.time 

#byN=0.1
#beta_HH is the relative attribution of carriage between H and H etc
#gamma is the per-capita rate at which states acquire resistant bacteria as a result of exposure

AMRmodel <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{
    dH <- gamma*LAMBDA_H*(1-H) + LAMBDA_H*beta_HH*H*(1-H) + LAMBDA_H*beta_AH*(1-H)*A + LAMBDA_H*beta_EH*(1-H)*E - mu_H*H
    dA <-  gamma*LAMBDA_A*(1-A) + LAMBDA_A*beta_AA*A*(1-A) + LAMBDA_A*beta_HA*(1-A)*H + LAMBDA_A*beta_EA*(1-A)*E - mu_A*A
    dE <-  LAMBDA_E*beta_EE*E*(1-E) + LAMBDA_E*beta_HE*(1-E)*H + LAMBDA_E*beta_AE*(1-E)*A - mu_E*E
    return(  list(c(dH,dA,dE)))
  })}



################################  epid simple simulator #####################################
epid <- function(LAMBDA_H, LAMBDA_A, LAMBDA_E,
                 beta_HH, beta_AA, 
                 beta_HA, beta_AH, 
                 beta_HE, beta_EH, 
                 beta_AE, beta_EA,
                 mu_H, mu_A, mu_E,gamma,returnout,beta_EE,epsilon
){
  params <- c(LAMBDA_H=LAMBDA_H,LAMBDA_A=LAMBDA_A,LAMBDA_E=LAMBDA_E,
              beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
              beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
              mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  
  
  #run the model using desolve
  out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel,parms=params))
  out$time = out$time+epid.start #rescale the time so that it runs from 2005 onwards 
  model2012.H <- out$H[out$time==2012] #(out$H[out$time==2013] +out$H[out$time==2012])/2
  model2012.A <- out$A[out$time==2012] #(out$A[out$time==2013] +out$A[out$time==2012])/2
  model2012.E <- out$E[out$time==2012] #(out$E[out$time==2013] +out$E[out$time==2012])/2
  
  model2005.H <- out$H[out$time==2005] 
  model2005.A <- out$A[out$time==2005] 
  model2005.E <- out$E[out$time==2005] 
  #model2004.H <- out$H[out$time==2004] 
  #model2004.A <- out$A[out$time==2004] 
  #model2004.E <- out$E[out$time==2004] 
  
  model2008.H <- out$H[out$time==2008]
  model2009.H <- out$H[out$time==2009] 
  model2010.H <- out$H[out$time==2010]
  model2011.H <- out$H[out$time==2011]
  
  
  if (returnout ==1){
    return(out)} else{
      
      return(c(model2012.H= model2012.H,
               model2012.A= model2012.A,
               model2012.E= model2012.E,
               model2005.H=model2005.H,
               model2005.A=model2005.A,
               model2005.E=model2005.E,
               # model2004.H=model2004.H,
               # model2004.A=model2004.A,
               # model2004.E=model2004.E,
               model2008.H=model2008.H,
               model2009.H=model2009.H,
               model2010.H=model2010.H,
               model2011.H=model2011.H))}
}


################################  example of epid #####################################
epid(LAMBDA_H=0,LAMBDA_A=0,LAMBDA_E=0,
     beta_HH=0.02,  beta_AA=0.02,  beta_HE=0.02,  beta_AH=0.02,
     beta_EH=0.02,  beta_HA=0.02,  beta_EA=0.02,  beta_AE=0.02,  
     beta_EE = 0.02,epsilon=1,
     mu_H = 0.1, mu_A =0.1, mu_E =0.1,gamma=0.25,returnout=0)



################################  Sampling function #####################################
#this is from NETHERLANDS - but surely the environment/animals -> HUMAN would be much greater?
Sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 17) #from https://www.sciencedirect.com/science/article/pii/S2542519619301305
  
  Samples_SIR[,2] <- 0.1 + (1-0.1)*Samples_SIR[,2] #LAMBDA_A #vary to max
  Samples_SIR[,1] <- 0 + (Samples_SIR[,2]-0)*Samples_SIR[,1]  #LAMBDA_H #vary to max
  Samples_SIR[,3] <- 0 + (Samples_SIR[,1]-0)*Samples_SIR[,3] #LAMBDA_E #vary to max
  
  Samples_SIR[,4] <- 0.3 + (1-0.3)*Samples_SIR[,4]  #beta_HH
  Samples_SIR[,5] <- 0 + (1-0)*Samples_SIR[,5]  #beta_AA

  
  Samples_SIR[,7] <- 0 + (Samples_SIR[,4]-0)*Samples_SIR[,7] #beta_AH
  Samples_SIR[,9] <- 0 + (Samples_SIR[,7]-0)*Samples_SIR[,9] #beta_HA #beta_AA should always be bigger than beta_HA
  
  Samples_SIR[,10] <- 0 + (Samples_SIR[,5]-0)*Samples_SIR[,10] #beta_EA
  Samples_SIR[,8] <- 0 + (Samples_SIR[,4]-0)*Samples_SIR[,8]  #beta_EH
  
  Samples_SIR[,6] <- 0 + (1-0)*Samples_SIR[,6]  #beta_HE
  Samples_SIR[,11] <- 0 + (1-0)*Samples_SIR[,11]  #beta_AE
  
  Samples_SIR[,12] <- 0 + (0.5-0)*Samples_SIR[,12]  #mu_H #Carriage of ESBL or CRE at 12 months  community 25.4% patients 35.2%
  Samples_SIR[,13] <-  Samples_SIR[,12]#0 + (1-0)*Samples_SIR[,13]  #mu_A
  Samples_SIR[,14] <-  Samples_SIR[,12]  #0 + (1-0)*Samples_SIR[,14]    #mu_E
  Samples_SIR[,15] <- 0 + (1-0)*Samples_SIR[,15]    #gamma
  Samples_SIR[,16] <-  0 + (1-0)*Samples_SIR[,16] #beta_EE
  Samples_SIR[,17] <- 0 + (1-0)*Samples_SIR[,17] #epsilon

  
  paramsMat_SIR <- data.frame(
    LAMBDA_H=Samples_SIR[,1],LAMBDA_A=Samples_SIR[,2],LAMBDA_E=Samples_SIR[,3],
    beta_HH=Samples_SIR[,4],  beta_AA=Samples_SIR[,5],   beta_HA=Samples_SIR[,9], beta_AH=Samples_SIR[,7],
    beta_HE=Samples_SIR[,6],  beta_EH=Samples_SIR[,8],    beta_AE=Samples_SIR[,11],   beta_EA=Samples_SIR[,10],
    mu_H = Samples_SIR[,12], mu_A =Samples_SIR[,13], mu_E = Samples_SIR[,14],gamma = Samples_SIR[,15],beta_EE = Samples_SIR[,16],epsilon=Samples_SIR[,17]
  )
  return(paramsMat_SIR)
}


################################  outFUN fitting function #####################################
outFUN <- function(bbb,a0){ #feed in the sample (bbb), along with the number of the file you want to name (a0)
  outMat_SIR = apply(bbb,1,function(x) { 
    epid( as.list(x)$LAMBDA_H, as.list(x)$LAMBDA_A, as.list(x)$LAMBDA_E,
          as.list(x)$beta_HH,   as.list(x)$beta_AA,   as.list(x)$beta_HA,   as.list(x)$beta_AH,
          as.list(x)$beta_HE,   as.list(x)$beta_EH,   as.list(x)$beta_AE,   as.list(x)$beta_EA,  
          as.list(x)$mu_H,  as.list(x)$mu_A,  as.list(x)$mu_E ,as.list(x)$gamma,0,
          as.list(x)$beta_EE,as.list(x)$epsilon
          
    )})
  #  print(outMat_SIR)
  ResMat_LHS = 
    cbind(bbb,t(outMat_SIR)) 
  
  write.csv(ResMat_LHS,paste0("OUT_", a0,".csv")) #will write a csv file called OUT_a0
}


################################  example of fitting run #####################################
p1<-Sampling(1000000)
#Run the simulator outFUN for the Latin-Hypercube samples generated above
ptm <- proc.time() #time run 
outFUN(p1,100 #the file name..
) 
proc.time() - ptm

################################  FITTING DATA #####################################
#Boonyasiri ESBR- E. coli in Thailand, healthy food factory workers 
fitting.data <- data.frame(N=c(120,160,445,417,574,54,25),
                           res=c(0,87,177,289,430,12,3), #lower bound for A in ESBL-producing E coli in fresh food
                           time=c(2004,2008,2009,2010,2012,2012,2012),
                           var=c("H","H","H","H","H","A","E"))
fitting.data$percent <- fitting.data$res / fitting.data$N
fitting.data$inf <- fitting.data$percent - (1.96*sqrt(fitting.data$percent*(1-fitting.data$percent)/fitting.data$N))
fitting.data$sup <- fitting.data$percent + (1.96*sqrt(fitting.data$percent*(1-fitting.data$percent)/fitting.data$N))
Aupper<-241/400 #upper bound from rectal swabs in animals
fitting.data[6,]$sup <- Aupper + (1.96*sqrt(Aupper*(1-Aupper)/400))

fitting.data$inf[fitting.data$inf<0] <- 0

##Tresco adding these next few lines because we don't have the .CSVs from Booton

sample100 <- Sampling(100)
sample1000000 <- Sampling(1000000)

outFUN(sample100, 100)

#ptm <- proc.time() - ptm
#outFUN(sample1000000, 1000000)
#proc.time() - ptm

#FULLDATA fitting read in 
#FULLDATA_orig <-fread("OUT_1000000.csv") 
FULLDATA <- fread("OUT_100.CSV") 
FULLDATA_orig <- FULLDATA

#beta_AA should be bigger than beta_EA
FULLDATA <- FULLDATA[FULLDATA$beta_AA >= FULLDATA$beta_HA, ]

FULLDATA <- FULLDATA[FULLDATA$model2012.H <= fitting.data$sup[fitting.data$var=="H" &fitting.data$time==2012 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2012.H >= fitting.data$inf[fitting.data$var=="H" &fitting.data$time==2012 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2012.A <= fitting.data$sup[fitting.data$var=="A" &fitting.data$time==2012 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2012.A >= fitting.data$inf[fitting.data$var=="A" &fitting.data$time==2012 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2012.E <= fitting.data$sup[fitting.data$var=="E" &fitting.data$time==2012 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2012.E >= fitting.data$inf[fitting.data$var=="E" &fitting.data$time==2012 ], ]

FULLDATA <- FULLDATA[FULLDATA$model2008.H <= fitting.data$sup[fitting.data$var=="H" &fitting.data$time==2008 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2008.H >= fitting.data$inf[fitting.data$var=="H" &fitting.data$time==2008 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2010.H <= fitting.data$sup[fitting.data$var=="H" &fitting.data$time==2010 ], ]
FULLDATA <- FULLDATA[FULLDATA$model2010.H >= fitting.data$inf[fitting.data$var=="H" &fitting.data$time==2010 ], ]

################################ boxplot of parameters which are selected #####################################
par(mfrow=c(2,2))
mycol <- rgb(255, 0, 0, max = 255, alpha = 100, names = "blue50")
boxplot(FULLDATA_orig$LAMBDA_H, FULLDATA_orig$LAMBDA_A,FULLDATA_orig$LAMBDA_E,col=grey(0.6), names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,main="USE",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2)
boxplot(FULLDATA$LAMBDA_H, FULLDATA$LAMBDA_A,FULLDATA$LAMBDA_E,col=mycol, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2)

boxplot(FULLDATA_orig$beta_HH, FULLDATA_orig$beta_AA,FULLDATA_orig$beta_EE,FULLDATA_orig$beta_HE,
        FULLDATA_orig$beta_AH, FULLDATA_orig$beta_EH,FULLDATA_orig$beta_HA,
        FULLDATA_orig$beta_EA, FULLDATA_orig$beta_AE
        ,col=grey(0.6), names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,main="TRANSMISSION",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,
        cex.lab=2)
boxplot(FULLDATA$beta_HH, FULLDATA$beta_AA,FULLDATA$beta_EE,FULLDATA$beta_HE,
        FULLDATA$beta_AH, FULLDATA$beta_EH,FULLDATA$beta_HA,
        FULLDATA$beta_EA, FULLDATA$beta_AE,
        col=mycol,names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2)
boxplot(FULLDATA_orig$mu_H, FULLDATA_orig$mu_A,FULLDATA_orig$mu_E,col=grey(0.6), names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,main="DECAY",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,cex.lab=2)
boxplot(FULLDATA$mu_H, FULLDATA$mu_A,FULLDATA$mu_E,col=mycol, names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2)
boxplot(FULLDATA_orig$gamma,col=grey(0.6), names=c(expression(gamma)),show.names=TRUE,main="EMERGENCE",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,cex.lab=2)
boxplot(FULLDATA$gamma,col=mycol, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2)

#10X3

################################  plotfits function, in order to plot the fits #####################################
plotfits <- function(SIR.unique){
  for(i in (1:nrow(SIR.unique))){
    assign(paste("plot", i, sep = ""), 
           epid(LAMBDA_H=SIR.unique[[i,2]],LAMBDA_A=SIR.unique[[i,3]],SIR.unique[[i,4]],SIR.unique[[i,5]],
                SIR.unique[[i,6]],SIR.unique[[i,7]],SIR.unique[[i,8]],SIR.unique[[i,9]],
                SIR.unique[[i,10]],SIR.unique[[i,11]],SIR.unique[[i,12]],SIR.unique[[i,13]],
                SIR.unique[[i,14]],SIR.unique[[i,15]],SIR.unique[[i,16]],1,SIR.unique[[i,17]],SIR.unique[[i,18]])
    )}
  
  dataH=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataH[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$H 
  }
  
 
  datatemp <- dataH[,2:ncol(dataH)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataH$median<- datatemp$median
  dataH$lower<- datatemp$lower
  dataH$upper<- datatemp$upper
  
  dataA=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataA[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$A
  }
  
  
  datatemp <- dataA[,2:ncol(dataA)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataA$median<- datatemp$median
  dataA$lower<- datatemp$lower
  dataA$upper<- datatemp$upper
  
  dataE=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataE[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$E 
  }
  
  
  datatemp <- dataE[,2:ncol(dataE)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataE$median<- datatemp$median
  dataE$lower<- datatemp$lower
  dataE$upper<- datatemp$upper
  
  P1<- ggplot(data=dataH, aes(x=time))+
    geom_line(data=dataA,aes(x=time,y=median*100,colour="Animals"),size=1.5,alpha=0.8) +
    geom_line(data=dataH,aes(x=time,y=median*100,colour="Humans"),size=1.5,alpha=0.8) +
    geom_line(data=dataE,aes(x=time,y=median*100,colour="Environment"),size=1.5,alpha=0.8) +
    scale_color_brewer(palette = "Set1")+
    ylab(label="% samples with resistant bacteria")+
    xlab(label="Year")+ 
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(epid.start, 2017)) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    # scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
    theme(legend.position = 'right')+guides(colour=guide_legend(ncol=1))+
    geom_ribbon(data=dataA,aes(ymin=dataA$lower*100, ymax=dataA$upper*100), linetype=0, alpha=0.1,fill = "darkgreen",size=0.5)+
    geom_ribbon(data=dataE,aes(ymin=dataE$lower*100, ymax=dataE$upper*100), linetype=0, alpha=0.1,fill = "#EBCC2A",size=0.5)+
    geom_ribbon(data=dataH,aes(ymin=dataH$lower*100, ymax=dataH$upper*100), linetype=0, alpha=0.1,fill = "#3B9AB2",size=0.5)+
    
    geom_errorbar(data=fitting.data[fitting.data$var =="H",],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Humans")), width=0.5,size=1) +
    #geom_point(data=fitting.data[fitting.data$var =="H",],aes(x=time,y=percent*100,colour=c("Humans")))+ #, colour=as.factor(fit)))+  #geom_errorbar(CL,mapping=aes(x=time,ymin=lower/100, ymax=upper/100,colour=var), width=0.6,inherit.aes = FALSE) +
    geom_errorbar(data=fitting.data[fitting.data$var =="A",],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Animals")), width=0.5,size=1) +
    #geom_point(data=fitting.data[fitting.data$var =="A",],aes(x=time,y=percent*100,colour=c("Animals")))+
    geom_errorbar(data=fitting.data[fitting.data$var =="E",],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Environment")), width=0.5,size=1) #+
  #geom_point(data=fitting.data[fitting.data$var =="E",],aes(x=time,y=percent*100,colour=c("Environment")))
  print(P1)
}

plotfits(FULLDATA) #6X4





################################ plotting the effect of intervention from int.time ##############
epid_intervention <- function(
  LAMBDA_H, LAMBDA_A, LAMBDA_E,
  beta_HH, beta_AA, #beta_EE, 
  beta_HA, beta_AH, 
  beta_HE, beta_EH, 
  beta_AE, beta_EA,
  mu_H, mu_A, mu_E,gamma,returnout,
  intervention,
  beta_EE,epsilon){
  params <- c(LAMBDA_H=LAMBDA_H,LAMBDA_A=LAMBDA_A,LAMBDA_E=LAMBDA_E,
              beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
              beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
              mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel,parms=params))
  out$time = out$time+epid.start #rescale the time so that it runs from 2005 onwards 
  state2<-c(H=out$H[out$time==int.time],A=out$A[out$time==int.time],E=out$E[out$time==int.time])
  if (intervention==1){ #
    params2 <- c(LAMBDA_H=0,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)}
  else if (intervention==2){ #
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=0, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==3) {
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=0,
                 beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==4){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH=0,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==5){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=0,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==6){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=0,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==7){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=0,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==8){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=0,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==9){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=0,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==10){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=0,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==11){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=0,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==12){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=0,epsilon=epsilon)
  }else if (intervention==13){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=0)
  }else if (intervention==14){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
    
  } else if (intervention==15){ #NSP-AMR
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A*0.7, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.8,  beta_AA=beta_AA*0.8,  beta_HE=beta_HE*0.8,  beta_AH=beta_AH*0.8,
                 beta_EH=beta_EH*0.8,  beta_HA=beta_HA*0.8,  beta_EA=beta_EA*0.8,  beta_AE=beta_AE*0.8,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==16){ #NSP-AMR with 70% instead of 80%
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A*0.7, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.7,  beta_AA=beta_AA*0.7,  beta_HE=beta_HE*0.7,  beta_AH=beta_AH*0.7,
                 beta_EH=beta_EH*0.7,  beta_HA=beta_HA*0.7,  beta_EA=beta_EA*0.7,  beta_AE=beta_AE*0.7,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==17){ #50% reduction in drinking water
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH*0.5,  beta_HA=beta_HA,  beta_EA=beta_EA*0.5,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==18){ #human animal interaction reduced by 50%
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH*0.5,
                 beta_EH=beta_EH,  beta_HA=beta_HA*0.5,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==19){ # 50 % in use
    params2 <- c(LAMBDA_H=LAMBDA_H*0.5,
                 LAMBDA_A=LAMBDA_A*0.5, 
                 LAMBDA_E=LAMBDA_E*0.5,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else {params2 <- 0}
  
  
  vectTime2 <- c( (int.time - epid.start):epid.duration)
  out2 <- as.data.frame(ode(y=state2,time=vectTime2,func=AMRmodel,parms=params2))
  out2$time = out2$time+epid.start #rescale the time so that it runs from 2005 onwards 
  
  modelend.time.H <- out$H[out$time==end.time]
  modelend.time.A <- out$A[out$time==end.time]
  modelend.time.E <- out$E[out$time==end.time] 
  
  modelend.time.H.int <- out2$H[out2$time==end.time]
  modelend.time.A.int <- out2$A[out2$time==end.time]
  modelend.time.E.int <- out2$E[out2$time==end.time] 
  
  differenceH <-   (modelend.time.H -  modelend.time.H.int)/modelend.time.H
  differenceA <-   (modelend.time.A -  modelend.time.A.int)/modelend.time.A
  differenceE <-   (modelend.time.E -  modelend.time.E.int)/modelend.time.E
  if (returnout ==1){
    return(out2)} else{
      
      return(c(modelend.time.H= modelend.time.H,
               modelend.time.A= modelend.time.A,
               modelend.time.E= modelend.time.E,
               modelend.time.H.int= modelend.time.H.int,
               modelend.time.A.int= modelend.time.A.int,
               modelend.time.E.int= modelend.time.E.int,
               differenceH =  differenceH,
               differenceA =  differenceA,
               differenceE =  differenceE
      ))}
}





plotfits_int <- function(SIR.unique){
  for(i in (1:nrow(SIR.unique))){
    assign(paste("plot", i, sep = ""), 
           epid(LAMBDA_H=SIR.unique[[i,2]],LAMBDA_A=SIR.unique[[i,3]],SIR.unique[[i,4]],SIR.unique[[i,5]],
                SIR.unique[[i,6]],SIR.unique[[i,7]],SIR.unique[[i,8]],SIR.unique[[i,9]],
                SIR.unique[[i,10]],SIR.unique[[i,11]],SIR.unique[[i,12]],SIR.unique[[i,13]],
                SIR.unique[[i,14]],SIR.unique[[i,15]],SIR.unique[[i,16]],1,SIR.unique[[i,17]],SIR.unique[[i,18]])
           #intervention = 1)
    )}
  
  dataH=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataH[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$H 
  }
  
  
  datatemp <- dataH[,2:ncol(dataH)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataH$median<- datatemp$median
  dataH$lower<- datatemp$lower
  dataH$upper<- datatemp$upper
  
  dataA=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataA[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$A
  }
  
  
  datatemp <- dataA[,2:ncol(dataA)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataA$median<- datatemp$median
  dataA$lower<- datatemp$lower
  dataA$upper<- datatemp$upper
  
  dataE=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataE[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$E 
  }
  
  
  datatemp <- dataE[,2:ncol(dataE)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataE$median<- datatemp$median
  dataE$lower<- datatemp$lower
  dataE$upper<- datatemp$upper
  
  
  for(i in (1:nrow(SIR.unique))){
    assign(paste("plotint", i, sep = ""), 
           epid_intervention(LAMBDA_H=SIR.unique[[i,2]],LAMBDA_A=SIR.unique[[i,3]],SIR.unique[[i,4]],SIR.unique[[i,5]],
                             SIR.unique[[i,6]],SIR.unique[[i,7]],SIR.unique[[i,8]],SIR.unique[[i,9]],
                             SIR.unique[[i,10]],SIR.unique[[i,11]],SIR.unique[[i,12]],SIR.unique[[i,13]],
                             SIR.unique[[i,14]],SIR.unique[[i,15]],SIR.unique[[i,16]],1,intervention = 1,
                             SIR.unique[[i,17]],SIR.unique[[i,18]])
    )}
  
  dataHint=data.frame(time=plotint1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataHint[,1+i] <- eval(parse( text=paste("plotint", i, sep = "") ))$H 
  }
  
  
  datatemp <- dataHint[,2:ncol(dataHint)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataHint$median<- datatemp$median
  dataHint$lower<- datatemp$lower
  dataHint$upper<- datatemp$upper
  
  dataAint=data.frame(time=plotint1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataAint[,1+i] <- eval(parse( text=paste("plotint", i, sep = "") ))$A
  }
  
  
  datatemp <- dataAint[,2:ncol(dataAint)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataAint$median<- datatemp$median
  dataAint$lower<- datatemp$lower
  dataAint$upper<- datatemp$upper
  
  dataEint=data.frame(time=plotint1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataEint[,1+i] <- eval(parse( text=paste("plotint", i, sep = "") ))$E 
  }
  
  
  datatemp <- dataEint[,2:ncol(dataEint)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataEint$median<- datatemp$median
  dataEint$lower<- datatemp$lower
  dataEint$upper<- datatemp$upper
  
  
  P1<- ggplot(data=dataH, aes(x=time))+
    geom_line(data=dataH,aes(x=time,y=median*100,colour="Humans"),size=1.5,alpha=0.5) +
    geom_line(data=dataA,aes(x=time,y=median*100,colour="Animals"),size=1.5,alpha=0.5) +
    geom_line(data=dataE,aes(x=time,y=median*100,colour="Environment"),size=1.5,alpha=0.5) +
    scale_color_brewer(palette = "Set1")+
    geom_errorbar(data=fitting.data,aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Humans","Humans","Humans","Humans","Humans","Animals","Environment")), width=0.6) +
    geom_point(data=fitting.data,aes(x=time,y=percent*100,colour=c("Humans","Humans","Humans","Humans","Humans","Animals","Environment")))+ #, colour=as.factor(fit)))+  #geom_errorbar(CL,mapping=aes(x=time,ymin=lower/100, ymax=upper/100,colour=var), width=0.6,inherit.aes = FALSE) +
    ylab(label="% resistant bacteria")+
    xlab(label="Year")+ 
    scale_y_continuous(limits = c(0, 100)) + theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    # scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
    theme(legend.position = 'bottom')+guides(colour=guide_legend(ncol=1))+
    geom_ribbon(data=dataH,aes(ymin=dataH$lower*100, ymax=dataH$upper*100), linetype=0, alpha=0.2,fill = "#3B9AB2",size=0.5)+
    geom_ribbon(data=dataA,aes(ymin=dataA$lower*100, ymax=dataA$upper*100), linetype=0, alpha=0.2,fill = "darkgreen",size=0.5)+
    geom_ribbon(data=dataE,aes(ymin=dataE$lower*100, ymax=dataE$upper*100), linetype=0, alpha=0.2,fill = "#EBCC2A",size=0.5)+
    geom_line(data=dataHint,aes(x=time,y=median*100,colour="Humans"),size=1.5,alpha=1)+
    geom_line(data=dataAint,aes(x=time,y=median*100,colour="Animals"),size=1.5,alpha=1) +
    geom_line(data=dataEint,aes(x=time,y=median*100,colour="Environment"),size=1.5,alpha=1) +
    geom_ribbon(data=dataHint,aes(ymin=dataHint$lower*100, ymax=dataHint$upper*100), linetype=0, alpha=0.3,fill = "#3B9AB2",size=0.5)+
    geom_ribbon(data=dataAint,aes(ymin=dataAint$lower*100, ymax=dataAint$upper*100), linetype=0, alpha=0.3,fill = "darkgreen",size=0.5)+
    geom_ribbon(data=dataEint,aes(ymin=dataEint$lower*100, ymax=dataEint$upper*100), linetype=0, alpha=0.3,fill = "#EBCC2A",size=0.5)
  print(P1)
}
plotfits_int(FULLDATA)


################################ boxplot of interventions  ################################ 
getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
cols <- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
          unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white")

plotfits_int_box <- function(SIR.unique){
  
  for(j in (1:19)){
    for(i in (1:nrow(SIR.unique))){
      assign(paste("int",j,"plotint", i, sep = ""), 
             epid_intervention(LAMBDA_H=SIR.unique[[i,2]],
                               LAMBDA_A=SIR.unique[[i,3]],
                               SIR.unique[[i,4]],SIR.unique[[i,5]],
                               SIR.unique[[i,6]],SIR.unique[[i,7]],SIR.unique[[i,8]],SIR.unique[[i,9]],
                               SIR.unique[[i,10]],SIR.unique[[i,11]],SIR.unique[[i,12]],SIR.unique[[i,13]],
                               SIR.unique[[i,14]],SIR.unique[[i,15]],SIR.unique[[i,16]]
                               ,0,intervention = j,SIR.unique[[i,17]],SIR.unique[[i,18]])
      )}
    
    tempdataint<- rep(0,nrow(SIR.unique))
    for (i in 1:nrow(SIR.unique)) {
      tempval <- as.list(eval(parse( text=paste("int",j,"plotint", i, sep = "") )))$differenceH
      tempdataint[i] <-tempval
    }
    
    assign( paste("dataint", j, sep = ""),t(tempdataint))
    
  }
  
  
  DATA<- data.frame(humanuse0=t(dataint1),animaluse0=t(dataint2),envuse0=t(dataint3),
                    HH0=t(dataint4),AA0=t(dataint5),EE0=t(dataint12),
                    HE0=t(dataint6),
                    AH0=t(dataint7),EH0=t(dataint8),HA0=t(dataint9),
                    EA0=t(dataint10),AE0=t(dataint11)
                    #    muH=t(dataint12),
                    #   muA=t(dataint13),muE=t(dataint14)
  )
  DATA<- reshape2::melt(DATA)
  
  P1<- ggplot(DATA)+ geom_boxplot(aes(x=factor(variable),y=value*100,fill=factor(variable)))+
    ylab(label="% Reduction in resistant bacteria in humans over 20 years")+
    xlab(label="Intervention")+ 
    scale_fill_manual(labels = c('no AB use in humans',
                                 'no AB use in animals',
                                 'no AB use in environment',
                                 'no human to human acquisition of resistance',
                                 'no animal to animal acquisition of resistance',
                                 'no environment to environment acquisition of resistance',
                                 
                                 'no human to environment acquisition of resistance',
                                 'no animal to human acquisition of resistance',
                                 
                                 'no environment to human acquisition of resistance',
                                 'no human to animal acquisition of resistance',
                                
                                 'no environment to animal acquisition of resistance',
                                 'no animal to environment acquisition of resistance'
                                 ),values=cols)+
    # 'no decay of AB-resistance in humans',
    # 'no decay of AB-resistance in animals',
    # 'no decay of AB-resistance in environment'))+
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
    theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=2))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  
  DATA2<- data.frame(sanit=t(dataint15), H80=t(dataint16),A70=t(dataint17),beh80=t(dataint18),
                     madeup1=t(dataint19) 
  )
  DATA2<- reshape2::melt(DATA2)
  P2<- ggplot(DATA2)+ 
    geom_boxplot(aes(x=factor(variable),y=value*100,fill=factor(variable)))+
    ylab(label="% Reduction in resistant bacteria in humans over 20 years")+
    xlab(label="Intervention")+ 
    scale_fill_manual(labels = c('NSP-AMR',
                                 'NSP-AMR with 30% sanitary knowledge',
                                 '30% reduction ABU animals',
                                 '20% sanitary knowledge',
                                 '50% reduction in ABU in humans/animals, sanitary practices '),values=cols)+
    # 'no decay of AB-resistance in humans',
    # 'no decay of AB-resistance in animals',
    # 'no decay of AB-resistance in environment'))+
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
    theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=2))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  
 # grid.arrange(P1) #P1 is 1st figure 
 return(DATA) #return the dataframe to find 95CI% and median below
  
 #grid.arrange(P2) #P2 is 2nd figure with hypothetical intervention and national plan
  #return(DATA2) #return the dataframe to find 95CI% and median below
}

DATA<- plotfits_int_box(FULLDATA) #10X8 for P1 or 15X7 for P2

DATA <- DATA %>% group_by(variable)
SUMMM<-DATA%>% summarise(
  median=round(median(value),3)*100 ,
  quantilelower=round(quantile(value, c(0.025, 0.975)) ,3)[[1]]*100,
  quantileupper=round(quantile(value, c(0.025, 0.975)) ,3)[[2]]*100
)


###other interventions

epid_intervention_pairs_2 <- function(
  LAMBDA_H, LAMBDA_A, LAMBDA_E,
  beta_HH, beta_AA, #beta_EE, 
  beta_HA, beta_AH, 
  beta_HE, beta_EH, 
  beta_AE, beta_EA,
  mu_H, mu_A, mu_E,gamma,returnout,
  intervention,
  beta_EE){
  params <- c(LAMBDA_H=LAMBDA_H,LAMBDA_A=LAMBDA_A,LAMBDA_E=LAMBDA_E,
              beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
              beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
              mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma, beta_EE= beta_EE)
  out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel,parms=params))
  out$time = out$time+epid.start #rescale the time so that it runs from 2005 onwards 
  state2<-c(H=out$H[out$time==int.time ],A=out$A[out$time==int.time ],E=out$E[out$time==int.time ])
  
  if (intervention==1){ #NSP 
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A*0.7, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.8,  beta_AA=beta_AA*0.8,  beta_HE=beta_HE*0.8,  beta_AH=beta_AH*0.8,
                 beta_EH=beta_EH*0.8,  beta_HA=beta_HA*0.8,  beta_EA=beta_EA*0.8,  beta_AE=beta_AE*0.8,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE)}
  else if (intervention==2){ #NSP with 30% increase in sanitary practices
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A*0.7, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.7,  beta_AA=beta_AA*0.7,  beta_HE=beta_HE*0.7,  beta_AH=beta_AH*0.7,
                 beta_EH=beta_EH*0.7,  beta_HA=beta_HA*0.7,  beta_EA=beta_EA*0.7,  beta_AE=beta_AE*0.7,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE)
  } else if (intervention==3) { #NSP with 30% reduction in humans
    params2 <-  c(LAMBDA_H=LAMBDA_H*0.7,
                  LAMBDA_A=LAMBDA_A*0.7, 
                  LAMBDA_E=LAMBDA_E,
                  beta_HH= beta_HH*0.8,  beta_AA=beta_AA*0.8,  beta_HE=beta_HE*0.8,  beta_AH=beta_AH*0.8,
                  beta_EH=beta_EH*0.8,  beta_HA=beta_HA*0.8,  beta_EA=beta_EA*0.8,  beta_AE=beta_AE*0.8,  
                  mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE)
  } else if (intervention==4){ #20% reduction in AMU H and sewage and manure cut by 95%
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E= LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE*0.05,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE*0.05,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma, beta_EE= beta_EE)
  }else if (intervention==5){  #20% reduction in AMU H and human transmission cut by 50% 
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E= LAMBDA_E,
                 beta_HH= beta_HH*0.5,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma, beta_EE= beta_EE)
  } else if (intervention==6){ #No change in use but 50% reduction in transmission 
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E= LAMBDA_E,
                 beta_HH= beta_HH*0.5,  beta_AA=beta_AA*0.5,  beta_HE=beta_HE*0.5,  beta_AH=beta_AH*0.5,
                 beta_EH=beta_EH*0.5,  beta_HA=beta_HA*0.5,  beta_EA=beta_EA*0.5,  beta_AE=beta_AE*0.5,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma, beta_EE= beta_EE)
  } else if (intervention==7){ #No change in use but 50% reduction in water related 
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E= LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE*0.5,  beta_AH=beta_AH,
                 beta_EH=beta_EH*0.5,  beta_HA=beta_HA,  beta_EA=beta_EA*0.5,  beta_AE=beta_AE*0.5,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma, beta_EE= beta_EE)
  } 
  
  #print(params2)
  
  vectTime2 <- c( (int.time  - epid.start):epid.duration)
  out2 <- as.data.frame(ode(y=state2,time=vectTime2,func=AMRmodel,parms=params2))
  out2$time = out2$time+epid.start #rescale the time so that it runs from 2005 onwards 
  
  modelend.time.H <- out$H[out$time==end.time ]
  modelend.time.A <- out$A[out$time==end.time ]
  modelend.time.E <- out$E[out$time==end.time ] 
  
  modelend.time.H.int <- out2$H[out2$time==end.time ]
  modelend.time.A.int <- out2$A[out2$time==end.time ]
  modelend.time.E.int <- out2$E[out2$time==end.time ] 
  
  differenceH <-   (modelend.time.H -  modelend.time.H.int)/modelend.time.H
  differenceA <-   (modelend.time.A -  modelend.time.A.int)/modelend.time.A
  differenceE <-   (modelend.time.E -  modelend.time.E.int)/modelend.time.E
  if (returnout ==1){
    return(out2)} else{
      
      return(c(modelend.time.H= modelend.time.H,
               modelend.time.A= modelend.time.A,
               modelend.time.E= modelend.time.E,
               modelend.time.H.int= modelend.time.H.int,
               modelend.time.A.int= modelend.time.A.int,
               modelend.time.E.int= modelend.time.E.int,
               differenceH =  differenceH,
               differenceA =  differenceA,
               differenceE =  differenceE
      ))}
}





getPalette = colorRampPalette(piratepal(palette = "basel",length=7))
cols <- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
          unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white")

plotfits_int_box_2 <- function(SIR.unique){
  
  for(j in (1:7)){
    for(i in (1:nrow(SIR.unique))){
      assign(paste("int",j,"plotint", i, sep = ""), 
             epid_intervention_pairs_2(LAMBDA_H=SIR.unique[[i,2]],
                                       LAMBDA_A=SIR.unique[[i,3]],
                                       SIR.unique[[i,4]],SIR.unique[[i,5]],
                                       SIR.unique[[i,6]],SIR.unique[[i,7]],SIR.unique[[i,8]],SIR.unique[[i,9]],
                                       SIR.unique[[i,10]],SIR.unique[[i,11]],SIR.unique[[i,12]],SIR.unique[[i,13]],
                                       SIR.unique[[i,14]],SIR.unique[[i,15]],SIR.unique[[i,16]]
                                       ,0,intervention = j,
                                       SIR.unique[[i,17]])
      )}
    
    tempdataint<- rep(0,nrow(SIR.unique))
    for (i in 1:nrow(SIR.unique)) {
      tempval <- as.list(eval(parse( text=paste("int",j,"plotint", i, sep = "") )))$differenceH
      tempdataint[i] <-tempval
    }
    
    assign( paste("dataint", j, sep = ""),t(tempdataint))
    
  }
  
  
  DATA<- data.frame("NSP 2021" = t(dataint1),
                    "NSP 2021 with 30% increase in sanitary practices" = t(dataint2),
                    "NSP 2021 with 30% AMU H reduction" = t(dataint3),
                    "20% AMU H with 95% sewage and manure transmission reduction"=t(dataint4),
                    "20% AMU H with 50% human-human tranmission reduction"=  t(dataint5),
                    "50% reduction in transmission"= t(dataint6),
                    "50% reduction in water related transmission"=t(dataint7)
                    
                    
  )
  DATA1<- reshape2::melt(DATA)

 
  P1<- ggplot(DATA1)+ 
    geom_boxplot(aes(x=factor(variable),y=value*100,fill=factor(variable)))+
    ylab(label="% Reduction in resistant bacteria in humans 2020-2040")+
    xlab(label="Intervention")+ 
    scale_fill_manual(labels = c('NSP-AMR 2021, Thailand',
                                 'NSP-AMR 2021 with 30% increase in sanitary practices',
                                 'NSP-AMR 2021 with 30% reduction in human ABU',
                                 '20% reduction in human ABU with 95% reduction in transmission from sewage/manure',
                                 '20% reduction in human ABU with 50% reduction in human-human transmission ',
                                 '50% reduction in all transmission',
                                 '50% reduction in water related transmission'),values=cols)+
    # 'no decay of AB-resistance in humans',
    # 'no decay of AB-resistance in animals',
    # 'no decay of AB-resistance in environment'))+
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
    theme(legend.position = 'right')+guides(fill=guide_legend(ncol=1))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  
 # return(DATA) #return the dataframe to find 95CI% and median below
return(P1) #plot the figure
}

DATA<- plotfits_int_box_2(FULLDATA) #10X8 for P1 or 15X7 for P2

DATA <- DATA %>% group_by(variable)

DATASUM <- DATA%>% summarise(
  median=round(median(value),4)*100 ,
  mean=round(mean(value),4)*100 ,
  quantilelower=round(quantile(value, c(0.025, 0.975)) ,4)[[1]]*100,
  quantileupper=round(quantile(value, c(0.025, 0.975)) ,4)[[2]]*100
)

DATASUM <- DATASUM[order(-DATASUM$median),]

print(DATASUM)

print(DATA)







