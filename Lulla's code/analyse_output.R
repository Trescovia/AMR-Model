
#####################################################################""
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2018/04/10

### Draw and save outcome for the RA model

output_save <- function(W_res,L_res, F_res,H1_res, H2_res, printdir=T, mainDir="", spec_dir="", savegraphs=T) 
{
    mainDir <- getwd()
    
    # get submodels outputs

    # environment submodels: W:water ; L:livestock ; F:food
    RW <- W_res[[1]]
    RL <- L_res[[1]]
    RF <- F_res[[1]]
 
    ECW <- W_res[[2]]
    ECL <- L_res[[2]]
    ECF <- F_res[[2]]
    
       
    # prevalence at the entry of community and hospital models
    R1 <- (RL + RF + RW)/100
    input_prev <- (Mcolo_duration * R1)/(Mcolo_duration * R1+1) # duration in year-1
    
    # human submodels: H1:hospital ; H2:community
    RH1 <- H1_res[[1]]
    RH2 <- H2_res[[1]]
    
    ECH <- H1_res[[2]]
    ECC <- H2_res[[2]]
    
    
    # define individual incidence risks of acquisition of ARB: per 100 individuals, per 365 days
    # risks <- rbind(cbind(as.vector(RW),rep("W",length(as.vector(RW)))),cbind(as.vector(RL),rep("L",length(as.vector(RL)))),
    #       cbind(as.vector(RF),rep("F",length(as.vector(RF)))),
    #       cbind(as.vector(RH1),rep("H",length(as.vector(RH1)))),
    #       cbind(as.vector(RH2),rep("C",length(as.vector(RH2))))
    #     )
    # test <- as.data.frame(risks)
    # names(test) <- c("risk","submodel")
    # test$submodel <- as.factor(test$submodel)
    # 
    # test$risk<- as.numeric(levels(test$risk))[test$risk]
    #     library(ggplot2)
    # p <- ggplot(test, aes(x=submodel, y=risk)) +
    #   geom_violin(trim=FALSE)
    # p

    # Final risk calculation
    FR = (RH1+RH2)/100
  
    results<- cbind(RW,RL,RF,input_prev,RH1,RH2,FR*100, FR*100)
    colnames(results) <- c("W","L","F", "input_prev","H","C", "Incidence","Risk")
    mycol <- c("blue", "brown", "orange", "grey", "bisque2", "pink", "red", "red")    
    
        
    # if printdir TRUE : need to create directory to write results
    if (printdir)
    {
#         subDir <- paste("mcmc_res", nbIter, "iter", rdWalk, "rdWalk", acceptanceRate, "accept", sep = "_") # modif LO
        subDir <- paste("RA_run", spec_dir, Sys.Date(), sep = "_")
        newDir <- file.path(mainDir, subDir)
        dir.create(newDir, showWarnings = FALSE)
    }
    # Save values of incidence risk per 100 per year at each step
    if (printdir)
    {
        Resfile <- file.path(newDir, "Risk_estimates.txt")
        
        for(i in 1:ncol(results)){
          estimates <- results[,i]
          med <- median(estimates, na.rm=T)
          moy <- mean(estimates, na.rm=T)
          ICneg <- quantile(estimates, 0.025, na.rm=T)
          ICpos <- quantile(estimates, 0.975, na.rm=T)
          min <- min(estimates, na.rm=T)
          max <- max(estimates, na.rm=T)
          mtext <- paste("mean", "median", "IC-","IC+\n", "min", "max", sep="\t")
          if(printdir) {
            write(colnames(results)[i], file=Resfile, append=TRUE)
            write(mtext, file=Resfile, append=TRUE)
          }
          else print(mtext)
          mtext <- paste(moy, med, "[", ICneg,",",ICpos,"]", min, max, sep="\t")
          if(printdir) write(mtext, file=Resfile, append=TRUE)
          else print(mtext)
        }
    }
    
    # Save correlation with incidence risk for each variable of each submodel 
    if (printdir)
    {
      Resfile <- file.path(newDir, "Risk_correlates.txt")
      data <- as.data.frame(tornado(ECW, use="complete.obs")$value)
      write.table(data, file=Resfile, append=TRUE, sep="\t")
      data <- as.data.frame(tornado(ECL, use="complete.obs")$value)
      write.table(data, file=Resfile, append=TRUE)
      data <- as.data.frame(tornado(ECF, use="complete.obs")$value)
      write.table(data, file=Resfile, append=TRUE, sep="\t")
      data <- as.data.frame(tornado(ECH, use="complete.obs")$value)
      write.table(data, file=Resfile, append=TRUE, sep="\t")
      data <- as.data.frame(tornado(ECC, use="complete.obs")$value)
      write.table(data, file=Resfile, append=TRUE, sep="\t")
    }    
    
    # plots correlations for sensitivity analysis for submodels 
    if (savegraphs)
      {
       plotfile <- file.path(newDir, "W_corr.png")
       png(file = plotfile)
       plot(tornado(ECW, use="complete.obs"),ylab="",main="W", col=mycol[1])
       dev.off()
       
       plotfile <- file.path(newDir, "L_corr.png")
       png(file = plotfile)
       plot(tornado(ECL, use="complete.obs"),main="L", col=mycol[2])
       dev.off()
       
       plotfile <- file.path(newDir, "F_corr.png")
       png(file = plotfile)
       plot(tornado(ECF, use="complete.obs"),main="F", col=mycol[3])
       dev.off()
       
       plotfile <- file.path(newDir, "H_corr.png")
       png(file = plotfile)
       plot(tornado(ECH, use="complete.obs"),main="H", col=mycol[5])
       dev.off()
       
       plotfile <- file.path(newDir, "C_corr.png")
       png(file = plotfile)
       plot(tornado(ECC, use="complete.obs"),main="C", col=mycol[6])
      dev.off()
       
    }
        
    
        
    # correct final risk to be maximum 100%
    results[(results[,"Risk"]>=100),"Risk"] <- 100
    results[(results[,"Incidence"]>=1000),"Incidence"] <- 1000
    
    # plots posterior distributions for submodels and for final risks and save files
    nbGraphs = ncol(results)
    if(nbGraphs%%2!=0) nbGraphs=nbGraphs+1
    
    if (savegraphs)
    {
        plotfile <- file.path(newDir, "posterior_densities.png")
        png(file = plotfile)
    }
    
    # definitions of breaks for the histograms
    if(length(which(results<0))>1) {
      print("some negative values corrected, n=")
      print(length(which(results<0)))
      results[(results<0)]=0
    }
    
    mybreaks <- rbind(
                      seq(from=0, to=2000, by=100),
                      seq(0,2000, by=100),
                      seq(0,800, by=40),
                      seq(0,1, by=0.05),
                      seq(0,50, by=2.5),
                      seq(0,2200, by=110),
                      seq(0,1000, by=50),
                      seq(0,100, by=5))
    # seq(0,1, by=0.05))

    par(mar=c(2,4,2,1))
    layout(matrix(1:nbGraphs, ncol = 2, byrow = T), 1, 1)
    # parameters
    for (i in 1:ncol(results))
    {
      print(i)
      # hist(results[,i], breaks=10,main=colnames(results)[i], col=mycol[i])
      # hist(as.vector(na.omit(results[,i])),main=colnames(results)[i], col=mycol[i], breaks=mybreaks[i,], ylim=c(0,100000))
      # hist(as.vector(na.omit(results[,i])),main=colnames(results)[i], col=mycol[i], breaks=mybreaks[i,])
      hist(as.vector(na.omit(results[,i])), main="",col=mycol[i], breaks=mybreaks[i,])
      title(main=colnames(results)[i],col.main=mycol[i],line=-1, outer=F, cex.main=1.5)
         abline(v=median(results[,i],na.rm=T),col="black",lwd=2,lty=2)
    }   
    if(savegraphs) dev.off()
    
   
}