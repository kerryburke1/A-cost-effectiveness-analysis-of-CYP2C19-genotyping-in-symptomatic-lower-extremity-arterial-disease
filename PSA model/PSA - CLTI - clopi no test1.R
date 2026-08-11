ncycles<-54

mcresults_CLTInotest1<-data.frame(matrix(nrow=mcruns,ncol=2))
colnames(mcresults_CLTInotest1)<-c("Cost","QALY")

for(j in 1:mcruns){
  n<-markovn2[j,5]
  source(file.path(root,"PSA model","Data","PSA - Transition probabilities.R"))
  source(file.path(root,"PSA model","Data","PSA - Costs.R"))
  source(file.path(root,"PSA model","Data","PSA - Utilities.R"))
  
  startdist<-c(0,n,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  markovtrace<-matrix(data=NA,
                      nrow=ncycles+1,
                      ncol=nstates,
                      dimnames=list(NULL,statenames))
  markovtrace[1,]<-startdist
  
  translist<-list()
  
  for(i in 1:ncycles){

  p_death<-as.numeric(acm_clti[i,2])
  
  probAtoA_6monthA<-1-(probAtoB_6monthA+probAtoC_6monthA+probAtoE_6monthA+probAtoAH_6monthA+probAtoAI_6monthA+probAtoJ_6monthA+p_death)
  probBtoB_6monthA<-1-(probBtoD_6monthA+probBtoE_6monthA+probBtoBH_6monthA+probBtoBI_6monthA+probBtoJ_6monthA+p_death)
  probCtoC_6monthA<-1-(probCtoB_6monthA+probCtoE_6monthA+probCtoG_6monthA+probCtoCH_6monthA+probCtoCI_6monthA+probCtoJ_6monthA+p_death)
  probDtoD_6monthA<-1-(probDtoE_6monthA+probDtoF_6monthA+probDtoDH_6monthA+probDtoDI_6monthA+probDtoJ_6monthA+p_death)
  probEtoE_6monthA<-1-(probEtoEH_6monthA+probEtoEI_6monthA+probEtoJ_6monthA+p_death)
  probFtoF_6monthA<-1-(probFtoE_6monthA+probFtoFH_6monthA+probFtoFI_6monthA+probFtoJ_6monthA+p_death)
  probGtoG_6monthA<-1-(probGtoB_6monthA+probGtoE_6monthA+probGtoGH_6monthA+probGtoGI_6monthA+probGtoJ_6monthA+p_death)
  
  probAHtoA_6monthA<-1-probAHtoJ_6monthA-p_death
  probBHtoB_6monthA<-1-probBHtoJ_6monthA-p_death
  probCHtoC_6monthA<-1-probCHtoJ_6monthA-p_death
  probDHtoD_6monthA<-1-probDHtoJ_6monthA-p_death
  probEHtoE_6monthA<-1-probEHtoJ_6monthA-p_death
  probFHtoF_6monthA<-1-probFHtoJ_6monthA-p_death
  probGHtoG_6monthA<-1-probGHtoJ_6monthA-p_death
  
  probAItoA_6month<-1-(p_death+probAItoJ_6month)
  probBItoB_6month<-1-(p_death+probBItoJ_6month)
  probCItoC_6month<-1-(p_death+probCItoJ_6month)
  probDItoD_6month<-1-(p_death+probDItoJ_6month)
  probEItoE_6month<-1-(p_death+probEItoJ_6month)
  probFItoF_6month<-1-(p_death+probFItoJ_6month)
  probGItoG_6month<-1-(p_death+probGItoJ_6month)
  
  #Transition Matrix            
    transmat<-matrix(c(probAtoA_6monthB,probAtoB_6monthB,probAtoC_6monthB,0,probAtoE_6monthB,0,0,probAtoAH_6monthB,0,0,0,0,0,0,probAtoAI_6monthB,0,0,0,0,0,0,probAtoJ_6monthB,p_death,
                       
                       0,probBtoB_6monthB,0,probBtoD_6monthB,probBtoE_6monthB,0,0,0,probBtoBH_6monthB,0,0,0,0,0,0,probBtoBI_6monthB,0,0,0,0,0,probBtoJ_6monthB,p_death,
                       
                       0,probCtoB_6monthB,probCtoC_6monthB,0,probCtoE_6monthB,0,probCtoG_6monthB,0,0,probCtoCH_6monthB,0,0,0,0,0,0,probCtoCI_6monthB,0,0,0,0,probCtoJ_6monthB,p_death,
                       
                       0,0,0,probDtoD_6monthB,probDtoE_6monthB,probDtoF_6monthB,0,0,0,0,probDtoDH_6monthB,0,0,0,0,0,0,probDtoDI_6monthB,0,0,0,probDtoJ_6monthB,p_death,
                       
                       0,0,0,0,probEtoE_6monthB,0,0,0,0,0,0,probEtoEH_6monthB,0,0,0,0,0,0,probEtoEI_6monthB,0,0,probEtoJ_6monthB,p_death,
                       
                       0,0,0,0,probFtoE_6monthB,probFtoF_6monthB,0,0,0,0,0,0,probFtoFH_6monthB,0,0,0,0,0,probFtoFI_6monthB,0,0,probFtoJ_6monthB,p_death,
                       
                       0,probGtoB_6monthB,0,0,probGtoE_6monthB,0,probGtoG_6monthB,0,0,0,0,0,0,probGtoGH_6monthB,0,0,0,0,0,probGtoGI_6monthB,0,probGtoJ_6monthB, p_death,
                       
                       probAHtoA_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthB,p_death,
                       
                       0,probBHtoB_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthB,p_death,
                       
                       0,0,probCHtoC_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthB,p_death,
                       
                       0,0,0,probDHtoD_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthB,p_death,
                       
                       0,0,0,0,probEHtoE_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthB,p_death,
                       
                       0,0,0,0,0,probFHtoF_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probFHtoJ_6monthB,p_death,
                       
                       0,0,0,0,0,0,probGHtoG_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoJ_6monthB,p_death,
                       
                       probAItoA_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probAItoJ_6month,p_death,
                       
                       0,probBItoB_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probBItoJ_6month,p_death,
                       
                       0,0,probCItoC_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probCItoJ_6month,p_death,
                       
                       0,0,0,probDItoD_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probDItoJ_6month,p_death,
                       
                       0,0,0,0,probEItoE_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probEItoJ_6month,p_death,
                       
                       0,0,0,0,0,probFItoF_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probFItoJ_6month,p_death,
                       
                       0,0,0,0,0,0,probGItoG_6month,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probGItoJ_6month,p_death,
                       
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                       
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                     nrow=23,byrow=T)
    
    rownames(transmat)<-colnames(transmat)<-statenames
    
    markovtrace[1+i,] <- markovtrace[i,] %*% transmat
    
    transflows <- sweep(transmat, 1, markovtrace[i,], "*")
    
    translist[[i]] <- transflows
    
  }
  
  markovtrace<-as.data.frame(markovtrace)
  
  markovtrace[,"SixMonths"]<-c(0:ncycles)
  
  markovtrace["Cost"]<-(markovtrace["IC"]*costA_C)+
    (markovtrace["CLTI"]*costB_C)+
    (markovtrace["RevascIC"]*costC_C)+
    (markovtrace["RevascCLTI"]*costD_C)+
    (markovtrace["Amputation"]*costE_C)+
    (markovtrace["ReintCLTI"]*costF_C)+
    (markovtrace["ReintIC"]*costG_C)+
    (markovtrace["ICStroke"]*costAH_C)+
    (markovtrace["CLTIStroke"]*costBH_C)+
    (markovtrace["RevascICStroke"]*costCH_C)+
    (markovtrace["RevascCLTIStroke"]*costDH_C)+
    (markovtrace["AmputationStroke"]*costEH_C)+
    (markovtrace["ReintCLTIStroke"]*costFH_C)+
    (markovtrace["ReintICStroke"]*costGH_C)+
    (markovtrace["ICMI"]*costAI_C)+
    (markovtrace["CLTIMI"]*costBI_C)+
    (markovtrace["RevascICMI"]*costCI_C)+
    (markovtrace["RevascCLTIMI"]*costDI_C)+
    (markovtrace["AmputationMI"]*costEI_C)+
    (markovtrace["ReintCLTIMI"]*costFI_C)+
    (markovtrace["ReintICMI"]*costGI_C)+
    (markovtrace["VascularDeath"]*costJ)+
    (markovtrace["OtherDeath"]*costK)
  
  markovtrace["QALY"]<-(markovtrace["IC"]*qolAc)+
    (markovtrace["CLTI"]*qolBc)+
    (markovtrace["RevascIC"]*qolCc)+
    (markovtrace["RevascCLTI"]*qolDc)+
    (markovtrace["Amputation"]*qolEc)+
    (markovtrace["ReintCLTI"]*qolFc)+
    (markovtrace["ReintIC"]*qolGc)+
    (markovtrace["ICStroke"]*qolAHc)+
    (markovtrace["CLTIStroke"]*qolBHc)+
    (markovtrace["RevascICStroke"]*qolCHc)+
    (markovtrace["RevascCLTIStroke"]*qolDHc)+
    (markovtrace["AmputationStroke"]*qolEHc)+
    (markovtrace["ReintCLTIStroke"]*qolFHc)+
    (markovtrace["ReintICStroke"]*qolGHc)+
    (markovtrace["ICMI"]*qolAIc)+
    (markovtrace["CLTIMI"]*qolBIc)+
    (markovtrace["RevascICMI"]*qolCIc)+
    (markovtrace["RevascCLTIMI"]*qolDIc)+
    (markovtrace["AmputationMI"]*qolEIc)+
    (markovtrace["ReintCLTIMI"]*qolFIc)+
    (markovtrace["ReintICMI"]*qolGIc)+
    (markovtrace["VascularDeath"]*qolJ)+
    (markovtrace["OtherDeath"]*qolK)
  
  tot_trans_cost <- numeric(ncycles)
  
  for(i in 1:ncycles){
  df <- 1/(1 + discountrate)^((i-1)/2)   # beginning of cycle i
  tot_trans_cost[i] <- sum(translist[[i]] * trans_cost_mat) * df
}
  
  sum(tot_trans_cost)
  
hcc_cost <- numeric(ncycles) 
hcc_qaly <- numeric(ncycles)
for(i in 1:ncycles){ 
hcc_cost[i] <- ( markovtrace[i, "Cost"] + markovtrace[i + 1, "Cost"] ) / 2 
hcc_qaly[i] <- ( markovtrace[i, "QALY"] + markovtrace[i + 1, "QALY"] ) / 2 } 
cycle_midpoint_years <- ((1:ncycles) - 0.5) * 0.5 
disc <- 1 / ((1 + discountrate)^cycle_midpoint_years)
disccost_hcc <- hcc_cost * disc 
discqaly_hcc <- hcc_qaly * disc
results <- c( sum(disccost_hcc) + sum(tot_trans_cost), sum(discqaly_hcc) )
  
  mcresults_CLTInotest1[j,]<-results
}
  
