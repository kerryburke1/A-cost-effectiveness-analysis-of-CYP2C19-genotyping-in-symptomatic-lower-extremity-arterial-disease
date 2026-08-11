ncycles<-66

mcresults_ICrivasptest<-data.frame(matrix(nrow=mcruns,ncol=2))
colnames(mcresults_ICrivasptest)<-c("Cost","QALY")

for(j in 1:mcruns){
  n<-markovn[j,3]
  
  source(file.path(root,"PSA model","Data","PSA - Transition probabilities.R"))
  source(file.path(root,"PSA model","Data","PSA - Costs.R"))
  source(file.path(root,"PSA model","Data","PSA - Utilities.R"))
  
  startdist<-c(n,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  markovtrace<-matrix(data=NA,
                      nrow=ncycles+1,
                      ncol=nstates,
                      dimnames=list(NULL,statenames))
  markovtrace[1,]<-startdist
  
  translist<-list()
  
  for(i in 1:ncycles){

    p_death<-as.numeric(acm_ic[i,2])
    
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
    transmat<-matrix(c(probAtoA_6monthR,probAtoB_6monthR,probAtoC_6monthR,0,probAtoE_6monthR,0,0,probAtoAH_6monthR,0,0,0,0,0,0,probAtoAI_6monthR,0,0,0,0,0,0,probAtoJ_6monthR,p_death,
                       
                       0,probBtoB_6monthR,0,probBtoD_6monthR,probBtoE_6monthR,0,0,0,probBtoBH_6monthR,0,0,0,0,0,0,probBtoBI_6monthR,0,0,0,0,0,probBtoJ_6monthR,p_death,
                       
                       0,probCtoB_6monthR,probCtoC_6monthR,0,probCtoE_6monthR,0,probCtoG_6monthR,0,0,probCtoCH_6monthR,0,0,0,0,0,0,probCtoCI_6monthR,0,0,0,0,probCtoJ_6monthR,p_death,
                       
                       0,0,0,probDtoD_6monthR,probDtoE_6monthR,probDtoF_6monthR,0,0,0,0,probDtoDH_6monthR,0,0,0,0,0,0,probDtoDI_6monthR,0,0,0,probDtoJ_6monthR,p_death,
                       
                       0,0,0,0,probEtoE_6monthR,0,0,0,0,0,0,probEtoEH_6monthR,0,0,0,0,0,0,probEtoEI_6monthR,0,0,probEtoJ_6monthR,p_death,
                       
                       0,0,0,0,probFtoE_6monthR,probFtoF_6monthR,0,0,0,0,0,0,probFtoFH_6monthR,0,0,0,0,0,probFtoFI_6monthR,0,0,probFtoJ_6monthR,p_death,
                       
                       0,probGtoB_6monthR,0,0,probGtoE_6monthR,0,probGtoG_6monthR,0,0,0,0,0,0,probGtoGH_6monthR,0,0,0,0,0,probGtoGI_6monthR,0,probGtoJ_6monthR, p_death,
                       
                       probAHtoA_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthR,p_death,
                       
                       0,probBHtoB_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthR,p_death,
                       
                       0,0,probCHtoC_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthR,p_death,
                       
                       0,0,0,probDHtoD_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthR,p_death,
                       
                       0,0,0,0,probEHtoE_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthR,p_death,
                       
                       0,0,0,0,0,probFHtoF_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probFHtoJ_6monthR,p_death,
                       
                       0,0,0,0,0,0,probGHtoG_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoJ_6monthR,p_death,
                       
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
  
  markovtrace["Cost"]<-(markovtrace["IC"]*costA_R)+
    (markovtrace["CLTI"]*costB_R)+
    (markovtrace["RevascIC"]*costC_R)+
    (markovtrace["RevascCLTI"]*costD_R)+
    (markovtrace["Amputation"]*costE_R)+
    (markovtrace["ReintCLTI"]*costF_R)+
    (markovtrace["ReintIC"]*costG_R)+
    (markovtrace["ICStroke"]*costAH_R)+
    (markovtrace["CLTIStroke"]*costBH_R)+
    (markovtrace["RevascICStroke"]*costCH_R)+
    (markovtrace["RevascCLTIStroke"]*costDH_R)+
    (markovtrace["AmputationStroke"]*costEH_R)+
    (markovtrace["ReintCLTIStroke"]*costFH_R)+
    (markovtrace["ReintICStroke"]*costGH_R)+
    (markovtrace["ICMI"]*costAI_R)+
    (markovtrace["CLTIMI"]*costBI_R)+
    (markovtrace["RevascICMI"]*costCI_R)+
    (markovtrace["RevascCLTIMI"]*costDI_R)+
    (markovtrace["AmputationMI"]*costEI_R)+
    (markovtrace["ReintCLTIMI"]*costFI_R)+
    (markovtrace["ReintICMI"]*costGI_R)+
    (markovtrace["VascularDeath"]*costJ)+
    (markovtrace["OtherDeath"]*costK)
  
  markovtrace["QALY"]<-(markovtrace["IC"]*qolAr)+
    (markovtrace["CLTI"]*qolBr)+
    (markovtrace["RevascIC"]*qolCr)+
    (markovtrace["RevascCLTI"]*qolDr)+
    (markovtrace["Amputation"]*qolEr)+
    (markovtrace["ReintCLTI"]*qolFr)+
    (markovtrace["ReintIC"]*qolGr)+
    (markovtrace["ICStroke"]*qolAHr)+
    (markovtrace["CLTIStroke"]*qolBHr)+
    (markovtrace["RevascICStroke"]*qolCHr)+
    (markovtrace["RevascCLTIStroke"]*qolDHr)+
    (markovtrace["AmputationStroke"]*qolEHr)+
    (markovtrace["ReintCLTIStroke"]*qolFHr)+
    (markovtrace["ReintICStroke"]*qolGHr)+
    (markovtrace["ICMI"]*qolAIr)+
    (markovtrace["CLTIMI"]*qolBIr)+
    (markovtrace["RevascICMI"]*qolCIr)+
    (markovtrace["RevascCLTIMI"]*qolDIr)+
    (markovtrace["AmputationMI"]*qolEIr)+
    (markovtrace["ReintCLTIMI"]*qolFIr)+
    (markovtrace["ReintICMI"]*qolGIr)+
    (markovtrace["VascularDeath"]*qolJ)+
    (markovtrace["OtherDeath"]*qolK)
  
  tot_trans_cost <- numeric(ncycles)
  
 for(i in 1:ncycles){
  df <- 1/(1 + discountrate)^((i-1)/2)   # beginning of cycle i
  tot_trans_cost[i] <- sum(translist[[i]] * trans_cost_mat) * df
}
  
  sum(tot_trans_cost)
  
  markovtrace["disc"] <- 1 / ((1 + discountrate)^(markovtrace["SixMonths"] * 0.5))
  
  markovtrace["disccost"] <- markovtrace["Cost"] * markovtrace["disc"]
  
  markovtrace["discqaly"] <- markovtrace["QALY"] * markovtrace["disc"]
  
 results <- c(
  sum(head(markovtrace$disccost, ncycles)) + (n * costTest) + sum(tot_trans_cost),
  sum(head(markovtrace$discqaly, ncycles))
)
  mcresults_ICrivasptest[j,]<-results
}
  
