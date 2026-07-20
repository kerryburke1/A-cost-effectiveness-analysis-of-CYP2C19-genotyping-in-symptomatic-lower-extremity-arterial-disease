ncycles<-54

mcresults_CLTIrivasp1<-data.frame(matrix(nrow=mcruns,ncol=2))
colnames(mcresults_CLTIrivasp1)<-c("Cost","QALY")

for(j in 1:mcruns){
  n<-markovn2[j,2]
  source(file.path(root,"PSA model","Data","PSA - Transition probabilities.R"))
  source(file.path(root,"PSA model","Data","PSA - Costs.R"))
  source(file.path(root,"PSA model","Data","PSA - Utilities.R"))
  #Transition Matrix            
  transmat<-matrix(c(probAtoA_6monthR,probAtoB_6monthR,probAtoC_6monthR,0,probAtoE_6monthR,0,0,probAtoAH_6monthR,0,0,0,0,0,0,probAtoAI_6monthR,0,0,0,0,0,0,probAtoJ_6monthR,p_death,
                     0,probBtoB_6monthR,0,probBtoD_6monthR,probBtoE_6monthR,0,0,0,probBtoBH_6monthR,0,0,0,0,0,0,probBtoBI_6monthR,0,0,0,0,0,probBtoJ_6monthR,p_death,
                     0,probCtoB_6monthR,probCtoC_6monthR,0,probCtoE_6monthR,0,probCtoG_6monthR,0,0,probCtoCH_6monthR,0,0,0,0,0,0,probCtoCI_6monthR,0,0,0,0,probCtoJ_6monthR,p_death,
                     0,0,0,probDtoD_6monthR,probDtoE_6monthR,probDtoF_6monthR,0,0,0,0,probDtoDH_6monthR,0,0,0,0,0,0,probDtoDI_6monthR,0,0,0,probDtoJ_6monthR,p_death,
                     0,0,0,0,probEtoE_6monthR,0,0,0,0,0,0,probEtoEH_6monthR,0,0,0,0,0,0,probEtoEI_6monthR,0,0,probEtoJ_6monthR,p_death,
                     0,0,0,0,probFtoE_6monthR,probFtoF_6monthR,0,0,0,0,0,0,probFtoFH_6monthR,0,0,0,0,0,0,probFtoFI_6monthR,0,probFtoJ_6monthR,p_death,
                     0,probGtoB_6monthR,0,0,probGtoE_6monthR,0,probGtoG_6monthR,0,0,0,0,0,0,probGtoGH_6monthR,0,0,0,0,0,0,probGtoGI_6monthR,probGtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,probAHtoA_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,probBHtoB_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,0,probCHtoC_6monthR,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,0,0,probDHtoD_6monthR,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,probEHtoE_6monthR,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,probFHtoF_6monthR,0,0,0,0,0,0,0,0,probFHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoG_6monthR,0,0,0,0,0,0,0,probGHtoJ_6monthR,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,probAItoA_6month,0,0,0,0,0,0,probAItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probBItoB_6month,0,0,0,0,0,probBItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probCItoC_6month,0,0,0,0,probCItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probDItoD_6month,0,0,0,probDItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probEItoE_6month,0,0,probEItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probFItoF_6month,0,probFItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probGItoG_6month,probGItoJ_6month,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                   nrow=23,byrow=T)
  
  startdist<-c(0,n,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  rownames(transmat)<-colnames(transmat)<-statenames
  
  markovtrace<-matrix(data=NA,
                      nrow=ncycles+1,
                      ncol=nstates,
                      dimnames=list(NULL,statenames))
  markovtrace[1,]<-startdist
  
  translist<-list()
  
  for(i in 1:ncycles){
    
    p_death<-as.numeric(acm_clti[i,2])
    
    probAtoA_6monthR<-1-(probAtoB_6monthR+probAtoC_6monthR+probAtoE_6monthR+probAtoAH_6monthR+probAtoAI_6monthR+probAtoJ_6monthR+p_death)
    probBtoB_6monthR<-1-(probBtoD_6monthR+probBtoE_6monthR+probBtoBH_6monthR+probBtoBI_6monthR+probBtoJ_6monthR+p_death)
    probCtoC_6monthR<-1-(probCtoB_6monthR+probCtoE_6monthR+probCtoG_6monthR+probCtoCH_6monthR+probCtoCI_6monthR+probCtoJ_6monthR+p_death)
    probDtoD_6monthR<-1-(probDtoE_6monthR+probDtoF_6monthR+probDtoDH_6monthR+probDtoDI_6monthR+probDtoJ_6monthR+p_death)
    probEtoE_6monthR<-1-(probEtoEH_6monthR+probEtoEI_6monthR+probEtoJ_6monthR+p_death)
    probFtoF_6monthR<-1-(probFtoE_6monthR+probFtoFH_6monthR+probFtoFI_6monthR+probFtoJ_6monthR+p_death)
    probGtoG_6monthR<-1-(probGtoB_6monthR+probGtoE_6monthR+probGtoGH_6monthR+probGtoGI_6monthR+probGtoJ_6monthR+p_death)
    
    probAHtoA_6monthR<-1-probAHtoJ_6monthR-p_death
    probBHtoB_6monthR<-1-probBHtoJ_6monthR-p_death
    probCHtoC_6monthR<-1-probCHtoJ_6monthR-p_death
    probDHtoD_6monthR<-1-probDHtoJ_6monthR-p_death
    probEHtoE_6monthR<-1-probEHtoJ_6monthR-p_death
    probFHtoF_6monthR<-1-probFHtoJ_6monthR-p_death
    probGHtoG_6monthR<-1-probGHtoJ_6monthR-p_death
    
    probAItoA_6month<-1-(p_death+probAItoJ_6month)
    probBItoB_6month<-1-(p_death+probBItoJ_6month)
    probCItoC_6month<-1-(p_death+probCItoJ_6month)
    probDItoD_6month<-1-(p_death+probDItoJ_6month)
    probEItoE_6month<-1-(p_death+probEItoJ_6month)
    probFItoF_6month<-1-(p_death+probFItoJ_6month)
    probGItoG_6month<-1-(p_death+probGItoJ_6month)
    
    transmat<-matrix(c(probAtoA_6monthR,probAtoB_6monthR,probAtoC_6monthR,0,probAtoE_6monthR,0,0,probAtoAH_6monthR,0,0,0,0,0,0,probAtoAI_6monthR,0,0,0,0,0,0,probAtoJ_6monthR,p_death,
                       0,probBtoB_6monthR,0,probBtoD_6monthR,probBtoE_6monthR,0,0,0,probBtoBH_6monthR,0,0,0,0,0,0,probBtoBI_6monthR,0,0,0,0,0,probBtoJ_6monthR,p_death,
                       0,probCtoB_6monthR,probCtoC_6monthR,0,probCtoE_6monthR,0,probCtoG_6monthR,0,0,probCtoCH_6monthR,0,0,0,0,0,0,probCtoCI_6monthR,0,0,0,0,probCtoJ_6monthR,p_death,
                       0,0,0,probDtoD_6monthR,probDtoE_6monthR,probDtoF_6monthR,0,0,0,0,probDtoDH_6monthR,0,0,0,0,0,0,probDtoDI_6monthR,0,0,0,probDtoJ_6monthR,p_death,
                       0,0,0,0,probEtoE_6monthR,0,0,0,0,0,0,probEtoEH_6monthR,0,0,0,0,0,0,probEtoEI_6monthR,0,0,probEtoJ_6monthR,p_death,
                       0,0,0,0,probFtoE_6monthR,probFtoF_6monthR,0,0,0,0,0,0,probFtoFH_6monthR,0,0,0,0,0,0,probFtoFI_6monthR,0,probFtoJ_6monthR,p_death,
                       0,probGtoB_6monthR,0,0,probGtoE_6monthR,0,probGtoG_6monthR,0,0,0,0,0,0,probGtoGH_6monthR,0,0,0,0,0,0,probGtoGI_6monthR,probGtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,probAHtoA_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,probBHtoB_6monthR,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,0,probCHtoC_6monthR,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,0,0,probDHtoD_6monthR,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,probEHtoE_6monthR,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,probFHtoF_6monthR,0,0,0,0,0,0,0,0,probFHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoG_6monthR,0,0,0,0,0,0,0,probGHtoJ_6monthR,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,probAItoA_6month,0,0,0,0,0,0,probAItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probBItoB_6month,0,0,0,0,0,probBItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probCItoC_6month,0,0,0,0,probCItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probDItoD_6month,0,0,0,probDItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probEItoE_6month,0,0,probEItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probFItoF_6month,0,probFItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probGItoG_6month,probGItoJ_6month,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                     nrow=23,byrow=T)
    
    if(run_tests){
      check_transition_rows(transmat)
      check_negative_transitions(transmat)
    }
    
    markovtrace[1+i,]<-markovtrace[i,] %*% transmat
    translist[[i]]<-markovtrace[i,]*transmat
  }
  
  translist[[5]]
  
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
  
  trans_cost_mat <- matrix(0, nrow=nstates, ncol=nstates,
                           dimnames=list(statenames, statenames))
  
  trans_cost_num <- unname(as.matrix(trans_cost_mat))
  
  tot_trans_cost<-data.frame(matrix(nrow=54,ncol=23))
  
  for(i in 1:54){
    
    df <- 1/(1+discountrate)^(i/2)
    
    tot_trans_cost[i] <- 
      sum(translist[[i]] * trans_cost_mat * df)
  }
  
  sum(tot_trans_cost)
  
  markovtrace["disc"]<-1/((1+discountrate)^(markovtrace["SixMonths"]*0.5))
  markovtrace["disccost"]<-markovtrace["Cost"]*
    markovtrace["disc"]
  markovtrace["discqaly"]<-markovtrace["QALY"]*
    markovtrace["disc"]
  
  results<-c(sum(markovtrace["disccost"]+(n*(costTest*0.5))+sum(rowSums(tot_trans_cost))),sum(markovtrace["discqaly"]))
  mcresults_CLTIrivasp1[j,]<-results
}
  
