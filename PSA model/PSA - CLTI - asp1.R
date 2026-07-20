ncycles<-54


mcresults_CLTIasp1<-data.frame(matrix(nrow=mcruns,ncol=2))
colnames(mcresults_CLTIasp1)<-c("Cost","QALY")

for(j in 1:mcruns){
  n<-markovn2[j,3]
  source(file.path(root,"PSA model","Data","PSA - Transition probabilities.R"))
  source(file.path(root,"PSA model","Data","PSA - Costs.R"))
  source(file.path(root,"PSA model","Data","PSA - Utilities.R"))
  #Transition Matrix            
  transmat<-matrix(c(probAtoA_6monthA,probAtoB_6monthA,probAtoC_6monthA,0,probAtoE_6monthA,0,0,probAtoAH_6monthA,0,0,0,0,0,0,probAtoAI_6monthA,0,0,0,0,0,0,probAtoJ_6monthA,p_death,
                     0,probBtoB_6monthA,0,probBtoD_6monthA,probBtoE_6monthA,0,0,0,probBtoBH_6monthA,0,0,0,0,0,0,probBtoBI_6monthA,0,0,0,0,0,probBtoJ_6monthA,p_death,
                     0,probCtoB_6monthA,probCtoC_6monthA,0,probCtoE_6monthA,0,probCtoG_6monthA,0,0,probCtoCH_6monthA,0,0,0,0,0,0,probCtoCI_6monthA,0,0,0,0,probCtoJ_6monthA,p_death,
                     0,0,0,probDtoD_6monthA,probDtoE_6monthA,probDtoF_6monthA,0,0,0,0,probDtoDH_6monthA,0,0,0,0,0,0,probDtoDI_6monthA,0,0,0,probDtoJ_6monthA,p_death,
                     0,0,0,0,probEtoE_6monthA,0,0,0,0,0,0,probEtoEH_6monthA,0,0,0,0,0,0,probEtoEI_6monthA,0,0,probEtoJ_6monthA,p_death,
                     0,0,0,0,probFtoE_6monthA,probFtoF_6monthA,0,0,0,0,0,0,probFtoFH_6monthA,0,0,0,0,0,0,probFtoFI_6monthA,0,probFtoJ_6monthA,p_death,
                     0,probGtoB_6monthA,0,0,probGtoE_6monthA,0,probGtoG_6monthA,0,0,0,0,0,0,probGtoGH_6monthA,0,0,0,0,0,0,probGtoGI_6monthA,probGtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,probAHtoA_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,0,probBHtoB_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,0,0,probCHtoC_6monthA,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,0,0,0,probDHtoD_6monthA,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,probEHtoE_6monthA,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,probFHtoF_6monthA,0,0,0,0,0,0,0,0,probFHtoJ_6monthA,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoG_6monthA,0,0,0,0,0,0,0,probGHtoJ_6monthA,p_death,
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
    
    transmat<-matrix(c(probAtoA_6monthA,probAtoB_6monthA,probAtoC_6monthA,0,probAtoE_6monthA,0,0,probAtoAH_6monthA,0,0,0,0,0,0,probAtoAI_6monthA,0,0,0,0,0,0,probAtoJ_6monthA,p_death,
                       0,probBtoB_6monthA,0,probBtoD_6monthA,probBtoE_6monthA,0,0,0,probBtoBH_6monthA,0,0,0,0,0,0,probBtoBI_6monthA,0,0,0,0,0,probBtoJ_6monthA,p_death,
                       0,probCtoB_6monthA,probCtoC_6monthA,0,probCtoE_6monthA,0,probCtoG_6monthA,0,0,probCtoCH_6monthA,0,0,0,0,0,0,probCtoCI_6monthA,0,0,0,0,probCtoJ_6monthA,p_death,
                       0,0,0,probDtoD_6monthA,probDtoE_6monthA,probDtoF_6monthA,0,0,0,0,probDtoDH_6monthA,0,0,0,0,0,0,probDtoDI_6monthA,0,0,0,probDtoJ_6monthA,p_death,
                       0,0,0,0,probEtoE_6monthA,0,0,0,0,0,0,probEtoEH_6monthA,0,0,0,0,0,0,probEtoEI_6monthA,0,0,probEtoJ_6monthA,p_death,
                       0,0,0,0,probFtoE_6monthA,probFtoF_6monthA,0,0,0,0,0,0,probFtoFH_6monthA,0,0,0,0,0,0,probFtoFI_6monthA,0,probFtoJ_6monthA,p_death,
                       0,probGtoB_6monthA,0,0,probGtoE_6monthA,0,probGtoG_6monthA,0,0,0,0,0,0,probGtoGH_6monthA,0,0,0,0,0,0,probGtoGI_6monthA,probGtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,probAHtoA_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,0,probBHtoB_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,0,0,probCHtoC_6monthA,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,0,0,0,probDHtoD_6monthA,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,probEHtoE_6monthA,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,probFHtoF_6monthA,0,0,0,0,0,0,0,0,probFHtoJ_6monthA,p_death,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoG_6monthA,0,0,0,0,0,0,0,probGHtoJ_6monthA,p_death,
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
  
  markovtrace["Cost"]<-(markovtrace["IC"]*costA_A)+
    (markovtrace["CLTI"]*costB_A)+
    (markovtrace["RevascIC"]*costC_A)+
    (markovtrace["RevascCLTI"]*costD_A)+
    (markovtrace["Amputation"]*costE_A)+
    (markovtrace["ReintCLTI"]*costF_A)+
    (markovtrace["ReintIC"]*costG_A)+
    (markovtrace["ICStroke"]*costAH_A)+
    (markovtrace["CLTIStroke"]*costBH_A)+
    (markovtrace["RevascICStroke"]*costCH_A)+
    (markovtrace["RevascCLTIStroke"]*costDH_A)+
    (markovtrace["AmputationStroke"]*costEH_A)+
    (markovtrace["ReintCLTIStroke"]*costFH_A)+
    (markovtrace["ReintICStroke"]*costGH_A)+
    (markovtrace["ICMI"]*costAI_A)+
    (markovtrace["CLTIMI"]*costBI_A)+
    (markovtrace["RevascICMI"]*costCI_A)+
    (markovtrace["RevascCLTIMI"]*costDI_A)+
    (markovtrace["AmputationMI"]*costEI_A)+
    (markovtrace["ReintCLTIMI"]*costFI_A)+
    (markovtrace["ReintICMI"]*costGI_A)+
    (markovtrace["VascularDeath"]*costJ)+
    (markovtrace["OtherDeath"]*costK)
  
  markovtrace["QALY"]<-(markovtrace["IC"]*qolAa)+
    (markovtrace["CLTI"]*qolBa)+
    (markovtrace["RevascIC"]*qolCa)+
    (markovtrace["RevascCLTI"]*qolDa)+
    (markovtrace["Amputation"]*qolEa)+
    (markovtrace["ReintCLTI"]*qolFa)+
    (markovtrace["ReintIC"]*qolGa)+
    (markovtrace["ICStroke"]*qolAHa)+
    (markovtrace["CLTIStroke"]*qolBHa)+
    (markovtrace["RevascICStroke"]*qolCHa)+
    (markovtrace["RevascCLTIStroke"]*qolDHa)+
    (markovtrace["AmputationStroke"]*qolEHa)+
    (markovtrace["ReintCLTIStroke"]*qolFHa)+
    (markovtrace["ReintICStroke"]*qolGHa)+
    (markovtrace["ICMI"]*qolAIa)+
    (markovtrace["CLTIMI"]*qolBIa)+
    (markovtrace["RevascICMI"]*qolCIa)+
    (markovtrace["RevascCLTIMI"]*qolDIa)+
    (markovtrace["AmputationMI"]*qolEIa)+
    (markovtrace["ReintCLTIMI"]*qolFIa)+
    (markovtrace["ReintICMI"]*qolGIa)+
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
mcresults_CLTIasp1[j,]<-results
}
