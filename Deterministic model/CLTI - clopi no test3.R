ncycles<-54
n<-markovn2[1]

#Transition Matrix            
transmat<-matrix(c(probAtoA_6monthB,probAtoB_6monthB,probAtoC_6monthB,0,probAtoE_6monthB,0,0,probAtoAH_6monthB,0,0,0,0,0,0,probAtoAI_6monthB,0,0,0,0,0,0,probAtoJ_6monthB,p_death,
                   0,probBtoB_6monthB,0,probBtoD_6monthB,probBtoE_6monthB,0,0,0,probBtoBH_6monthB,0,0,0,0,0,0,probBtoBI_6monthB,0,0,0,0,0,probBtoJ_6monthB,p_death,
                   0,probCtoB_6monthB,probCtoC_6monthB,0,probCtoE_6monthB,0,probCtoG_6monthB,0,0,probCtoCH_6monthB,0,0,0,0,0,0,probCtoCI_6monthB,0,0,0,0,probCtoJ_6monthB,p_death,
                   0,0,0,probDtoD_6monthB,probDtoE_6monthB,probDtoF_6monthB,0,0,0,0,probDtoDH_6monthB,0,0,0,0,0,0,probDtoDI_6monthB,0,0,0,probDtoJ_6monthB,p_death,
                   0,0,0,0,probEtoE_6monthB,0,0,0,0,0,0,probEtoEH_6monthB,0,0,0,0,0,0,probEtoEI_6monthB,0,0,probEtoJ_6monthB,p_death,
                   0,0,0,0,probFtoE_6monthB,probFtoF_6monthB,0,0,0,0,0,0,probFtoFH_6monthB,0,0,0,0,0,0,probFtoFI_6monthB,0,probFtoJ_6monthB,p_death,
                   0,probGtoB_6monthB,0,0,probGtoE_6monthB,0,probGtoG_6monthB,0,0,0,0,0,0,probGtoGH_6monthB,0,0,0,0,0,0,probGtoGI_6monthB,probGtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,probAHtoA_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,0,probBHtoB_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,0,0,probCHtoC_6monthB,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,0,0,0,probDHtoD_6monthB,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,0,0,0,0,probEHtoE_6monthB,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,0,0,0,0,0,probFHtoF_6monthB,0,0,0,0,0,0,0,0,probFHtoJ_6monthB,p_death,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoG_6monthB,0,0,0,0,0,0,0,probGHtoJ_6monthB,p_death,
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
  
  probAtoA_6monthB<-1-(probAtoB_6monthB+probAtoC_6monthB+probAtoE_6monthB+probAtoAH_6monthB+probAtoAI_6monthB+probAtoJ_6monthB+p_death)
  probBtoB_6monthB<-1-(probBtoD_6monthB+probBtoE_6monthB+probBtoBH_6monthB+probBtoBI_6monthB+probBtoJ_6monthB+p_death)
  probCtoC_6monthB<-1-(probCtoB_6monthB+probCtoE_6monthB+probCtoG_6monthB+probCtoCH_6monthB+probCtoCI_6monthB+probCtoJ_6monthB+p_death)
  probDtoD_6monthB<-1-(probDtoE_6monthB+probDtoF_6monthB+probDtoDH_6monthB+probDtoDI_6monthB+probDtoJ_6monthB+p_death)
  probEtoE_6monthB<-1-(probEtoEH_6monthB+probEtoEI_6monthB+probEtoJ_6monthB+p_death)
  probFtoF_6monthB<-1-(probFtoE_6monthB+probFtoFH_6monthB+probFtoFI_6monthB+probFtoJ_6monthB+p_death)
  probGtoG_6monthB<-1-(probGtoB_6monthB+probGtoE_6monthB+probGtoGH_6monthB+probGtoGI_6monthB+probGtoJ_6monthB+p_death)
  
  probAHtoA_6monthB<-1-probAHtoJ_6monthB-p_death
  probBHtoB_6monthB<-1-probBHtoJ_6monthB-p_death
  probCHtoC_6monthB<-1-probCHtoJ_6monthB-p_death
  probDHtoD_6monthB<-1-probDHtoJ_6monthB-p_death
  probEHtoE_6monthB<-1-probEHtoJ_6monthB-p_death
  probFHtoF_6monthB<-1-probFHtoJ_6monthB-p_death
  probGHtoG_6monthB<-1-probGHtoJ_6monthB-p_death
  
  probAItoA_6month<-1-(p_death+probAItoJ_6month)
  probBItoB_6month<-1-(p_death+probBItoJ_6month)
  probCItoC_6month<-1-(p_death+probCItoJ_6month)
  probDItoD_6month<-1-(p_death+probDItoJ_6month)
  probEItoE_6month<-1-(p_death+probEItoJ_6month)
  probFItoF_6month<-1-(p_death+probFItoJ_6month)
  probGItoG_6month<-1-(p_death+probGItoJ_6month)
  
  transmat<-matrix(c(probAtoA_6monthB,probAtoB_6monthB,probAtoC_6monthB,0,probAtoE_6monthB,0,0,probAtoAH_6monthB,0,0,0,0,0,0,probAtoAI_6monthB,0,0,0,0,0,0,probAtoJ_6monthB,p_death,
                     0,probBtoB_6monthB,0,probBtoD_6monthB,probBtoE_6monthB,0,0,0,probBtoBH_6monthB,0,0,0,0,0,0,probBtoBI_6monthB,0,0,0,0,0,probBtoJ_6monthB,p_death,
                     0,probCtoB_6monthB,probCtoC_6monthB,0,probCtoE_6monthB,0,probCtoG_6monthB,0,0,probCtoCH_6monthB,0,0,0,0,0,0,probCtoCI_6monthB,0,0,0,0,probCtoJ_6monthB,p_death,
                     0,0,0,probDtoD_6monthB,probDtoE_6monthB,probDtoF_6monthB,0,0,0,0,probDtoDH_6monthB,0,0,0,0,0,0,probDtoDI_6monthB,0,0,0,probDtoJ_6monthB,p_death,
                     0,0,0,0,probEtoE_6monthB,0,0,0,0,0,0,probEtoEH_6monthB,0,0,0,0,0,0,probEtoEI_6monthB,0,0,probEtoJ_6monthB,p_death,
                     0,0,0,0,probFtoE_6monthB,probFtoF_6monthB,0,0,0,0,0,0,probFtoFH_6monthB,0,0,0,0,0,0,probFtoFI_6monthB,0,probFtoJ_6monthB,p_death,
                     0,probGtoB_6monthB,0,0,probGtoE_6monthB,0,probGtoG_6monthB,0,0,0,0,0,0,probGtoGH_6monthB,0,0,0,0,0,0,probGtoGI_6monthB,probGtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,probAHtoA_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,0,probBHtoB_6monthB,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,0,0,probCHtoC_6monthB,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,0,0,0,probDHtoD_6monthB,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,probEHtoE_6monthB,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,probFHtoF_6monthB,0,0,0,0,0,0,0,0,probFHtoJ_6monthB,p_death,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoG_6monthB,0,0,0,0,0,0,0,probGHtoJ_6monthB,p_death,
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
CLTInotest3<-results
CLTInotest3meanresults<-results/n

CLTInotest3amps<-0

for (i in 1:length(translist)){
  CLTInotest3amps<-CLTInotest3amps+sum(translist[[i]][-c(5, 12, 19),5])
}

CLTInotest3revasc<-0

for (i in 1:length(translist)){
  CLTInotest3revasc<-CLTInotest3revasc+sum((translist[[i]][-c(3, 10, 11, 13, 14, 17, 18, 20, 21),3])+(translist[[i]][-c(4, 10, 11, 13, 14, 17, 18, 20, 21),4])+(translist[[i]][-c(6, 10, 11, 13, 14, 17, 18, 20, 21),6])+(translist[[i]][-c(7, 10, 11, 13, 14, 17, 18, 20, 21),7]))
}

CLTInotest3stroke<-0

for (i in 1:length(translist)){
  CLTInotest3stroke<-CLTInotest3stroke+sum((translist[[i]][-c(8),8])+(translist[[i]][-c(9),9])+(translist[[i]][-c(10),10])+(translist[[i]][-c(11),11])+(translist[[i]][-c(12),12])+(translist[[i]][-c(13),13])+(translist[[i]][-c(14),14]))
}

CLTInotest3MI<-0

for (i in 1:length(translist)){
  CLTInotest3MI<-CLTInotest3MI+sum((translist[[i]][-c(15),15])+(translist[[i]][-c(16),16])+(translist[[i]][-c(17),17])+(translist[[i]][-c(18),18])+(translist[[i]][-c(19),19])+(translist[[i]][-c(20),20])+(translist[[i]][-c(21),21]))
}

CLTInotest3death<-0

for (i in 1:length(translist)){
  CLTInotest3death<-CLTInotest3death+sum((translist[[i]][-c(22),22]))
}
