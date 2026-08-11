ncycles<-54
n<-markovn2[4]

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
  transmat<-matrix(c(probAtoA_6monthA,probAtoB_6monthA,probAtoC_6monthA,0,probAtoE_6monthA,0,0,probAtoAH_6monthA,0,0,0,0,0,0,probAtoAI_6monthA,0,0,0,0,0,0,probAtoJ_6monthA,p_death,
                     
                     0,probBtoB_6monthA,0,probBtoD_6monthA,probBtoE_6monthA,0,0,0,probBtoBH_6monthA,0,0,0,0,0,0,probBtoBI_6monthA,0,0,0,0,0,probBtoJ_6monthA,p_death,
                     
                     0,probCtoB_6monthA,probCtoC_6monthA,0,probCtoE_6monthA,0,probCtoG_6monthA,0,0,probCtoCH_6monthA,0,0,0,0,0,0,probCtoCI_6monthA,0,0,0,0,probCtoJ_6monthA,p_death,
                     
                     0,0,0,probDtoD_6monthA,probDtoE_6monthA,probDtoF_6monthA,0,0,0,0,probDtoDH_6monthA,0,0,0,0,0,0,probDtoDI_6monthA,0,0,0,probDtoJ_6monthA,p_death,
                     
                     0,0,0,0,probEtoE_6monthA,0,0,0,0,0,0,probEtoEH_6monthA,0,0,0,0,0,0,probEtoEI_6monthA,0,0,probEtoJ_6monthA,p_death,
                     
                     0,0,0,0,probFtoE_6monthA,probFtoF_6monthA,0,0,0,0,0,0,probFtoFH_6monthA,0,0,0,0,0,probFtoFI_6monthA,0,0,probFtoJ_6monthA,p_death,
                     
                     0,probGtoB_6monthA,0,0,probGtoE_6monthA,0,probGtoG_6monthA,0,0,0,0,0,0,probGtoGH_6monthA,0,0,0,0,0,probGtoGI_6monthA,0,probGtoJ_6monthA, p_death,
                     
                     probAHtoA_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probAHtoJ_6monthA,p_death,
                     
                     0,probBHtoB_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probBHtoJ_6monthA,p_death,
                     
                     0,0,probCHtoC_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probCHtoJ_6monthA,p_death,
                     
                     0,0,0,probDHtoD_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probDHtoJ_6monthA,p_death,
                     
                     0,0,0,0,probEHtoE_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probEHtoJ_6monthA,p_death,
                     
                     0,0,0,0,0,probFHtoF_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probFHtoJ_6monthA,p_death,
                     
                     0,0,0,0,0,0,probGHtoG_6monthA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,probGHtoJ_6monthA,p_death,
                     
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
  
  row_sums <- rowSums(transmat)
  
  print(data.frame(
    state = statenames,
    row_sum = row_sums,
    difference = row_sums - 1
  ))
  
  if(run_tests){
  check_transition_rows(transmat)
  check_negative_transitions(transmat)
  }
  
  markovtrace[1+i,] <- markovtrace[i,] %*% transmat
  
  transflows <- sweep(transmat, 1, markovtrace[i,], "*")
  
  translist[[i]] <- transflows
  
}

dim(transmat)
length(transmat)

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

tot_trans_cost <- numeric(ncycles)

for(i in 1:ncycles){
  df <- 1/(1 + discountrate)^((i-1)/2)   # beginning of cycle i
  tot_trans_cost[i] <- sum(translist[[i]] * trans_cost_mat) * df
}

sum(tot_trans_cost)

hcc_cost <- numeric(ncycles) hcc_qaly <- numeric(ncycles)
for(i in 1:ncycles){ hcc_cost[i] <- ( markovtrace[i, "Cost"] + markovtrace[i + 1, "Cost"] ) / 2 
hcc_qaly[i] <- ( markovtrace[i, "QALY"] + markovtrace[i + 1, "QALY"] ) / 2 } 
cycle_midpoint_years <- ((1:ncycles) - 0.5) * 0.5 
disc <- 1 / ((1 + discountrate)^cycle_midpoint_years)
disccost_hcc <- hcc_cost * disc 
discqaly_hcc <- hcc_qaly * disc
results <- c( sum(disccost_hcc) + (n * costTest) + sum(tot_trans_cost), sum(discqaly_hcc) )

CLTIasptest<-results
CLTIasptestmeanresults<-results/n

CLTIasptestamps<-0

for (i in 1:length(translist)){
  CLTIasptestamps<-CLTIasptestamps+sum(translist[[i]][-c(5, 12, 19),5])
}

CLTIasptestrevasc<-0

for (i in 1:length(translist)){
  CLTIasptestrevasc<-CLTIasptestrevasc+sum((translist[[i]][-c(3, 10, 11, 13, 14, 17, 18, 20, 21),3])+(translist[[i]][-c(4, 10, 11, 13, 14, 17, 18, 20, 21),4])+(translist[[i]][-c(6, 10, 11, 13, 14, 17, 18, 20, 21),6])+(translist[[i]][-c(7, 10, 11, 13, 14, 17, 18, 20, 21),7]))
}

CLTIaspteststroke<-0

for (i in 1:length(translist)){
  CLTIaspteststroke<-CLTIaspteststroke+sum((translist[[i]][-c(8),8])+(translist[[i]][-c(9),9])+(translist[[i]][-c(10),10])+(translist[[i]][-c(11),11])+(translist[[i]][-c(12),12])+(translist[[i]][-c(13),13])+(translist[[i]][-c(14),14]))
}

CLTIasptestMI<-0

for (i in 1:length(translist)){
  CLTIasptestMI<-CLTIasptestMI+sum((translist[[i]][-c(15),15])+(translist[[i]][-c(16),16])+(translist[[i]][-c(17),17])+(translist[[i]][-c(18),18])+(translist[[i]][-c(19),19])+(translist[[i]][-c(20),20])+(translist[[i]][-c(21),21]))
}

CLTIasptestdeath<-0

for (i in 1:length(translist)){
  CLTIasptestdeath<-CLTIasptestdeath+sum((translist[[i]][-c(22),22]))

}
