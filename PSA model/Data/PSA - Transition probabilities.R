#MACE transition probabilities

probAtoAI_1_91yr<-rbeta(1,50,3173)
AtoAIrate<- -log(1-probAtoAI_1_91yr)/1.91
probAtoAI_6monthB<-1-exp(-0.5*AtoAIrate)

probGtoGI_6monthB<-probCtoCI_6monthB<-probAtoAI_6monthB

HRmean <- 1.14
HRlow <- 1.04
HRhigh <- 1.26
SElogHR <- (log(HRhigh) - log(HRlow)) / (2 * 1.96)
logHR_draw <- rnorm(1, (log(HRlow) + log(HRhigh)) / 2, SElogHR)
HR_draw <- exp(logHR_draw)
prev_LOF <- rbeta(1, 2145 + 1, 5338 + 1)
h_overall <- -log(1 - probAtoAI_6monthB) / 0.5
h_noLOF <- h_overall / ((1 - prev_LOF) + (prev_LOF * HR_draw))
probAtoAI_6monthC <- 1 - exp(-h_noLOF * 0.5)

probGtoGI_6monthC<-probCtoCI_6monthC<-probAtoAI_6monthC

probAtoAI_1_91yr<-rbeta(1,81,3148)
AtoAIrate<--log(1-probAtoAI_1_91yr)/1.91
probAtoAI_6monthA <-1-exp(-0.5*AtoAIrate)

probGtoGI_6monthA<-probCtoCI_6monthA<-probAtoAI_6monthA

HR_MI <- rlnorm(
  1,
  meanlog = log(0.86),
  sdlog = (log(1.05) - log(0.70)) / (2 * 1.96)
)

probAtoAI_6monthR <- probAtoAI_6monthA * HR_MI

probGtoGI_6monthR<-probCtoCI_6monthR<-probAtoAI_6monthR

probBtoBI_2_45yr<-rbeta(1,160,1274)
BtoBIrate<--log(1-probBtoBI_2_45yr)/2.45
probBtoBI_6monthB <-1-exp(-0.5*BtoBIrate)

probEtoEI_6monthB<-probFtoFI_6monthB<-probDtoDI_6monthB<-probBtoBI_6monthB

HRmean <- 1.14
HRlow <- 1.04
HRhigh <- 1.26
SElogHR <- (log(HRhigh) - log(HRlow)) / (2 * 1.96)
logHR_draw <- rnorm(1, (log(HRlow) + log(HRhigh)) / 2, SElogHR)
HR_draw <- exp(logHR_draw)
prev_LOF <- rbeta(1, 2145 + 1, 5338 + 1)
h_overall <- -log(1 - probBtoBI_6monthB) / 0.5
h_noLOF <- h_overall / ((1 - prev_LOF) + (prev_LOF * HR_draw))
probBtoBI_6monthC <- 1 - exp(-h_noLOF * 0.5)

probEtoEI_6monthC<-probFtoFI_6monthC<-probDtoDI_6monthC<-probBtoBI_6monthC

probBtoBI_1_91yr<-rbeta(1,50,3173) 
BtoBIrate<--log(1-probBtoBI_1_91yr)/1.91
probBtoBI_6monthB_caprie <-1-exp(-0.5*BtoBIrate)

CLTIriskexcess <- probBtoBI_6monthB / probBtoBI_6monthB_caprie
probBtoBI_6monthA <- probAtoAI_6monthA * CLTIriskexcess

probEtoEI_6monthA<-probFtoFI_6monthA<-probDtoDI_6monthA<-probBtoBI_6monthA

HR_MI <- rlnorm(
  1,
  meanlog = log(0.86),
  sdlog = (log(1.05) - log(0.70)) / (2 * 1.96)
)

probBtoBI_6monthR <- probBtoBI_6monthA * HR_MI

probEtoEI_6monthR<-probFtoFI_6monthR<-probDtoDI_6monthR<-probBtoBI_6monthR

probAtoAH_2_6yr<-rbeta(1,81,3142)
AtoAHrate<--log(1-probAtoAH_2_6yr)/2.6
probAtoAH_6monthB <-1-exp(-0.5*AtoAHrate)

probGtoGH_6monthB<-probCtoCH_6monthB<-probAtoAH_6monthB
probEtoEH_6monthB<-probFtoFH_6monthB<-probDtoDH_6monthB<-probBtoBH_6monthB<-probAtoAH_6monthB

HRmean <- 1.53
HRlow <- 1.04
HRhigh <- 2.26
SElogHR <- (log(HRhigh) - log(HRlow)) / (2 * 1.96)
logHR_draw <- rnorm(1, (log(HRlow) + log(HRhigh)) / 2, SElogHR)
HR_draw <- exp(logHR_draw)
prev_LOF <- rbeta(1, 2145 + 1, 5338 + 1)
h_overall <- -log(1 - probAtoAH_6monthB) / 0.5
h_noLOF <- h_overall / ((1 - prev_LOF) + (prev_LOF * HR_draw))
probAtoAH_6monthC <- 1 - exp(-h_noLOF * 0.5)

probGtoGH_6monthC<-probCtoCH_6monthC<-probAtoAH_6monthC
probEtoEH_6monthC<-probFtoFH_6monthC<-probDtoDH_6monthC<-probBtoBH_6monthC<-probAtoAH_6monthC

probAtoAH_1_9yr<-rbeta(1,70,3159)
AtoAHrate<--log(1-probAtoAH_1_9yr)/1.9
probAtoAH_6monthA <-1-exp(-0.5*AtoAHrate)

probGtoGH_6monthA<-probCtoCH_6monthA<-probAtoAH_6monthA
probEtoEH_6monthA<-probFtoFH_6monthA<-probDtoDH_6monthA<-probBtoBH_6monthA<-probAtoAH_6monthA

HR_Stroke <- rlnorm(
  1,
  meanlog = log(0.58),
  sdlog = (log(0.76) - log(0.44)) / (2 * 1.96)
)

probAtoAH_6monthR <- probEtoEH_6monthA * HR_Stroke

probGtoGH_6monthR<-probCtoCH_6monthR<-probAtoAH_6monthR
probEtoEH_6monthR<-probFtoFH_6monthR<-probDtoDH_6monthR<-probBtoBH_6monthR<-probAtoAH_6monthR

probBtoBH_2_45yr<-rbeta(1,83,1351) 
BtoBHrate<--log(1-probBtoBH_2_45yr)/2.45
probBtoBH_6monthB <-1-exp(-0.5*BtoBHrate)

probEtoEH_6monthB<-probFtoFH_6monthB<-probDtoDH_6monthB<-probBtoBH_6monthB

HRmean <- 1.53
HRlow <- 1.04
HRhigh <- 2.26
SElogHR <- (log(HRhigh) - log(HRlow)) / (2 * 1.96)
logHR_draw <- rnorm(1, (log(HRlow) + log(HRhigh)) / 2, SElogHR)
HR_draw <- exp(logHR_draw)
prev_LOF <- rbeta(1, 2145 + 1, 5338 + 1)
h_overall <- -log(1 - probBtoBH_6monthB) / 0.5
h_noLOF <- h_overall / ((1 - prev_LOF) + (prev_LOF * HR_draw))
probBtoBH_6monthC <- 1 - exp(-h_noLOF * 0.5)

probEtoEH_6monthC<-probFtoFH_6monthC<-probDtoDH_6monthC<-probBtoBH_6monthC

probBtoBH_1_91yr<-rbeta(1,70,3153)
BtoHrate<--log(1-probBtoBH_1_91yr)/1.91
probBtoBH_6monthB_caprie <-1-exp(-0.5*BtoBHrate)

CLTIriskexcess <- probBtoBH_6monthB / probBtoBH_6monthB_caprie
probBtoBH_6monthA <- probAtoAH_6monthA * CLTIriskexcess

HR_Stroke <- rlnorm(
  1,
  meanlog = log(0.58),
  sdlog = (log(0.76) - log(0.44)) / (2 * 1.96)
)
probBtoBH_6monthR <- probBtoBH_6monthA * HR_Stroke

probAtoJ_1_91yr<-rbeta(1,95,3128)
AtoJrate<--log(1-probAtoJ_1_91yr)/1.91
probAtoJ_6monthB<-1-exp(-0.5*AtoJrate)

probGtoJ_6monthB<-probCtoJ_6monthB<-probAtoJ_6monthB
probDtoJ_6monthB<-probEtoJ_6monthB<-probFtoJ_6monthB<-probBtoJ_6monthB<-probAtoJ_6monthB

probCtoJ_6monthC<-probGtoJ_6monthC<-probAtoJ_6monthC<-probAtoJ_6monthB
probDtoJ_6monthC<-probEtoJ_6monthC<-probFtoJ_6monthC<-probBtoJ_6monthC<-probAtoJ_6monthB

probAtoJ_1_91yr<-rbeta(1,122,2985)   
AtoJrate<--log(1-probAtoJ_1_91yr)/1.91
probAtoJ_6monthA<-1-exp(-0.5*AtoJrate)

probGtoJ_6monthA<-probCtoJ_6monthA<-probAtoJ_6monthA
probEtoJ_6monthA<-probFtoJ_6monthA<-probDtoJ_6monthA<-probBtoJ_6monthA<-probAtoJ_6monthA

HR_VD <- rlnorm(
  1,
  meanlog = log(0.78),
  sdlog = (log(0.96) - log(0.64)) / (2 * 1.96)
)

probEtoJ_6monthR <- probEtoJ_6monthA * HR_VD

probGtoJ_6monthR<-probCtoJ_6monthR<-probAtoJ_6monthR
probEtoJ_6monthR<-probFtoJ_6monthR<-probDtoJ_6monthR<-probBtoJ_6monthR<-probAtoJ_6monthR

probAtoAI_1_91yr<-rbeta(1,81,3148)
AtoAIrate<--log(1-probAtoAI_1_91yr)/1.91
probAtoAI_6monthA <-1-exp(-0.5*AtoAIrate)

probGtoJ_6monthA<-probCtoJ_6monthA<-probAtoJ_6monthA
probEtoJ_6monthA<-probFtoJ_6monthA<-probDtoJ_6monthA<-probBtoJ_6monthA<-probAtoJ_6monthA

probAHtoJ_1_1yr<-rbeta(1,13,612)
AHtoJrate<--log(1-probAHtoJ_1_1yr)/1.1
probAHtoJ_6monthB <-1-exp(-0.5*AHtoJrate)

probBHtoJ_6monthB<-probCHtoJ_6monthB<-probDHtoJ_6monthB<-probEHtoJ_6monthB<-probFHtoJ_6monthB<-probGHtoJ_6monthB<-probAHtoJ_6monthB
probBHtoJ_6monthC<-probCHtoJ_6monthC<-probDHtoJ_6monthC<-probEHtoJ_6monthC<-probFHtoJ_6monthC<-probGHtoJ_6monthC<-probAHtoJ_6monthC<-probAHtoJ_6monthB
probBHtoJ_6monthR<-probCHtoJ_6monthR<-probDHtoJ_6monthR<-probEHtoJ_6monthR<-probFHtoJ_6monthR<-probGHtoJ_6monthR<-probAHtoJ_6monthR<-probAHtoJ_6monthB
probBHtoJ_6monthA<-probCHtoJ_6monthA<-probDHtoJ_6monthA<-probEHtoJ_6monthA<-probFHtoJ_6monthA<-probGHtoJ_6monthA<-probAHtoJ_6monthA<-probAHtoJ_6monthB

probAItoJ_1yr<-rbeta(1,208,936)
AItoJrate<--log(1-probAItoJ_1yr)/1
probAItoJ_6month<-1-exp(-0.5*AItoJrate)

probBItoJ_6month<-probCItoJ_6month<-probDItoJ_6month<-probEItoJ_6month<-probFItoJ_6month<-probGItoJ_6month<-probAItoJ_6month

#MALE transition probabilities
probAtoB_5_3yr<-rbeta(1,53,670)
AtoBrate<--log(1-probAtoB_5_3yr)/5.3
probAtoB_6monthB<-1-exp(-0.5*AtoBrate)

probAtoB_6monthA<-probAtoB_6monthR<-probAtoB_6monthC<-probAtoB_6monthB

probCtoB_5_3yr<-rbeta(1,68,260)
CtoBrate<--log(1-probCtoB_5_3yr)/5.3
probCtoB_6monthB<-1-exp(-0.5*CtoBrate)

probCtoB_6monthA<-probCtoB_6monthR<-probCtoB_6monthC<-probCtoB_6monthB
probGtoB_6monthA<-probGtoB_6monthR<-probGtoB_6monthC<-probGtoB_6monthB<-probCtoB_6monthB

probAtoE_5yr<- rbeta(1,3,275)
AtoErate<--log(1-probAtoE_5yr)/5
probAtoE_6monthB <-1-exp(-0.5*AtoErate)

probBtoE_6monthB<-probAtoE_6monthB
probBtoE_6monthC<-probAtoE_6monthC<-probAtoE_6monthB

probAtoE_1_75yr<-rbeta(1,5,2134)
AtoErate<--log(1-probAtoE_1_75yr)/1.75
probAtoE_6monthR <-1-exp(-0.5*AtoErate)

probCtoE_6monthR<-probGtoE_6monthR<-probAtoE_6monthR
probDtoE_6monthR<-probFtoE_6monthR<-probBtoE_6monthR<-probAtoE_6monthR

probAtoE_1_75yr<-rbeta(1,15,2108)
AtoErate<--log(1-probAtoE_1_75yr)/1.75
probAtoE_6monthA <-1-exp(-0.5*AtoErate)

probCtoE_6monthA<-probGtoE_6monthA<-probAtoE_6monthA
probDtoE_6monthA<-probFtoE_6monthA<-probBtoE_6monthA<-probAtoE_6monthA

probCtoE_1yr<-rbeta(1,72,206)
CtoErate<--log(1-probCtoE_1yr)/1
probCtoE_6monthB <-1-exp(-0.5*CtoErate)

probFtoE_6monthB<-probDtoE_6monthB<-probGtoE_6monthB<-probCtoE_6monthB

probCtoE_1yr<-rbeta(1,28,125)
CtoErate<--log(1-probCtoE_1yr)/1
probCtoE_6monthC <-1-exp(-0.5*CtoErate)

probGtoE_6monthC<-probCtoE_6monthC
probFtoE_6monthC<-probDtoE_6monthC<-probCtoE_6monthC

probCtoE_2_33yr<-rbeta(1,103,3183)
CtoErate<--log(1-probCtoE_2_33yr)/2.33
probCtoE_6monthR <-1-exp(-0.5*CtoErate)

probGtoE_6monthR<-probCtoE_6monthR
probFtoE_6monthR<-probDtoE_6monthR<-probCtoE_6monthR

probCtoE_2_33yr<-rbeta(1,115,3163)
CtoErate<--log(1-probCtoE_2_33yr)/2.33
probCtoE_6monthA <-1-exp(-0.5*CtoErate)

probGtoE_6monthA<-probCtoE_6monthA
probFtoE_6monthA<-probDtoE_6monthA<-probCtoE_6monthA

probCtoG_5yr<-rbeta(1,81,97)
CtoGrate<--log(1-probCtoG_5yr)/5
probCtoG_6monthB <-1-exp(-0.5*CtoGrate)
probDtoF_6monthB <- probCtoG_6monthB

ORmean <- 1.88
ORlow <- 0.69
ORhigh <- 5.10
SElogOR <- (log(ORhigh) - log(ORlow)) / (2*1.96)
logOR <- rnorm(1, log(ORmean), SElogOR)
OR_draw <- exp(logOR)
prev_noLOF <- rbeta(1, 231 + 1, 95 + 1)
prev_LOF <- 1 - prev_noLOF
f <- function(p_noLOF){
  odds_noLOF <- p_noLOF/(1-p_noLOF)
  odds_LOF <- OR_draw * odds_noLOF
  p_LOF <- odds_LOF/(1+odds_LOF)
  prev_noLOF*p_noLOF + prev_LOF*p_LOF - probCtoG_6monthB
}
probCtoG_6monthC <- uniroot(f, c(1e-8, 0.999))$root
probDtoF_6monthC <- probCtoG_6monthC

probCtoG_2_33yr<-rbeta(1,584,2702)
CtoGrate<--log(1-probCtoG_2_33yr)/2.33
probCtoG_6monthR <-1-exp(-0.5*CtoGrate)

probDtoF_6monthR<-probCtoG_6monthR

probCtoG_2_33yr<-rbeta(1,655,2623)
CtoGrate<--log(1-probCtoG_2_33yr)/2.33
probCtoG_6monthA <-1-exp(-0.5*CtoGrate)

probDtoF_6monthA<-probCtoG_6monthA

IC_revasc_2024 = rbeta(1,3490,138563)  
probAtoC_6monthB<-IC_revasc_2024*0.5
probAtoC_6monthA<-probAtoC_6monthR<-probAtoC_6monthC<-probAtoC_6monthB

CLTI_revasc_2024<-rbeta(1,12630,34640)
probBtoD_6monthB<-CLTI_revasc_2024*0.5
probBtoD_6monthA<-probBtoD_6monthR<-probBtoD_6monthC<-probBtoD_6monthB

#Add probs of remaining in state as 1 minus the prob of transition
p_death<-0

probAtoA_6monthA<-1-(probAtoB_6monthA+probAtoC_6monthA+probAtoE_6monthA+probAtoAH_6monthA+probAtoAI_6monthA+probAtoJ_6monthA+p_death)
probBtoB_6monthA<-1-(probBtoD_6monthA+probBtoE_6monthA+probBtoBH_6monthA+probBtoBI_6monthA+probBtoJ_6monthA+p_death)
probCtoC_6monthA<-1-(probCtoB_6monthA+probCtoE_6monthA+probCtoG_6monthA+probCtoCH_6monthA+probCtoCI_6monthA+probCtoJ_6monthA+p_death)
probDtoD_6monthA<-1-(probDtoE_6monthA+probDtoF_6monthA+probDtoDH_6monthA+probDtoDI_6monthA+probDtoJ_6monthA+p_death)
probEtoE_6monthA<-1-(probEtoEH_6monthA+probEtoEI_6monthA+probEtoJ_6monthA+p_death)
probFtoF_6monthA<-1-(probFtoE_6monthA+probFtoFH_6monthA+probFtoFI_6monthA+probFtoJ_6monthA+p_death)
probGtoG_6monthA<-1-(probGtoB_6monthA+probGtoE_6monthA+probGtoGH_6monthA+probGtoGI_6monthA+probGtoJ_6monthA+p_death)

probAtoA_6monthB<-1-(probAtoB_6monthB+probAtoC_6monthB+probAtoE_6monthB+probAtoAH_6monthB+probAtoAI_6monthB+probAtoJ_6monthB+p_death)
probBtoB_6monthB<-1-(probBtoD_6monthB+probBtoE_6monthB+probBtoBH_6monthB+probBtoBI_6monthB+probBtoJ_6monthB+p_death)
probCtoC_6monthB<-1-(probCtoB_6monthB+probCtoE_6monthB+probCtoG_6monthB+probCtoCH_6monthB+probCtoCI_6monthB+probCtoJ_6monthB+p_death)
probDtoD_6monthB<-1-(probDtoE_6monthB+probDtoF_6monthB+probDtoDH_6monthB+probDtoDI_6monthB+probDtoJ_6monthB+p_death)
probEtoE_6monthB<-1-(probEtoEH_6monthB+probEtoEI_6monthB+probEtoJ_6monthB+p_death)
probFtoF_6monthB<-1-(probFtoE_6monthB+probFtoFH_6monthB+probFtoFI_6monthB+probFtoJ_6monthB+p_death)
probGtoG_6monthB<-1-(probGtoB_6monthB+probGtoE_6monthB+probGtoGH_6monthB+probGtoGI_6monthB+probGtoJ_6monthB+p_death)

probAtoA_6monthR<-1-(probAtoB_6monthR+probAtoC_6monthR+probAtoE_6monthR+probAtoAH_6monthR+probAtoAI_6monthR+probAtoJ_6monthR+p_death)
probBtoB_6monthR<-1-(probBtoD_6monthR+probBtoE_6monthR+probBtoBH_6monthR+probBtoBI_6monthR+probBtoJ_6monthR+p_death)
probCtoC_6monthR<-1-(probCtoB_6monthR+probCtoE_6monthR+probCtoG_6monthR+probCtoCH_6monthR+probCtoCI_6monthR+probCtoJ_6monthR+p_death)
probDtoD_6monthR<-1-(probDtoE_6monthR+probDtoF_6monthR+probDtoDH_6monthR+probDtoDI_6monthR+probDtoJ_6monthR+p_death)
probEtoE_6monthR<-1-(probEtoEH_6monthR+probEtoEI_6monthR+probEtoJ_6monthR+p_death)
probFtoF_6monthR<-1-(probFtoE_6monthR+probFtoFH_6monthR+probFtoFI_6monthR+probFtoJ_6monthR+p_death)
probGtoG_6monthR<-1-(probGtoB_6monthR+probGtoE_6monthR+probGtoGH_6monthR+probGtoGI_6monthR+probGtoJ_6monthR+p_death)

probAtoA_6monthC<-1-(probAtoB_6monthC+probAtoC_6monthC+probAtoE_6monthC+probAtoAH_6monthC+probAtoAI_6monthC+probAtoJ_6monthC+p_death)
probBtoB_6monthC<-1-(probBtoD_6monthC+probBtoE_6monthC+probBtoBH_6monthC+probBtoBI_6monthC+probBtoJ_6monthC+p_death)
probCtoC_6monthC<-1-(probCtoB_6monthC+probCtoE_6monthC+probCtoG_6monthC+probCtoCH_6monthC+probCtoCI_6monthC+probCtoJ_6monthC+p_death)
probDtoD_6monthC<-1-(probDtoE_6monthC+probDtoF_6monthC+probDtoDH_6monthC+probDtoDI_6monthC+probDtoJ_6monthC+p_death)
probEtoE_6monthC<-1-(probEtoEH_6monthC+probEtoEI_6monthC+probEtoJ_6monthC+p_death)
probFtoF_6monthC<-1-(probFtoE_6monthC+probFtoFH_6monthC+probFtoFI_6monthC+probFtoJ_6monthC+p_death)
probGtoG_6monthC<-1-(probGtoB_6monthC+probGtoE_6monthC+probGtoGH_6monthC+probGtoGI_6monthC+probGtoJ_6monthC+p_death)

probAHtoA_6monthB<-1-probAHtoJ_6monthB-p_death
probBHtoB_6monthB<-1-probBHtoJ_6monthB-p_death
probCHtoC_6monthB<-1-probCHtoJ_6monthB-p_death
probDHtoD_6monthB<-1-probDHtoJ_6monthB-p_death
probEHtoE_6monthB<-1-probEHtoJ_6monthB-p_death
probFHtoF_6monthB<-1-probFHtoJ_6monthB-p_death
probGHtoG_6monthB<-1-probGHtoJ_6monthB-p_death

probAHtoA_6monthC<-1-probAHtoJ_6monthC-p_death
probBHtoB_6monthC<-1-probBHtoJ_6monthC-p_death
probCHtoC_6monthC<-1-probCHtoJ_6monthC-p_death
probDHtoD_6monthC<-1-probDHtoJ_6monthC-p_death
probEHtoE_6monthC<-1-probEHtoJ_6monthC-p_death
probFHtoF_6monthC<-1-probFHtoJ_6monthC-p_death
probGHtoG_6monthC<-1-probGHtoJ_6monthC-p_death

probAHtoA_6monthA<-1-probAHtoJ_6monthA-p_death
probBHtoB_6monthA<-1-probBHtoJ_6monthA-p_death
probCHtoC_6monthA<-1-probCHtoJ_6monthA-p_death
probDHtoD_6monthA<-1-probDHtoJ_6monthA-p_death
probEHtoE_6monthA<-1-probEHtoJ_6monthA-p_death
probFHtoF_6monthA<-1-probFHtoJ_6monthA-p_death
probGHtoG_6monthA<-1-probGHtoJ_6monthA-p_death

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

acm_ic<-read_excel(file.path(root,"PSA model","Data","acm_ic.xlsx"))
acm_clti<-read_excel(file.path(root,"PSA model","Data","acm_clti.xlsx"))
