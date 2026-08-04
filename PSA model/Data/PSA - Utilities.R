#Quality of life values per 6 months

#Limb states PSA

clopiAEminor <- (1 - (1 - 0.0093)^0.5) * (-0.0033)
aspAEminor <- (1 - (1 - 0.0093)^0.5) * (-0.0033)

clopiAEmajor <- (1 - (1 - 0.0041)^0.5) * (-0.1426)
aspAEmajor <- (1 - (1 - 0.0054)^0.5) * (-0.1426)


qolAmu<-0.65    
qolAVar<-0.002^2
alphaA<-qolAmu*((qolAmu*(1-qolAmu))/qolAVar-1)
betaA<-(1-qolAmu)*((qolAmu*(1-qolAmu))/qolAVar-1)
qolA1<-rbeta(1,alphaA,betaA)
qolAc<-(qolA1+ clopiAEminor + clopiAEmajor) /2
qolAa<-(qolA1+ aspAEminor + aspAEmajor) /2

qolBmu<-0.42    
qolBVar<-0.144^2
alphaB<-qolBmu*((qolBmu*(1-qolBmu))/qolBVar-1)
betaB<-(1-qolBmu)*((qolBmu*(1-qolBmu))/qolBVar-1)
qolB1<-rbeta(1,alphaB,betaB)
qolBc<-(qolB1+ clopiAEminor + clopiAEmajor) /2
qolBa<-(qolB1+ aspAEminor + aspAEmajor) /2

FiveyrqolCmu<-0.07/5   
FiveyrqolCSD<-((0.16-(-0.27))/(2*1.96))/5
qolC1<-(rnorm(1,FiveyrqolCmu,FiveyrqolCSD))
qolCc <- (pmax(0, pmin(1, qolAc + qolC1)) ) /2
qolCa <- (pmax(0, pmin(1, qolAa + qolC1)) ) /2

u_baseline <- rnorm(1, 0.5, 0.2)
u_followup <- rnorm(1, 0.7, 0.2)
qolD1 <- u_followup - u_baseline
qolDc <- (pmax(0, pmin(1, qolBc + qolD1)) ) /2
qolDa <- (pmax(0, pmin(1, qolBa + qolD1)) ) /2

qolEmu<-0.54    
qolEVar<-0.076^2
alphaE<-qolEmu*((qolEmu*(1-qolEmu))/qolEVar-1)
betaE<-(1-qolEmu)*((qolEmu*(1-qolEmu))/qolEVar-1)
qolE1<-rbeta(1,alphaE,betaE)
qolEc<-(qolE1+ clopiAEminor + clopiAEmajor) /2
qolEa<-(qolE1+ aspAEminor + aspAEmajor) /2

u_baseline <- rnorm(1, 0.5, 0.2)
u_followup <- rnorm(1, 0.7, 0.2)
qolF1 <- u_followup - u_baseline
qolFc <- (pmax(0, pmin(1, qolDc + qolF1)) ) /2
qolFa <- (pmax(0, pmin(1, qolDa + qolF1)) ) /2

FiveyrqolGmu<-0.07/5   
FiveyrqolGSD<-((0.16-(-0.27))/(2*1.96))/5
qolG1<-(rnorm(1,FiveyrqolGmu,FiveyrqolGSD))
qolGc <- (pmax(0, pmin(1, qolCc + qolG1)) ) /2
qolGa <- (pmax(0, pmin(1, qolCa + qolG1)) ) /2

#Stroke PSA
strokemu <- -0.18
strokeSE <- (0.28 - 0.08)/(2*1.96)
strokeutilityloss <- (rnorm(1, strokemu, strokeSE) ) /2

qolAHc<-qolAc + strokeutilityloss
qolAHa<-qolAa + strokeutilityloss

qolBHc<-qolBc + strokeutilityloss
qolBHa<-qolBa + strokeutilityloss

qolCHc<-qolCc + strokeutilityloss
qolCHa<-qolCa + strokeutilityloss

qolDHc<-qolDc + strokeutilityloss
qolDHa<-qolDa + strokeutilityloss

qolEHc<-qolEc + strokeutilityloss
qolEHa<-qolEa + strokeutilityloss

qolFHc<-qolFc + strokeutilityloss
qolFHa<-qolFa + strokeutilityloss

qolGHc<-qolGc + strokeutilityloss
qolGHa<-qolGa + strokeutilityloss

#MI PSA
MImu <- -0.06
MISE <- (0.11 - 0.01)/(2*1.96)
qolAIc<-qolAc + MI_utility
qolAIa<-qolAa + MI_utility

qolBIc<-qolBc + MI_utility
qolBIa<-qolBa + MI_utility

qolCIc<-qolCc + MI_utility
qolCIa<-qolCa + MI_utility

qolDIc<-qolDc + MI_utility
qolDIa<-qolDa + MI_utility

qolEIc<-qolEc + MI_utility
qolEIa<-qolEa + MI_utility

qolFIc<-qolFc + MI_utility
qolFIa<-qolFa + MI_utility

qolGIc<-qolGc + MI_utility
qolGIa<-qolGa + MI_utility

#Death states
qolJ<-0
qolK<-0
