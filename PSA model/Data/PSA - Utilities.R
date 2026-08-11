#Quality of life values per 6 months

#Limb states PSA

clopiAEminor <- 0.0093 * -(runif(1,0.00297,0.00363))
aspAEminor <- 0.0093 * -(runif(1,0.00297,0.00363))
rivaspAEminor <- 0.0488 * -(runif(1,0.00297,0.00363))

clopiAEmajor <- 0.0041 * -(runif(1,0.12834,0.15686))
aspAEmajor <- 0.0054 * -(runif(1,0.12834,0.15686))
rivaspAEmajor <- 0.0165 * -(runif(1,0.12834,0.15686))

qolAmu<-0.65    
qolAVar<-0.002^2
alphaA<-qolAmu*((qolAmu*(1-qolAmu))/qolAVar-1)
betaA<-(1-qolAmu)*((qolAmu*(1-qolAmu))/qolAVar-1)
qolA1<-rbeta(1,alphaA,betaA)
qolAc<-(qolA1+ clopiAEminor + clopiAEmajor) /2
qolAa<-(qolA1+ aspAEminor + aspAEmajor) /2
qolAr<-(qolA1+ rivaspAEminor + rivaspAEmajor) /2

qolBmu<-0.42    
qolBVar<-0.144^2
alphaB<-qolBmu*((qolBmu*(1-qolBmu))/qolBVar-1)
betaB<-(1-qolBmu)*((qolBmu*(1-qolBmu))/qolBVar-1)
qolB1<-rbeta(1,alphaB,betaB)
qolBc<-(qolB1+ clopiAEminor + clopiAEmajor) /2
qolBa<-(qolB1+ aspAEminor + aspAEmajor) /2
qolBr<-(qolB1+ rivaspAEminor + rivaspAEmajor) /2

FiveyrqolCmu<-0.07/5   
FiveyrqolCSD<-((0.16-(-0.27))/(2*1.96))/5
qolC1<-(rnorm(1,FiveyrqolCmu,FiveyrqolCSD)) /2
qolCc <- pmax(0, pmin(1, qolAc + qolC1)) 
qolCa <- pmax(0, pmin(1, qolAa + qolC1)) 
qolCr <- pmax(0, pmin(1, qolAr + qolC1)) 

u_baseline <- rnorm(1, 0.5, 0.2)
u_followup <- rnorm(1, 0.7, 0.2)
qolD1 <- (u_followup - u_baseline) /2
qolDc <- pmax(0, pmin(1, qolBc + qolD1)) 
qolDa <- pmax(0, pmin(1, qolBa + qolD1)) 
qolDr <- pmax(0, pmin(1, qolBr + qolD1)) 

qolEmu<-0.54    
qolEVar<-0.076^2
alphaE<-qolEmu*((qolEmu*(1-qolEmu))/qolEVar-1)
betaE<-(1-qolEmu)*((qolEmu*(1-qolEmu))/qolEVar-1)
qolE1<-rbeta(1,alphaE,betaE)
qolEc<-(qolE1+ clopiAEminor + clopiAEmajor) /2
qolEa<-(qolE1+ aspAEminor + aspAEmajor) /2
qolEr<-(qolE1+ rivaspAEminor + rivaspAEmajor) /2

u_baseline <- pmax(0, pmin(1, rnorm(1, 0.5, 0.2)))
u_followup <- pmax(0, pmin(1, rnorm(1, 0.7, 0.2)))
qolF1 <- (u_followup - u_baseline) /2
qolFc <- pmax(0, pmin(1, qolDc + qolF1)) 
qolFa <- pmax(0, pmin(1, qolDa + qolF1)) 
qolFr <- pmax(0, pmin(1, qolDr + qolF1)) 

FiveyrqolGmu<-0.07/5   
FiveyrqolGSD<-((0.16-(-0.27))/(2*1.96))/5
qolG1<-(rnorm(1,FiveyrqolGmu,FiveyrqolGSD))/2
qolGc <- pmax(0, pmin(1, qolCc + qolG1))
qolGa <- pmax(0, pmin(1, qolCa + qolG1))
qolGr <- pmax(0, pmin(1, qolCr + qolG1))

#Stroke PSA
strokemu <- -0.18
strokeSE <- (0.28 - 0.08)/(2*1.96)
strokeutilityloss <- (rnorm(1, strokemu, strokeSE) ) /2

qolAHc <- pmax(0, pmin(1, qolAc + strokeutilityloss))
qolAHr <- pmax(0, pmin(1, qolAr + strokeutilityloss))
qolAHa <- pmax(0, pmin(1, qolAa + strokeutilityloss))

qolBHc <- pmax(0, pmin(1, qolBc + strokeutilityloss))
qolBHr <- pmax(0, pmin(1, qolBr + strokeutilityloss))
qolBHa <- pmax(0, pmin(1, qolBa + strokeutilityloss))

qolCHc <- pmax(0, pmin(1, qolCc + strokeutilityloss))
qolCHr <- pmax(0, pmin(1, qolCr + strokeutilityloss))
qolCHa <- pmax(0, pmin(1, qolCa + strokeutilityloss))

qolDHc <- pmax(0, pmin(1, qolDc + strokeutilityloss))
qolDHr <- pmax(0, pmin(1, qolDr + strokeutilityloss))
qolDHa <- pmax(0, pmin(1, qolDa + strokeutilityloss))

qolEHc <- pmax(0, pmin(1, qolEc + strokeutilityloss))
qolEHr <- pmax(0, pmin(1, qolEr + strokeutilityloss))
qolEHa <- pmax(0, pmin(1, qolEa + strokeutilityloss))

qolFHc <- pmax(0, pmin(1, qolFc + strokeutilityloss))
qolFHr <- pmax(0, pmin(1, qolFr + strokeutilityloss))
qolFHa <- pmax(0, pmin(1, qolFa + strokeutilityloss))

qolGHc <- pmax(0, pmin(1, qolGc + strokeutilityloss))
qolGHr <- pmax(0, pmin(1, qolGr + strokeutilityloss))
qolGHa <- pmax(0, pmin(1, qolGa + strokeutilityloss))

#MI PSA
MImu <- -0.06
MISE <- (0.11 - 0.01)/(2*1.96)

MI_utility<-(rnorm(1, MImu, MISE) ) /2

qolAIc <- pmax(0, pmin(1, qolAc + MI_utility))
qolAIr <- pmax(0, pmin(1, qolAr + MI_utility))
qolAIa <- pmax(0, pmin(1, qolAa + MI_utility))

qolBIc <- pmax(0, pmin(1, qolBc + MI_utility))
qolBIr <- pmax(0, pmin(1, qolBr + MI_utility))
qolBIa <- pmax(0, pmin(1, qolBa + MI_utility))

qolCIc <- pmax(0, pmin(1, qolCc + MI_utility))
qolCIr <- pmax(0, pmin(1, qolCr + MI_utility))
qolCIa <- pmax(0, pmin(1, qolCa + MI_utility))

qolDIc <- pmax(0, pmin(1, qolDc + MI_utility))
qolDIr <- pmax(0, pmin(1, qolDr + MI_utility))
qolDIa <- pmax(0, pmin(1, qolDa + MI_utility))

qolEIc <- pmax(0, pmin(1, qolEc + MI_utility))
qolEIr <- pmax(0, pmin(1, qolEr + MI_utility))
qolEIa <- pmax(0, pmin(1, qolEa + MI_utility))

qolFIc <- pmax(0, pmin(1, qolFc + MI_utility))
qolFIr <- pmax(0, pmin(1, qolFr + MI_utility))
qolFIa <- pmax(0, pmin(1, qolFa + MI_utility))

qolGIc <- pmax(0, pmin(1, qolGc + MI_utility))
qolGIr <- pmax(0, pmin(1, qolGr + MI_utility))
qolGIa <- pmax(0, pmin(1, qolGa + MI_utility))

#Death states
qolJ<-0
qolK<-0
