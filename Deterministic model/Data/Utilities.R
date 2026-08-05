#Utility values

#Limb states

clopiAEminor <- 0.0093 * -0.0033
aspAEminor <- 0.0093 * -0.0033
rivaspAEminor <- 0.0488 * -0.0033

clopiAEmajor <- 0.0041 * -0.1426
aspAEmajor <- 0.0054 * -0.1426
rivaspAEmajor <- 0.0165 * -0.1426

qolAc<-(0.65+ clopiAEminor + clopiAEmajor) /2
qolBc<-(0.42+ clopiAEminor + clopiAEmajor) /2
qolEc<-(0.54+ clopiAEminor + clopiAEmajor) /2

qolAa<-(0.65+ aspAEminor + aspAEmajor) /2
qolBa<-(0.42+ aspAEminor + aspAEmajor) /2
qolEa<-(0.42+ aspAEminor + aspAEmajor) /2

qolAr<-(0.65+ rivaspAEminor + rivaspAEmajor) /2
qolBr<-(0.42+ rivaspAEminor + rivaspAEmajor) /2
qolEr<-(0.42+ rivaspAEminor + rivaspAEmajor) /2

qolCSixmonthTotalLoss<-(0.07/5) /2
qolCc<-qolAc + qolCSixmonthTotalLoss
qolCa<-qolAa + qolCSixmonthTotalLoss
qolCr<-qolAr + qolCSixmonthTotalLoss

qolDutilitygain<-0.20 /2
qolDc<-qolBc + qolDutilitygain
qolDa<-qolBa + qolDutilitygain
qolDr<-qolBr + qolDutilitygain

qolFutilitygain<-0.20 /2
qolFc<-qolDc + qolFutilitygain
qolFa<-qolDa + qolFutilitygain
qolFr<-qolDr + qolFutilitygain

qolGSixmonthTotalLoss<-(0.07/5) /2
qolGc<-qolCc + qolCSixmonthTotalLoss
qolGa<-qolCa + qolCSixmonthTotalLoss
qolGr<-qolCr + qolCSixmonthTotalLoss

#Stroke
strokeutilityloss <- -0.18 /2

qolAHc<-qolAc + strokeutilityloss
qolAHr<-qolAr + strokeutilityloss
qolAHa<-qolAa + strokeutilityloss

qolBHc<-qolBc + strokeutilityloss
qolBHr<-qolBr + strokeutilityloss
qolBHa<-qolBa + strokeutilityloss

qolCHc<-qolCc + strokeutilityloss
qolCHr<-qolCr + strokeutilityloss
qolCHa<-qolCa + strokeutilityloss

qolDHc<-qolDc + strokeutilityloss
qolDHr<-qolDr + strokeutilityloss
qolDHa<-qolDa + strokeutilityloss

qolEHc<-qolEc + strokeutilityloss
qolEHr<-qolEr + strokeutilityloss
qolEHa<-qolEa + strokeutilityloss

qolFHc<-qolFc + strokeutilityloss
qolFHr<-qolFr + strokeutilityloss
qolFHa<-qolFa + strokeutilityloss

qolGHc<-qolGc + strokeutilityloss
qolGHr<-qolGr + strokeutilityloss
qolGHa<-qolGa + strokeutilityloss

#MI
MI_utility <- -0.06 /2

qolAIc<-qolAc + MI_utility
qolAIr<-qolAr + MI_utility
qolAIa<-qolAa + MI_utility

qolBIc<-qolBc + MI_utility
qolBIr<-qolBr + MI_utility
qolBIa<-qolBa + MI_utility

qolCIc<-qolCc + MI_utility
qolCIr<-qolCr + MI_utility
qolCIa<-qolCa + MI_utility

qolDIc<-qolDc + MI_utility
qolDIr<-qolDr + MI_utility
qolDIa<-qolDa + MI_utility

qolEIc<-qolEc + MI_utility
qolEIr<-qolEr + MI_utility
qolEIa<-qolEa + MI_utility

qolFIc<-qolFc + MI_utility
qolFIr<-qolFr + MI_utility
qolFIa<-qolFa + MI_utility

qolGIc<-qolGc + MI_utility
qolGIr<-qolGr + MI_utility
qolGIa<-qolGa + MI_utility

#Death states
qolJ<-0
qolK<-0
