#Utility values

#Limb states

clopiAEminor <- (1 - (1 - 0.0093)^0.5) * (-0.0033)
aspAEminor <- (1 - (1 - 0.0093)^0.5) * (-0.0033)

clopiAEmajor <- (1 - (1 - 0.0041)^0.5) * (-0.1426)
aspAEmajor <- (1 - (1 - 0.0054)^0.5) * (-0.1426)

qolAc<-(0.65+ clopiAEminor + clopiAEmajor) /2
qolBc<-(0.42+ clopiAEminor + clopiAEmajor) /2
qolEc<-(0.54+ clopiAEminor + clopiAEmajor) /2

qolAa<-(0.65+ aspAEminor + aspAEmajor) /2
qolBa<-(0.42+ aspAEminor + aspAEmajor) /2
qolEa<-(0.42+ aspAEminor + aspAEmajor) /2

qolCSixmonthTotalLoss<-(0.07/5) /2
qolCc<-qolAc + qolCSixmonthTotalLoss
qolCa<-qolAa + qolCSixmonthTotalLoss

qolDutilitygain<-0.20 /2
qolDc<-qolBc + qolDutilitygain
qolDa<-qolBa + qolDutilitygain

qolFutilitygain<-0.20 /2
qolFc<-qolDc + qolFutilitygain
qolFa<-qolDa + qolFutilitygain

qolGSixmonthTotalLoss<-(0.07/5) /2
qolGc<-qolCc + qolCSixmonthTotalLoss
qolGa<-qolCa + qolCSixmonthTotalLoss

#Stroke
strokeutilityloss <- -0.18 /2

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

#MI
MI_utility <- -0.06 /2

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
