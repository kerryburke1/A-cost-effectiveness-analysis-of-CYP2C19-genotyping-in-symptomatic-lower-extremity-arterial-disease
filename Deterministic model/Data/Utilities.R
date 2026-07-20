#Utility values

#Limb states
qolAc<-0.65+((-0.1426*0.5)*0.004113)+((-0.0033*0.5)*0.009313)
qolBc<-0.42+((-0.1426*0.5)*0.004113)+((-0.0033*0.5)*0.009313)
qolEc<-0.54+((-0.1426*0.5)*0.004113)+((-0.0033*0.5)*0.009313)

qolAr<-0.65+((-0.1426*0.5)*0.0155)+((-0.0033*0.5)*0.0920)
qolBr<-0.42+((-0.1426*0.5)*0.0155)+((-0.0033*0.5)*0.0920)
qolEr<-0.42+((-0.1426*0.5)*0.0155)+((-0.0033*0.5)*0.0920)

qolAa<-0.65+((-0.1426*0.5)*0.0054)+((-0.0033*0.5)*0.0093)
qolBa<-0.42+((-0.1426*0.5)*0.0054)+((-0.0033*0.5)*0.0093)
qolEa<-0.42+((-0.1426*0.5)*0.0054)+((-0.0033*0.5)*0.0093)

qolCSixmonthTotalLoss<-0.07/5
qolCc<-qolAc + qolCSixmonthTotalLoss
qolCr<-qolAr + qolCSixmonthTotalLoss
qolCa<-qolAa + qolCSixmonthTotalLoss

qolDutilitygain<-0.20
qolDc<-qolBc + qolDutilitygain
qolDr<-qolBr + qolDutilitygain
qolDa<-qolBa + qolDutilitygain

qolFutilitygain<-0.20
qolFc<-qolDc + qolFutilitygain
qolFr<-qolDr + qolFutilitygain
qolFa<-qolDa + qolFutilitygain

qolGSixmonthTotalLoss<-0.07/5
qolGc<-qolCc + qolCSixmonthTotalLoss
qolGr<-qolCr + qolCSixmonthTotalLoss
qolGa<-qolCa + qolCSixmonthTotalLoss

#Stroke
strokeutilityloss <- -0.18

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
MI_utility <- -0.06

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
