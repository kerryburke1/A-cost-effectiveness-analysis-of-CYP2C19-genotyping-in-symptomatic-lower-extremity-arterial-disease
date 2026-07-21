install.packages("data.tree")
install.packages("rprojroot")
install.packages("testthat")
install.packages("readxl")
install.packages("DiagrammeR")

library("data.tree")
library("testthat")
library("readxl")
library("DiagrammeR")
library("rprojroot")

mixed_prescribing<-FALSE
#FALSE = dropout go into alternate arm 1 (all on clopi), testing compared to all on clopi
#TRUE = dropout go into alternate arm 2 (mixed prescribing), testing compared to mixed prescribing

n<-138563 
nstates<-23
discountrate<-0.035
run_tests=TRUE

statenames<-c("IC","CLTI","RevascIC","RevascCLTI","Amputation","ReintCLTI","ReintIC","ICStroke","CLTIStroke","RevascICStroke","RevascCLTIStroke","AmputationStroke","ReintCLTIStroke","ReintICStroke","ICMI","CLTIMI","RevascICMI","RevascCLTIMI","AmputationMI","ReintCLTIMI","ReintICMI","VascularDeath","OtherDeath")

root <- "C:/Users/Kerry/Documents/GitHub/A-cost-effectiveness-analysis-of-CYP2C19-genotyping-in-symptomatic-lower-extremity-arterial-disease"

source(file.path(root,"Deterministic model","Data","Transition probabilities.R"))
source(file.path(root,"Deterministic model","Data","Costs.R"))
source(file.path(root,"Deterministic model","Data","Utilities.R"))

#Decision Tree 1 (IC)
tree<-Node$new("IC")
tree$AddChild("Genotyping")
tree$AddChild("NoGenotyping")
tree$AddChild("NoGenotyping_allClopi")
tree$NoGenotyping$p<-ifelse(mixed_prescribing,1,0)
tree$NoGenotyping_allClopi$p<-ifelse(mixed_prescribing,0,1)
tree$Genotyping$AddChild("Dropout")
tree$Genotyping$Dropout$p <- 0.10
tree$Genotyping$AddChild("NoLOF")
tree$Genotyping$NoLOF$p <- 0.9*0.70
tree$Genotyping$NoLOF$AddChild("ICclopi")
tree$Genotyping$AddChild("LOF")
tree$Genotyping$LOF$p <- 0.9*0.30
tree$Genotyping$LOF$AddChild("ICrivasptest")
tree$Genotyping$LOF$ICrivasptest$p <- 0.53
tree$Genotyping$LOF$AddChild("ICasptest")
tree$Genotyping$LOF$ICasptest$p <- 0.47

tree$Genotyping$Dropout$AddChild("ICnotest3")
tree$Genotyping$Dropout$ICnotest3$p<- ifelse(mixed_prescribing,0.72,1)
tree$Genotyping$Dropout$AddChild("ICrivasp1")
tree$Genotyping$Dropout$ICrivasp1$p<- ifelse(mixed_prescribing,0.09,0)
tree$Genotyping$Dropout$AddChild("ICasp1")
tree$Genotyping$Dropout$ICasp1$p<- ifelse(mixed_prescribing,0.19,0)

tree$NoGenotyping_allClopi$AddChild("ICnotest1")
tree$NoGenotyping_allClopi$ICnotest1$p<-1

tree$NoGenotyping$AddChild("ICnotest2")
tree$NoGenotyping$ICnotest2$p<-tree$Genotyping$Dropout$ICnotest3$p
tree$NoGenotyping$AddChild("ICrivasp2")
tree$NoGenotyping$ICrivasp2$p<-tree$Genotyping$Dropout$ICrivasp1$p
tree$NoGenotyping$AddChild("ICasp2")
tree$NoGenotyping$ICasp2$p<-tree$Genotyping$Dropout$ICasp1$p
plot(tree)
calculate_path_prob <- function(node) {
  
  path_prob <- 1
  current <- node
  
  while (!current$isRoot) {
    if (!is.null(current$p)) {
      path_prob <- path_prob * current$p
    }
    current <- current$parent
  }
  
  return(path_prob)
}

leaf_probs <- data.frame(
  Path = sapply(tree$leaves, function(x) paste(x$path, collapse = " -> ")),
  Probability = sapply(tree$leaves, calculate_path_prob)
)

if(run_tests){
  check_tree_probs(tree)}
print(leaf_probs)

markovn=n*leaf_probs[,2]
print(markovn)

n<-34640 

#Decision Tree 2 (CLTI)
tree<-Node$new("CLTI")
tree$AddChild("Genotyping")
tree$AddChild("NoGenotyping")
tree$AddChild("NoGenotyping_allClopi")
tree$NoGenotyping$p<-ifelse(mixed_prescribing,1,0)
tree$NoGenotyping_allClopi$p<-ifelse(mixed_prescribing,0,1)
tree$Genotyping$AddChild("Dropout")
tree$Genotyping$Dropout$p <- 0.10
tree$Genotyping$AddChild("NoLOF")
tree$Genotyping$NoLOF$p <- 0.9*0.70
tree$Genotyping$NoLOF$AddChild("CLTIclopi")
tree$Genotyping$AddChild("LOF")
tree$Genotyping$LOF$p <- 0.9*0.30
tree$Genotyping$LOF$AddChild("CLTIrivasptest")
tree$Genotyping$LOF$CLTIrivasptest$p <- 0.73
tree$Genotyping$LOF$AddChild("CLTIasptest")
tree$Genotyping$LOF$CLTIasptest$p <- 0.27

tree$Genotyping$Dropout$AddChild("CLTInotest3")
tree$Genotyping$Dropout$CLTInotest3$p<- ifelse(mixed_prescribing,0.65,1)
tree$Genotyping$Dropout$AddChild("CLTIrivasp1")
tree$Genotyping$Dropout$CLTIrivasp1$p<- ifelse(mixed_prescribing,0.22,0)
tree$Genotyping$Dropout$AddChild("CLTIasp1")
tree$Genotyping$Dropout$CLTIasp1$p<- ifelse(mixed_prescribing,0.13,0)

tree$NoGenotyping_allClopi$AddChild("CLTInotest1")
tree$NoGenotyping_allClopi$CLTInotest1$p<-1

tree$NoGenotyping$AddChild("CLTInotest2")
tree$NoGenotyping$CLTInotest2$p<-tree$Genotyping$Dropout$CLTInotest3$p
tree$NoGenotyping$AddChild("CLTIrivasp2")
tree$NoGenotyping$CLTIrivasp2$p<-tree$Genotyping$Dropout$CLTIrivasp1$p
tree$NoGenotyping$AddChild("CLTIasp2")
tree$NoGenotyping$CLTIasp2$p<-tree$Genotyping$Dropout$CLTIasp1$p

calculate_path_prob <- function(node) {
  
  path_prob <- 1
  current <- node
  
  while (!current$isRoot) {
    if (!is.null(current$p)) {
      path_prob <- path_prob * current$p
    }
    current <- current$parent
  }
  
  return(path_prob)
}

leaf_probs <- data.frame(
  Path = sapply(tree$leaves, function(x) paste(x$path, collapse = " -> ")),
  Probability = sapply(tree$leaves, calculate_path_prob)
)

if(run_tests){
  check_tree_probs(tree)}

print(leaf_probs)
markovn2=n*leaf_probs[,2]
print(markovn2)

source(file.path(root,"Deterministic model","IC - clopi no test1.R"))
source(file.path(root,"Deterministic model","IC - clop no test2.R"))
source(file.path(root,"Deterministic model","IC - clopi no test3.R"))
source(file.path(root,"Deterministic model","IC - clopi.R"))
source(file.path(root,"Deterministic model","IC - riv-asp1.R"))
source(file.path(root,"Deterministic model","IC - riv-asp2.R"))
source(file.path(root,"Deterministic model","IC - riv-asp + test.R"))
source(file.path(root,"Deterministic model","IC - asp1.R"))
source(file.path(root,"Deterministic model","IC - asp2.R"))
source(file.path(root,"Deterministic model","IC - asp + test.R"))
source(file.path(root,"Deterministic model","CLTI - clopi no test1.R"))
source(file.path(root,"Deterministic model","CLTI - clopi no test2.R"))
source(file.path(root,"Deterministic model","CLTI - clopi no test3.R"))
source(file.path(root,"Deterministic model","CLTI - clopi.R"))
source(file.path(root,"Deterministic model","CLTI - riv-asp1.R"))
source(file.path(root,"Deterministic model","CLTI - riv-asp2.R"))
source(file.path(root,"Deterministic model","CLTI - riv-asp + test.R"))
source(file.path(root,"Deterministic model","CLTI - asp1.R"))
source(file.path(root,"Deterministic model","CLTI - asp2.R"))  
source(file.path(root,"Deterministic model","CLTI - asp + test.R"))

#Calculate ICERs

#IC: testing vs all on clopi
ICER1 = ((ICclopimeanresults[1]+ICasptestmeanresults[1]+ICrivasptestmeanresults[1]+ICnotest3meanresults[1]) - (ICnotest1meanresults[1])) /((ICclopimeanresults[2]+ICasptestmeanresults[2]+ICrivasptestmeanresults[2]+ICnotest3meanresults[2]) - (ICnotest1meanresults[2])) 
print(ICER1)

#IC: testing vs range of medications
ICER2 = ((ICclopimeanresults[1]+ICasptestmeanresults[1]+ICrivasptestmeanresults[1]+ICnotest3meanresults[1]+ ICrivasp1meanresults[1] + ICasp1meanresults[1]) - (ICnotest2meanresults[1]+ICrivasp2meanresults[1]+ICasp2meanresults[1])) /((ICclopimeanresults[2]+ICasptestmeanresults[2]+ICrivasptestmeanresults[2]+ICnotest3meanresults[2]+ICrivasp1meanresults[2]+ICasp1meanresults[2]) - (ICnotest2meanresults[2]+ICrivasp2meanresults[2]+ICasp2meanresults[2]))
print(ICER2)

#CLTI: testing vs all on clopi
ICER3 = ((CLTIclopimeanresults[1]+CLTIasptestmeanresults[1]+CLTIrivasptestmeanresults[1]+CLTInotest3meanresults[1]) - (CLTInotest1meanresults[1])) /((CLTIclopimeanresults[2]+CLTIasptestmeanresults[2]+CLTIrivasptestmeanresults[2]+CLTInotest3meanresults[2]) - (CLTInotest1meanresults[2])) 
print(ICER3) 

#CLTI: testing vs range of medications
ICER4 = ((CLTIclopimeanresults[1]+CLTIasptestmeanresults[1]+CLTIrivasptestmeanresults[1]+CLTInotest3meanresults[1]+CLTIrivasp1meanresults[1]+CLTIasp1meanresults[1]) - (CLTInotest2meanresults[1]+CLTIrivasp2meanresults[1]+CLTIasp2meanresults[1])) /((CLTIclopimeanresults[2]+CLTIasptestmeanresults[2]+CLTIrivasptestmeanresults[2]+CLTInotest3meanresults[2]+CLTIrivasp1meanresults[2]+CLTIasp1meanresults[2]) - (CLTInotest2meanresults[2]+CLTIrivasp2meanresults[2]+CLTIasp2meanresults[2]))
print(ICER4)

#total costs and QALYs
#IC testing (clopi dropout)
total_cost1 = (ICclopi[1]+ICasptest[1]+ICrivasptest[1]+ICnotest3[1]) / 138563
total_qaly1 = (ICclopi[2]+ICasptest[2]+ICrivasptest[2]+ICnotest3[2]) / 138563
print(total_cost1)
print(total_qaly1)

#IC all on clopi
total_cost2 = (ICnotest1[1]) / 138563
total_qaly2 = (ICnotest1[2]) / 138563
print(total_cost2)
print(total_qaly2)

#IC testing (mixed prescribing dropout)
total_cost3 = (ICclopi[1]+ICasptest[1]+ICrivasptest[1]+ICnotest3[1]+ ICrivasp1[1] + ICasp1[1]) / 138563
total_qaly3 = (ICclopi[2]+ICasptest[2]+ICrivasptest[2]+ICnotest3[2]+ ICrivasp1[2] + ICasp1[2]) / 138563
print(total_cost3)
print(total_qaly3)

#IC mixed prescribing
total_cost4 = (ICnotest2[1]+ICrivasp2[1]+ICasp2[1]) / 138563
total_qaly4 = (ICnotest2[2]+ICrivasp2[2]+ICasp2[2]) / 138563
print(total_cost4)
print(total_qaly4)

#CLTI testing (clopi dropout)
total_cost5 = (CLTIclopi[1]+CLTIasptest[1]+CLTIrivasptest[1]+CLTInotest3[1]) / 34640 
total_qaly5 = (CLTIclopi[2]+CLTIasptest[2]+CLTIrivasptest[2]+CLTInotest3[2]) / 34640 
print(total_cost5)
print(total_qaly5)

#CLTI testing  (mixed prescribing dropout)
total_cost6 = (CLTIclopi[1]+CLTIasptest[1]+CLTIrivasptest[1]+CLTInotest3[1] +CLTIrivasp1[1] + ICasp1[1]) / 34640 
total_qaly6 = (CLTIclopi[2]+CLTIasptest[2]+CLTIrivasptest[2]+CLTInotest3[2] +CLTIrivasp1[2] + CLTIasp1[2]) / 34640 
print(total_cost6)
print(total_qaly6)

#CLTI all on clopi
total_cost7 = (CLTInotest1[1]) / 34640 
total_qaly7 = (CLTInotest1[2]) / 34640 
print(total_cost7)
print(total_qaly7)

#CLTI mixed prescribing
total_cost8 = (CLTInotest2[1]+CLTIrivasp2[1]+CLTIasp2[1]) / 34640 
total_qaly8 = (CLTInotest2[2]+CLTIrivasp2[2]+CLTIasp2[2]) / 34640 
print(total_cost8)
print(total_qaly8)

#total amputations

#IC testing arm (vs all on clopi)
print(ICclopiamps+ICasptestamps+ICrivasptestamps+ICnotest3amps)
#IC all on clopi
print(ICnotest1amps)
#IC testing arm (vs mixed prescribing)
print(ICclopiamps+ICasptestamps+ICrivasptestamps+ICnotest3amps+ICrivasp1amps+ICasp1amps)
#IC range of medications
print(ICnotest2amps+ICrivasp2amps+ICasp2amps)

#CLTI testing arm (vs all on clopi)
print(CLTIclopiamps+CLTIasptestamps+CLTIrivasptestamps+CLTInotest3amps)
#CLTI all on clopi
print(CLTInotest1amps)
#CLTI testing arm (vs mixed prescribing)
print(CLTIclopiamps+CLTIasptestamps+CLTIrivasptestamps+CLTInotest3amps+CLTIrivasp1amps+CLTIasp1amps)
#CLTI range of medications
print(CLTInotest2amps+CLTIrivasp2amps+CLTIasp2amps)

#total revascs

#IC testing arm (vs all on clopi)
print(ICclopirevasc+ICasptestrevasc+ICrivasptestrevasc+ICnotest3revasc)
#IC all on clopi
print(ICnotest1revasc)
#IC testing arm (vs mixed prescribing)
print(ICclopirevasc+ICasptestrevasc+ICrivasptestrevasc+ICnotest3revasc+ICrivasp1revasc+ICasp1revasc)
#IC range of medications 
print(ICnotest2revasc+ICrivasp2revasc+ICasp2revasc)

#CLTI testing arm (vs all on clopi)
print(CLTIclopirevasc+CLTIasptestrevasc+CLTIrivasptestrevasc+CLTInotest3revasc)
#CLTI all on clopi
print(CLTInotest1revasc)
#CLTI testing arm (vs mixed prescribing)
print(CLTIclopirevasc+CLTIasptestrevasc+CLTIrivasptestrevasc+CLTInotest3revasc+CLTIrivasp1revasc+CLTIasp1revasc)
#CLTI range of medications
print(CLTInotest2revasc+CLTIrivasp2revasc+CLTIasp2revasc)

#total strokes

#IC testing arm (vs all on clopi)
print(ICclopistroke+ICaspteststroke+ICrivaspteststroke+ICnotest3stroke)
#IC all on clopi
print(ICnotest1stroke)
#IC testing arm (vs mixed prescribing)
print(ICclopistroke+ICaspteststroke+ICrivaspteststroke+ICnotest3stroke+ICrivasp1stroke+ICasp1stroke)
#IC range of medications
print(ICnotest2stroke+ICrivasp2stroke+ICasp2stroke)

#CLTI testing arm (vs all on clopi)
print(CLTIclopistroke+CLTIaspteststroke+CLTIrivaspteststroke+CLTInotest3stroke)
#CLTI all on clopi
print(CLTInotest1stroke)
#CLTI testing arm (vs mixed prescribing)
print(CLTIclopistroke+CLTIaspteststroke+CLTIrivaspteststroke+CLTInotest3stroke+CLTIrivasp1stroke+CLTIasp1stroke)
#CLTI range of medications
print(CLTInotest2stroke+CLTIrivasp2stroke+CLTIasp2stroke)

#total MIs

#IC testing arm (vs all on clopi)
print(ICclopiMI+ICasptestMI+ICrivasptestMI+ICnotest3MI)
#IC all on clopi
print(ICnotest1MI)
#IC testing arm (vs mixed prescribing)
print(ICclopiMI+ICasptestMI+ICrivasptestMI+ICnotest3MI+ICrivasp1MI+ICasp1MI)
#IC range of medications
print(ICnotest2MI+ICrivasp2MI+ICasp2MI)

#CLTI testing arm (vs all on clopi)
print(CLTIclopiMI+CLTIasptestMI+CLTIrivasptestMI+CLTInotest3MI)
#CLTI all on clopi
print(CLTInotest1MI)
#CLTI testing arm (vs mixed prescribing)
print(CLTIclopiMI+CLTIasptestMI+CLTIrivasptestMI+CLTInotest3MI+CLTIrivasp1MI+CLTIasp1MI)
#CLTI range of medications
print(CLTInotest2MI+CLTIrivasp2MI+CLTIasp2MI)

#total deaths

#IC testing arm (vs all on clopi)
print(ICclopideath+ICasptestdeath+ICrivasptestdeath+ICnotest3death)
#IC all on clopi
print(ICnotest1death)
#IC testing arm (vs mixed prescribing)
print(ICclopideath+ICasptestdeath+ICrivasptestdeath+ICnotest3death+ICrivasp1death+ICasp1death)
#IC range of medications
print(ICnotest2death+ICrivasp2death+ICasp2death)

#CLTI testing arm (vs all on clopi)
print(CLTIclopideath+CLTIasptestdeath+CLTIrivasptestdeath+CLTInotest3death)
#CLTI all on clopi
print(CLTInotest1death)
#CLTI testing arm (vs mixed prescribing)
print(CLTIclopideath+CLTIasptestdeath+CLTIrivasptestdeath+CLTInotest3death+CLTIrivasp1death+CLTIasp1death)
#CLTI range of medications
print(CLTInotest2death+CLTIrivasp2death+CLTIasp2death)
