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

n<-138563 
nstates<-23
discountrate<-0.035
run_tests=FALSE

statenames<-c("IC","CLTI","RevascIC","RevascCLTI","Amputation","ReintCLTI","ReintIC","ICStroke","CLTIStroke","RevascICStroke","RevascCLTIStroke","AmputationStroke","ReintCLTIStroke","ReintICStroke","ICMI","CLTIMI","RevascICMI","RevascCLTIMI","AmputationMI","ReintCLTIMI","ReintICMI","VascularDeath","OtherDeath")

root <- "C:/Users/Kerry/Documents/GitHub/A-cost-effectiveness-analysis-of-CYP2C19-genotyping-in-symptomatic-lower-extremity-arterial-disease"

source(file.path(root,"Deterministic model","Data","Transition probabilities.R"))
source(file.path(root,"Deterministic model","Data","Costs.R"))
source(file.path(root,"Deterministic model","Data","Utilities.R"))

#Decision Tree 1 (IC)
tree<-Node$new("IC")
tree$AddChild("Genotyping")
tree$AddChild("NoGenotyping_allClopi")
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
tree$Genotyping$Dropout$ICnotest3$p<-1

tree$NoGenotyping_allClopi$AddChild("ICnotest1")
tree$NoGenotyping_allClopi$ICnotest1$p<-1

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
tree$AddChild("NoGenotyping_allClopi")
tree$Genotyping$AddChild("Dropout")
tree$Genotyping$Dropout$p <- 0.10
tree$Genotyping$AddChild("NoLOF")
tree$Genotyping$NoLOF$p <- 0.9*0.70
tree$Genotyping$NoLOF$AddChild("CLTIclopi")
tree$Genotyping$AddChild("LOF")
tree$Genotyping$LOF$p <- 0.9*0.30
tree$Genotyping$LOF$AddChild("CLTIrivasptest")
tree$Genotyping$LOF$ICrivasptest$p <- 0.53
tree$Genotyping$LOF$AddChild("CLTIasptest")
tree$Genotyping$LOF$ICasptest$p <- 0.47

tree$Genotyping$Dropout$AddChild("CLTInotest3")
tree$Genotyping$Dropout$CLTInotest3$p<-1

tree$NoGenotyping_allClopi$AddChild("CLTInotest1")
tree$NoGenotyping_allClopi$CLTInotest1$p<-1

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
source(file.path(root,"Deterministic model","IC - clopi no test3.R"))
source(file.path(root,"Deterministic model","IC - clopi.R"))
source(file.path(root,"Deterministic model","IC - asp + test.R"))
source(file.path(root,"Deterministic model","IC - rivasp + test.R"))
source(file.path(root,"Deterministic model","CLTI - clopi no test1.R"))
source(file.path(root,"Deterministic model","CLTI - clopi no test3.R"))
source(file.path(root,"Deterministic model","CLTI - clopi.R"))
source(file.path(root,"Deterministic model","CLTI - asp + test.R"))
source(file.path(root,"Deterministic model","CLTI - rivasp + test.R"))

#total costs and QALYs
#IC testing (clopi dropout)
total_cost1 = (ICclopi[1]+ICasptest[1]+ICnotest3[1]) / 138563
total_qaly1 = (ICclopi[2]+ICasptest[2]+ICnotest3[2]) / 138563
print(total_cost1)
print(total_qaly1)

#IC all on clopi
total_cost2 = (ICnotest1[1]) / 138563
total_qaly2 = (ICnotest1[2]) / 138563
print(total_cost2)
print(total_qaly2)

#CLTI testing (clopi dropout)
total_cost5 = (CLTIclopi[1]+CLTIasptest[1]+CLTInotest3[1]) / 34640 
total_qaly5 = (CLTIclopi[2]+CLTIasptest[2]+CLTInotest3[2]) / 34640 
print(total_cost5)
print(total_qaly5)

#CLTI all on clopi
total_cost7 = (CLTInotest1[1]) / 34640 
total_qaly7 = (CLTInotest1[2]) / 34640 
print(total_cost7)
print(total_qaly7)

#total amputations

#IC testing arm (vs all on clopi)
print(ICclopiamps+ICasptestamps+ICnotest3amps)
#IC all on clopi
print(ICnotest1amps)

#CLTI testing arm (vs all on clopi)
print(CLTIclopiamps+CLTIasptestamps+CLTInotest3amps)
#CLTI all on clopi
print(CLTInotest1amps)

#total revascs

#IC testing arm (vs all on clopi)
print(ICclopirevasc+ICasptestrevasc+ICnotest3revasc)
#IC all on clopi
print(ICnotest1revasc)

#CLTI testing arm (vs all on clopi)
print(CLTIclopirevasc+CLTIasptestrevasc+CLTInotest3revasc)
#CLTI all on clopi
print(CLTInotest1revasc)

#total strokes

#IC testing arm (vs all on clopi)
print(ICclopistroke+ICaspteststroke+ICnotest3stroke)
#IC all on clopi
print(ICnotest1stroke)

#CLTI testing arm (vs all on clopi)
print(CLTIclopistroke+CLTIaspteststroke+CLTInotest3stroke)
#CLTI all on clopi
print(CLTInotest1stroke)

#total MIs

#IC testing arm (vs all on clopi)
print(ICclopiMI+ICasptestMI+ICnotest3MI)
#IC all on clopi
print(ICnotest1MI)

#CLTI testing arm (vs all on clopi)
print(CLTIclopiMI+CLTIasptestMI+CLTInotest3MI)
#CLTI all on clopi
print(CLTInotest1MI)

#total deaths

#IC testing arm (vs all on clopi)
print(ICclopideath+ICasptestdeath+ICnotest3death)
#IC all on clopi
print(ICnotest1death)

#CLTI testing arm (vs all on clopi)
print(CLTIclopideath+CLTIasptestdeath+CLTInotest3death)
#CLTI all on clopi
print(CLTInotest1death)