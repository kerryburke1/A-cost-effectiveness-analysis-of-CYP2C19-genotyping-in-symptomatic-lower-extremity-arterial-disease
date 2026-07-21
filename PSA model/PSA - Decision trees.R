install.packages("data.tree")
install.packages("rprojroot")
install.packages("testthat")
install.packages("readxl")
install.packages("DiagrammeR")
install.packages("dampack")

library("data.tree")
library("testthat")
library("readxl")
library("DiagrammeR")
library("rprojroot")
library("dampack")

mixed_prescribing<-FALSE
#FALSE = dropout go into alternate arm 1 (all on clopi), testing compared to all on clopi
#TRUE = dropout go into alternate arm 2 (mixed prescribing), testing compared to mixed prescribing

n<-138563 
nstates<-23
discountrate<-0.035
run_tests=FALSE
mcruns<-1000

statenames<-c("IC","CLTI","RevascIC","RevascCLTI","Amputation","ReintCLTI","ReintIC","ICStroke","CLTIStroke","RevascICStroke","RevascCLTIStroke","AmputationStroke","ReintCLTIStroke","ReintICStroke","ICMI","CLTIMI","RevascICMI","RevascCLTIMI","AmputationMI","ReintCLTIMI","ReintICMI","VascularDeath","OtherDeath")

root <- "C:/Users/Kerry/Documents/Github/A-cost-effectiveness-analysis-of-CYP2C19-genotyping-in-symptomatic-lower-extremity-arterial-disease"

source(file.path(root,"PSA model","Data","PSA - Transition probabilities.R"))
source(file.path(root,"PSA model","Data","PSA - Costs.R"))
source(file.path(root,"PSA model","Data","PSA - Utilities.R"))
source(file.path(root,"PSA model","tests","testthat","test_transitions.R"))
source(file.path(root,"PSA model","tests","testthat","test_trees.R"))
source(file.path(root,"PSA model","tests","testthat","test_transitions.R"))
source(file.path(root,"PSA model","tests","testthat","test_negatives.R"))

markovn<-data.frame(matrix(nrow=mcruns,ncol=10))

for (i in 1:mcruns){
  
  #Decision Tree 1 (IC)
  tree<-Node$new("IC")
  tree$AddChild("Genotyping")
  tree$AddChild("NoGenotyping")
  tree$AddChild("NoGenotyping_allClopi")
  tree$NoGenotyping$p<-ifelse(mixed_prescribing,1,0)
  tree$NoGenotyping_allClopi$p<-ifelse(mixed_prescribing,0,1)
  tree$Genotyping$AddChild("Dropout")
  tree$Genotyping$Dropout$p<-rbeta(1,1,9)
  tree$Genotyping$AddChild("NoLOF")
  tree$Genotyping$NoLOF$p<-(1-tree$Genotyping$Dropout$p)*0.7029
  tree$Genotyping$NoLOF$AddChild("ICclopi")
  tree$Genotyping$AddChild("LOF")
  tree$Genotyping$LOF$p<-(1-tree$Genotyping$Dropout$p)*0.2971
  tree$Genotyping$LOF$AddChild("ICrivasptest")
  tree$Genotyping$LOF$ICrivasptest$p<-rbeta(1,0.53*29,0.47*29)
  tree$Genotyping$LOF$AddChild("ICasptest")
  tree$Genotyping$LOF$ICasptest$p<-1-tree$Genotyping$LOF$ICrivasptest$p
  
  tree$Genotyping$Dropout$AddChild("ICnotest3")
  tree$Genotyping$Dropout$ICnotest3$p<-ifelse(mixed_prescribing,rbeta(1,0.72*29,0.28*29),1)
  tree$Genotyping$Dropout$AddChild("ICrivasp1")
  tree$Genotyping$Dropout$ICrivasp1$p<-ifelse(mixed_prescribing,rbeta(1,0.09*29,0.91*29),0)
  tree$Genotyping$Dropout$AddChild("ICasp1")
  tree$Genotyping$Dropout$ICasp1$p<-ifelse(mixed_prescribing,1-((tree$Genotyping$Dropout$ICnotest3$p)+(tree$Genotyping$Dropout$ICrivasp1$p)),0)
  tree$NoGenotyping_allClopi$AddChild("ICnotest1")
  tree$NoGenotyping_allClopi$ICnotest1$p<-1
  
  tree$NoGenotyping$AddChild("ICnotest2")
  tree$NoGenotyping$ICnotest2$p<-tree$Genotyping$Dropout$ICnotest3$p
  tree$NoGenotyping$AddChild("ICrivasp2")
  tree$NoGenotyping$ICrivasp2$p<-tree$Genotyping$Dropout$ICrivasp1$p
  tree$NoGenotyping$AddChild("ICasp2")
  tree$NoGenotyping$ICasp2$p<-tree$Genotyping$Dropout$ICasp1$p
  
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
  
  markovn[i,]=n*leaf_probs[,2]
  
}
markovn
n<-34640 

markovn2<-data.frame(matrix(nrow=mcruns,ncol=10))

for (i in 1:mcruns){
  
  #Decision Tree 2 (CLTI)
  tree<-Node$new("CLTI")
  tree$AddChild("Genotyping")
  tree$AddChild("NoGenotyping")
  tree$AddChild("NoGenotyping_allClopi")
  tree$NoGenotyping$p<-ifelse(mixed_prescribing,1,0)
  tree$NoGenotyping_allClopi$p<-ifelse(mixed_prescribing,0,1)
  tree$Genotyping$AddChild("Dropout")
  tree$Genotyping$Dropout$p<-rbeta(1,1,9)
  tree$Genotyping$AddChild("NoLOF")
  tree$Genotyping$NoLOF$p<-(1-tree$Genotyping$Dropout$p)*0.7029
  tree$Genotyping$NoLOF$AddChild("CLTIclopi")
  tree$Genotyping$AddChild("LOF")
  tree$Genotyping$LOF$p<-(1-tree$Genotyping$Dropout$p)*0.2971
  tree$Genotyping$LOF$AddChild("CLTIrivasptest")
  tree$Genotyping$LOF$CLTIrivasptest$p<-rbeta(1,0.73*29,0.27*29)
  tree$Genotyping$LOF$AddChild("CLTIasptest")
  tree$Genotyping$LOF$CLTIasptest$p<-1-tree$Genotyping$LOF$CLTIrivasptest$p
  
  tree$Genotyping$Dropout$AddChild("CLTInotest3")
  tree$Genotyping$Dropout$CLTInotest3$p<-ifelse(mixed_prescribing,rbeta(1,0.65*29,0.35*29),1)
  tree$Genotyping$Dropout$AddChild("CLTIrivasp1")
  tree$Genotyping$Dropout$CLTIrivasp1$p<-ifelse(mixed_prescribing,rbeta(1,0.22*29,0.78*29),0)
  tree$Genotyping$Dropout$AddChild("CLTIasp1")
  tree$Genotyping$Dropout$CLTIasp1$p<-ifelse(mixed_prescribing,1-((tree$Genotyping$Dropout$CLTInotest3$p)+(tree$Genotyping$Dropout$CLTIrivasp1$p)),0)
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
  
  markovn2[i,]=n*leaf_probs[,2]
}


source(file.path(root,"PSA model","PSA - IC - clopi no test1.R"))
source(file.path(root,"PSA model","PSA - IC - clopi no test2.R"))
source(file.path(root,"PSA model","PSA - IC - clopi no test3.R"))
source(file.path(root,"PSA model","PSA - IC - clopi.R"))
source(file.path(root,"PSA model","PSA - IC - riv-asp1.R"))
source(file.path(root,"PSA model","PSA - IC - riv-asp2.R"))
source(file.path(root,"PSA model","PSA - IC - riv-asp + test.R"))
source(file.path(root,"PSA model","PSA - IC - asp1.R"))
source(file.path(root,"PSA model","PSA - IC - asp2.R"))
source(file.path(root,"PSA model","PSA - IC - asp + test.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi no test1.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi no test2.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi no test3.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi.R"))
source(file.path(root,"PSA model","PSA - CLTI - riv-asp1.R"))
source(file.path(root,"PSA model","PSA - CLTI - riv-asp2.R"))
source(file.path(root,"PSA model","PSA - CLTI - riv-asp + test.R"))
source(file.path(root,"PSA model","PSA - CLTI - asp1.R"))
source(file.path(root,"PSA model","PSA - CLTI - asp2.R"))  
source(file.path(root,"PSA model","PSA - CLTI - asp + test.R"))

#Calculate ICERS
#IC
total_cost_test_IC<-mcresults_ICnotest3[,1]+mcresults_ICrivasp1[,1]+mcresults_ICasp1[,1]+
  mcresults_ICclopi[,1]+mcresults_ICrivasptest[,1]+mcresults_ICasptest[,1]
print(paste("Mean cost of genotyping IC:",mean(total_cost_test_IC/138563) ))
total_qaly_test_IC<-mcresults_ICnotest3[,2]+mcresults_ICrivasp1[,2]+mcresults_ICasp1[,2]+
  mcresults_ICclopi[,2]+mcresults_ICrivasptest[,2]+mcresults_ICasptest[,2]
print(paste("Mean QALYs of genotyping IC:",mean(total_qaly_test_IC/138563) ))

total_cost_comp_IC<-mcresults_ICnotest1[,1]+mcresults_ICnotest2[,1]+mcresults_ICrivasp2[,1]+mcresults_ICasp2[,1]
print(paste("Mean cost of nogenotyping IC:",mean(total_cost_comp_IC/138563) ))
total_qaly_comp_IC<-mcresults_ICnotest1[,2]+mcresults_ICnotest2[,2]+mcresults_ICrivasp2[,2]+mcresults_ICasp2[,2]
print(paste("Mean QALYs of nogenotyping IC:",mean(total_qaly_comp_IC/138563) ))

ICERS_IC<-(total_cost_test_IC-total_cost_comp_IC)/(total_qaly_test_IC-total_qaly_comp_IC)
NB_IC<-((total_qaly_test_IC-total_qaly_comp_IC)*20000)-(total_cost_test_IC-total_cost_comp_IC)
plot(density(NB_IC))
PCE_IC<-mean(NB_IC>0)
print(paste("Probability of genotyping being cost-effective IC:",PCE_IC*100,"%"))

#CLTI
total_cost_test_CLTI<-mcresults_CLTInotest3[,1]+mcresults_CLTIrivasp1[,1]+mcresults_CLTIasp1[,1]+
  mcresults_CLTIclopi[,1]+mcresults_CLTIrivasptest[,1]+mcresults_CLTIasptest[,1]
print(paste("Mean Cost of genotyping CLTI:",mean(total_cost_test_CLTI/34640 ) ))
total_qaly_test_CLTI<-mcresults_CLTInotest3[,2]+mcresults_CLTIrivasp1[,2]+mcresults_CLTIasp1[,2]+
  mcresults_CLTIclopi[,2]+mcresults_CLTIrivasptest[,2]+mcresults_CLTIasptest[,2]
print(paste("Mean QALYs of nogenotyping CLTI:",mean(total_qaly_test_CLTI/34640 ) ))

total_cost_comp_CLTI<-mcresults_CLTInotest1[,1]+mcresults_CLTInotest2[,1]+mcresults_CLTIrivasp2[,1]+mcresults_CLTIasp2[,1]
print(paste("Mean Cost of nogenotyping CLTI:",mean(total_cost_comp_CLTI/34640 ) ))
total_qaly_comp_CLTI<-mcresults_CLTInotest1[,2]+mcresults_CLTInotest2[,2]+mcresults_CLTIrivasp2[,2]+mcresults_CLTIasp2[,2]
print(paste("Mean QALYs of nogenotyping CLTI:",mean(total_qaly_comp_CLTI/34640 ) ))

ICERS_CLTI<-(total_cost_test_CLTI-total_cost_comp_CLTI)/(total_qaly_test_CLTI-total_qaly_comp_CLTI)
NB_CLTI<-((total_qaly_test_CLTI-total_qaly_comp_CLTI)*20000)-(total_cost_test_CLTI-total_cost_comp_CLTI)
PCE_CLTI<-mean(NB_CLTI>0)

print(paste("Probability of genotyping being cost-effective CLTI:",PCE_CLTI*100,"%"))

print(PCE_IC)
print(PCE_CLTI)

#Graphs IC
strategies<-list("Genotyping","No Genotyping")
cost<-data.frame("Genotyping_Cost"=c(total_cost_test_IC/138563),
                 "NoGenotyping_Cost"=c(total_cost_comp_IC/138563))
effectiveness<-data.frame("Genotyping_Eff"=c(total_qaly_test_IC/138563),
                          "NoGenotyping_Eff"=c(total_qaly_comp_IC/138563))
IC_psa_obj1<-make_psa_obj(cost=cost,
                          effectiveness=effectiveness,
                          strategies=strategies,
                          currency="£")

IC_ceac_obj1<-ceac(wtp=c(0,5000,10000,20000,40000,50000,100000),
                   psa=IC_psa_obj1)

plot(IC_psa_obj1)
plot(IC_ceac_obj1)

p <- plot(IC_ceac_obj1)

p +
  labs(x = "Willingness to Pay (Thousand £ / QALY)")



strategies<-list("Genotyping","No Genotyping")
cost<-data.frame("Genotyping_Cost"=c(total_cost_test_IC/138563),
                 "NoGenotyping_Cost"=c(total_cost_comp_IC/138563))
effectiveness<-data.frame("Genotyping_Eff"=c(total_qaly_test_IC/138563),
                          "NoGenotyping_Eff"=c(total_qaly_comp_IC/138563))
IC_psa_obj2<-make_psa_obj(cost=cost,
                          effectiveness=effectiveness,
                          strategies=strategies,
                          currency="£")

IC_ceac_obj2<-ceac(wtp=c(0,5000,10000,20000,40000,50000,100000),
                   psa=IC_psa_obj2)

plot(IC_psa_obj2)
plot(IC_ceac_obj2)

p <- plot(IC_ceac_obj2)

p +
  labs(x = "Willingness to Pay (Thousand £ / QALY)")

#Graphs CLTI
cost<-data.frame("Genotyping_Cost"=c(total_cost_test_CLTI/138563),
                 "NoGenotyping_Cost"=c(total_cost_comp_CLTI/138563))
effectiveness<-data.frame("Genotyping_Eff"=c(total_qaly_test_CLTI/138563),
                          "NoGenotyping_Eff"=c(total_qaly_comp_CLTI/138563))
CLTI_psa_obj1<-make_psa_obj(cost=cost,
                            effectiveness=effectiveness,
                            strategies=strategies,
                            currency="£")
CLTI_ceac_obj1<-ceac(wtp=c(0,5000,10000,20000,40000,50000,100000),
                     psa=CLTI_psa_obj)
plot(CLTI_psa_obj1)
plot(CLTI_ceac_obj1)


p <- plot(CLTI_ceac_obj1)

p +
  labs(x = "Willingness to Pay (Thousand £ / QALY)")



cost<-data.frame("Genotyping_Cost"=c(total_cost_test_CLTI/138563),
                 "NoGenotyping_Cost"=c(total_cost_comp_CLTI/138563))
effectiveness<-data.frame("Genotyping_Eff"=c(total_qaly_test_CLTI/138563),
                          "NoGenotyping_Eff"=c(total_qaly_comp_CLTI/138563))
CLTI_psa_obj2<-make_psa_obj(cost=cost,
                            effectiveness=effectiveness,
                            strategies=strategies,
                            currency="£")
CLTI_ceac_obj2<-ceac(wtp=c(0,5000,10000,20000,40000,50000,100000),
                     psa=CLTI_psa_obj2)
plot(CLTI_psa_obj2)
plot(CLTI_ceac_obj2)


p <- plot(CLTI_ceac_obj2)

p +
  labs(x = "Willingness to Pay (Thousand £ / QALY)")

#combine into 2x2 plot
install.packages("patchwork")
library(patchwork)

p1 <- plot(IC_ceac_obj1) + labs(title = "IC genotyping versus all on clopidogrel") +
  theme(plot.title = element_text(size = 12))
p2 <- plot(IC_ceac_obj2) + labs(title = "IC genotyping versus all on mixed prescribing") +
  theme(plot.title = element_text(size = 12))
p3 <- plot(CLTI_ceac_obj1) + labs(title = "CLTI genotyping versus all on clopidogrel") +
  theme(plot.title = element_text(size = 12))
p4 <- plot(CLTI_ceac_obj2) + labs(title = "CLTI genotyping versus all on mixed prescribing") +
  theme(plot.title = element_text(size = 12))

p1 <- p1 + labs(x = "Willingness to Pay (Thousand £ / QALY)")
p2 <- p2 + labs(x = "Willingness to Pay (Thousand £ / QALY)")
p3 <- p3 + labs(x = "Willingness to Pay (Thousand £ / QALY)")
p4 <- p4 + labs(x = "Willingness to Pay (Thousand £ / QALY)")

combined <- (p1 | p2) /
  (p3 | p4)

combined +
  scale_x_continuous(
    labels = function(x) paste0("£", x/1000)
  )
