install.packages("data.tree")
install.packages("rprojroot")
install.packages("testthat")
install.packages("readxl")
install.packages("DiagrammeR")
install.packages("dampack")
install.packages("patchwork")

library("data.tree")
library("testthat")
library("readxl")
library("DiagrammeR")
library("rprojroot")
library("dampack")

n<-138563 
nstates<-23
discountrate<-0.035
mcruns<-10000

statenames<-c("IC","CLTI","RevascIC","RevascCLTI","Amputation","ReintCLTI","ReintIC","ICStroke","CLTIStroke","RevascICStroke","RevascCLTIStroke","AmputationStroke","ReintCLTIStroke","ReintICStroke","ICMI","CLTIMI","RevascICMI","RevascCLTIMI","AmputationMI","ReintCLTIMI","ReintICMI","VascularDeath","OtherDeath")

root <- "C:/Users/Kerry/Documents/Github/A-cost-effectiveness-analysis-of-CYP2C19-genotyping-in-symptomatic-lower-extremity-arterial-disease"

source(file.path(root,"PSA model","Data","PSA - Transition probabilities.R"))
source(file.path(root,"PSA model","Data","PSA - Costs.R"))
source(file.path(root,"PSA model","Data","PSA - Utilities.R"))

markovn<-data.frame(matrix(nrow=mcruns,ncol=5))

for (i in 1:mcruns){
  
  #Decision Tree 1 (IC)
  tree<-Node$new("IC")
  tree$AddChild("Genotyping")
  tree$AddChild("NoGenotyping_allClopi")
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
  
  markovn[i,]=n*leaf_probs[,2]
}

markovn
n<-34640 

markovn2<-data.frame(matrix(nrow=mcruns,ncol=5))

for (i in 1:mcruns){
  
  #Decision Tree 2 (CLTI)
  tree<-Node$new("CLTI")
  tree$AddChild("Genotyping")
  tree$AddChild("NoGenotyping_allClopi")
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
  
  markovn2[i,]=n*leaf_probs[,2]
}

source(file.path(root,"PSA model","PSA - IC - clopi no test1.R"))
source(file.path(root,"PSA model","PSA - IC - clopi no test3.R"))
source(file.path(root,"PSA model","PSA - IC - clopi.R"))
source(file.path(root,"PSA model","PSA - IC - asp + test.R"))
source(file.path(root,"PSA model","PSA - IC - rivasp + test.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi no test1.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi no test3.R"))
source(file.path(root,"PSA model","PSA - CLTI - clopi.R"))
source(file.path(root,"PSA model","PSA - CLTI - asp + test.R"))
source(file.path(root,"PSA model","PSA - CLTI - rivasp + test.R"))

#Calculate ICERS
#IC
total_cost_test_IC<-mcresults_ICnotest3[,1]+
  mcresults_ICclopi[,1]+mcresults_ICasptest[,1]+mcresults_ICrivasptest[,1]
print(paste("Mean cost of genotyping IC:",mean(total_cost_test_IC/138563) ))
total_qaly_test_IC<-mcresults_ICnotest3[,2]+
  mcresults_ICclopi[,2]+mcresults_ICasptest[,2]+mcresults_ICrivasptest[,2]
print(paste("Mean QALYs of genotyping IC:",mean(total_qaly_test_IC/138563) ))

total_cost_comp_IC<-mcresults_ICnotest1[,1]
print(paste("Mean cost of nogenotyping IC:",mean(total_cost_comp_IC/138563) ))
total_qaly_comp_IC<-mcresults_ICnotest1[,2]
print(paste("Mean QALYs of nogenotyping IC:",mean(total_qaly_comp_IC/138563) ))

ICERS_IC<-(total_cost_test_IC-total_cost_comp_IC)/(total_qaly_test_IC-total_qaly_comp_IC)
NB_IC<-((total_qaly_test_IC-total_qaly_comp_IC)*20000)-(total_cost_test_IC-total_cost_comp_IC)
PCE_IC<-mean(NB_IC>0)
print(paste("Probability of genotyping being cost-effective IC:",PCE_IC*100,"%"))

#CLTI
total_cost_test_CLTI<-mcresults_CLTInotest3[,1]+
  mcresults_CLTIclopi[,1]+mcresults_CLTIasptest[,1]+mcresults_CLTIrivasptest[,1]
print(paste("Mean Cost of genotyping CLTI:",mean(total_cost_test_CLTI/34640 ) ))
total_qaly_test_CLTI<-mcresults_CLTInotest3[,2]+
  mcresults_CLTIclopi[,2]+mcresults_CLTIasptest[,2]+mcresults_CLTIrivasptest[,2]
print(paste("Mean QALYs of genotyping CLTI:",mean(total_qaly_test_CLTI/34640 ) ))

total_cost_comp_CLTI<-mcresults_CLTInotest1[,1]
print(paste("Mean Cost of nogenotyping CLTI:",mean(total_cost_comp_CLTI/34640 ) ))
total_qaly_comp_CLTI<-mcresults_CLTInotest1[,2]
print(paste("Mean QALYs of nogenotyping CLTI:",mean(total_qaly_comp_CLTI/34640 ) ))

ICERS_CLTI<-(total_cost_test_CLTI-total_cost_comp_CLTI)/(total_qaly_test_CLTI-total_qaly_comp_CLTI)
NB_CLTI<-((total_qaly_test_CLTI-total_qaly_comp_CLTI)*20000)-(total_cost_test_CLTI-total_cost_comp_CLTI)
PCE_CLTI<-mean(NB_CLTI>0)

print(paste("Probability of genotyping being cost-effective CLTI:",PCE_CLTI*100,"%"))

print(PCE_IC)
print(PCE_CLTI)

# Graphs IC
strategies <- list("Genotyping", "No_Genotyping")

cost <- data.frame(
  "Genotyping_Cost" = total_cost_test_IC / 138563,
  "NoGenotyping_Cost" = total_cost_comp_IC / 138563
)

effectiveness <- data.frame(
  "Genotyping_Eff" = total_qaly_test_IC / 138563,
  "NoGenotyping_Eff" = total_qaly_comp_IC / 138563
)

IC_psa_obj1 <- make_psa_obj(
  cost = cost,
  effectiveness = effectiveness,
  strategies = strategies,
  currency = "£"
)

IC_ceac_obj1 <- ceac(
  wtp = c(0, 5000, 10000, 20000, 40000, 50000, 100000),
  psa = IC_psa_obj1
)


# Graphs CLTI
cost <- data.frame(
  "Genotyping_Cost" = total_cost_test_CLTI / 34640,
  "NoGenotyping_Cost" = total_cost_comp_CLTI / 34640
)

effectiveness <- data.frame(
  "Genotyping_Eff" = total_qaly_test_CLTI / 34640,
  "NoGenotyping_Eff" = total_qaly_comp_CLTI / 34640
)

CLTI_psa_obj1 <- make_psa_obj(
  cost = cost,
  effectiveness = effectiveness,
  strategies = strategies,
  currency = "£"
)

CLTI_ceac_obj1 <- ceac(
  wtp = c(0, 5000, 10000, 20000, 40000, 50000, 100000),
  psa = CLTI_psa_obj1
)


# Create plots with the desired x-axis
p1 <- plot(IC_ceac_obj1) +
  labs(
    title = "IC genotyping versus all on clopidogrel",
    x = "Willingness to Pay (£000s / QALY)"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 50, 100),
    labels = c("£0", "£10k", "£20k", "£50k", "£100k")
  ) +
  theme(
    plot.title = element_text(size = 12)
  )


p2 <- plot(CLTI_ceac_obj1) +
  labs(
    title = "CLTI genotyping versus all on clopidogrel",
    x = "Willingness to Pay (£000s / QALY)"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 50, 100),
    labels = c("£0", "£10k", "£20k", "£50k", "£100k")
  ) +
  theme(
    plot.title = element_text(size = 12)
  )


# Combine into 2x2 plot
library(patchwork)

p1 <- plot(
  IC_ceac_obj1,
  xbreaks = c(0, 10, 20, 50, 100)
) +
  labs(
    title = "IC genotyping versus all on clopidogrel",
    x = "Willingness to Pay (Thousand £ / QALY)"
  ) +
  theme(
    plot.title = element_text(size = 12)
  )

p2 <- plot(CLTI_ceac_obj1) +
  labs(
    title = "CLTI genotyping versus all on clopidogrel",
    x = "Willingness to Pay (Thousand £ / QALY)"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 50, 100),
    labels = c("0", "10", "20", "50", "100")
  ) +
  theme(plot.title = element_text(size = 12))

p1 <- p1 +
  scale_colour_discrete(
    labels = c("Genotyping", "No genotyping")
  )

p2 <- p2 +
  scale_colour_discrete(
    labels = c("Genotyping", "No genotyping")
  )

combined <- p1 / p2
combined