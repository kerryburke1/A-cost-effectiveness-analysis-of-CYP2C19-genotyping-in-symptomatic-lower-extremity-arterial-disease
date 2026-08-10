#State costs

#NHS cost inflation index
costinflator_2009_10_to_2024 <- (1.028*1.041*1.031*1.018*
                                   1.0040*1.0209*1.0124*1.016*
                                   1.0214*1.0249*1.0258*
                                   1.0732*1.0431)

costinflator_2013_14_to_2024 <- (1.011*1.009*1.0040*
                                   1.0209*1.0124*1.016*
                                   1.0214*1.0249*1.0258*
                                   1.0732*1.0431)

costinflator_2014_15_to_2024 <- (1.009*1.0040*
                                   1.0209*1.0124*1.016*
                                   1.0214*1.0249*1.0258*
                                   1.0732*1.0431)

costinflator_2020_21_to_2024 <- (1.0258*1.0732*1.0431)

costTest<-100
costStaffTime<-(69/4)
costClopi<-0.86*6
costClopiAE<-((2010.35*costinflator_2009_10_to_2024)*(1 - (1 - 0.0041)^0.5)) + (111.57*costinflator_2009_10_to_2024)*(1 - (1 - 0.0093)^0.5)
costAsp<-1.12*6 
costAspAE<-((2010.35*costinflator_2009_10_to_2024)*(1 - (1 - 0.0054)^0.5)) + (111.57*costinflator_2009_10_to_2024)*(1 - (1 - 0.0093)^0.5)
costRivAsp<-(1*6) + costAsp
costRivAspAE<-((2010.35*costinflator_2009_10_to_2024)* (1-(1-0.0315)^(6/23))) + ((111.57*costinflator_2009_10_to_2024)*(1-(1-0.0916)^(6/23)))

costA_C<-costClopi+costClopiAE
costA_A<-costAsp+costAspAE
costA_R<-costRivAsp+costRivAspAE

costB_C<-(((5226*costinflator_2013_14_to_2024)*0.5)*0.73)+costClopi+costClopiAE 
costB_A<-(((5226*costinflator_2013_14_to_2024)*0.5)*0.73)+costAsp+costAspAE
costB_R<-(((5226*costinflator_2013_14_to_2024)*0.5)*0.73)+costRivAsp+costRivAspAE

costC_C<-costClopi+costClopiAE
costC_A<-costAsp+costAspAE
costC_R<-costRivAsp+costRivAspAE

costD_C<-costClopi+costClopiAE
costD_A<-costAsp+costAspAE
costD_R<-costRivAsp+costRivAspAE

cost_E<-(14293.65*0.5)*costinflator_2020_21_to_2024
costE_C<-cost_E +costClopi+costClopiAE
costE_A<-cost_E +costAsp+costAspAE
costE_R<-cost_E +costRivAsp+costRivAspAE

costF_C<-costClopi+costClopiAE
costF_A<-costAsp+costAspAE
costF_R<-costRivAsp+costRivAspAE

costG_C<-costClopi+costClopiAE
costG_A<-costAsp+costAspAE
costG_R<-costRivAsp+costRivAspAE

costAH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costAH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costAH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costBH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costBH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costBH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costCH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costCH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costCH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costDH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costDH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costDH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costEH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costEH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costEH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costFH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costFH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costFH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costGH_C<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costGH_A<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costGH_R<-((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costAI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costAI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costAI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costBI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costBI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costBI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costCI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costCI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costCI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costDI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costDI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costDI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costEI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costEI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costEI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costFI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costFI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costFI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costGI_C<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE
costGI_A<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE
costGI_R<-((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE

costJ<-0
costK<-0

trans_cost_mat <- matrix(
  0,
  nrow = nstates,
  ncol = nstates,
  dimnames = list(statenames, statenames)
)

trans_cost_mat["IC", "RevascIC"] <- ((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024  
trans_cost_mat["IC", "Amputation"] <- ((1130*costinflator_2014_15_to_2024 )+(35804.46*costinflator_2020_21_to_2024))-cost_E

trans_cost_mat["CLTI", "RevascCLTI"] <- ((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024 
trans_cost_mat["CLTI", "Amputation"] <- ((1130*costinflator_2014_15_to_2024 )+(35804.46*costinflator_2020_21_to_2024))-cost_E

trans_cost_mat["RevascIC", "ReintIC"] <- ((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024 
trans_cost_mat["RevascIC", "Amputation"] <- ((1130*costinflator_2014_15_to_2024 )+(35804.46*costinflator_2020_21_to_2024))-cost_E

trans_cost_mat["RevascCLTI", "ReintCLTI"] <- ((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024 
trans_cost_mat["RevascCLTI", "Amputation"] <- ((1130*costinflator_2014_15_to_2024 )+(35804.46*costinflator_2020_21_to_2024))-cost_E

trans_cost_mat["ReintCLTI", "Amputation"] <- ((1130*costinflator_2014_15_to_2024 )+(35804.46*costinflator_2020_21_to_2024))-cost_E

trans_cost_mat["ReintIC", "Amputation"] <- ((1130*costinflator_2014_15_to_2024 )+(35804.46*costinflator_2020_21_to_2024))-cost_E

vasc_death<-2726*costinflator_2009_10_to_2024 
trans_cost_mat["IC", "VascularDeath"] <- vasc_death
trans_cost_mat["CLTI", "VascularDeath"] <- vasc_death
trans_cost_mat["RevascIC", "VascularDeath"] <- vasc_death
trans_cost_mat["RevascCLTI", "VascularDeath"] <- vasc_death
trans_cost_mat["Amputation", "VascularDeath"] <- vasc_death
trans_cost_mat["ReintCLTI", "VascularDeath"] <- vasc_death
trans_cost_mat["ReintIC", "VascularDeath"] <- vasc_death
trans_cost_mat["ICStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["CLTIStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["RevascICStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["RevascCLTIStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["AmputationStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["ReintCLTIStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["ReintICStroke", "VascularDeath"] <- vasc_death
trans_cost_mat["ICMI", "VascularDeath"] <- vasc_death
trans_cost_mat["CLTIMI", "VascularDeath"] <- vasc_death
trans_cost_mat["RevascICMI", "VascularDeath"] <- vasc_death
trans_cost_mat["RevascCLTIMI", "VascularDeath"] <- vasc_death
trans_cost_mat["AmputationMI", "VascularDeath"] <- vasc_death
trans_cost_mat["ReintCLTIMI", "VascularDeath"] <- vasc_death
trans_cost_mat["ReintICMI", "VascularDeath"] <- vasc_death

other_death<-250*costinflator_2009_10_to_2024 
trans_cost_mat["IC", "OtherDeath"] <- other_death
trans_cost_mat["CLTI", "OtherDeath"] <- other_death
trans_cost_mat["RevascIC", "OtherDeath"] <- other_death
trans_cost_mat["RevascCLTI", "OtherDeath"] <- other_death
trans_cost_mat["Amputation", "OtherDeath"] <- other_death
trans_cost_mat["ReintCLTI", "OtherDeath"] <- other_death
trans_cost_mat["ReintIC", "OtherDeath"] <- other_death
trans_cost_mat["ICStroke", "OtherDeath"] <- other_death
trans_cost_mat["CLTIStroke", "OtherDeath"] <- other_death
trans_cost_mat["RevascICStroke", "OtherDeath"] <- other_death
trans_cost_mat["RevascCLTIStroke", "OtherDeath"] <- other_death
trans_cost_mat["AmputationStroke", "OtherDeath"] <- other_death
trans_cost_mat["ReintCLTIStroke", "OtherDeath"] <- other_death
trans_cost_mat["ReintICStroke", "OtherDeath"] <- other_death
trans_cost_mat["ICMI", "OtherDeath"] <- other_death
trans_cost_mat["CLTIMI", "OtherDeath"] <- other_death
trans_cost_mat["RevascICMI", "OtherDeath"] <- other_death
trans_cost_mat["RevascCLTIMI", "OtherDeath"] <- other_death
trans_cost_mat["AmputationMI", "OtherDeath"] <- other_death
trans_cost_mat["ReintCLTIMI", "OtherDeath"] <- other_death
trans_cost_mat["ReintICMI", "OtherDeath"] <- other_death