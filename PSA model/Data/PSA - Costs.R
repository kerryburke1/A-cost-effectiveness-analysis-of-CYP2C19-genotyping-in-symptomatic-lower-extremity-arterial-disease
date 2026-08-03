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

costTest<-100 *rnorm(1,1,0.05102041)
costStaffTime<-(69/4) *rnorm(1,1,0.05102041)
costClopi<-0.86*6 *rnorm(1,1,0.05102041)
costClopiAE<-(((2010.35*costinflator_2009_10_to_2024)*(1 - (1 - 0.0041)^0.5)) + (111.57*costinflator_2009_10_to_2024)*(1 - (1 - 0.0093)^0.5)) *rnorm(1,1,0.05102041)
costAsp<-1.12*6 *rnorm(1,1,0.05102041)
costAspAE<-(((2010.35*costinflator_2009_10_to_2024)*(1 - (1 - 0.0054)^0.5)) + (111.57*costinflator_2009_10_to_2024)*(1 - (1 - 0.0093)^0.5)) *rnorm(1,1,0.05102041)

costA_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costA_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costA_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costB_C<-((((5226*costinflator_2013)*0.5)*0.73)+costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costB_R<-((((5226*costinflator_2013)*0.5)*0.73)+costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costB_A<-((((5226*costinflator_2013)*0.5)*0.73)+costAsp+costAspAE) *rnorm(1,1,0.05102041)

costC_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costC_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costC_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costD_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costD_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costD_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

cost_E<-((14293.65*0.5)*costinflator_2020_21_to_2024)*rnorm(1,1,0.05102041)
costE_C<-(((14293.65*0.5)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costE_R<-(((14293.65*0.5)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costE_A<-(((14293.65*0.5)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costF_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costF_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costF_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costG_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costG_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costG_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costAH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costAH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costAH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costBH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costBH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costBH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costCH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costCH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costCH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costDH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costDH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costDH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costEH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costEH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costEH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costFH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costFH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costFH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costGH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costGH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costGH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costAI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costAI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costAI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costBI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costBI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costBI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costCI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costCI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costCI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costDI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costDI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costDI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costEI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costEI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costEI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costFI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costFI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costFI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costGI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costGI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costGI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020_21_to_2024) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costJ<-0
costK<-0

trans_cost_mat <- matrix(0, nrow=nstates, ncol=nstates,
                         dimnames=list(statenames, statenames))

trans_cost_mat["IC", "RevascIC"] <- (((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024  ) *rnorm(1,1,0.05102041)
trans_cost_mat["IC", "Amputation"] <- (((1130*costinflator_2014_15_to_2024  )+(35804.46*costinflator_2020_21_to_2024)) *rnorm(1,1,0.05102041)) -cost_E

trans_cost_mat["CLTI", "RevascCLTI"] <- (((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024  ) *rnorm(1,1,0.05102041) 
trans_cost_mat["CLTI", "Amputation"] <- (((1130*costinflator_2014_15_to_2024  )+(35804.46*costinflator_2020_21_to_2024)) *rnorm(1,1,0.05102041)) -cost_E

trans_cost_mat["RevascIC", "ReintIC"] <- (((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024  ) *rnorm(1,1,0.05102041) 
trans_cost_mat["RevascIC", "Amputation"] <- (((1130*costinflator_2014_15_to_2024  )+(costinflator_2020_21_to_2024)) *rnorm(1,1,0.05102041)) -cost_E

trans_cost_mat["RevascCLTI", "ReintCLTI"] <- (((20401*0.42) + (12298*0.58))*costinflator_2014_15_to_2024  ) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTI", "Amputation"] <- (((1130*costinflator_2014_15_to_2024  )+(costinflator_2020_21_to_2024)) *rnorm(1,1,0.05102041)) -cost_E

trans_cost_mat["ReintCLTI", "Amputation"] <- (((1130*costinflator_2014_15_to_2024  )+(costinflator_2020_21_to_2024)) *rnorm(1,1,0.05102041)) -cost_E

trans_cost_mat["ReintIC", "Amputation"] <- (((1130*costinflator_2014_15_to_2024  )+(costinflator_2020_21_to_2024)) *rnorm(1,1,0.05102041)) -cost_E

vasc_death<-(2726*costinflator_2009_10_to_2024 ) *rnorm(1,1,0.05102041)
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

other_death<-(250*costinflator_2009_10_to_2024 ) *rnorm(1,1,0.05102041)
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