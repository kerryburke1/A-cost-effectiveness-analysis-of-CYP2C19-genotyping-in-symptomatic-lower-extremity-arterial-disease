#State costs

#NHS cost inflation index
costinflator_2011<-(1.021*1.017*1.011*1.009*1.0040*1.0209*1.0124*1.016*1.0214*1.0249*1.0258*1.0732*1.0431)
costinflator_2016<-(1.0209*1.0124*1.016*1.0214*1.0249*1.0258*1.0732*1.0431)
costinflator_2019<-(1.0214*1.0249*1.0258*1.0732*1.0431)
costinflator_2020<-(1.0249*1.0258*1.0732*1.0431)
costinflator_2021<-(1.0258*1.0732*1.0431)
costinflator_2022<-(1.0732*1.0431)

costTest<-100 *rnorm(1,1,0.05102041)
costStaffTime<-(69/4) *rnorm(1,1,0.05102041)
costClopi<-0.86*6 *rnorm(1,1,0.05102041)
costClopiAE<-((((2010.35*costinflator_2011)*0.0041) + ((111.57*costinflator_2011)*0.0093))/2) *rnorm(1,1,0.05102041)
costAsp<-2.17*6 *rnorm(1,1,0.05102041)
costAspAE<-((((2010.35*costinflator_2011)*0.0054) + ((111.57*costinflator_2011)*0.0093))/2) *rnorm(1,1,0.05102041)
costRivAsp<-((1.08*6) + costAsp) *rnorm(1,1,0.05102041)
costRivAspAE<-(((((2010.35*costinflator_2011)*0.031) + ((111.57*costinflator_2011)*0.092)/1.9)*0.5) + costAspAE) *rnorm(1,1,0.05102041)

costA_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costA_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costA_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costB_C<-((((7344.86*costinflator_2016)*0.5)*0.73)+costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costB_R<-((((7344.86*costinflator_2016)*0.5)*0.73)+costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costB_A<-((((7344.86*costinflator_2016)*0.5)*0.73)+costAsp+costAspAE) *rnorm(1,1,0.05102041)

costC_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costC_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costC_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costD_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costD_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costD_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costE_C<-(((14293.65*0.5)*costinflator_2022) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costE_R<-(((14293.65*0.5)*costinflator_2022) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costE_A<-(((14293.65*0.5)*costinflator_2022) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costF_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costF_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costF_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costG_C<-(costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costG_R<-(costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costG_A<-(costAsp+costAspAE) *rnorm(1,1,0.05102041)

costAH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costAH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costAH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costBH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costBH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costBH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costCH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costCH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costCH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costDH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costDH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costDH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costEH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costEH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costEH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costFH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costFH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costFH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costGH_C<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costGH_R<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costGH_A<-(((((360+5590)+(430+6170))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costAI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costAI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costAI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costBI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costBI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costBI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costCI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costCI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costCI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costDI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costDI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costDI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costEI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costEI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costEI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costFI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costFI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costFI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costGI_C<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costClopi+costClopiAE) *rnorm(1,1,0.05102041)
costGI_R<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costRivAsp+costRivAspAE) *rnorm(1,1,0.05102041)
costGI_A<-(((((190+4340)+(230+5610))/2)*costinflator_2020) +costAsp+costAspAE) *rnorm(1,1,0.05102041)

costJ<-0
costK<-0

trans_cost_mat <- matrix(0, nrow=nstates, ncol=nstates,
                         dimnames=list(statenames, statenames))

trans_cost_mat["IC", "RevascIC"] <- (((20402*0.42) + (12298*0.58))*costinflator_2019) *rnorm(1,1,0.05102041)
trans_cost_mat["IC", "Amputation"] <- (((13320*costinflator_2019)+(35804.46*costinflator_2022))-14293.65) *rnorm(1,1,0.05102041)

trans_cost_mat["CLTI", "RevascCLTI"] <- (((20402*0.42) + (12298*0.58))*costinflator_2019) *rnorm(1,1,0.05102041) 
trans_cost_mat["CLTI", "Amputation"] <- (((13320*costinflator_2019)+(35804.46*costinflator_2022))-14293.65) *rnorm(1,1,0.05102041)

trans_cost_mat["RevascIC", "ReintIC"] <- (((20402*0.42) + (12298*0.58))*costinflator_2019) *rnorm(1,1,0.05102041) 
trans_cost_mat["RevascIC", "Amputation"] <- (((13320*costinflator_2019)+(35804.46*costinflator_2022))-14293.65) *rnorm(1,1,0.05102041)

trans_cost_mat["RevascCLTI", "ReintCLTI"] <- (((20402*0.42) + (12298*0.58))*costinflator_2019) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTI", "Amputation"] <- (((13320*costinflator_2019)+(35804.46*costinflator_2022))-14293.65) *rnorm(1,1,0.05102041)

trans_cost_mat["ReintCLTI", "Amputation"] <- (((13320*costinflator_2019)+(35804.46*costinflator_2022))-14293.65) *rnorm(1,1,0.05102041)

trans_cost_mat["ReintIC", "Amputation"] <- (((13320*costinflator_2019)+(35804.46*costinflator_2022))-14293.65) *rnorm(1,1,0.05102041)

trans_cost_mat["IC", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["CLTI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascIC", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["Amputation", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintCLTI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintIC", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ICStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["CLTIStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascICStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTIStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["AmputationStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintCLTIStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintICStroke", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ICMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["CLTIMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascICMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTIMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["AmputationMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintCLTIMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintICMI", "VascularDeath"] <- (10855*costinflator_2021) *rnorm(1,1,0.05102041)


trans_cost_mat["IC", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["CLTI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascIC", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["Amputation", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintCLTI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintIC", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ICStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["CLTIStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascICStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTIStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["AmputationStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintCLTIStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintICStroke", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ICMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["CLTIMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascICMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["RevascCLTIMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["AmputationMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintCLTIMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
trans_cost_mat["ReintICMI", "OtherDeath"] <- (2755*costinflator_2021) *rnorm(1,1,0.05102041)
