Question 1)  Let's check the measurement quality criteria for our model.








setwd("C:/Users/USER/Documents/BASM")
library(seminr)


#### HW15 ----
sec <- read.csv("security_data.csv")
sec_mm<-measure(
  reflect("TRUST",multi_items("TRST",1:4)),
  reflect("SEC",multi_items("PSEC",1:4)),
  reflect("REP",multi_items("PREP",1:4)),
  reflect("INV",multi_items("PINV",1:3)),
  reflect("POL",multi_items("PPSS",1:3)),
  reflect("FAML","FAML1")
)

sec_intxn<-interact(interaction_ortho("REP","POL"))

sec_sm_intxn <- structure(
  paths(from = c("REP", "INV", "POL", "FAML", "REP.POL"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)

sec_pls_intxn <- estimate_model(data = sec,
                                measurement_model = sec_mm,
                                structural_model = sec_sm_intxn)




#### a.	Check item reliability of all factors: ----

# 1.	Reflective factors: ?? > 0.70 
 # "Do items individually share variance with their proper constructs?"
sec_pls_intxn$outer_loadings    #POL INV SEC FAML

#2.	Formative factors: VIF of items < 5 
#     "Do items individually contribute substantially meaningful variance to their constructs?

sec_pls_intxn$outer_weights[multi_items("PREP", 1:4), "REP"]

prep1_regr <- lm(sec$PREP1 ~ sec$PREP2 + sec$PREP3 + sec$PREP4)
prep1_r2 <- summary(prep1_regr)$r.squared
prep1_vif <- 1 / (1 - prep1_r2)

prep2_regr <- lm(sec$PREP2 ~ sec$PREP1 + sec$PREP3 + sec$PREP4)
prep2_r2 <- summary(prep2_regr)$r.squared
prep2_vif <- 1 / (1 - prep2_r2)

prep3_regr <- lm(sec$PREP3 ~ sec$PREP1 + sec$PREP2 + sec$PREP4)
prep3_r2 <- summary(prep3_regr)$r.squared
prep3_vif <- 1 / (1 - prep3_r2)

prep4_regr <- lm(sec$PREP4 ~ sec$PREP1 + sec$PREP2 + sec$PREP3)
prep4_r2 <- summary(prep4_regr)$r.squared
prep4_vif <- 1 / (1 - prep4_r2)

prep1_vif
prep2_vif
prep3_vif
prep4_vif



#### b.	Convergent validity (reflective factors only): ----

#  1.	Composite Reliability (CR) of factors: CR > 0.70
#   "How much do the items of a reflect factor agree with one another?"

POL_items <- multi_items("PPSS",1:3)
POL_loadings <- sec_pls_intxn$outer_loadings[POL_items, "POL"]
POL_CR <- sum(POL_loadings)^2 / (sum(POL_loadings)^2 + sum(1-POL_loadings)^2)

INV_items <- multi_items("PINV",1:3)
INV_loadings <- sec_pls_intxn$outer_loadings[INV_items, "INV"]
INV_CR <- sum(INV_loadings)^2 / (sum(INV_loadings)^2 + sum(1-INV_loadings)^2)

FAML_items <- multi_items("FAML",1)
FAML_loadings <- sec_pls_intxn$outer_loadings[FAML_items, "FAML"]
FAML_CR <- sum(FAML_loadings)^2 / (sum(FAML_loadings)^2 + sum(1-FAML_loadings)^2)

SEC_items <- multi_items("PSEC",1:4)
SEC_loadings <- sec_pls_intxn$outer_loadings[SEC_items, "SEC"]
SEC_CR <- sum(SEC_loadings)^2 / (sum(SEC_loadings)^2 + sum(1-SEC_loadings)^2)

POL_CR
INV_CR
FAML_CR
SEC_CR


#    2.	Average Variance Extracted (AVE) of factors: AVE > 0.50
#    "How much variance, on average, does a reflective factor explain of its own items?"

POL_AVE <- sum(POL_loadings^2) / (sum(POL_loadings^2) + sum(1-POL_loadings^2) )
INV_AVE <- sum(INV_loadings^2) / (sum(INV_loadings^2) + sum(1-INV_loadings^2) )
FAML_AVE <- sum(FAML_loadings^2) / (sum(FAML_loadings^2) + sum(1-FAML_loadings^2) )
SEC_AVE <- sum(SEC_loadings^2) / (sum(SEC_loadings^2) + sum(1-SEC_loadings^2) )

POL_AVE
INV_AVE
FAML_AVE
SEC_AVE

#### c.	Discriminant Validity (reflective factors only):  -----

#1.	Loadings of all items on own factors greater than cross-loadings with other factors
#"Are items more correlated with their own factors than other factors?"

sec_pls_intxn$outer_loadings[POL_items, "POL"]
cor( sec[,POL_items], sec_pls_intxn$fscores)

sec_pls_intxn$outer_loadings[INV_items, "INV"]
cor( sec[,INV_items], sec_pls_intxn$fscores)

sec_pls_intxn$outer_loadings[FAML_items, "FAML"]
cor( sec[,FAML_items], sec_pls_intxn$fscores)

sec_pls_intxn$outer_loadings[SEC_items, "SEC"]
cor( sec[,SEC_items], sec_pls_intxn$fscores)



#2.	Correlation of factor with other factors smaller than factor's square root of AVE
#"Is a factor more related to its own items than it is to other factors?"

sqrt(POL_AVE)
sqrt(INV_AVE)
sqrt(FAML_AVE)
sqrt(SEC_AVE)
cor(sec_pls_intxn$fscores)

##### Question 2)  Does SEC really mediate relationships between REP, POL, INV ??? TRUST?  -----

# a.	With each of the three factors (REP, POL, INV), check the four parts of the mediation analysis we discussed in class

#      1.	Try using three models to test for mediation:
#            I.	   the proposed model




sec_sm_intxn <- structure(
  paths(from = c("REP", "INV", "POL", "FAML"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)

sec_pls_intxn <- estimate_model(data = sec,
                                measurement_model = sec_mm,
                                structural_model = sec_sm_intxn)
print_paths(sec_pls_intxn)

#            II.	 the proposed model without the mediator



sec_sm_intxn_no_med <- structure(
  paths(from = c("REP", "INV", "POL", "FAML" ), to = "TRUST"))
 

sec_pls_intxn_no_med <- estimate_model(data = sec,
                                measurement_model = sec_mm,
                                structural_model = sec_sm_intxn_no_med)
print_paths(sec_pls_intxn_no_med)



#            III.	 the proposed model with paths from antecedents to outcomes


sec_sm_intxn_b <- structure(
  paths(from = "SEC", to = "TRUST"))


sec_pls_intxn_b <- estimate_model(data = sec,
                                       measurement_model = sec_mm,
                                       structural_model = sec_sm_intxn_b)
print_paths(sec_pls_intxn_b)






### 2.	When testing each of the three factors (REP, POL, INV), remove the other ----
#       two factors, but keep FAML as a control

sec_sm_REP <- structure(
  paths(from = c("REP", "FAML"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)

sec_pls_REP <- estimate_model(data = sec,
                                measurement_model = sec_mm,
                                structural_model = sec_sm_REP)

sec_sm_INV <- structure(
  paths(from = c("INV", "FAML"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)

sec_pls_INV <- estimate_model(data = sec,
                              measurement_model = sec_mm,
                              structural_model = sec_sm_INV)

sec_sm_POL <- structure(
  paths(from = c("POL", "FAML"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)

sec_pls_POL <- estimate_model(data = sec,
                              measurement_model = sec_mm,
                              structural_model = sec_sm_POL)

print_paths(sec_pls_REP)
print_paths(sec_pls_INV)
print_paths(sec_pls_POL)

#### b.	Which factors are fully mediated by SEC, which are partially mediated by SEC, and which are ----
#       not at all mediated by SEC?

boot_pls <- bootstrap_model(data = sec,  
                           measurement_model = sec_mm,  
                           structural_model = sec_sm_intxn,  
                           nboot = 1000)  
 
boot_pls_no_med <- bootstrap_model(data = sec,  
                            measurement_model = sec_mm,  
                            structural_model = sec_sm_intxn_no_med,  
                            nboot = 1000)  

boot_pls_REP <- bootstrap_model(data = sec,  
                              measurement_model = sec_mm,  
                              structural_model = sec_sm_REP,  
                              nboot = 1000) 

boot_pls_INV <- bootstrap_model(data = sec,  
                                measurement_model = sec_mm,  
                                structural_model = sec_sm_INV,  
                                nboot = 1000) 

boot_pls_POL <- bootstrap_model(data = sec,  
                                measurement_model = sec_mm,  
                                structural_model = sec_sm_POL,  
                                nboot = 1000) 



print_paths(boot_pls)  
print_paths(boot_pls_no_med)
print_paths(boot_pls_REP)
print_paths(boot_pls_INV)
print_paths(boot_pls_POL)
