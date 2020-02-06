setwd("C:/Users/USER/Documents/BASM")
library(seminr)
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
                                interactions = sec_intxn,
                                structural_model = sec_sm_intxn)
print_paths(sec_pls_intxn)

boot_pls <- bootstrap_model(data = sec,
                            measurement_model = sec_mm,
                            interaction = sec_intxn,
                            structural_model = sec_sm_intxn,
                            nboot = 1000)


sec_pls_intxn$outer_loadings
sec_pls_intxn$outer_weights
sec_pls_intxn$fscores

print_paths(boot_pls)
