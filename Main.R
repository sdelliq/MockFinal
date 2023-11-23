source("Library.R")
source("Functions.R")
source("Functions_Andrea.R")

###-----------------------------------------------------------------------###
#-----          - Reading the files                                         -----         
###-----------------------------------------------------------------------###


df0 = list(
  main = read_("Report", "Borrowers"),
  loan = read_("Report", "Loans"),
  
  counterparty = read_("Report", "Counterparties"),
  entity = read_("Report", "Entities"),
  
  collection = read_("Report", "Collections"),
  
  guarantee = read_("Report", "Guarantees")
)

infoprov=list(
  infoprov.pf=read_("Infoprov", "PF"),
  infoprov.pg=read_("Infoprov", "PG")  
)

link0 <- list(
  counterparty.entity = read_("Link", "Counterparty_Entity"),
  guarantee.loan = read_("Link", "Guarantee_Loan"),
  guarantee.entity = read_("Link", "Guarantee_Entity")
)


###-----------------------------------------------------------------------###
#-----          - Check pre-analysis                                        -----         
###-----------------------------------------------------------------------###

#check that the info in infoprov.pf is in entities 
infoprov.tocheck <- infoprov$infoprov.pf %>% select(cf.piva, name, city, province, region, solvency.pf=solvency.adj, income.pf=income.net)
entities.tocheck <- df0$entity %>% filter(type.subject=="individual" & !is.na(cf.piva) & cf.piva %in% infoprov.tocheck$cf.piva) %>% select(cf.piva, name, city, province, region, solvency.pf, income.pf)
result_df <- anti_join(infoprov.tocheck, entities.tocheck) # If it's empty the info in infoproviding was added to Entities 


#check that the info in infoprov.pg is in entities 
infoprov.tocheck <- infoprov$infoprov.pg %>% select(cf.piva, name, city, province, region, date.cessation, status.pg=status, type.pg=type)
entities.tocheck <- df0$entity %>% filter(type.subject!="individual" & !is.na(cf.piva) & cf.piva %in% infoprov.tocheck$cf.piva) %>% select(cf.piva, name, city, province, region, date.cessation, type.pg, status.pg)
result_df <- anti_join(infoprov.tocheck, entities.tocheck) # If it's empty the info in infoproviding was added to Entities 