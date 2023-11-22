systime.0 <- Sys.time()

###-----------------------------------------------------------------------###
#-----          - Intro                                           -----         
###-----------------------------------------------------------------------###


df0 = list(
  main = read_("Report", "Borrowers"),
  loan = read_("Report", "Loans"),
  
  counterparty = read_("Report", "Counterparties"),
  entity = read_("Report", "Entities"),
  
  collection = read_("Report", "Collections"),
  
  guarantee = read_("Report", "Guarantees")
)


link0 <- list(
  counterparty.entity = read_("Link", "Counterparty_Entity"),
  guarantee.loan = read_("Link", "Guarantee_Loan"),
  guarantee.entity = read_("Link", "Guarantee_Entity")
)

###-----------------------------------------------------------------------###
#-----          - Tables                                           -----         
###-----------------------------------------------------------------------###

gbv.by.range.income <- df0$entity %>%

  filter(grepl("^pens|^empl", solvency.pf)) %>%

  mutate(range = cut(income.pf,
                         c(0, 500, 800, 1200, 1500, 2000, Inf),
                         include.lowest = T)) %>% 
  mutate(n = 1) %>% 
  group_by(range) %>% 
  summarise(n = sum(n),
            income = mean(income.pf)) %>% 
  ungroup() %>% 
  mutate(p.n = n/sum(n)) 
  
export(freq(df0$entity, type.subject, income.pf, type="mean"))

###-----------------------------------------------------------------------###


loan.by.type.and.gbv <- df0$loan %>% 
  
  mutate(range.gbv = cut(gbv.residual/10^3, 
                         c(0, 15, 30, 50, 100, 250, 500, Inf),
                         include.lowest = T)) %>% 
  
  # freq(range.gbv, gbv.residual, scale = "m") 
  # freq(range.gbv, gbv.residual, type = "mean", n.dec = 0)

  arrange(desc(gbv.residual)) %>% 
  group_by(type, range.gbv) %>% 
  summarize_at(vars(gbv.original, gbv.residual), 
               function(x) sum(x)/10^6) %>% 
  ungroup %>% 
  mutate(p.res = gbv.residual/sum(gbv.residual)) #%>% 
  
 
  


###-----------------------------------------------------------------------###
#-----          Export                                          -----         
###-----------------------------------------------------------------------###
  export(list.df= list((gbv.by.range.income), (loan.by.type.and.gbv)))