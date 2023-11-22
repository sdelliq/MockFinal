
make.title("Reports")
systime.0 <- Sys.time()

###-----------------------------------------------------------------------###
#-----          - Intro                                           -----         
###-----------------------------------------------------------------------###

list.files(here(.profile$path.data))

read_ <- function(x, y) feather::read_feather(here(.profile$path.data, paste("Test", x, y, sep = "_") %>% paste0(".feather")))

df.loan <- read_("Report", "Loans")

df.collection <- read_("Report", "Collections")

df0$loan


df0 = list(
  main = read_("Report", "Borrowers"),
  loan = read_("Report", "Loans"),
  
  counterparty = read_("Report", "Counterparties"),
  entity = read_("Report", "Entities"),
  
  procedure = read_("Report", "Procedures"),
  
  collection = read_("Report", "Collections"),
  
  guarantee = read_("Report", "Guarantees")
)


link0 <- list(
  counterparty.entity = read_("Link", "Counterparty_Entity"),

  proc.loan = read_("Link", "Procedure_Loan"),
  proc.collateral = read_("Link", "Procedure_Collateral"),
  proc.entity = read_("Link", "Procedure_Entity"),
  
  guarantee.cadastral = read_("Link", "Guarantee_Cadastral"),
  guarantee.loan = read_("Link", "Guarantee_Loan"),
  guarantee.entity = read_("Link", "Guarantee_Entity")
)


###... Check
# df0$main %>% inspect
# df0$loan %>% inspect
# df0$counterparties %>% View


df0$entity %>%

  filter(grepl("^pens|^empl", solvency.pf)) %>%

  mutate(range = cut(income.pf/1000,
                         c(0, 500, 800, 1200, 1500, 2000, Inf)/1000,
                         include.lowest = T)) %>%

  # freq(range, income.pf, type = "median", scale = "base")
  
  mutate(n = 1) %>% 
  group_by(range) %>% 
  summarize(n = sum(n),
            income = median(income.pf),
            .groups = "drop") %>% 
  mutate(p.n = n/sum(n)) %>%
  # mutate(p.n = ((n/sum(n))) %>% printf.perc(dec = 2)) %>% 
  export
  


###-----------------------------------------------------------------------###
#-----          - Tables                                           -----         
###-----------------------------------------------------------------------###

df0$loan %>% arrange(desc(gbv.residual)) %>% inspect

df0$loan %>% 
  
  mutate(range.gbv = cut(gbv.residual/10^3, 
                         c(0, 15, 30, 50, 100, 250, 500, Inf),
                         include.lowest = T)) %>% 
  
  # freq(range.gbv, gbv.residual, scale = "m")
  # freq(range.gbv, gbv.residual, type = "mean", n.dec = 0)

  
  arrange(desc(gbv.residual)) %>% 
  # select(id.loan, id.bor, range.gbv, gbv.original, gbv.residual) %>% 
  # inspect(n.dec = 0)

  group_by(type, range.gbv) %>% 
  # summarize(gbv = sum(gbv.residual))
  summarize_at(vars(gbv.original, gbv.residual), 
               function(x) sum(x)/10^6) %>% 
  ungroup %>% 
  mutate(p.res = gbv.residual/sum(gbv.residual)) %>% 
  
  # View
  export


###-----------------------------------------------------------------------###
#-----          - Intro                                           -----         
###-----------------------------------------------------------------------###
