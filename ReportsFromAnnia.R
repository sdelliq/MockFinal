# # Tables:
###-----------------------------------------------------------------------###
#-----   Creation of Tables by Borrower and By Loan                     -----         
###-----------------------------------------------------------------------###
# -number of borrowers overall
r.numberOfBorrowersOverall <- LOANS_FROM_METADATA %>% summarise("# Borrowers" = n_distinct(id.bor))


# -sum gbv overall
r.sumGBVoverall <- LOANS_FROM_METADATA %>% summarise("GBV Sum" = sum(gbv.original))
r.sumGBVoverall <- as.data.frame(r.sumGBVoverall)

r.summary_table <- data.frame(
  n.loans = nrow(LOANS_FROM_METADATA),
  n.borrowers = n_distinct(LOANS_FROM_METADATA$id.bor),
  gbv.sum =  sum(LOANS_FROM_METADATA$gbv.original)
)
names(r.summary_table) <- c(
  "# loans", "# borrowers", "gbv sum"
)
# -number and ratio of borrowers, number of loans, by area
borrowerANDarea <- COUNTERPARTIES %>% select (id.counterparty, role) %>% filter(role == "borrower") %>%
  left_join(link.counterparties.entities, by= "id.counterparty") %>% 
  left_join(ENTITIES %>% select(id.entity, area), by= "id.entity") %>% 
  left_join(link.loans.counterparties, relationship = "many-to-many", by="id.counterparty")
uniqueIDbor.combination <- borrowerANDarea %>% select(id.counterparty, area) %>% n_distinct()
r.borrowersBy.Area <- borrowerANDarea %>% 
  group_by(area) %>%
  summarise(
    '# Borrowers' = n_distinct(id.counterparty),
    '% Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.combination,
    '# Loans' = n_distinct(id.loans),
  )
summary_row <- r.borrowersBy.Area %>%
  summarize(
    area = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`), sum)
  ) 
r.borrowersBy.Area <- rbind(summary_row, r.borrowersBy.Area) 


###-----------------------------------------------------------------------###
#-----             Creation of Tables only by Borrower                  -----         
###-----------------------------------------------------------------------###

#Creation of a Mart Table with Borrowers information
borrowerMartTable <- COUNTERPARTIES %>% select (id.counterparty, role) %>%
  left_join(link.loans.counterparties, by= "id.counterparty") %>% 
  group_by(id.loans) %>%
  mutate(flag_Guarantor = ifelse(n() > 1, TRUE, FALSE)) %>%
  filter(role == "borrower") %>% 
  left_join(LOANS_FROM_METADATA %>% select (id.loans, gbv.original), by= "id.loans")

borrowerMartTable <- borrowerMartTable %>%  group_by(id.counterparty) %>% summarise(
  'id.loans' = n(),
  'gbv.original' = sum(gbv.original),
  flag_Guarantor = ifelse(any(flag_Guarantor), TRUE, FALSE) )


# -number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors 
r.borrowersByLoans.W.and.WO.guarantors <- borrowerMartTable %>% 
  mutate(
    has_guarantor = if_else(flag_Guarantor, "With Guarantor", "Without Guarantor")
  ) %>%
  group_by(has_guarantor) %>%
  summarise(
    '# Borrowers' = n(),
    '% Borrowers' = n() / nrow(borrowerMartTable),
    '# Loans' = sum(id.loans),
    'GBV Sum' = sum(gbv.original),
    'GBV Mean' = sum(gbv.original)/n()
  )
summary_row <- r.borrowersByLoans.W.and.WO.guarantors %>%
  summarize(
    has_guarantor = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersByLoans.W.and.WO.guarantors <- rbind(summary_row, r.borrowersByLoans.W.and.WO.guarantors) %>% rename (" " = has_guarantor)


# -number and ratio of borrowers, number of loans, sum and mean gbv by type of loan
uniqueIDbor.combination <- LOANS_FROM_METADATA %>% select(id.bor, type) %>% n_distinct()
r.borrowersBy.TypeOfLoans <- LOANS_FROM_METADATA %>%
  select(id.loans, id.bor, type, gbv.original) %>%
  group_by(id.bor) %>%
  slice(which.max(as.numeric(gbv.original))) %>%
  ungroup() %>%
  group_by(type) %>%
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor) / uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )
r.TypeOfLoans.ForGraph <- r.borrowersBy.TypeOfLoans
summary_row <- r.borrowersBy.TypeOfLoans %>%
  summarize(
    type = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersBy.TypeOfLoans <- rbind(summary_row, r.borrowersBy.TypeOfLoans) %>% rename ("Loan Type" = type)


# -number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
quantiles <- quantile(borrowerMartTable$gbv.original, c(1/3, 2/3))
print(quantiles)
breaks <- c(0, 60000, 360000, Inf)
range.gbv <- c("0 - 60k", "60k - 360k", "360k - 360k+")
borrowerWithGBVbucket <- borrowerMartTable %>%
  select(id.loans, id.counterparty, gbv.original) 
borrowerWithGBVbucket$gbv_range <- cut(borrowerWithGBVbucket$gbv.original, breaks = breaks, labels = range.gbv)

uniqueIDbor.combination <- borrowerWithGBVbucket %>% select(id.counterparty,gbv_range) %>% n_distinct()
r.borrowersBy.GBVclusters <-  borrowerWithGBVbucket %>%    
  group_by(gbv_range)  %>%
  summarise(
    '# Borrowers' = n_distinct(id.counterparty),
    '% Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.combination,
    '# Loans' = sum(id.loans),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )
summary_row <- r.borrowersBy.GBVclusters %>%
  summarize(
    gbv_range = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersBy.GBVclusters <- rbind(summary_row, r.borrowersBy.GBVclusters) %>% rename ("GBV" = gbv_range)


# -number and ratio of borrowers, number of loans, sum and mean gbv by status of loan and gbv clusters
r.borrowersBy.StatusOfLoans <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, status, gbv.original) 
r.borrowersBy.StatusOfLoans$gbv_range <- cut(r.borrowersBy.StatusOfLoans$gbv.original, breaks = breaks, labels = range.gbv) 
r.borrowersBy.StatusOfLoans <- r.borrowersBy.StatusOfLoans %>%
  group_by(id.bor) %>%
  mutate(id.loans_count = n(), gbv.original = sum (gbv.original)) %>%
  slice(which.max(as.numeric(gbv_range)))
uniqueIDbor.combination <- r.borrowersBy.StatusOfLoans %>% select(id.bor, status) %>% n_distinct()
complete_ref <- expand.grid(status = unique(r.borrowersBy.StatusOfLoans$status), gbv_range = unique(r.borrowersBy.StatusOfLoans$gbv_range))
r.borrowersBy.StatusOfLoans <- r.borrowersBy.StatusOfLoans %>%
  group_by(status, gbv_range) %>% 
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor)/uniqueIDbor.combination,
    '# Loans' = sum(id.loans_count),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = sum(as.numeric(gbv.original)/n_distinct(id.bor))
  )  %>%
  right_join(complete_ref, by = c("status", "gbv_range")) %>%
  replace(is.na(.), 0)
summary_row <- r.borrowersBy.StatusOfLoans %>%
  group_by(status) %>%
  summarize(
    gbv_range = n(),
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
summary_row$status <- ifelse(summary_row$status == "utp", "Subtotal UTP", "Subtotal Bad")
r.borrowersBy.StatusOfLoans <- rbind(summary_row, r.borrowersBy.StatusOfLoans) 
summary_row <- summary_row %>% summarize(
  status= "Totals",
  across(c(gbv_range, `# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
  `GBV Mean` = mean(`GBV Mean`)
) 
r.borrowersBy.StatusOfLoans <- rbind(summary_row, r.borrowersBy.StatusOfLoans) %>% rename ("Loan Status" = status)
r.borrowersBy.StatusOfLoans <- r.borrowersBy.StatusOfLoans %>%
  mutate(Sort_Order = case_when(
    `Loan Status` == "Totals" ~ 1,
    `Loan Status` == "bad" ~ 2,
    `Loan Status` == "Subtotal Bad" ~ 3,
    `Loan Status` == "utp" ~ 4,
    `Loan Status` == "Subtotal UTP" ~ 5
  )) %>%
  arrange(Sort_Order) %>%
  select(-Sort_Order)



# -a pivot(cross table or contingency table (https://en.wikipedia.org/wiki/Contingency_table)) of sum gvb by gbv clusters that you create and loans with/without guarantors
pivot_table <- borrowerMartTable %>%
  mutate(bucket = cut(gbv.original, breaks = breaks, labels = range.gbv, include.lowest = TRUE))
pivot_table <- pivot_table %>%
  group_by(flag_Guarantor, bucket) %>%
  summarise(gbv_sum = sum(gbv.original)) %>%
  pivot_wider(
    names_from = bucket,
    values_from = gbv_sum,
    values_fill = 0
  ) 
pivot_table$flag_Guarantor <- ifelse(pivot_table$flag_Guarantor == TRUE, "With Guarantor", "Without Guarantor")

summary_row <- pivot_table %>%
  summarize(
    flag_Guarantor = "Totals",
    across(range.gbv, sum)
  ) %>%
  summarize(
    flag_Guarantor = "Totals",
    across(range.gbv, sum)
  ) 
pivot_table <- rbind(summary_row, pivot_table) 


###-----------------------------------------------------------------------###
#-----             Creation of Tables only by Loan                      -----         
###-----------------------------------------------------------------------###

#Creation of a Mart Table with Borrowers information
loansMartTable <- COUNTERPARTIES %>% select (id.counterparty, role) %>%
  left_join(link.loans.counterparties, by= "id.counterparty") %>% 
  group_by(id.loans) %>%
  mutate(flag_Guarantor = ifelse(n() > 1, TRUE, FALSE)) %>%
  filter(role == "borrower") %>% 
  left_join(LOANS_FROM_METADATA %>% select (id.loans, gbv.original, type, status), by= "id.loans")


# -number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors 
r.byLoans.W.WO.guarantors <- loansMartTable %>% 
  mutate(
    has_guarantor = if_else(flag_Guarantor, "With Guarantor", "Without Guarantor")
  ) %>%
  group_by(has_guarantor) %>%
  summarise(
    '# Borrowers' = n_distinct(id.counterparty),
    '# Loans' = n(),
    'GBV Sum' = sum(gbv.original),
    'GBV Mean' = sum(gbv.original)/n()
  )%>%
  mutate(
    '% Borrowers' = `# Borrowers` /  sum(`# Borrowers`)
  ) 
r.byLoans.W.WO.guarantors_forGraph <- r.byLoans.W.WO.guarantors
summary_row <- r.byLoans.W.WO.guarantors %>%
  summarize(
    has_guarantor = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.byLoans.W.WO.guarantors <- rbind(summary_row, r.byLoans.W.WO.guarantors) %>% rename (" " = has_guarantor)


# -number and ratio of borrowers, number of loans, sum and mean gbv by type of loan
uniqueIDbor.combination <- LOANS_FROM_METADATA %>% select(id.bor, type) %>% n_distinct()
r.byLoans.Type <- LOANS_FROM_METADATA %>%
  select(id.loans, id.bor, type, gbv.original) %>%
  group_by(id.bor) %>%
  ungroup() %>%
  group_by(type) %>%
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor) / uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )
r.TypeOfLoans.ForGraph <- r.byLoans.Type
summary_row <- r.byLoans.Type %>%
  summarize(
    type = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.byLoans.Type <- rbind(summary_row, r.byLoans.Type) %>% rename ("Loan Type" = type)


# -number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
quantiles <- quantile(loansMartTable$gbv.original, c(1/3, 2/3))
print(quantiles)
breaks <- c(0, 1000, 120000, Inf)
range.gbv <- c("0 - 1k", "1k - 120k", "120k - 120k+")
borrowerWithGBVbucket <- loansMartTable %>%
  select(id.loans, id.counterparty, gbv.original) 
borrowerWithGBVbucket$gbv_range <- cut(borrowerWithGBVbucket$gbv.original, breaks = breaks, labels = range.gbv)

uniqueIDbor.combination <- borrowerWithGBVbucket %>% select(id.counterparty,gbv_range) %>% n_distinct()
r.byLoan.GBVclusters <-  borrowerWithGBVbucket %>%    
  group_by(gbv_range)  %>%
  summarise(
    '# Borrowers' = n_distinct(id.counterparty),
    '% Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )
summary_row <- r.byLoan.GBVclusters %>%
  summarize(
    gbv_range = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.byLoan.GBVclusters <- rbind(summary_row, r.byLoan.GBVclusters) %>% rename ("GBV" = gbv_range)


# -number and ratio of borrowers, number of loans, sum and mean gbv by status of loan and gbv clusters
r.byLoan.Status <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, status, gbv.original) 
uniqueIDbor.combination <- r.byLoan.Status %>% select(id.bor, status) %>% n_distinct()
r.byLoan.Status$gbv_range <- cut(r.byLoan.Status$gbv.original, breaks = breaks, labels = range.gbv)
r.byLoan.Status <- r.byLoan.Status %>%
  group_by(status, gbv_range) %>%
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor)/uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )
r.byLoan.Status_forGraph <- r.byLoan.Status
summary_row <- r.byLoan.Status %>%
  group_by(status) %>%
  summarize(
    gbv_range = n(),
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
summary_row$status <- ifelse(summary_row$status == "utp", "Subtotal UTP", "Subtotal Bad")
r.byLoan.Status <- rbind(summary_row, r.byLoan.Status) 
summary_row <- summary_row %>% summarize(
  status= "Totals",
  across(c(gbv_range, `# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
  `GBV Mean` = mean(`GBV Mean`)
) 
r.byLoan.Status <- rbind(summary_row, r.byLoan.Status) %>% rename ("Loan Status" = status)
r.byLoan.Status <- r.byLoan.Status %>%
  mutate(Sort_Order = case_when(
    `Loan Status` == "Totals" ~ 1,
    `Loan Status` == "bad" ~ 2,
    `Loan Status` == "Subtotal Bad" ~ 3,
    `Loan Status` == "utp" ~ 4,
    `Loan Status` == "Subtotal UTP" ~ 5
  )) %>%
  arrange(Sort_Order) %>%
  select(-Sort_Order)



# -a pivot(cross table or contingency table (https://en.wikipedia.org/wiki/Contingency_table)) of sum gvb by gbv clusters that you create and loans with/without guarantors
r.byLoans.pivot_table <- loansMartTable %>% select(id.counterparty, id.loans, gbv.original, flag_Guarantor) %>%
  mutate(bucket = cut(gbv.original, breaks = breaks, labels = range.gbv, include.lowest = TRUE))
r.byLoans.pivot_table <- r.byLoans.pivot_table %>%
  group_by(flag_Guarantor, bucket) %>%
  summarise(gbv_sum = sum(gbv.original)) %>%
  pivot_wider(
    names_from = bucket,
    values_from = gbv_sum,
    values_fill = 0
  ) 
r.byLoans.pivot_table$flag_Guarantor <- ifelse(r.byLoans.pivot_table$flag_Guarantor == TRUE, "With Guarantor", "Without Guarantor")

summary_row <- r.byLoans.pivot_table %>%
  summarize(
    flag_Guarantor = "Totals",
    across(range.gbv, sum)
  ) %>%
  summarize(
    flag_Guarantor = "Totals",
    across(range.gbv, sum)
  ) 
r.byLoans.pivot_table <- rbind(summary_row, r.byLoans.pivot_table) 


############ Analysis of guarantees ############
LOANS <- LOANS %>% mutate(gbv.original = as.numeric(gbv.original))
r.guarantees <- GUARANTEES  %>% left_join(link_Guarantee_Loan , by="id.guarantee") %>% left_join(LOANS, by=c("id.loan"="id.loans")) %>% filter(!is.na(id.loan))
r.sum.guarantees <- data.frame(
  n.guarantees = n_distinct(r.guarantees$id.guarantee),
  n.loans = n_distinct(r.guarantees$id.loan),
  n.borrowers = n_distinct(r.guarantees$id.bor.x)
)
r.guarantees.loan.level <- r.guarantees %>% group_by(id.loan) %>% summarise(
  gbv.original = first(gbv.original),
  id.bor = first(id.bor.x),
  type= ifelse("lien" %in% type.x,  "lien", ifelse("confidi" %in% type.x, "confidi", "surety")),
  origin.lien = first(origin.lien)
)
r.loans.without.guarantee <- data.frame(id.loans = setdiff(LOANS$id.loans, r.guarantees$id.loan)) %>% left_join(LOANS %>% select(id.loans,gbv.original), by="id.loans")

#GBV by guarantee presence 
r.with_guarantee <- data.frame(
  guarantee = "with guarantee",
  n.loans = n_distinct(r.guarantees$id.loan),
  gbv.sum = sum(r.guarantees.loan.level$gbv.original),
  perc.gbv = sum(r.guarantees.loan.level$gbv.original)/r.sumGBVoverall$`GBV Sum`
)
r.without_guarantee <- data.frame(
  guarantee = "without guarantee",
  n.loans = nrow(r.loans.without.guarantee),
  gbv.sum = sum(r.loans.without.guarantee$gbv.original),
  perc.gbv = sum(r.loans.without.guarantee$gbv.original)/r.sumGBVoverall$`GBV Sum`
)
r.gbv.by.guarantee.presence <- rbind(r.with_guarantee, r.without_guarantee)
r.gbv.by.guarantee.presence.graph <- r.gbv.by.guarantee.presence
summary_row <- r.gbv.by.guarantee.presence %>% summarise(
  guarantee = "totals",
  n.loans = sum(n.loans),
  gbv.sum = sum(gbv.sum),
  perc.gbv = sum(perc.gbv)
)
r.gbv.by.guarantee.presence <- bind_rows(summary_row, r.gbv.by.guarantee.presence)
names(r.gbv.by.guarantee.presence) <- c(
  "guarantee", "# loans", "gbv sum", "% gbv"
)

#GBV (%) by Type of Guarantee
r.type.of.guarantee.by.loan <- r.guarantees.loan.level %>% group_by(type) %>% summarise(
  n.loans = n_distinct(id.loan),
  sum.gbv = sum(gbv.original),
  perc.gbv = round(sum(gbv.original)/r.with_guarantee$gbv.sum,3)
)
r.type.of.guarantee.by.loan.graph <- r.type.of.guarantee.by.loan
summary_row <- r.type.of.guarantee.by.loan %>% summarise(
  type = "totals",
  n.loans = sum(n.loans),
  sum.gbv = sum (sum.gbv),
  perc.gbv = sum(perc.gbv)
)
r.type.of.guarantee.by.loan <- bind_rows(summary_row, r.type.of.guarantee.by.loan)
names(r.type.of.guarantee.by.loan) <- c(
  "type", "# loans", "gbv sum", "% gbv"
)

#GBV (%) by Origin Lien
r.lien.of.guarantee.by.loan <- r.guarantees.loan.level %>% group_by(origin.lien) %>% summarise(
  n.loans = n_distinct(id.loan),
  sum.gbv = sum(gbv.original),
  perc.gbv = round(sum(gbv.original)/r.with_guarantee$gbv.sum,3)
)
summary_row <- r.lien.of.guarantee.by.loan %>% summarise(
  origin.lien = "totals",
  n.loans = sum(n.loans),
  sum.gbv = sum (sum.gbv),
  perc.gbv = sum(perc.gbv)
)
r.lien.of.guarantee.by.loan <- bind_rows(summary_row, r.lien.of.guarantee.by.loan)
names(r.lien.of.guarantee.by.loan) <- c(
  "origin lien", "# loans", "gbv sum", "% gbv"
)
