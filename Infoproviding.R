###########-------- Infoprov PF ---#######
infoprov.tocheck <- infoprov$infoprov.pf %>% select(cf.piva, name, city, province, region, solvency.pf=solvency.adj, income.pf=income.net) %>% distinct()
entities.tocheck <- df0$entity %>% filter(type.subject=="individual" & !is.na(cf.piva) & cf.piva %in% infoprov.tocheck$cf.piva) %>% select(cf.piva, name, city, province, region, solvency.pf, income.pf)

entities.tocheck <- entities.tocheck %>% mutate(across(where(is.factor), as.character))
infoprov.tocheck <- infoprov.tocheck %>% mutate(across(where(is.factor), as.character))
#We deal with the duplicated cf.piva keeping only individuals (assuming the corporate name is longer) and with the solvency.pf with more priority
dup_info_prov <- find_duplicates(infoprov.tocheck, cf.piva)
infoprov.tocheck <- infoprov.tocheck %>% filter(!(cf.piva %in% dup_info_prov$cf.piva))
dup_info_prov <- dup_info_prov %>% group_by(cf.piva) %>% arrange(cf.piva, income.pf) %>%
  mutate(
    name= ifelse(nchar(first(name)) <= nchar(last(name)), first(name), last(name))) %>% 
  summarise(
    across(c(name, city, province, region, income.pf), first),
    solvency.pf= ifelse("insolvent" %in% solvency.pf & "self employed" %in% solvency.pf, "insolvent", solvency.pf)
  )
infoprov.tocheck <- bind_rows(dup_info_prov, infoprov.tocheck)


#Keep the info from infoprov
updated_entities<-left_join(entities.tocheck,infoprov.tocheck,by="cf.piva")
updated_entities<-updated_entities%>%
  mutate(
    city= ifelse(!is.na(city.y), city.y, city.x),
    province= ifelse(!is.na(province.y), province.y, province.x),
    region= ifelse(!is.na(region.y), region.y, region.x),
    solvency.pf= ifelse(!is.na(solvency.pf.y), solvency.pf.y, solvency.pf.x),
    income.pf= ifelse(!is.na(income.pf.y), income.pf.y, income.pf.x),
    dummy.info = ifelse(cf.piva %in% infoprov.tocheck$cf.piva, 1, 0)
  )%>%
  select(name=name.y,cf.piva,solvency.pf,income.pf,city,province,region,dummy.info)


###########-------- Infoprov PG ---#######
infoprov.tocheck <- infoprov$infoprov.pg %>% select(cf.piva, name, city, province, region, date.cessation, status.pg=status, type.pg=type) %>% distinct()
entities.tocheck <- df0$entity %>% filter(type.subject!="individual" & !is.na(cf.piva) & cf.piva %in% infoprov.tocheck$cf.piva) %>% select(cf.piva, name, city, province, region, date.cessation, status.pg, type.pg)

#We deal with the duplicated cf.piva keeping only individuals (assuming the corporate name is longer) and with the solvency.pf with more priority
entities.tocheck <- entities.tocheck %>% mutate(across(where(is.factor), as.character))
infoprov.tocheck <- infoprov.tocheck %>% mutate(across(where(is.factor), as.character))

#infoprov.tocheck %>% group_by(cf.piva) %>% summarise_all(n_distinct) %>% View()

dup_info_prov <- find_duplicates(infoprov.tocheck, cf.piva)
infoprov.tocheck <- infoprov.tocheck %>% filter(!(cf.piva %in% dup_info_prov$cf.piva))
dup_info_prov <- dup_info_prov %>% group_by(cf.piva, status.pg) %>%
  mutate(
    name= ifelse(nchar(first(name)) <= nchar(last(name)), first(name), last(name))) %>% 
  reframe(
    across(c(name, city, province, region), first),
    date.cessation = na.omit(date.cessation)[1]
  )  %>% group_by(cf.piva) %>%
  arrange(factor(status.pg,levels=c("canceled","ceased","inactive","bankruptcy","liquidation", "insolvency", "other", "active"))) %>%
  slice(1) 
  
infoprov.tocheck <- bind_rows(dup_info_prov, infoprov.tocheck)


#Keep the info from infoprov
updated_entities_pg<-left_join(entities.tocheck,infoprov.tocheck,by="cf.piva") %>%
  mutate(
    city= ifelse(!is.na(city.y), city.y, city.x),
    province= ifelse(!is.na(province.y), province.y, province.x),
    region= ifelse(!is.na(region.y), region.y, region.x),
    status.pg= ifelse(!is.na(status.pg.y), status.pg.y, status.pg.x),
    type.pg= ifelse(!is.na(type.pg.y), type.pg.y, type.pg.x),
    date.cessation = ifelse(!is.na(date.cessation.y), date.cessation.y, date.cessation.x),
    dummy.info = ifelse(cf.piva %in% infoprov.tocheck$cf.piva, 1, 0)
  )%>%
  select(name=name.y,cf.piva, date.cessation, type.pg, status.pg,city,province,region,dummy.info) %>%
  mutate(date.cessation= as.Date(date.cessation)) %>% update_type.pg_column() %>% distinct()

updated_entities = bind_rows(updated_entities_pg, updated_entities)
updated_entities = updated_entities %>% bind_rows(df0$entity %>% filter(!(cf.piva %in% updated_entities$cf.piva)))

#Check - the filter doesn't work
#updated_entities %>% group_by(cf.piva) %>% summarise_all(n_distinct) %>% filter(across(-cf.piva, ~any(. > 1))) %>% View()
