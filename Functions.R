read_ <- function(x, y) feather::read_feather(paste0("Data/", paste("Test", x, y, sep = "_") %>% paste0(".feather")))


find_duplicates <- function(df, column){
  return(df %>% filter(duplicated({{column}})|duplicated({{column}}, fromLast=T)))
}
#find_duplicates(infoprov.tocheck, cf.piva) %>% View()



update_type.pg_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "societa' a responsabilita' limitata|srl\\b|s.r.l\\b|s.r.l.\\b|srls\\b")  ~ "srl",
      str_detect(name, "societa' per azioni|spa\\b|s.p.a\\b|s.p.a.\\b")  ~ "spa",
      str_detect(name, "d.i\\b|d.i.\\b")  ~ "di",
      str_detect(name, "ss\\b|s.s\\b|s.s.\\b|societa' semplice\\b")  ~ "ss",
      str_detect(name, "societa' in accomandita semplice|sas \\b|s.a.s\\b|s.a.s.\\b")  ~ "sas",
      str_detect(name, "societa' in nome collettivo|snc\\b|s.n.c\\b|s.n.c.\\b|sncs\\b")  ~ "snc",
      str_detect(name, "cooperativa|consortile|fidi|sc\\b |s.c\\b|s.c.\\b|scs\\b")  ~ "sc",
      str_detect(name, "ass.zione|associazione|non precisata") ~ "other",
      TRUE ~ type.pg
    ))
}