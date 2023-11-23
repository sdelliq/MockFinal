infoprov.tocheck <- infoprov$infoprov.pf %>% select(cf.piva, name, city, province, region, solvency.pf=solvency.adj, income.pf=income.net)
entities.tocheck <- df0$entity %>% filter(type.subject=="individual" & !is.na(cf.piva) & cf.piva %in% infoprov.tocheck$cf.piva) %>% select(cf.piva, name, city, province, region, solvency.pf, income.pf)


#Try tomorrow
# Function to update columns in Entities based on conditions
update_entities_columns <- function(entities_df, infoprov_df) {
  common_columns <- intersect(names(entities_df), names(infoprov_df))
  
  for (col in common_columns) {
    if (!all(is.na(infoprov_df[[col]]))) {
      entities_df[[col]] <- ifelse(!is.na(infoprov_df[[col]]), infoprov_df[[col]], entities_df[[col]])
    }
  }
  
  return(entities_df)
}

# Update common columns in Entities from infoprov.tocheck
updated_entities <- update_entities_columns(Entities, infoprov.tocheck)

# View the updated_entities dataframe
head(updated_entities)
