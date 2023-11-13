#FUNCTIONS

#in_metadata_nets <- tar_read(metadata_nets)
format_metadata_nets <- function(in_metadata_nets) {
  
  #Rename columns
  cols_rename <- list(
    c("Numéro", 'dep_code'),
    c("Département", 'dep_name'),
    c("Nombre total d'écoulements référencés", "nlines_reported"),
    c("Nom d'attributs", "attri_names"),
    c("Nom de l'attribut désignant le type d'écoulement", "type_stand_name"),
    c("Catégories de l'attribut désignant le type d'écoulement", "type_stand_cats"),
    c("CE_recatégorisé", "ce_recat"),
    c("NCE_recatégorisé", "nce_recat"),
    c("Indéterminé_ou_autre_recatégorisé", "ind_recat"),
    c("Inexistant_recatégorisé", "inx_recat"),
    c("Hors_département_recatégorisé", "hd_recat"),
    c("Nom de l'attribut auxiliaire désignant le type d'écoulement", "type_aux_name"),
    c("Nom de l'attribut désignant le régime hydrologique", "regime_name"),
    c("Nom de l'attribut désignant la méthode d'identification de l'écoulement", "nat_id_name"),
    c("Nom de l'attribut désignant la source de la modification; de la suppression du tronçon BD TOPO®; ou de l’ajout d’un nouveau tronçon", "orig_mo_name"),
    c("Nom de l'attribut désignant la date de l'identification du type d'écoulement", "date_id_name"),
    c("Origine des données", "data_orig"),
    c("Commentaire", "com"),
    c("Date de revision affichée (XML)", "date_rev"),
    c("Date de publication affichée (XML)", "date_pub"),
    c("Date d'obtention", "date_get"),
    c("Inclusion des non cours d'eau", "nce_included"),
    c("Lien local données", "local_url")
  ) %>%
    as.data.frame %>%
    t %>%
    as.data.table %>%
    setnames(c('old_col', 'new_col'))
  
  #Remove superfluous columns
  mdat_dt <- as.data.table(in_metadata_nets) %>%
    .[, -c('Titre de la couche selon XML', 'Titre du fichier','Statut (XML)', 
           'Abstract (XML)', 'Mise a jour (XML)', 'Format', 'Encodage',
           'Echelle (XML)', 'Lien URL', 'Lineage (XML)', 'Projection',
           "Explication des catégories de l'attribut désignant le statut du cours d'eau si besoin")] %>%
    setnames(cols_rename$old_col, cols_rename$new_col)
  
  #Compile instances with multiple layers (3 departments)
  mdat_dt_comb1 <- mdat_dt[
    ,.(nlines_reported = sum(nlines_reported),
       nce_included = data.table::last(sort(nce_included)), #Keep yes if there is a Yes (O) and No (N)
       date_rev= data.table::first(date_rev),
       date_pub= data.table::first(date_pub),
       date_get= data.table::first(date_get)
    ),
    by=c('dep_code', 'dep_name')]
  
  collapse_cols <- c("attri_names", "type_stand_name", "type_stand_cats",
                     "ce_recat","nce_recat", "ind_recat", "inx_recat", 
                     "hd_recat", "nce_included", "type_aux_name", "regime_name",
                     "nat_id_name", "orig_mo_name", "date_id_name", "date_rev",
                     "date_pub", "data_orig", "date_get", "local_url", "com")
  
  mdat_dt_comb <- 
    lapply(collapse_cols, function(col_name) {
      mdat_dt[
        , paste(get(col_name), collapse=';')
        , by='dep_code'][, 'V1', with=F] %>%
        setnames(col_name)
    }) %>% do.call(cbind, .) %>%
    cbind(mdat_dt_comb1, .)
  
  mdat_dt_uform <-  mdat_dt_comb[
    dep_code %in% mdat_dt[duplicated(mdat_dt$dep_code), dep_code],
    (collapse_cols) := sapply(.SD, function(string) {
      strsplit(string, ';')[[1]] %>%
        unique %>%
        .[!(. %in% "Pas de catégorie correspondante")] %>%
        paste(collapse=";")
    },
    simplify=FALSE),
    .SDcols = collapse_cols,
    by='dep_code']
  
  
  return( mdat_dt_uform)
}

# tar_load(bcae_bvinters)
# tar_load(bdtopo_bvinters)
# tar_load(rht_bvinters)
# tar_load(carthage_bvinters)
# tar_load(ddtnets_bvinters)