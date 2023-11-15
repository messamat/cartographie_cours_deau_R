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
                     "hd_recat", "type_aux_name", "regime_name",
                     "nat_id_name", "orig_mo_name", "date_id_name","data_orig", 
                     "local_url", "com")
  
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

# in_net_bvinters <- tar_read(bcae_bvinters)
# in_net_bvinters <- tar_read(bdtopo_bvinters)
# in_net_bvinters <- tar_read(rht_bvinters)
# in_net_bvinters <- tar_read(carthage_bvinters)
#in_net_bvinters <- tar_read(ddtnets_bvinters)

in_ddtnets_bvinters <- tar_read(ddtnets_bvinters)

format_ddtnets_bvinters <- function(in_ddtnets_bvinters) {
  # names(in_net_bvinters)
  # summary(in_net_bvinters)
  # in_net_bvinters[geom_Length < 0.01, .N]
  # unique(stringr::str_to_lower(in_net_bvinters$regime))
  # unique(stringr::str_to_lower(in_net_bvinters$regime2))
  
  
  #Format regime
  perrenial_termlist <- c(
    "permanent", "perm. natura", "plein", "pemanent", "continu", "p")      
  intermittent_termlist <- c(
    "intermittent", "intermitent", "pointill?","temporaire", "ecou_tres_te", 
    "ecoul_tempor", "intermitant", "pointille", "intermitten","intermittant", 
    "intermittent non suffisant", "pointillã¯â¿â½?ã¯â¿â", "pointillã¯â¿â½?ã¯â¿â",
    "pointillï¿½?ï¿½?ï¿½?", "pointillãƒâ¯ã‚â¿ã‚â½?ãƒâ¯ã‚â¿ã‚",
    "continuitãƒâ¯ã‚â¿ã‚â½?ãƒâ¯ã‚â¿", "pointillã¯â¿â½", "i")
  
  in_ddtnets_bvinters[, `:=`(regime = stringr::str_to_lower(regime),
                         regime2 = stringr::str_to_lower(regime2)
  )]
  
  in_ddtnets_bvinters[
    , regime_formatted := fcase(
      regime %in% perrenial_termlist, 'perennial', 
      regime %in% intermittent_termlist, 'intermittent',
      default='undetermined')]
  in_ddtnets_bvinters[!is.na(regime2) &  (regime2 %in% perrenial_termlist),
                  regime_formatted := 'perennial']
  in_ddtnets_bvinters[!is.na(regime2) &  (regime2 %in% intermittent_termlist),
                  regime_formatted := 'intermittent']
  
  #Summarize length of lines by attributes
  ddtnet_bv_stats <- in_ddtnets_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(geom_Length),
           bvs_area = sum(POLY_AREA)
    ),
    by=c('UID_BV', 'type_stand', 'regime_formatted', 
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv]  #Proportion of the BV's total line length that this row represents
  
  
  ddtnet_dep_stats <-  ddtnet_bv_stats[
    , list(length_cat_dep = sum(length_cat_bv))
    , by=c('INSEE_DEP', 'NOM', 'type_stand', 'regime_formatted')] %>%
    .[, total_length_dep := sum(length_cat_dep), by=c('INSEE_DEP', 'NOM')] %>%
    .[, length_per_cat_dep := length_cat_dep/total_length_dep] %>% #Proportion of the BV's total line length that this row represents
    .[, per_ce := .SD[type_stand == "Cours d'eau", 
                      sum(length_per_cat_dep)], by=INSEE_DEP]  %>% 
    .[, per_nocat := .SD[type_stand %in% c("Indéterminé", "Hors département"), 
                         sum(length_per_cat_dep)], by=INSEE_DEP]  %>% 
    .[, per_int := .SD[regime_formatted == "intermittent", 
                       sum(length_per_cat_dep)], by=INSEE_DEP]
  
  return(ddtnet_dep_stats)
}


plot_ddtnet_dep <- function(in_ddtnet_dep_stats) {
  plot_ddtnet_dep_cetype <- ggplot(in_ddtnet_dep_stats, 
                                   aes(x=reorder(NOM, per_ce), 
                                       y=length_per_cat_dep, 
                                       fill=type_stand)) +
    geom_bar(stat='identity', 
             position = position_stack(reverse = TRUE)) +
    scale_x_discrete('Department') +
    scale_y_continuous('Percentage of total length of segments in Department',
                       expand=c(0,0), labels = scales::percent) +
    scale_fill_discrete(
      stringr::str_wrap('Standardized category of watercourse according to DDT', 20)
    ) +
    coord_flip() + 
  theme_bw()

  plot_ddtnet_dep_regime <- ggplot(in_ddtnet_dep_stats, 
                                   aes(x=reorder(NOM, per_int), 
                                       y=length_per_cat_dep, 
                                       fill=regime_formatted)) +
    geom_bar(stat='identity', 
             position = position_stack(reverse = TRUE)) +
    scale_x_discrete('Department') +
    scale_y_continuous('Percentage of total length of segments in Department',
                       expand=c(0,0), labels = scales::percent) +
    scale_fill_discrete(
      stringr::str_wrap('Standardized category of regime', 20)
  ) +
  coord_flip() + 
  theme_bw()

  plot_ddtnet_dep_regime_vs_cetype <- ggplot(
    in_ddtnet_dep_stats[per_ce < 1,],
    aes(x=per_int, y=per_ce)) +
  geom_point() +
  geom_quantile()
  
  return(list(
    plot_ddtnet_dep_cetype,
    plot_ddtnet_dep_regime,
    plot_ddtnet_dep_regime_vs_cetype
  ))
}


#in_carthage_bvinters <- tar_read(carthage_bvinters)
format_carthage <- function(in_carthage_bvinters) {
  names(in_carthage_bvinters)
  head(in_carthage_bvinters)
  #BD Carthage
  # ETAT: Intermittent, Permanent, Fictif (traverse un corps d'eau ou soutterrain)
  #       Inconnu, A sec, En attente de mise a jour
  #       -> keep fictif, because it includes a lot of mainstem rivers
  # NATURE: Cours d'eau naturel; Canal, chenal; Sans objet; En attente de mise à jour. 
  #       -> some Canal, chenal are where there used to be a real watercourse... keep them for now
  
  carthage_bvinters_sub <- in_carthage_bvinters[
    !(NATURE %in% c('Estuaire', 'Aqueduc, conduite forcée')) &
      !(POS_SOL %in% c('Au sol', 'Sur pont')) & 
      !(NATURE=='Canal, chenal' &
          SS_MILIEU %in% c('G', 'H', 'J', 'K', 'L', 'M', 'N')),
    ]
  
  
  #Summarize length of lines by attributes
  carthage_bv_stats <- carthage_bvinters_sub[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length),
           bvs_area = sum(POLY_AREA)
    ),
    by=c('UID_BV', 'NATURE', 'ETAT', 
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv]  #Proportion of the BV's total line length that this row represents
  
  return(carthage_bv_stats)
}

#in_bcae_bvinters <- tar_read(bcae_bvinters)
format_bcae <- function(in_bcae_bvinters) {
  names(in_bcae_bvinters)
  head(in_bcae_bvinters)

  #Summarize length of lines by attributes
  bcae_bv_stats <- in_bcae_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length),
           bvs_area = sum(POLY_AREA)
    ),
    by=c('UID_BV', 'ORIGINE1', 'ORIGINE2',
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv]  #Proportion of the BV's total line length that this row represents
  
  return(bcae_bv_stats)
}


#------------- TO BE CONTINUED ###################################################
#in_bcae_bvinters <- tar_read(bcae_bvinters)
format_rht <- function(in_rht_bvinters) {
  names(in_rht_bvinters)
  head(in_rht_bvinters)
  
  #Summarize length of lines by attributes
  rht_bv_stats <- in_rht_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length),
           bvs_area = sum(POLY_AREA)
    ),
    by=c('UID_BV', 'ORIGINE1', 'ORIGINE2',
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv]  #Proportion of the BV's total line length that this row represents
  
  return(rht_bv_stats)
}

# in_net_bvinters <- tar_read(bdtopo_bvinters)
format_bdtopo <- function(in_bdtopo_bvinters) {
  names(in_bdtopo_bvinters)
  head(in_bdtopo_bvinters)
  
  #Summarize length of lines by attributes
  bdtopo_bv_stats <- in_bdtopo_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length),
           bvs_area = sum(POLY_AREA)
    ),
    by=c('UID_BV', 'ORIGINE1', 'ORIGINE2',
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv]  #Proportion of the BV's total line length that this row represents
  
  return(bdtopo_bv_stats)
}
  