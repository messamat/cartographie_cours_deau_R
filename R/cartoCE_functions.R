############################ UTILITY FUNCTIONS ##################################
#-------------------------- mergDTlist -----------------------------------------
mergeDTlist <- function(dt_list, by = NULL, all = TRUE, sort = FALSE,
                        set_suffix=TRUE) {
  
  if (set_suffix) {
    dt_list <-  Map(function(dt_name, dt) {
      dt_copy <- copy(dt)
      cols_to_rename <- names(dt)[!(names(dt) %in% by)]
      setnames(dt_copy,
               old=cols_to_rename,
               new=paste(cols_to_rename, dt_name, sep='_'))
      return(dt_copy)
    },
    names(dt_list), dt_list
    )
  }
  
  Reduce(
    function(...) {
      merge(..., by = by, all = all, sort = sort)
    }, dt_list)
}

#-------------------------- prettydend --------------------------------------
#Make a nice looking dendogram
prettydend <- function(hclus_out, colorder=NULL, colors=NULL, labels=NULL,
                       kclass=7, classnames = NULL) {
  classr <- dendextend::cutree(hclus_out, k=kclass, 
                               order_clusters_as_data = FALSE)
  classr_df <- data.frame(ID=names(classr), gclass=classr) 
  
  if (!is.null(classnames)) {
    classr_df <- merge(classr_df, classnames, by='gclass')
    grouplabels <- classnames$classnames
  } else {
    grouplabels <- TRUE
  }
  
  hclus_out_name <- hclus_out
  if (!is.null(labels)) {
    hclus_out_name$labels <- labels
  }
  
  #,paste(RGS_No,"-",RGS_Loc," R. at ", RGS_Name,sep=""))
  
  dendname <- as.dendrogram(hclus_out_name)
  
  if (is.null(colorder)) colorder = 1:kclass
  
  par(cex=0.7, mar=c(2.5, 1, 0, 9)) #bottom left top right
  dendname %>% set("branches_lwd", 3) %>% 
    color_branches(k=kclass, col=colors[colorder]) %>% 
    #color_branches(clusters=as.numeric(temp_col), col=levels(temp_col), groupLabels=as.character(as.numeric(temp_col))) %>% 
    color_labels(k=kclass, col=colors[colorder]) %>%
    plot(horiz=TRUE,xlab="Gower's distance", ylab="",mgp=c(1.5,0.5,0))
  title(ylab="Department", line=0, cex=1)
  
  return(list(classr_df, dendname))
}

############################ ANALYSIS FUNCTIONS ##################################
#-------------------------- format_metadata_nets -----------------------------------------
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
    c("Nom de l'attribut désignant la source de la modification; de la suppression du tronçon BD TOPO; ou de l’ajout d’un nouveau tronçon", "orig_mo_name"),
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

#-------------------------- impute_refids_ddtnets -------------------------------
# in_ddtnets_path = tar_read(ddtnets_path)
# in_ddtnets_bdtopo_polyinters = tar_read(ddtnets_bdtopo_polyinters)
# in_ddtnets_carthage_polyinters = tar_read(ddtnets_carthage_polyinters)

impute_refids_ddtnets <- function(in_ddtnets_path,
                                  in_ddtnets_bdtopo_polyinters,
                                  in_ddtnets_carthage_polyinters) {
  ddtnet <- vect(
    x = dirname(in_ddtnets_path),
    layer = basename(in_ddtnets_path),
    what='attributes'
  )

  #--------------------------- Process match with BD topo ----------------------
  #Remove instances of multiple segments with the same pair of DDT and BD Topo segments
  in_ddtnets_bdtopo_polyinters <- in_ddtnets_bdtopo_polyinters[, list(
    length_inters_bdtopo=sum(length_inters_bdtopo)
  ), by=c('UID_CE', 'ID_bdtopo', 'length_bdtopo')]
  
  ddtnet_bdtopo <- merge(in_ddtnets_bdtopo_polyinters,
                         ddtnet,
                         by.x='UID_CE',
                         by.y='UID_CE',
                         all.x=T
                         ) %>%
    .[
      , `:=`(
      inters_to_bdtopo_ratio = length_inters_bdtopo/length_bdtopo,
      inters_to_ddt_ratio =length_inters_bdtopo/geom_Length,
      ddt_to_bdtopo_ratio = geom_Length/length_bdtopo
      )] %>%
    .[!is.infinite(inters_to_bdtopo_ratio) & !is.infinite(inters_to_ddt_ratio),] %>%#Take out records for which the intersection is a single point
    .[,  `:=`(
      max_dev = max(sapply(.SD, function(x) {abs(1-x)})),
      mean_dev = mean(sapply(.SD, function(x) {abs(1-x)}))
      ),
      .SDcols = c('inters_to_bdtopo_ratio', 
                  'inters_to_ddt_ratio', 
                  'ddt_to_bdtopo_ratio'),
      by=c('UID_CE', 'ID_bdtopo')] %>%
    .[, inters_diff := abs(inters_to_bdtopo_ratio - inters_to_ddt_ratio)]
   
  
  # sensitivity_analysis_bdtopo_ddtinters_2 <- lapply(
  #   seq(0.02, 0.4, 0.02), function(thresh) {
  #     ddtnet_bdtopo_sub <- ddtnet_bdtopo[
  #       (max_dev < thresh) | ((abs(1-ddt_to_bdtopo_ratio) < thresh*0.1) & inters_diff < thresh*0.1),]
  #     ddtnet_bdtopo_nodupliddt <- ddtnet_bdtopo_sub[
  #       order(max_dev),
  #       .SD[!duplicated(UID_CE),] #Keep the BD Topo record with the highest intersect with the DDT line
  #     ]
  #     
  #     #Precision: number of valid matches identified divided by the total number of identified matches
  #     precision <- ddtnet_bdtopo_nodupliddt[(ID_bdtopo==id) & !is.na(id), .N]/ddtnet_bdtopo_nodupliddt[!is.na(id),.N]
  #     #Sensitivity: Number of valid matches identified divided by the total number of known matches
  #     sensitivity <- ddtnet_bdtopo_nodupliddt[(ID_bdtopo==id) & !is.na(id), .N]/ddtnet_bdtopo[!duplicated(id), .N]
  #     
  #     return(data.table(
  #       thresh=thresh,
  #       precision=precision,
  #       sensitivity=sensitivity
  #     ))
  #   }) %>% rbindlist
  # 
  # ggplot(melt(sensitivity_analysis_bdtopo_ddtinters_2, id.vars = 'thresh'),
  #             aes(x=thresh, y=value, color=variable)) +
  #   geom_line()
  # 
  ddtnet_bdtopo_sub <- ddtnet_bdtopo[
    (max_dev < 0.4) | ((abs(1-ddt_to_bdtopo_ratio) < 0.1) & inters_diff < 0.1),] %>%
    .[order(max_dev), .SD[!duplicated(UID_CE)]]

  ddtnet_bdtopo_edit <- merge(ddtnet, 
                             ddtnet_bdtopo_sub[, .(UID_CE, ID_bdtopo)],
                             by='UID_CE', all.x=T) %>%
    as.data.table %>%
    .[grep('^TRON_EAU.*', id), ID_bdtopo_merge := id]
  
  ddtnet_bdtopo_edit[is.na(id) & !is.na(ID_bdtopo),.N] #Added ~700k records
  ddtnet_bdtopo_edit[is.na(ID_bdtopo_merge) & !is.na(ID_bdtopo),
                    ID_bdtopo_merge := ID_bdtopo] 
  ddtnet_bdtopo_edit[
    ,.SD[!is.na(ID_bdtopo_merge) | !is.na(code_hydro), sum(geom_Length)]/
      sum(geom_Length)]
  
  
  others <- ddtnet_bdtopo_edit[is.na(ID_bdtopo_merge) & is.na(code_hydro),]
  
  #--------------------------- Process match with BD Carthage ------------------
    #Remove instances of multiple segments with the same pair of DDT and BD Topo segments
  in_ddtnets_carthage_polyinters <- in_ddtnets_carthage_polyinters[, list(
    length_inters_carthage=sum(length_inters_carthage)
  ), by=c('UID_CE', 'ID_carthage', 'length_carthage')]
  
  ddtnet_carthage <- merge(in_ddtnets_carthage_polyinters,
                         ddtnet,
                         by.x='UID_CE',
                         by.y='UID_CE',
                         all.x=T
                         ) %>%
    .[
      , `:=`(
      inters_to_carthage_ratio = length_inters_carthage/length_carthage,
      inters_to_ddt_ratio =length_inters_carthage/geom_Length,
      ddt_to_carthage_ratio = geom_Length/length_carthage
      )] %>%
    .[!is.infinite(inters_to_carthage_ratio) & !is.infinite(inters_to_ddt_ratio),] %>%#Take out records for which the intersection is a single point
    .[,  `:=`(
      max_dev = max(sapply(.SD, function(x) {abs(1-x)})),
      mean_dev = mean(sapply(.SD, function(x) {abs(1-x)}))
      ),
      .SDcols = c('inters_to_carthage_ratio', 
                  'inters_to_ddt_ratio', 
                  'ddt_to_carthage_ratio'),
      by=c('UID_CE', 'ID_carthage')] %>%
    .[, inters_diff := abs(inters_to_carthage_ratio - inters_to_ddt_ratio)]
   
  
  ddtnet_carthage_sub <- ddtnet_carthage[
    (max_dev < 0.2) | ((abs(1-ddt_to_carthage_ratio) < 0.1) & inters_diff < 0.1),] %>%
    .[order(max_dev), .SD[!duplicated(UID_CE)]] %>%
    .[grep('^TRON_EAU.*', id, invert=T),]

  ddtnet_carthage_edit <- merge(ddtnet, 
                             ddtnet_carthage_sub[, .(UID_CE, ID_carthage)],
                             by='UID_CE', all.x=T) %>%
    as.data.table
  
  ddtnet_carthage_edit[is.na(id) & !is.na(ID_carthage),.N] #Added >50k records

  return(list(
    ddtnet_bdtopo_edit = ddtnet_bdtopo_edit[, .(UID_CE, ID_bdtopo_merge)],
    ddtnet_carthage_edit = ddtnet_carthage_edit[, .(UID_CE, ID_carthage)]
  ))
}
#-------------------------- format_carthage ------------------------------------
#in_carthage_bvinters <- tar_read(carthage_bvinters)
format_carthage <- function(in_carthage_bvinters) {
  #names(in_carthage_bvinters)
  #head(in_carthage_bvinters)
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
           length_cat_bv = sum(Shape_Length)
    ),
    by=c('UID_BV', 'NATURE', 'ETAT', 
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv] %>% #Proportion of the BV's total line length that this row represents
    setnames('NOM', 'NOM_DEP')
  
  return(carthage_bv_stats)
}

#-------------------------- format_bcae ----------------------------------------
#in_bcae_bvinters <- tar_read(bcae_bvinters)
format_bcae <- function(in_bcae_bvinters) {
  names(in_bcae_bvinters)
  head(in_bcae_bvinters)
  
  #Summarize length of lines by attributes
  bcae_bv_stats <- in_bcae_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length)
    ),
    by=c('UID_BV', 'ORIGINE1', 'ORIGINE2',
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv] %>% #Proportion of the BV's total line length that this row represents
    setnames('NOM', 'NOM_DEP')
  
  return(bcae_bv_stats)
}

#-------------------------- format_rht -----------------------------------------
#in_rht_bvinters <- tar_read(rht_bvinters)
format_rht <- function(in_rht_bvinters) {
  # names(in_rht_bvinters)
  # head(in_rht_bvinters)
  
  
  in_rht_bvinters[, .N, by=StatutOH]
  #Summarize length of lines by attributes
  rht_bv_stats <- in_rht_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length)
    ),
    by=c('UID_BV', 'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv] %>% #Proportion of the BV's total line length that this row represents
    setnames('NOM', 'NOM_DEP')
  return(rht_bv_stats)
}

#-------------------------- format_bdtopo -----------------------------------------
#in_bdtopo_bvinters <- tar_read(bdtopo_bvinters)
format_bdtopo <- function(in_bdtopo_bvinters) {
  names(in_bdtopo_bvinters)
  head(in_bdtopo_bvinters)
  unique(in_bdtopo_bvinters$POS_SOL)
  
  #Summarize length of lines by attributes
  bdtopo_bv_stats <- in_bdtopo_bvinters[
    , list(n_lines = .N,
           length_cat_bv = sum(Shape_Length)
    ),
    by=c('UID_BV', 'ARTIF', 'FICTIF', 'REGIME',
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM_1')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv] %>% #Proportion of the BV's total line length that this row represents
    setnames('NOM_1', 'NOM_DEP')
  
  return(bdtopo_bv_stats)
}

#-------------------------- format_ddtnets_bvinters -----------------------------
#in_ddtnets_bvinters <- tar_read(ddtnets_bvinters)
# in_bdtopo_bvinters <- tar_read(bdtopo_bvinters)
# in_carthage_bvinters <- tar_read(carthage_bvinters)
# in_ddtnets_refids_imputed = tar_read(ddtnets_refids_imputed)

format_ddtnets_bvinters <- function(in_ddtnets_bvinters,
                                    in_bdtopo_bvinters,
                                    in_carthage_bvinters,
                                    in_ddtnets_refids_imputed) {
  #  names(in_ddtnets_bvinters)
  # summary(in_ddtnets_bvinters)
  # in_ddtnets_bvinters[geom_Length < 0.01, .N]
  # unique(stringr::str_to_lower(in_ddtnets_bvinters$regime))
  # unique(stringr::str_to_lower(in_ddtnets_bvinterss$regime2))
  
  #----- Format regime, artif, nom from BDTOPO and Carthage --------------------
  #Format column names
  bdtopo_cols_to_transfer <- c('ID_bdtopo_orig', 'ARTIF_bdtopo_orig', 
                               'NOM_bdtopo_orig', 'REGIME_bdtopo_orig')
  names(in_bdtopo_bvinters)[
    names(in_bdtopo_bvinters) 
    %in% c('ID', 'ARTIF', 'NOM', 'REGIME')] =  bdtopo_cols_to_transfer
  #Setnames doesn't work because of R/RStudio glitch
  # setnames(in_bdtopo_bvinters, 
  #          old=c('ID', 'ARTIF', 'NOM'), 
  #          new=c('ID_bdtopo_orig', 'ARTIF_bdtopo_orig', 'NOM_bdtopo_orig')
  # )
  
  carthage_cols_to_transfer <- c('ID_BDCARTH', 'ETAT_carthage', 
                                   'NATURE_carthage', 'TOPONYME1_carthage')
  names(in_carthage_bvinters)[
    names(in_carthage_bvinters) 
    %in% c('ID_BDCARTH', 'ETAT', 'NATURE', 'TOPONYME1')] = carthage_cols_to_transfer
  
  ddtnets_bvinters_format <- merge(   #Merge with BD TOPO
    in_ddtnets_bvinters, in_ddtnets_refids_imputed$ddtnet_bdtopo_edit, 
    by='UID_CE', all.x=T) %>%
    merge(in_bdtopo_bvinters[!duplicated(ID_bdtopo_orig),  
                             bdtopo_cols_to_transfer, with=F],
          by.x='ID_bdtopo_merge', by.y='ID_bdtopo_orig', all.x=T) %>%
    merge( #Merge with Carthage
      in_ddtnets_refids_imputed$ddtnet_carthage_edit, 
      by='UID_CE', all.x=T) %>%
    merge(in_carthage_bvinters[!duplicated(ID_BDCARTH),  
                             carthage_cols_to_transfer, with=F],
          by.x='ID_carthage', by.y='ID_BDCARTH', all.x=T)

  perrenial_termlist <- c(
    "permanent", "perm. natura", "plein", "pemanent", "continu", "p")      
  intermittent_termlist <- c(
    "intermittent", "intermitent", "pointill?","temporaire", "ecou_tres_te", 
    "ecoul_tempor", "intermitant", "pointille", "intermitten","intermittant", 
    "intermittent non suffisant", "pointillã¯â¿â½?ã¯â¿â", "pointillã¯â¿â½?ã¯â¿â",
    "pointillï¿½?ï¿½?ï¿½?", "pointillãƒâ¯ã‚â¿ã‚â½?ãƒâ¯ã‚â¿ã‚",
    "continuitãƒâ¯ã‚â¿ã‚â½?ãƒâ¯ã‚â¿", "pointillã¯â¿â½", "i")
  
  ddtnets_bvinters_format[, `:=`(regime = stringr::str_to_lower(regime),
                             regime2 = stringr::str_to_lower(regime2)
  )]
  
  ddtnets_bvinters_format[, regime_formatted := 
                        fcase(ETAT_carthage=='Permanent', 'perennial',
                              ETAT_carthage=='Intermittent', 'intermittent',
                              default='undetermined')]
  
  ddtnets_bvinters_format[!is.na(REGIME_bdtopo_orig) &  
                        REGIME_bdtopo_orig == 'Permanent',
                      regime_formatted := 'perennial']
  ddtnets_bvinters_format[!is.na(REGIME_bdtopo_orig) &  
                        REGIME_bdtopo_orig=='Intermittent',
                      regime_formatted := 'intermittent']
  
  ddtnets_bvinters_format[!is.na(regime) &  regime %in% perrenial_termlist,
                      regime_formatted := 'perennial']
  ddtnets_bvinters_format[!is.na(regime) &  regime %in% intermittent_termlist,
                      regime_formatted := 'intermittent']
  ddtnets_bvinters_format[!is.na(regime2) &  (regime2 %in% perrenial_termlist),
                      regime_formatted := 'perennial']
  ddtnets_bvinters_format[!is.na(regime2) &  (regime2 %in% intermittent_termlist),
                      regime_formatted := 'intermittent']
  
  # ddtnets_bvinters_format[regime_formatted == 'undetermined', sum(geom_Length)]/
  #   ddtnets_bvinters_format[, sum(geom_Length)]
  
  #Remove lines from a departmental layer outside of that department
  ddtnets_bvinters_format[, orig_dep := as.integer(str_extract(orig_layer, "[0-9]{1,2}"))]
  
  ddtnets_bvinters_format <- ddtnets_bvinters_format[(orig_dep == INSEE_DEP) |
                                               (INSEE_DEP %in% c(92, 93 ,94)),]
  
  #For the section of the watercourse that is actually within the department
  #Re-assign "Hors département" to "Indéterminé"
  ddtnets_bvinters_format[type_stand == "Hors département", 
                          type_stand := 'Indéterminé'] 
  #Only 88 actually labeled as "Hors département"
  
  #Summarize length of lines by attributes
  bv_stats <- ddtnets_bvinters_format[
    , list(n_lines = .N,
           length_cat_bv = sum(geom_Length)
    ),
    by=c('UID_BV', 'type_stand', 'regime_formatted', 
         'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM')
  ] %>%
    .[, total_length_bv := sum(length_cat_bv), by=c('UID_BV')] %>% #Total line length in bv
    .[, length_per_cat_bv := length_cat_bv/total_length_bv] %>% #Proportion of the BV's total line length that this row represents
    .[, per_ce := .SD[type_stand == "Cours d'eau", 
                      sum(length_per_cat_bv)], by=UID_BV]  %>% 
    .[, per_ind:= .SD[type_stand == "Indéterminé", 
                      sum(length_per_cat_bv)], by=UID_BV]  %>% 
    .[, per_nce:= .SD[type_stand %in% c("Non cours d'eau", "Inexistant"), 
                      sum(length_per_cat_bv)], by=UID_BV]  %>% 
    .[, per_int := .SD[regime_formatted == "intermittent", 
                       sum(length_per_cat_bv)], by=UID_BV] %>%
    setnames('NOM', 'NOM_DEP')
  
  dep_stats <-  bv_stats[
    , list(length_cat_dep = sum(length_cat_bv))
    , by=c('INSEE_DEP', 'NOM_DEP', 'type_stand', 'regime_formatted')] %>%
    .[, total_length_dep := sum(length_cat_dep), by=c('INSEE_DEP', 'NOM_DEP')] %>%
    .[, length_per_cat_dep := length_cat_dep/total_length_dep] %>% #Proportion of the BV's total line length that this row represents
    .[, per_ce := .SD[type_stand == "Cours d'eau", 
                      sum(length_per_cat_dep)], by=INSEE_DEP]  %>% 
    .[, per_ind:= .SD[type_stand == "Indéterminé", 
                      sum(length_per_cat_dep)], by=INSEE_DEP]  %>% 
    .[, per_nce:= .SD[type_stand %in% c("Non cours d'eau", "Inexistant"), 
                      sum(length_per_cat_dep)], by=INSEE_DEP]  %>% 
    .[, per_int := .SD[regime_formatted == "intermittent", 
                       sum(length_per_cat_dep)], by=INSEE_DEP]
  
  return(list(
    ddtnets_bvinters_format = ddtnets_bvinters_format,
    bv_stats=bv_stats,
    dep_stats=dep_stats)
  )
}

#-------------------------- plot_ddtnet_dep -----------------------------------
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

#-------------------------- format fish data -----------------------------------
# in_fish_data_tablelist = tar_read(fish_data_tablelist)
# in_fish_stations_bvinters = tar_read(fish_stations_bvinters)
# format_fish_data <- function(in_fish_data_tablelist,
#                              in_fish_stations_bvinters
# ) {
#   operations_ipr <- fread(in_fish_data_tablelist[[1]])
#   operations <- fread(in_fish_data_tablelist[[2]])
#   pop <- fread(in_fish_data_tablelist[[3]])
#   
#   fish_data_merged <- merge(
#     in_fish_stations_bvinters, pop, by.x='sta_id', by.y='pop_sta_id',
#     all.x=T, all.y=F) #%>%
#     
#     
#   check <- merge(pop, operations, by.x='pop_id', by.y='ope_pop_id', all.x=T, all.y=F) %>%
#     merge(operations_ipr, by.x='ope_id', by.y='opi_ope_id', all.x=T, all.y=F)
#   
# }
#-------------------------- format amber ---------------------------------------
#in_amber_bvinters <- tar_read(amber_bvinters)
format_amber <- function(in_amber_bvinters,
                         id_cols) {
  #summary(in_amber_bvinters)
  amber_fr <- in_amber_bvinters[Country == 'FRANCE',] %>%
    setnames('NOM', 'NOM_DEP')
  
  barriers_bv <- amber_fr[,  .N, by=c('LabelAtlas', 'UID_BV')] %>%
    dcast(UID_BV~LabelAtlas, value.var='N', fill=0) %>%
    .[, TOTAL := rowSums(.SD), by=UID_BV]
  
  oldcols <- c(unique(amber_fr$LabelAtlas), 'TOTAL')
  newcols <- paste0('n_barriers_', oldcols)
  setnames(barriers_bv, oldcols, newcols)
  
  return(barriers_bv)
}
#-------------------------- format bdforet ---------------------------------------
#in_bdforet_bvinters <- tar_read(bdforet_bvinters)
format_bdforet <- function(in_bdforet_bvinters) {
  #names(bdforet_bvinters)
  
  #TFV_G11: cet attribut renseigne en 11 postes le type de couverture et la 
  #composition générale de la formation végétale en fonction des niveaux I,II et
  #III du code TFV. Il s’agit d’un regroupement de types de formation végétale.
  forest_bv <- in_bdforet_bvinters[, list(forest_cat_area = sum(Shape_Area)),  
                                   by=c('TFV_G11', 'UID_BV')] %>%
    .[, forest_total_area := sum(forest_cat_area, na.rm=T), by=UID_BV] %>%
    .[, forest_cat_percarea := forest_cat_area/forest_total_area] %>%
    .[order(-forest_cat_percarea), list(
      for_cl_s1 = .SD[1, TFV_G11],
      for_cl_s1pc = .SD[1, forest_cat_percarea],
      for_cl_s2 = .SD[2, TFV_G11],
      for_cl_s2pc = .SD[2, forest_cat_percarea],
      for_cl_s3 = .SD[3, TFV_G11],
      for_cl_s3pc = .SD[3, forest_cat_percarea]
    ), by=UID_BV] 
  
  forest_dep <- in_bdforet_bvinters[, list(forest_cat_area = sum(Shape_Area)),  
                                    by=c('TFV_G11', 'INSEE_DEP')] %>%
    .[, forest_total_area := sum(forest_cat_area, na.rm=T), by=INSEE_DEP] %>%
    .[, forest_cat_percarea := forest_cat_area/forest_total_area] %>%
    .[order(-forest_cat_percarea), list(
      for_cl_s1 = .SD[1, TFV_G11],
      for_cl_s1pc = .SD[1, forest_cat_percarea],
      for_cl_s2 = .SD[2, TFV_G11],
      for_cl_s2pc = .SD[2, forest_cat_percarea],
      for_cl_s3 = .SD[3, TFV_G11],
      for_cl_s3pc = .SD[3, forest_cat_percarea]
    ), by=INSEE_DEP] 
  
  # check <- forest_bv[, rowSums(.SD), .SDcols = c('for_cl_s1per')]
  # quantile(check, 0.1, na.rm=T)
  return(list(
    forest_bv=forest_bv,
    forest_dep=forest_dep
  ))
}

#-------------------------- format irrigation data -----------------------------
format_irrig <- function(in_comirrig_bvinters) {
  #names(in_comirrig_bvinters)
  
  #-------------- Assign irrigated area to BVs ---------------------------------
  #Compute the area of primary irrigated crops and the total agricultural area
  #in the BV-commune intersections
  # According to the agricultural census, the following crops had more than 10% 
  #of their surface area irrigated in 2020: Corn, Vegetables, Orchards, Potatoes, Beets, Soy,
  # https://agreste.agriculture.gouv.fr/agreste-web/download/publication/publie/GraFra2022Chap3.1/GraFra2022_pratiques-culturales.pdf
  in_comirrig_bvinters[, `:=`(
    primary_irrcrops_area_bvcom=
      Shape_Area*(VALUE_8 + VALUE_10 + VALUE_12 + VALUE_14)/(10^6*rowSums(.SD)),
    crops_area_bvcom = 
      Shape_Area*(VALUE_5 + VALUE_6 + VALUE_7 + VALUE_8 + VALUE_9 + VALUE_10 
                  + VALUE_11+ VALUE_12 + VALUE_13 + VALUE_14)/(10^6*rowSums(.SD))
  ),
  .SDcols = grep("^VALUE_[0-9]{1,2}$", names(in_comirrig_bvinters), perl=T, value=T)
  ]
  
  
  #Compute the total area of these land covers in each commune  
  in_comirrig_bvinters[, `:=`(
    primary_irrcrops_area_com = sum(primary_irrcrops_area_bvcom),
    crops_area_com = sum(crops_area_bvcom)
  ), by='INSEE_COM'
  ]
  
  #Compute the percent area equivalents by bvcom/com
  in_comirrig_bvinters[
    , 
    bvcom_percirrig := fcase(
      primary_irrcrops_area_com > 0, primary_irrcrops_area_bvcom/primary_irrcrops_area_com,
      crops_area_com > 0, crops_area_bvcom/crops_area_com,
      default = 0
    )
  ]
  
  #There are some communes for which the SAU exceeds the commune area
  #leading to irrig_area_com_all > COMMUNE_AREA
  in_comirrig_bvinters[(irrig_area_com_all/100)/COMMUNE_AREA>1,
                       irrig_area_com_all := 0.95*COMMUNE_AREA*100]
  
  
  # Compute the irrigated area in each BV as the sum across all communes that 
  #intersect this BV of the product between the total irrigated area in the 
  #commune and the percentage of potentially irrigated crops from that commune 
  #in that BV
  irrig_bv <- in_comirrig_bvinters[, list(
    ire_pc_sse=sum(irrig_area_com_all*bvcom_percirrig/100)
  ), by=UID_BV
  ]
  
  return(irrig_bv)
}

#-------------------------- format bd charm -----------------------------
#in_bdcharm_bvinters <- tar_read(bdcharm_bvinters)

format_bdcharm <- function(in_bdcharm_bvinters) {
  # unique(in_bdcharm_bvinters$DESCR)
  # check <- in_bdcharm_bvinters[, .N, by=DESCR]
  # 
  #Get the three most prevalent geological formations in BV and the % area that
  #they represent
  lit_cl_smj_bv <- in_bdcharm_bvinters[
    !(NOTATION %in% c('hydro', 'Hydro')), list(
      lit_areasum = sum(Shape_Area, na.rm=T))
    , by=c('NOTATION', 'UID_BV')] %>%
    .[, lit_areaper := lit_areasum/sum(lit_areasum, na.rm=T), by=c('UID_BV')] %>%
    .[order(-lit_areaper), list(
      lit_cl_s1 = .SD[1, NOTATION],
      lit_cl_s1pc = .SD[1, lit_areaper],
      lit_cl_s2 = .SD[2, NOTATION],
      lit_cl_s2pc = .SD[2, lit_areaper],
      lit_cl_s3 = .SD[3, NOTATION],
      lit_cl_s3pc = .SD[3, lit_areaper]
    ), by=UID_BV]
  
  lit_cl_smj_dep <- in_bdcharm_bvinters[
    !(NOTATION %in% c('hydro', 'Hydro')), list(
      lit_areasum = sum(Shape_Area, na.rm=T))
    , by=c('NOTATION', 'INSEE_DEP')] %>%
    .[, lit_areaper := lit_areasum/sum(lit_areasum, na.rm=T), by=c('INSEE_DEP')] %>%
    .[order(-lit_areaper), list(
      lit_cl_s1 = .SD[1, NOTATION],
      lit_cl_s1pc = .SD[1, lit_areaper],
      lit_cl_s2 = .SD[2, NOTATION],
      lit_cl_s2pc = .SD[2, lit_areaper],
      lit_cl_s3 = .SD[3, NOTATION],
      lit_cl_s3pc = .SD[3, lit_areaper]
    ), by=INSEE_DEP]
  
  #Check the total proportion of the BV's area made up by the three top lithology
  #>50% for >90% of BVs
  # lit_cl_smj[, lit_cl_3sum := sum(lit_cl_s1per, lit_cl_s2per, lit_cl_s3per, na.rm=T),
  #            by=UID_BV]
  
  return(list(
    lit_cl_smj_bv=lit_cl_smj_bv,
    lit_cl_smj_dep=lit_cl_smj_dep
  ))
}

#-------------------------- format snelder ires data -----------------------------
#in_snelder_bvinters <- tar_read(snelder_bvinters)
format_snelder <- function(in_snelder_bvinters) {
  rht_snelder_stats  <- in_snelder_bvinters[
    , 
    list(irs_pc_sav = sum(V1*Shape_Length)/sum(Shape_Length),
         sne_km_ssu = sum(Shape_Length),
         dis_m3_yr = max(MODULE)),
    by='UID_BV'
  ]
  return(rht_snelder_stats)
}


#-------------------------- format bnpe withdrawal data -----------------------------
format_bnpe <- function(in_bnpe_bvinters,
                        in_bnpe_timeseries,
                        in_bnpe_ouvrages) {
  #names(in_bnpe_bvinters)
  
  #The ones that drop out are off the coast or in estuaries  
  # in_bnpe_timeseries[!code_ouvrage %in% in_bnpe_bvinters$code_ouvrage,
  #                    unique(code_ouvrage)]
  
  bnpe_ts_bv <- merge(in_bnpe_timeseries,
                      in_bnpe_bvinters[, .(code_ouvrage, UID_BV)],
                      by='code_ouvrage',
                      all.x=T) %>%
    merge(in_bnpe_ouvrages[, .(code_ouvrage, libelle_type_milieu)],
          by='code_ouvrage',
          all.x=T) 
  
  bnpe_bv_stats <- bnpe_ts_bv[, list(
    mean_vol = mean(volume, na.rm=T)),
    by=c('code_ouvrage', 'libelle_type_milieu', 
         'libelle_usage', 'UID_BV')] %>%
    .[, list(vww_mk_syr = sum(mean_vol)),
      by=c('libelle_type_milieu', 'libelle_usage', 'UID_BV')] %>%
    .[, type := gsub(' ', '_', 
                     paste(libelle_type_milieu,
                           libelle_usage
                     ))] %>%
    dcast(UID_BV~type, value.var='vww_mk_syr', fill=0)
  
  
  oldcols <-names(bnpe_bv_stats[,-c('UID_BV'),with=F])
  setnames(bnpe_bv_stats,  oldcols, paste0('vww_mk_syr_', oldcols))
  
  return(bnpe_bv_stats)
}

#-------------------------- compile all environmental variables in one table ----
# tar_load(bvdep_inters_tab)
# tar_load(env_gdbtabs)
# tar_load(barriers_formatted)
# tar_load(lithology_formatted)
# tar_load(forest_formatted)
# tar_load(ires_formatted)
# tar_load(withdrawals_formatted)
# tar_load(irrig_formatted)
# 
# in_bvdep_inters <- tar_read(bvdep_inters_tab)
# in_envlist <- list(
#   env_gdbtabs=env_gdbtabs
#   , barriers_formatted=barriers_formatted
#   , lithology_formatted=lithology_formatted
#   , forest_formatted=forest_formatted
#   , ires_formatted=ires_formatted
#   , withdrawals_formatted=withdrawals_formatted
#   , irrig_formatted=irrig_formatted
# )

compile_all_env <- function(in_bvdep_inters,
                            in_envlist) {
  
  
  # Extract department statistics for categorical data
  lithology_formatted_dep <- in_envlist$lithology_formatted$lit_cl_smj_dep
  forest_formatted_dep <- in_envlist$forest_formatted$forest_dep
  in_envlist$lithology_formatted <- in_envlist$lithology_formatted$lit_cl_smj_bv
  in_envlist$forest_formatted <- in_envlist$forest_formatted$forest_bv
  
  #---------------- Format data at the BV level -----------------------------
  in_envlist$env_gdbtabs <- merge(
    in_envlist$env_gdbtabs,
    in_bvdep_inters[, .(UID_BV, POLY_AREA, INSEE_DEP)],
    by='UID_BV')
  
  #Format raster-based data
  in_envlist$env_gdbtabs <- in_envlist$env_gdbtabs[
    , list(
      bv_area_km2 = POLY_AREA
      , slo_dg_sav = MEAN_slo_dg_sav
      , ari_ix_ssu = 1/(MEAN_ari_ix_ssu/10000)
      , ari_ix_syr = 1/(MEAN_ari_ix_syr/10000)
      , ppc_pk_sav = 10^6*SUM_ppc_in_sav/(AREA_ppc_in_sav)
      , awc_mm_sav = MEAN_awc_mm_sav0_5 + MEAN_awc_mm_sav5_15 +
        MEAN_awc_mm_sav15_30 + MEAN_awc_mm_sav30_60 +
        MEAN_awc_mm_sav60_100 + MEAN_awc_mm_sav100_200
      , cly_pc_sav = ((MEAN_cly_pc_sav0_5*5) + (MEAN_cly_pc_sav5_15*10) +
                        (MEAN_cly_pc_sav15_30*15) + (MEAN_cly_pc_sav30_60*30) +
                        (MEAN_cly_pc_sav60_100*40) + (MEAN_cly_pc_sav100_200*100)
      )/200
      , snd_pc_sav = ((MEAN_snd_pc_sav0_5*5) + (MEAN_snd_pc_sav5_15*10) +
                        (MEAN_snd_pc_sav15_30*15) + (MEAN_snd_pc_sav30_60*30) +
                        (MEAN_snd_pc_sav60_100*40) + (MEAN_snd_pc_sav100_200*100)
      )/200
      , slt_pc_sav = ((MEAN_slt_pc_sav0_5*5) + (MEAN_slt_pc_sav5_15*10) +
                        (MEAN_slt_pc_sav15_30*15) + (MEAN_slt_pc_sav30_60*30) +
                        (MEAN_slt_pc_sav60_100*40) + (MEAN_slt_pc_sav100_200*100)
      )/200
      , pst_pc_sse = (VALUE_13_lc_pc_s19+VALUE_13_lc_pc_s20+VALUE_13_lc_pc_s21)/
        (3*(10^6)*POLY_AREA)
      , orc_pc_sse = (VALUE_14_lc_pc_s19+VALUE_14_lc_pc_s20+VALUE_14_lc_pc_s21)/
        (3*(10^6)*POLY_AREA)
      , vny_pc_sse = (VALUE_15_lc_pc_s19+VALUE_15_lc_pc_s20+VALUE_15_lc_pc_s21)/
        (3*(10^6)*POLY_AREA)
      , wet_pc_sse = (VALUE_23_lc_pc_s19+VALUE_23_lc_pc_s20+VALUE_23_lc_pc_s21)/
        (3*(10^6)*POLY_AREA)
      , gla_pc_sse = (VALUE_22_lc_pc_s19+VALUE_22_lc_pc_s20+VALUE_22_lc_pc_s21)/
        (3*(10^6)*POLY_AREA)
    ),
    by=.(UID_BV, INSEE_DEP)] %>%
    merge(
      in_envlist$env_gdbtabs[, list(UID_BV=UID_BV,
                                    veg_pc_sse = rowSums(.SD)/(3*(10^6)*POLY_AREA)),
                             .SDcols = paste0(rep('VALUE_', 12), 16:19,
                                              rep('_lc_pc_s',12), 19:21)],
      by='UID_BV'
    ) %>%
    merge(
      in_envlist$env_gdbtabs[, list(UID_BV=UID_BV,
                                    imp_pc_sse = rowSums(.SD)/(3*(10^6)*POLY_AREA)),
                             .SDcols = paste0(rep('VALUE_', 12), 1:4,
                                              rep('_lc_pc_s',12), 19:21)],
      by='UID_BV'
    ) %>%
    merge(
      in_envlist$env_gdbtabs[, list(UID_BV=UID_BV,
                                    agr_pc_sse = rowSums(.SD)/(3*(10^6)*POLY_AREA)),
                             .SDcols = paste0(rep('VALUE_', 12), 5:16,
                                              rep('_lc_pc_s',12), 19:21)],
      by='UID_BV'
    ) %>%
    merge(
      in_envlist$env_gdbtabs[, list(UID_BV=UID_BV,
                                    scr_pc_sse = rowSums(.SD)/(3*(10^6)*POLY_AREA)),
                             .SDcols = paste0(rep('VALUE_', 12), 8:13,
                                              rep('_lc_pc_s',12), 19:21)],
      by='UID_BV'
    ) %>%
    merge(
      in_envlist$env_gdbtabs[, list(UID_BV=UID_BV,
                                    wcr_pc_sse = rowSums(.SD)/(3*(10^6)*POLY_AREA)),
                             .SDcols = paste0(rep('VALUE_', 12), 5:7,
                                              rep('_lc_pc_s',12), 19:21)],
      by='UID_BV'
    )
  
  # Merge all tables together
  out_tab_bv <- Reduce(function(x, y) merge(x, y, by="UID_BV", all.x=T, all.y=T),
                       in_envlist)
  
  #Compute areal densities for withdrawals and irrigation
  cols_to_divide_by_area <- grep('(vww_mk_syr)|(ire_pc_sse)',
                                 names(out_tab_bv), value=T)
  out_tab_bv[, (cols_to_divide_by_area) := sapply(.SD,
                                                  function(x) {
                                                    fifelse(is.na(x), 0,
                                                            x/bv_area_km2)
                                                  }, simplify=F),
             .SDcols = cols_to_divide_by_area]
  
  
  #---------------- Format data at the department level ------------------------
  cols_to_wmean_by_area <- c(
    "slo_dg_sav", "ari_ix_ssu", "ari_ix_syr", "ppc_pk_sav",
    "awc_mm_sav","cly_pc_sav", "snd_pc_sav", "slt_pc_sav",
    grep("(^vww_mk_syr)|(_pc_sse$)", names(out_tab_bv), value=T)
  )
  
  out_tab_dep_1 <- out_tab_bv[,
                              list(sapply(.SD, function(x) weighted.mean(x, bv_area_km2, na.rm=T)),
                                   simplify=F),
                              .SDcols = cols_to_wmean_by_area,
                              by= 'INSEE_DEP'] %>%
    .[!is.na(INSEE_DEP),] %>%
    cbind(rep(cols_to_wmean_by_area, .[, length(unique(INSEE_DEP))])) %>%
    dcast(INSEE_DEP~V2, value.var='V1')
  
  cols_to_sum <-  grep('n_barriers', names(out_tab_bv), value=T)
  
  out_tab_dep_2 <- out_tab_bv[,
                              list(sapply(.SD, function(x) sum(x,na.rm=T)),
                                   simplify=F),
                              .SDcols = cols_to_sum,
                              by= 'INSEE_DEP'] %>%
    .[!is.na(INSEE_DEP),] %>%
    cbind(rep(cols_to_sum, .[, length(unique(INSEE_DEP))])) %>%
    dcast(INSEE_DEP~V2, value.var='V1')
  
  out_tab_dep <- cbind(out_tab_dep_1,
                       out_tab_dep_2[,-c('INSEE_DEP'), with=F]) %>%
    merge(out_tab_bv[, list(
      irs_pc_sav=weighted.mean(irs_pc_sav, sne_km_ssu, na.rm=T)), 
      by=INSEE_DEP], by='INSEE_DEP') %>%
    merge(lithology_formatted_dep, by='INSEE_DEP') %>%
    merge(forest_formatted_dep ,by='INSEE_DEP') 
  
  return(list(
    bv = out_tab_bv[!is.na(UID_BV),],
    dep = out_tab_dep
  ))
}

############################ Evaluate coverage of point-based monitoring networks #######
#-------------------------- evaluate_onde_coverage -----------------------------
#in_onde_ddtnets_spjoin <- tar_read(onde_ddtnets_spjoin)
#in_onde_stations_bvinters <- tar_read(onde_stations_bvinters)
#in_ddtnets_path <- tar_read(ddtnets_path)

evaluate_onde_coverage <- function(in_onde_ddtnets_spjoin,
                                   in_onde_stations_bvinters,
                                   in_ddtnets_path) {
  
  ddtnet <- vect(
    x = dirname(in_ddtnets_path),
    layer = basename(in_ddtnets_path),
    what = 'attributes'
  ) 
  
  onde_ddtnets_join <- merge(in_onde_stations_bvinters,
                             in_onde_ddtnets_spjoin,
                             by='CdSiteHydro',
                             all.x=F, all.y=T) %>%
    merge(ddtnet,
          by='UID_CE',
          all.x=T) %>%
    unique(by=c('CoordXSiteHydro', 'CoordYSiteHydro'))

  onde_deleted_segments <- onde_ddtnets_join[
    CdSiteHydro  %in% 
      c('M1060001', 'M1050001', 'F4620002', 'P1130001', 'P3110001', 'P1560002',
        'P3322511', 'P3800001', 'P3800002', 'P1560002')
  ]
  
  onde_nce_segments <- onde_ddtnets_join[
    CdSiteHydro  %in% 
      c('B5130001', 'Y4306511', 'V7300001', 'X3500011', 'X3500012', 'H0220001',
        'K4320001')
  ]
  
  return(list(
    attris_ddtnets = onde_ddtnets_join,
    deleted_segments = onde_deleted_segments,
    nce_segments = onde_nce_segments
  ))
}


#-------------------------- evaluate fish stations coverage -----------------------------
# in_fish_ddtnets_spjoin <- tar_read(fish_ddtnets_spjoin)
# in_fish_pop_bvinters <- tar_read(fish_pop_bvinters)
# in_fish_data_tablelist <- tar_read(fish_data_tablelist)
# in_ddtnets_path <- tar_read(ddtnets_path)
evaluate_fish_coverage <- function(in_fish_ddtnets_spjoin,
                                   in_fish_pop_bvinters,
                                   in_fish_data_tablelist,
                                   in_ddtnets_path) {
  
  operation <- fread(in_fish_data_tablelist[[1]], encoding='Latin-1')
  stations <- fread(in_fish_data_tablelist[[2]], encoding='Latin-1')
  operation_ipr <- fread(in_fish_data_tablelist[[3]], encoding='Latin-1')
  
  ddtnet <- vect(
    x = dirname(in_ddtnets_path),
    layer = basename(in_ddtnets_path),
    what = 'attributes'
  ) 
  
  #station (sta_id) -> pop (pop_id) -> operation (ope_id) -> operation_ipr (opi_id)
  fish_ddtnets_join <- merge(in_fish_pop_bvinters,
                             in_fish_ddtnets_spjoin,
                             by='pop_id',
                             all.x=F, all.y=F) %>%
    merge(ddtnet,
          by='UID_CE',
          all.x=T) %>%
    unique(by=c('pop_coordonnees_x', 'pop_coordonnees_y')) %>%
    merge(stations, by.x = 'pop_sta_id', by.y='sta_id', all.x=T) %>%
    merge(operation, by.x='pop_id', by.y='ope_pop_id', all.x=F) %>%
    merge(operation_ipr, by.x = 'ope_id', by.y='opi_ope_id',all.x=T)
    
  # word_vec <- lapply(stations$sta_libelle_sandre, function(s) {
  #   str_split(str_to_lower(gsub('"',"", gsub("'", "", s))), pattern='[,; ]')
  # }) %>% unlist %>% table %>% as.data.table
  regex <- paste(c(
    "canal", "foss[eé]", "roubine", "craste", "lac", "[eé]tang","r[ée]servoir",
    "rade", "chenal", "ballasti[eè]re", "barrage", "d[ée]rivation", "retenue", 
    "complexe", "gravi[eé]re", "pom.*", "secours", "prise", "bief", "aber",
    "hydraulique"),
    collapse='|'
  )
  
  fish_ddtnets_join_sub <- fish_ddtnets_join[
    grep(regex, sta_libelle_sandre, ignore.case=T, invert=T),] %>%
    .[grep(regex, pop_libelle_wama, ignore.case=T, invert=T),]

  #Use site name BD Topo name, Carthage, satellite imagery when unsure that a stream actually exists
  #Only count those where a BD Topo + satellite imagery confirm potential for channel
  #Exclude any site called fossé, roubine, craste, canal. Check at leastg 50 m
  #Remove all those when counting too. lac, étang, réservoir, rade, ballastière, 
 #barrage, derivation, retenue, complexe, annexe, gravieres, derivation, pompage, prise d'eau
  #bief, 
  
  fish_deleted_segments <-  fish_ddtnets_join_sub[
    pop_id  %in% 
      c(21773, 24094, 42007, 42008, 42020, 42021, 42022, 42023, 42026, 42028,
        47925, 47939, 59846, 59859, 59860, 59861, 95961, 95964, 95967, 102497,
        107162, 163534, 194805, 315621, 315622, 5808312, 6350996, 6350997,
        10033556, 3957, 3956, 104234, 66168, 108839, 102499, 95963, 21752, 35705,
        47544, 20413, 107816, 107602, 9345, 66167,316140, 108033, 66172, 
        9521, 208166, 9588, 66170,  66171, 9450, 9448, 11695036, 19101, 9443, 
        9585, 9586, 3242226, 46350, 99307, 95958, 21861, 208294, 316101, 56968, 
        75343, 21798, 11695033, 21797, 97649, 315432, 90955, 10063100, 21452,
        17397, 16660, 804, 5833, 41982, 30981, 213018, 107227, 12818168,  24048,
        86192, 103171, 315482, 89220, 316011, 21792,201208, 21791,424675, 92567,
        4191, 95962, 9528, 1236235, 103172, 20985, 201860, 5729960,  106893, 24053, 
        50235, 20139, 163530, 201858, 11695038, 316142, 13861, 6460300, 5804462,
        95544, 19771),] 
  fish_deleted_segments_wfish <- fish_deleted_segments[!is.na(opi_effectif),]
  
  
  fish_nce_segments <- fish_ddtnets_join_sub[
    pop_id  %in% 
      c(63506, 66502, 56865, 11742626, 31013, 62214, 378284, 66732,
        103992, 164095, 110357, 164219, 185534, 184735, 66731, 56878, 378433,
        104076, 56852, 58964, 83805, 58808, 2309768, 84253, 99853, 56778, 10312,
        2309767, 67487, 10316, 68233, 10345, 57164, 6401, 58961, 185498, 52533,
        202827, 56870, 58061, 30451, 12506, 32867, 10339, 7795, 4458088,
        85715, 7793, 10630, 98630, 58055, 7794, 66730, 56876, 1901, 164218,
        4840685, 315853, 4808, 315438, 315904, 315913),]
  fish_nce_segments_wfish <- fish_nce_segments[!is.na(opi_effectif),]
  
  return(list(
    attris_ddtnets_sub = fish_ddtnets_join_sub,
    deleted_segments = fish_deleted_segments,
    nce_segments = fish_nce_segments,
    deleted_segments_wfish = fish_deleted_segments_wfish,
    nce_segments_wfish = fish_nce_segments_wfish
  ))
}

#-------------------------- evaluate hydrobio stations coverage -----------------------------
#  in_hydrobio_ddtnets_spjoin <- tar_read(hydrobio_ddtnets_spjoin)
#  in_hydrobio_stations_bvinters <- tar_read(hydrobio_stations_bvinters)
#  in_ddtnets_path <- tar_read(ddtnets_path)
evaluate_hydrobio_coverage <- function(in_hydrobio_ddtnets_spjoin,
                                       in_hydrobio_stations_bvinters,
                                       in_ddtnets_path) {
  
  
  ddtnet <- vect(
    x = dirname(in_ddtnets_path),
    layer = basename(in_ddtnets_path),
    what = 'attributes'
  ) 
  
  hydrobio_ddtnets_join <- merge(in_hydrobio_stations_bvinters,
                                 in_hydrobio_ddtnets_spjoin,
                                 by='CdStationMesureEauxSurface',
                                 all.x=F, all.y=F) %>%
    merge(ddtnet,
          by='UID_CE',
          all.x=T) %>%
    unique(by=c('CoordXStationMesureEauxSurfa_6', 
                'CoordYStationMesureEauxSurfa_7')) %>%
    .[(INSEE_DEP != 76) & (CodeTypEthStationMesureEauxS_27 != 1),]  #Exclude Charente-Maritimes and standing water bodies

  regex <- paste(c(
    "canal", "foss[eé]", "roubine", "craste", "lac", "[eé]tang","r[ée]servoir",
    "rade", "chenal", "ballasti[eè]re", "barrage", "d[ée]rivation", "retenue", 
    "complexe", "gravi[eé]re", "pom.*", "secours", "prise", "bief", "aber",
    "hydraulique"),
    collapse='|'
  )
  
  hydrobio_ddtnets_join_sub <- hydrobio_ddtnets_join[
    grep(regex, LbStationMesureEauxSurface, ignore.case=T, invert=T),]

  #Manual checking Hydrobio fish stations that are on a BD Topo or BD Carthage segment but not on a DDT segment by looking at all those
  #beyong 50 m from a DDT segment + Manual checking DCE hydrobio sites whose closest segment is a non-watercourse:
  #Excluded even channelized rivers that originally were flowing in a natural bed 
  #(Somme, Meuse, etc.)
  hydrobio_deleted_segments <-    hydrobio_ddtnets_join_sub[
    CdStationMesureEauxSurface  %in% 
      c('04164950', '04306005', '05068475', '02093200', '04362016', '05225085', 
        '05225090', '03149229', '04401016', '03269250', '03189652', '03149223', 
        '04379004', '02042865', '02118748', '03207021', '04371012')] #03035734 was erased by a field

  hydrobio_nce_segments <-    hydrobio_ddtnets_join_sub[
    CdStationMesureEauxSurface  %in% 
      c('02098396', '02098397', '02098398', '03006417', '03006590', '03011740',
        '03014470', '04431025', '05234025', '06011900', '06011965', '06048420',
        '06590892', '06830132'
      )] #'04441014' - bief de moulin # '06580359' erased by a field
        
  return(list(
    attris_ddtnets_sub = hydrobio_ddtnets_join_sub,
    deleted_segments = hydrobio_deleted_segments,
    nce_segments = hydrobio_nce_segments
  ))
}


############################ ANALYZE DRAINAGE DENSITY ################################
# ------------------------- assess expertise effort ----------------------------
# in_ddtnets_bvinters = tar_read(ddtnets_bvinters)
# in_nat_id_cats <- tar_read(nat_id_cats)

evaluate_effort <- function(in_ddtnets_bvinters,
                            in_nat_id_cats) {
  #Remove lines from a departmental layer outside of that department
  in_ddtnets_bvinters[, orig_dep := as.integer(str_extract(orig_layer, "[0-9]{1,2}"))]
  
  in_ddtnets_bvinters <- in_ddtnets_bvinters[(orig_dep == INSEE_DEP) |
                                               (INSEE_DEP %in% c(92, 93 ,94)),]
  
  #Format data
  in_ddtnets_bvinters$nat_id <- str_to_lower(as.character(in_ddtnets_bvinters$nat_id))
  
  in_ddtnets_bvinters[nat_id=='', nat_id := NA]
  in_ddtnets_bvinters[nat_id2=='', nat_id2 := NA]
  
  #Merge with standardized categories
  nat_id_cats_melt <- melt(in_nat_id_cats,
                           variable.name = 'nat_id_std',
                           value.name = 'nat_id',
                           measure.vars=names(in_nat_id_cats)) %>%
    .[nat_id_std=='V9', nat_id := NA] %>%
    .[nat_id != '',]

  
  ddtnets_nat_id <- merge(in_ddtnets_bvinters, nat_id_cats_melt, 
                          by='nat_id', all.x=T)[
    , c('UID_BV', 'INSEE_DEP', 'nat_id', 'nat_id_std', 'geom_Length'), with=F]
  
  ddtnets_nat_id[str_split_i(nat_id, " ", 1)=='visite', nat_id_std := 'terrain']
  
  ddtnets_nat_id[is.na(nat_id_std) & !is.na(nat_id), nat_id_std := 'indéterminé']
  
  return(ddtnets_nat_id)
}


#-------------------------- summarize_drainage_density ---------------------------
# in_ddtnets_stats <- tar_read(ddtnets_bvinters_stats)$bv_stats
# in_carthage_stats <- tar_read(carthage_bvinters_stats)
# in_bcae_stats <- tar_read(bcae_bvinters_stats)
# in_bdtopo_stats <- tar_read(bdtopo_bvinters_stats)
# in_rht_stats <- tar_read(rht_bvinters_stats)
# in_bvdep_inters <- tar_read(bvdep_inters)
# id_cols <- c('UID_BV', 'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM_DEP')
# 
# #
# in_othernets_statlist<- list(carthage=in_carthage_stats,
#                    bcae=in_bcae_stats,
#                    bdtopo=in_bdtopo_stats,
#                    rht=in_rht_stats)
summarize_drainage_density <- function(in_ddtnets_stats, 
                                       in_othernets_statlist, 
                                       in_bvdep_inters, 
                                       manual_edit_missing_bvs=TRUE,
                                       outdir) {
  #in_dt_list <- c(as.list(environment()))
  
  id_cols <- c('UID_BV', 'PFAF_ID08', 'PFAF_ID09', 'INSEE_DEP', 'NOM_DEP')
  
  
  #Compute statistics for BV level (catchment) ----------------------------------
  
  #Compute the length of non-perennial segments classified as non-watercourse
  #and the ratio between the share of non-perennial segments classified as non-watercourse
  #to the share of non-perennial segments in the BV (to control for differences 
  #in non-perennial segment prevalence)
  int_nce_stats_bv <-   in_ddtnets_stats[, `:=`(
    int_length = .SD[regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)],
    nce_int_length = .SD[type_stand=="Non cours d'eau" & 
                           regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)],
    per_nce_determined_regime = .SD[regime_formatted != 'undetermined' & 
                                      type_stand=="Non cours d'eau", sum(length_cat_bv, na.rm=T)]/
      .SD[type_stand=="Non cours d'eau", sum(length_cat_bv, na.rm=T)],
    per_nce_intratio = (
      .SD[type_stand=="Non cours d'eau" & regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)]/
        .SD[type_stand=="Non cours d'eau", sum(length_cat_bv, na.rm=T)]
    )/
      (.SD[regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)]/
         .SD[, sum(length_cat_bv, na.rm=T)])
  ), by='UID_BV']
  
  
  ddtnets_stats <-  dcast(in_ddtnets_stats, 
                          as.formula(paste(paste(id_cols, collapse='+'),
                                           '~type_stand')),
                          value.var='length_cat_bv', fun.aggregate=sum) %>%
    merge(int_nce_stats_bv[!duplicated(UID_BV), 
                           .(UID_BV, total_length_bv, per_int,
                             per_ce, per_ind, per_nce, 
                             int_length, nce_int_length,per_nce_determined_regime, 
                             per_nce_intratio)], by='UID_BV') %>%
    setnames(c("Cours d'eau", "Indéterminé", "Inexistant", "Non cours d'eau"),
             c('length_bv_ce', 'length_bv_ind', 'length_bv_inx', 'length_bv_nce'))
  
  
  #Get rid of sub-categories - compute total drainage length
  dt_list_nocats_bv <- lapply(in_othernets_statlist, function(dt) {
    dt[, list(length_bv = sum(length_cat_bv)),
       by=c(id_cols)]
  })
  
  dt_list_nocats_bv[['ddtnets']] <- ddtnets_stats
  
  nets_stats_merged_bv <- mergeDTlist(
    dt_list = dt_list_nocats_bv,
    by = id_cols,
    all = TRUE,
    set_suffix = TRUE
  ) %>%
    #.[!(INSEE_DEP %in% c(92, 93, 94)),] %>%   #Remove departments from ile de france included with Paris
    merge(in_bvdep_inters[, .(UID_BV, POLY_AREA)], by="UID_BV") %>%
    setnames(c('length_bv_carthage', 'length_bv_bcae',
               'length_bv_bdtopo', 'length_bv_rht', 'POLY_AREA'),
             c('carthage', 'bcae', 'bdtopo', 'rht', 'bv_area'))
  
  nets_stats_melt <- melt(
    nets_stats_merged_bv,
    measure.vars = c('carthage', 'bcae', 'bdtopo', 'rht')
    # id.vars = names(nets_stats_merged_bv)[
    #   !names(nets_stats_merged_bv) %in% c('carthage', 'bcae', 'bdtopo', 'rht')],
  ) %>%
    .[, variable := factor(variable,
                           levels=c('bdtopo', 'carthage', 'bcae', 'rht'))]
  
  #Evaluate missing areas based on bdtopo----------------------------------
  nodata_bvs <- nets_stats_melt[variable=='bdtopo' & (
    (is.na(total_length_bv_ddtnets) | 
       ((total_length_bv_ddtnets/value) < 0.05 & value > 1000)) &
      bv_area > 5
  ), c('UID_BV', 'INSEE_DEP', 'bv_area'), with=F]
  
  if (manual_edit_missing_bvs) {
    nodata_bvs <- rbind(
      nodata_bvs,
      nets_stats_melt[UID_BV %in% c(1556, 7701, 7797, 7967, 8242, 8326),
                      c('UID_BV', 'INSEE_DEP', 'bv_area'), with=F]
    ) %>%
      .[INSEE_DEP %in% c(6, 8, 12, 28, 31, 35, 76, 88),]
  }

  #Compute statistics for HydroBASINS level 8-----------------------------------
  # nets_stats_melt_b8 <-nets_stats_melt[, list(
  #   length_ddtnets_ce = sum(length_bv_ce_ddtnets, na.rm=T),
  #   length_ddtnets_ind = sum(length_bv_ind_ddtnets, na.rm=T),
  #   length_others = sum(value, na.rm=T),
  #   b8_area = sum(bv_area)
  # ), by=c('PFAF_ID08', 'INSEE_DEP', 'NOM_DEP', 'variable')] %>%
  #   .[, length_ddtnets_ceind := length_ddtnets_ce + length_ddtnets_ind]
  
  #Compute statistics for departments ------------------------------------------
  #Compute IRES representativeness at the department level
  int_nce_stats_dep <-   in_ddtnets_stats[
    !(UID_BV %in% nodata_bvs$UID_BV), list(
      per_nce_determined_regime = .SD[regime_formatted != 'undetermined' & 
                                        type_stand=="Non cours d'eau", sum(length_cat_bv, na.rm=T)]/
        .SD[type_stand=="Non cours d'eau", sum(length_cat_bv, na.rm=T)],
      int_length = .SD[regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)],
      nce_int_length = .SD[type_stand=="Non cours d'eau" & 
                             regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)],
      per_nce_intratio = (
        .SD[type_stand=="Non cours d'eau" & regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)]/
          .SD[type_stand=="Non cours d'eau", sum(length_cat_bv, na.rm=T)]
      )/
        (.SD[regime_formatted=='intermittent', sum(length_cat_bv, na.rm=T)]/
           .SD[, sum(length_cat_bv, na.rm=T)])
    ), by='INSEE_DEP']
  
  #Compute general statistics
  nets_stats_melt_dep <-nets_stats_melt[
    !(UID_BV %in% nodata_bvs$UID_BV),
    list(
      length_ddtnets_total = sum(total_length_bv_ddtnets, na.rm=T),
      length_ddtnets_ce = sum(length_bv_ce_ddtnets, na.rm=T),
      length_ddtnets_ind = sum(length_bv_ind_ddtnets, na.rm=T),
      length_others = sum(value, na.rm=T),
      per_ce = sum(length_bv_ce_ddtnets, na.rm=T)/sum(total_length_bv_ddtnets, na.rm=T),
      per_ind = sum(length_bv_ind_ddtnets, na.rm=T)/sum(total_length_bv_ddtnets, na.rm=T),
      per_nce = sum(length_bv_nce_ddtnets, na.rm=T)/sum(total_length_bv_ddtnets, na.rm=T)
    ), by=c('INSEE_DEP', 'NOM_DEP', 'variable')] %>%
    .[, lengthratio_ddt_ce_to_other := length_ddtnets_ce/length_others,] %>%
    .[, lengthratio_ddt_ceind_to_other := (length_ddtnets_ce + length_ddtnets_ind)/length_others,] %>%
    .[, lengthratio_ddt_ce_to_other_standardized :=
        (lengthratio_ddt_ce_to_other-mean(lengthratio_ddt_ce_to_other))
      , by=c('variable')] %>%
    .[, lengthratio_ddt_ceind_to_other_standardized :=
        (lengthratio_ddt_ceind_to_other-mean(lengthratio_ddt_ceind_to_other))
      , by=c('variable')] %>%
    .[, mean_lengthratio_ddt_ce_to_other_standardized :=
        mean(lengthratio_ddt_ce_to_other_standardized)
      , by=c('INSEE_DEP', 'NOM_DEP')] %>%
    .[, mean_lengthratio_ddt_ceind_to_other_standardized :=
        mean(lengthratio_ddt_ceind_to_other_standardized)
      , by=c('INSEE_DEP', 'NOM_DEP')] %>%
    merge(.[variable=='bdtopo', list(
      lengthratio_ddt_ce_to_bdtopo_standardized=lengthratio_ddt_ce_to_other_standardized,
      lengthratio_ddt_ceind_to_bdtopo_standardized=lengthratio_ddt_ceind_to_other_standardized,
      INSEE_DEP = INSEE_DEP
    )],
    by=c('INSEE_DEP')) %>%
    merge(int_nce_stats_dep, by='INSEE_DEP')
  
  #Compute drainage density ratios at the bv level -----------------------------
  nets_stats_merged_bv[, `:=`(
    ddt_to_carthage_ddratio_ce =  length_bv_ce_ddtnets/carthage,
    ddt_to_bcae_ddratio_ce =  length_bv_ce_ddtnets/bcae,
    ddt_to_bdtopo_ddratio_ce =  length_bv_ce_ddtnets/bdtopo,
    ddt_to_rht_ddratio_ce =  length_bv_ce_ddtnets/rht,
    ddt_to_carthage_ddratio_ceind =  (length_bv_ce_ddtnets + 
                                        length_bv_ind_ddtnets)/carthage,
    ddt_to_bcae_ddratio_ceind =  (length_bv_ce_ddtnets + 
                                    length_bv_ind_ddtnets)/bcae,
    ddt_to_bdtopo_ddratio_ceind =  (length_bv_ce_ddtnets + 
                                      length_bv_ind_ddtnets)/bdtopo,
    ddt_to_rht_ddratio_ceind =  (length_bv_ce_ddtnets + 
                                   length_bv_ind_ddtnets)/rht
  )]
  
  #Return ---------------------------------------------------------------------
  return(list(
    nets_stats_merged_bv=nets_stats_merged_bv[!(UID_BV %in% nodata_bvs$UID_BV),],
    nets_stats_melt=nets_stats_melt[!(UID_BV %in% nodata_bvs$UID_BV),],
    #nets_stats_melt_b8=nets_stats_melt_b8,
    nets_stats_melt_dep=nets_stats_melt_dep,
    nodata_bvs = nodata_bvs
  ))
}

#-------------------------- evaluate missing area ------------------------------
# in_ddtnets_stats <- tar_read(ddtnets_bvinters_stats)$bv_stats
# in_drainage_density_summary <- tar_read(drainage_density_summary)
# in_bvdeps_inters <- tar_read(bvdep_inters_tab)

evaluate_missing_areas <- function(in_ddtnets_stats,
                                   in_drainage_density_summary,
                                   in_bvdep_inters) {
  
  nodata_bvs_attris <- merge(
    in_drainage_density_summary$nodata_bvs, in_bvdep_inters,
    by='UID_BV', all.x=F)
  
  
  ddtnets_stats_attris <- merge(
    in_ddtnets_stats,
    in_bvdep_inters[, c('UID_BV', 'POLY_AREA'), with=F],
    by='UID_BV'
  ) 
  
  return(list(
    per_area_nodata = (sum(nodata_bvs_attris$POLY_AREA)/
                         in_bvdep_inters[POLY_AREA>5,sum(POLY_AREA)]),
    per_area_indo50 = ddtnets_stats_attris[!duplicated(UID_BV) & per_ind > 0.5, sum(POLY_AREA)]/
      in_bvdep_inters[POLY_AREA>5,sum(POLY_AREA)]
  ))
}

#-------------------------- export sub-basins with missing ddt data -----------
# in_drainage_density_summary = tar_read(drainage_density_summary)
# in_bvdep_inters_gdb_path = tar_read(bvdep_inters_gdb_path)
# out_shapefile = file.path(resdir, 'carto_loi_eau_missing_data_bvs.shp')

export_missing_bvs <- function(in_drainage_density_summary,
                               in_bvdep_inters_gdb_path,
                               out_shapefile,
                               overwrite=T) {
  bvdep_inters_vect <- vect(dirname(in_bvdep_inters_gdb_path),
                            layer=basename(in_bvdep_inters_gdb_path))
  missing_bvs_vect <- merge(bvdep_inters_vect,
                            in_drainage_density_summary$nodata_bvs,
                            by='UID_BV', all.x=F)
  writeVector(missing_bvs_vect, out_shapefile, overwrite=overwrite)
  return(out_shapefile)
}

#-------------------------- merge env to drainage density at bv level ----------
# in_drainage_density_summary <- tar_read(drainage_density_summary)
# in_env_bv_dt <- tar_read(env_bv_dt)

merge_env_dd_bv <- function(in_drainage_density_summary,
                            in_env_bv_dt) {
  dt <- merge(in_drainage_density_summary$nets_stats_merged_bv,
              in_env_bv_dt$bv,
              by=c('UID_BV', 'INSEE_DEP')) 
  #Compute stats with CE and IND
  dt[, ddt_to_bdtopo_ddratio_ceind_depmean := weighted.mean(
    ddt_to_bdtopo_ddratio_ceind, 
    ((length_bv_ce_ddtnets+length_bv_ind_ddtnets)+bdtopo)/2,
    na.rm=T),
    by='INSEE_DEP'] %>%
    .[, ddt_to_bdtopo_ddratiodev_ceind := 
        ddt_to_bdtopo_ddratio_ceind-ddt_to_bdtopo_ddratio_ceind_depmean] %>%
    .[, ddt_ceind_dd := (length_bv_ce_ddtnets+length_bv_ind_ddtnets)/(1000*bv_area)] 
  
  #Compute stats with ce only
  dt[, ddt_to_bdtopo_ddratio_ce_depmean := weighted.mean(
    ddt_to_bdtopo_ddratio_ce, 
    ((length_bv_ce_ddtnets+length_bv_ind_ddtnets)+bdtopo)/2,
    na.rm=T),
    by='INSEE_DEP'] %>%
    .[, ddt_to_bdtopo_ddratiodev_ce := 
        ddt_to_bdtopo_ddratio_ce-ddt_to_bdtopo_ddratio_ce_depmean] %>%
    .[, ddt_ce_dd := (length_bv_ce_ddtnets+length_bv_ind_ddtnets)/(1000*bv_area)] 
  
  cols_to_divide_by_length <- grep('n_barriers',
                                   names(dt), value=T)
  dt[, (cols_to_divide_by_length) := sapply(
    .SD, 
    function(x) {
      fifelse(is.na(x), 0,
              1000*x/(length_bv_ce_ddtnets+length_bv_ind_ddtnets))
    }, simplify=F),
    .SDcols = cols_to_divide_by_length]
  
  dt[, per_nce_dep := sum(length_bv_nce_ddtnets,na.rm=T)/
       sum(total_length_bv_ddtnets), by=INSEE_DEP]
  
  new_colnames <- gsub('n_barriers', 'bar_bk_ssu', cols_to_divide_by_length)
  setnames(dt, cols_to_divide_by_length, new_colnames)
  
  return(dt)
}

#-------------------------- merge env to drainage density at dep level ----------
# in_drainage_density_summary <- tar_read(drainage_density_summary)
# in_env_bv_dt <- tar_read(env_bv_dt)

merge_env_dd_dep <- function(in_drainage_density_summary,
                             in_env_bv_dt) {
  
  dt <- merge(in_drainage_density_summary[['nets_stats_melt_dep']],
              in_env_bv_dt[['dep']],
              by='INSEE_DEP') 
  
  cols_to_divide_by_length <- grep('n_barriers',
                                   names(dt), value=T)
  dt[, (cols_to_divide_by_length) := sapply(
    .SD, 
    function(x) {
      fifelse(is.na(x), 0,
              1000*x/(length_ddtnets_ce+length_ddtnets_ind))
    }, simplify=F),
    .SDcols = cols_to_divide_by_length,
    by='INSEE_DEP']
  
  new_colnames <- gsub('n_barriers', 'bar_bk_ssu', cols_to_divide_by_length)
  setnames(dt, cols_to_divide_by_length, new_colnames)
  
  return(dt)
}


#-------------------------- plot and summarize drainage density and deviations at the dep level ---------------------------
# in_drainage_density_summary <- tar_read(drainage_density_summary)
# in_env_dd_merged_dep <- tar_read(env_dd_merged_dep)
# in_bvdep_inters <- tar_read(bvdep_inters)
# in_varnames <- tar_read(varnames)

plot_envdd_dep <- function(in_drainage_density_summary,
                           in_env_dd_merged_dep,
                           in_bvdep_inters,
                           in_varnames) {
  dep_stats <- in_drainage_density_summary$nets_stats_melt_dep
  
  #------------------------- Plots of drainage density and deviation -----------
  #Scartterplot of drainage density at department level (without Paris)
  p_dd_scatter_dep <- ggplot(
    dep_stats[!(INSEE_DEP %in% c(75,92,93,94)),],
    aes(x=length_others,
        y=(length_ddtnets_ce)+(length_ddtnets_ind),
        color=variable
    )) +
    #geom_point() +
    geom_text(aes(label=INSEE_DEP)) +
    geom_smooth(method='rlm') +
    facet_wrap(~variable, scales = 'free_y') +
    theme_bw()+
    scale_y_log10() +
    scale_x_log10()
  
  #Bar chart of drainage density ratio at the department level
  p_ddratio_bars_dep <- ggplot(
    dep_stats,
    aes(x=reorder(INSEE_DEP, lengthratio_ddt_ceind_to_bdtopo_standardized),
        y=lengthratio_ddt_ceind_to_other,
        fill=variable
    )) +
    geom_bar(stat='identity', alpha=0.5) +
    geom_bar(aes(y=(per_ce/(per_ce+per_ind))*lengthratio_ddt_ceind_to_other), 
             stat='identity') +
    geom_hline(yintercept=1) +
    scale_y_continuous(expand=c(0,0)) +
    #geom_text(aes(label=INSEE_DEP)) +
    coord_flip() +
    facet_grid(~variable, scales = 'free') +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))
  
  #Bar chart of intermittent-river representativeness
  p_intratio_bars_dep <- ggplot(
    dep_stats[!duplicated(INSEE_DEP) & 
                per_nce_determined_regime>0.5 &
                per_nce>0.01,],
    aes(x=reorder(INSEE_DEP,per_nce_intratio),
        y=per_nce_intratio,
        fill=nce_int_length/1000
    )) +
    geom_bar(stat='identity') +
    geom_hline(yintercept=1) +
    scale_x_discrete(name='Department number') +
    scale_y_continuous(
      name='Representativeness of intermittent rivers in segments classified as non-watercourses', 
      expand=c(0,0)) +
    scale_fill_distiller(name=str_wrap('Length of intermittent rivers classified
                                       as non-watercourses (km)', 30),
                         palette='Spectral', trans = "sqrt",
                         breaks=c(2, 100, 500, 1000, 2000, 6000))+
    #geom_text(aes(label=INSEE_DEP)) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position=c(0.8,0.2),
      legend.background = element_blank()
    )
  
  #------------------------- Plots of drivers of drainage density deviation -----------
  driver_cols_dt <- data.table(
    driver=c('agr_pc_sse'
             , 'pst_pc_sse'
             , 'wcr_pc_sse'
             , 'orc_pc_sse'
             , 'vny_pc_sse'
             , 'awc_mm_sav'
             , 'imp_pc_sse'
             , 'ppc_pk_sav'
             , 'ari_ix_ssu'
             , 'ari_ix_syr'
             , 'irs_pc_sav'
             , 'ire_pc_sse'
             , 'vww_mk_syr_Souterrain_IRRIGATION'
             , 'vww_mk_syr_Souterrain_EAU_POTABLE'
             , 'vww_mk_syr_Surface_continental_IRRIGATION'
             , 'vww_mk_syr_Surface_continental_EAU_POTABLE'
             , 'slo_dg_sav'
             , 'bar_bk_ssu_TOTAL')
  ) %>%
    merge(in_varnames, by.x='driver', by.y='variable', sort=F) 
  stat_cols <- c('INSEE_DEP', 'lengthratio_ddt_ce_to_other',
                 'lengthratio_ddt_ceind_to_other','per_ce', 'per_nce')
  
  driver_cols_dt[, description := factor(description, levels=.SD[,description])] 
  
  plot_env_lengthratio <- function(in_refnet, in_dt) {
    env_dd_melt <- melt(
      in_dt[
        variable==in_refnet,
        c(driver_cols_dt$driver, stat_cols),
        with=F],
      id.vars=stat_cols) %>%
      merge(in_varnames, by='variable', sort=F) 
    
    plot_env_lengthratio_ceind <- ggplot(
      env_dd_melt, 
      aes(x=value, y=lengthratio_ddt_ceind_to_other,
          color=per_nce)) +
      geom_point() +
      geom_hline(yintercept=1, alpha=0.5) +
      geom_smooth(method='gam') +
      scale_color_distiller(palette='Spectral') +
      facet_wrap(~description, scales='free',
                 labeller = label_wrap_gen()) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank()
      )
    
    return(plot_env_lengthratio_ceind)
  }
  
  p_env_lengthratio_bdtopo <- plot_env_lengthratio(in_refnet='bdtopo',
                                                   in_dt=in_env_dd_merged_dep)
  p_env_lengthratio_carthage <- plot_env_lengthratio(in_refnet='carthage',
                                                     in_dt=in_env_dd_merged_dep)
  
  # ggplot(env_dd_melt, aes(x=value, y=lengthratio_ddt_ce_to_other,
  #                         color=per_nce)) +
  #   geom_point() +
  #   geom_hline(yintercept=1, alpha=0.5) +
  #   geom_smooth(method='gam') +
  #   scale_color_distiller(palette='Spectral') +
  #   facet_wrap(~variable, scales='free') +
  #   theme_bw()
  
  # ggplot(env_dd_melt[per_nce>0.01,],
  #        aes(x=value, y=per_nce,
  #            color=lengthratio_ddt_ce_to_other)) +
  #   geom_point() +
  #   geom_hline(yintercept=1, alpha=0.5) +
  #   #geom_quantile() +
  #   geom_smooth(method='gam') +
  #   scale_color_distiller(palette='Spectral') +
  #   facet_wrap(~variable, scales='free') +
  #   theme_bw()
  
  
  # #Distribution of drainage density ratios at the department level
  # #After standardization within reference network 
  # plot_ddratio_standardized_dep <- ggplot(
  #   dep_stats,
  #   aes(x=reorder(INSEE_DEP, mean_lengthratio_ddt_ceind_to_other_standardized),
  #       y=lengthratio_ddt_ceind_to_other_standardized,
  #       color=variable
  #   )) +
  #   geom_point() +
  #   geom_point(aes(y=mean_lengthratio_ddt_ceind_to_other_standardized),
  #              color='black', shape='square',size=2) +
  #   geom_smooth() +
  #   scale_y_continuous(expand=c(0,0)) +
  #   coord_flip() +
  #   theme_bw()
  
  return(list(
    dd_scatter_dep = p_dd_scatter_dep,
    ddratio_bars_dep =p_ddratio_bars_dep,
    intratio_bars_dep =p_intratio_bars_dep,
    env_lengthratio_bdtopo = p_env_lengthratio_bdtopo,
    env_lengthratio_carthage = p_env_lengthratio_carthage
  ))
}

#-------------------------- multivariate analysis env-dd for bvs across departments --------
# in_env_dd_merged_bv <- tar_read(env_dd_merged_bv)
# in_varnames <- tar_read(varnames)
# in_bvdep_inters <- tar_read(bvdep_inters)

corclus_envdd_bv <- function(in_env_dd_merged_bv,
                             in_varnames,
                             in_bvdep_inters) {
  
  ########Only keep bvs > 10 km2, with at least 500 m of BD topo and with ddt data #####
  dt <- in_env_dd_merged_bv
  dt_sub <- dt[bv_area>10 & bdtopo >500 & total_length_bv_ddtnets>0,] 
  
  
  #Compute correlation for each department  ------------------------------------
  #between environmental drivers and 
  #drainage density ratio between DDT maps and BD Topo
  
  #Weights are for gower distance computation
  driver_cols_dt <- list(
    data.table('agr_pc_sse', 0.25)
    ,data.table('pst_pc_sse', 0.25)
    ,data.table('scr_pc_sse', 0.25)
    ,data.table('wcr_pc_sse', 0.25)
    ,data.table('orc_pc_sse', 0.25)
    ,data.table('vny_pc_sse', 0.25)
    ,data.table('awc_mm_sav', 0.5)
    
    ,data.table('imp_pc_sse', 0.5)
    ,data.table('ppc_pk_sav', 0.5)
    
    ,data.table('ari_ix_ssu', 0.25)
    ,data.table('ari_ix_syr', 0.25)
    ,data.table('irs_pc_sav', 0.5)
    
    ,data.table('ire_pc_sse', 1)
    ,data.table('vww_mk_syr_Souterrain_IRRIGATION', 0.25)
    ,data.table('vww_mk_syr_Souterrain_EAU_POTABLE', 0.25)
    ,data.table('vww_mk_syr_Surface_continental_IRRIGATION', 0.25)
    ,data.table('vww_mk_syr_Surface_continental_EAU_POTABLE', 0.25)
    
    ,data.table('slo_dg_sav', 1)
    ,data.table('bar_bk_ssu_TOTAL',1)
  ) %>% rbindlist %>%
    setnames(c('variable', 'weight'))  %>%
    merge(in_varnames, by='variable', sort=F) 
  
  #Order variables for weight attribution
  driver_cols_dt[, variable := factor(variable, levels=.SD[,variable])]
  driver_cols_dt[, description := factor(description, levels=.SD[,description])] 
  
  #dt_sub[, variable := factor(variable, levels= driver_cols_dt$variable)]
  
  stat_cols <- c('UID_BV','INSEE_DEP', 'bv_area',
                 'ddt_to_bdtopo_ddratio_ce',
                 'ddt_to_bdtopo_ddratio_ceind',
                 'per_ce_ddtnets', 'per_nce_ddtnets')
  
  env_dd_melt <- melt(
    dt_sub[,
           c(as.character(driver_cols_dt$variable), stat_cols),
           with=F],
    id.vars=stat_cols) %>%
    merge(driver_cols_dt,
          by='variable') %>%
    merge(in_bvdep_inters[!duplicated(INSEE_DEP),
                          .(INSEE_DEP, NOM)], 
          by='INSEE_DEP')
  
  env_dd_dep_cor <- env_dd_melt[, list(
    cor= .SD[!is.na(value), 
             cor(ddt_to_bdtopo_ddratio_ceind, value, method='spearman')],
    n_bvs = .SD[!is.na(value), .N])
    , by=c("INSEE_DEP", "NOM", "description")] %>%
    .[n_bvs>10,] #Removing those departments with 10 or less bvs (really only removes those with 5 or less)
      
  excluded_deps <- env_dd_melt[!(NOM %in% env_dd_dep_cor$NOM), unique(NOM)]
  
  #NAs are due to several reasons:
  # - population density is not available yet for Ain and Sarthe
  # - 0 IRS in some departments
  # - 0 water withdrawals
  
  env_dd_dep_cormat <- dcast(env_dd_dep_cor, NOM+INSEE_DEP~description,
                             value.var='cor')
  mat_names <-  env_dd_dep_cormat$NOM
  env_dd_dep_cormat <- as.matrix(env_dd_dep_cormat[, -c('NOM', 'INSEE_DEP')])
  row.names(env_dd_dep_cormat) <- mat_names
  
  #Cluster  ---------------------------------------------------------------------
  #Compute Gower's distance based on correlation coefficients and variable weights
  env_dd_dep_gowdist <- cluster::daisy(env_dd_dep_cormat, 
                                       metric = "gower",
                                       weights = driver_cols_dt$weight) %>%
    as.dist
  
  #Cluster departments based on UPGMA or Ward's
  env_dd_dep_hclust_avg <- hclust(env_dd_dep_gowdist, method='average')
  env_dd_dep_hclust_ward <- hclust(env_dd_dep_gowdist, method='ward.D2')
  
  #Keep UPGMA based on cophcor
  cophcor_avg <- cor(env_dd_dep_gowdist, cophenetic(env_dd_dep_hclust_avg))
  cophcor_ward <- cor(env_dd_dep_gowdist, cophenetic(env_dd_dep_hclust_ward))
  
  dist_cophcor_dt_avg <- merge(
    reshape2::melt(as.matrix(env_dd_dep_gowdist)),
    reshape2::melt(as.matrix(cophenetic(env_dd_dep_hclust_avg))),
    by=c('Var1', 'Var2')) %>%
    setnames(c('value.x', 'value.y'), c("Gower's distance", "Cophenetic dissimilarity"))
  
  #Plot cophenetic correlation
  p_cophcor_avg <- ggplot(dist_cophcor_dt_avg, 
                          aes(x=`Gower's distance`, y=`Cophenetic dissimilarity`)) +
    geom_point() +
    geom_abline() +
    annotate('text', x = 0.5, y=0.1,
             label=paste('Cophenetic correlation =', 
                         round(cophcor_avg, 2))) +
    coord_fixed(expand=F, 
                ylim=c(0, max(dist_cophcor_dt_avg$`Cophenetic dissimilarity`)+0.05)) +
    theme_classic()
  
  dist_cophcor_dt_ward <- merge(
    reshape2::melt(as.matrix(env_dd_dep_gowdist)),
    reshape2::melt(as.matrix(cophenetic(env_dd_dep_hclust_ward))),
    by=c('Var1', 'Var2')) %>%
    setnames(c('value.x', 'value.y'), c("Gower's distance", "Cophenetic dissimilarity"))
  
  #Plot cophenetic correlation
  p_cophcor_ward <- ggplot(dist_cophcor_dt_ward, 
                           aes(x=`Gower's distance`, y=`Cophenetic dissimilarity`)) +
    geom_point() +
    geom_abline() +
    annotate('text', x = 0.5, y=0.1,
             label=paste('Cophenetic correlation =', 
                         round(cophcor_ward, 2))) +
    coord_fixed(expand=F, 
                ylim=c(0, max(dist_cophcor_dt_ward$`Cophenetic dissimilarity`)+0.05)) +
    theme_classic()
  
  #Graph scree plot
  scree_dt_avg <- data.table(height=env_dd_dep_hclust_avg$height,
                             groups=length(env_dd_dep_hclust_avg$height):1)
  
  p_scree_avg <- ggplot(scree_dt_avg,
                        aes(x=groups, y=height)) +
    geom_point() +
    geom_line() + 
    theme_classic()
  
  scree_dt_ward <- data.table(height=env_dd_dep_hclust_ward$height,
                              groups=length(env_dd_dep_hclust_ward$height):1)
  
  p_scree_ward <- ggplot(scree_dt_ward,
                         aes(x=groups, y=height)) +
    geom_point() +
    geom_line() + 
    theme_classic()
  
  # Check pvclust (significance testing)
  
  #Define class colors
  classcol<- c("#176c93","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#7a5614","#6baed6","#00441b", '#e41a1c') #9 classes with darker color (base blue-green from Colorbrewer2 not distinguishable on printed report and ppt)
  classcol_temporal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#666666','#a65628')
  
  
  #Make table of gauge classes and good looking dendogram
  env_dd_dendo_avg_lesscl <-prettydend(hclus_out = env_dd_dep_hclust_avg, 
                                    kclass=5, colors=classcol,
                                    classnames= NULL)
  env_dd_dendo_avg_morecl <-prettydend(hclus_out = env_dd_dep_hclust_avg, 
                                    kclass=9, colors=classcol,
                                    classnames= NULL)
  p_dendo_avg_lesscl <- env_dd_dendo_avg_lesscl[[2]]
  p_dendo_avg_morecl <- env_dd_dendo_avg_morecl[[2]]
  
  
  env_dd_dep_cor_avg_lesscl <- merge(env_dd_dep_cor, env_dd_dendo_avg_lesscl[[1]],
                                  by.x='NOM', by.y='ID')
  env_dd_dep_cor_avg_morecl <- merge(env_dd_dep_cor, env_dd_dendo_avg_morecl[[1]],
                                  by.x='NOM', by.y='ID')
  
  p_cluster_boxplot_avg_lesscl <- ggplot(
    env_dd_dep_cor_avg_lesscl, 
    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  
  p_cluster_boxplot_avg_morecl <- ggplot(
    env_dd_dep_cor_avg_morecl, 
    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  
  #Make table of gauge classes and good looking dendogram
  env_dd_dendo_ward_lesscl <-prettydend(hclus_out = env_dd_dep_hclust_ward, 
                                     kclass=5, colors=classcol,
                                     classnames= NULL)
  env_dd_dendo_ward_morecl <-prettydend(hclus_out = env_dd_dep_hclust_ward, 
                                     kclass=10, colors=classcol,
                                     classnames= NULL)
  p_dendo_ward_lesscl <- env_dd_dendo_ward_lesscl[[2]]
  p_dendo_ward_morecl <- env_dd_dendo_ward_morecl[[2]]
  
  env_dd_dep_cor_ward_lesscl <- merge(env_dd_dep_cor, env_dd_dendo_ward_lesscl[[1]],
                                   by.x='NOM', by.y='ID')
  
  p_cluster_boxplot_ward_lesscl <- ggplot(env_dd_dep_cor_ward_lesscl, 
                                    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  env_dd_dep_cor_ward_morecl <- merge(env_dd_dep_cor, env_dd_dendo_ward_morecl[[1]],
                                   by.x='NOM', by.y='ID')
  
  p_cluster_boxplot_ward_morecl <- ggplot(env_dd_dep_cor_ward_morecl, 
                                    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  
  #Correlation among variables  ------------------------------------------------
  var_cor <- dt_sub[, driver_cols_dt$variable, with=F] %>%
    setnames(as.character(driver_cols_dt$description)) %>%
    setnames(gsub("water withdrawals", "ww", names(.))) %>%
    setnames(gsub("surface water", "sw", names(.))) %>%
    setnames(gsub("gw", "gw", names(.))) %>%
    cor( method='spearman', use="pairwise.complete.obs")
  
  p_varscor <- ggcorrplot(var_cor, #method = "circle", 
                          hc.order = TRUE, hc.method = 'average',
                          type = "upper", lab=T, lab_size =3,
                          digits=1, insig='blank',
                          outline.color = "white") +
    scale_fill_distiller(
      name=str_wrap("Correlation coefficient Spearman's rho", 20),
      palette='RdBu', 
      limits=c(-1, 1), 
      breaks=c(-0.8, -0.5, 0, 0.5, 0.8)) +
    theme(legend.position = c(0.8, 0.3))
  
  var_cor_gclass_preformat <- merge(dt_sub, env_dd_dendo_avg_morecl[[1]], 
                          by.x='NOM_DEP', by.y='ID') %>%
    .[, c(as.character(driver_cols_dt$variable), 'gclass'), with=F] %>%
    setnames(c(as.character(driver_cols_dt$description), 'gclass')) %>%
    setnames(gsub("water withdrawals", "ww", names(.))) %>%
    setnames(gsub("surface water", "sw", names(.))) %>%
    setnames(gsub("gw", "gw", names(.)))
  
  var_cor_byclass <- lapply(unique(var_cor_gclass_preformat$gclass), 
                            function(class) {
    cor(var_cor_gclass_preformat[gclass==class,],
        method='spearman', use="pairwise.complete.obs")
  })
  names(var_cor_byclass) = unique(var_cor_gclass_preformat$gclass)
  
  #By department
  
  #Heatmap of correlation between drainage density ratio and variables----------
  colnames(env_dd_dep_cormat) <- 
    gsub("gw", "gw",
         gsub("surface water", "sw",
              gsub("water withdrawals", "ww", colnames(env_dd_dep_cormat))))
  
  max(env_dd_dep_cormat, na.rm=T)
  min(env_dd_dep_cormat, na.rm=T)
  
  env_dd_dep_cormat_avg_morecl <- env_dd_dep_cormat[env_dd_dep_hclust_avg$order,]
  #as.data.table(env_dd_dendo_avg_morecl[[1]])[order(ID),order(gclass)],]
  
  class_colors_avg_morecl <- classcol[as.data.table(
    env_dd_dendo_avg_morecl[[1]])[order(gclass, ID),gclass]]
  
  env_ddratio_corheatmap_avg_morecl <- 
    ggcorrplot(corr=env_dd_dep_cormat_avg_morecl, #method = "circle", 
               #hc.order = TRUE, hc.method = 'average',
               lab=T, lab_size =3,
               digits=1, insig='blank',
               outline.color = "white") +
    scale_fill_distiller(
      name=str_wrap("Correlation coefficient Spearman's rho", 20),
      palette='RdBu', 
      limits=c(-0.8, 0.81), 
      breaks=c(-0.7, -0.5, 0, 0.5, 0.7))  +
    coord_flip() +
    theme(axis.text.y = ggtext::element_markdown(
      colour = class_colors_avg_morecl))
  
  #Plot heatmap of env-dd correlations, clustered
  env_dd_dep_cormat_ward_morecl <- env_dd_dep_cormat[env_dd_dep_hclust_ward$order,]
  
  class_colors_ward_morecl <- classcol[as.data.table(
    env_dd_dendo_ward_morecl[[1]])[order(gclass, ID),gclass]]
  

  env_ddratio_corheatmap_ward_morecl <- 
    ggcorrplot(env_dd_dep_cormat_ward_morecl, #method = "circle", 
               #hc.order = TRUE, hc.method = 'average',
               lab=T, lab_size =3,
               digits=1, insig='blank',
               outline.color = "white") +
    scale_fill_distiller(
      name=str_wrap("Correlation coefficient Spearman's rho", 20),
      palette='RdBu', 
      limits=c(-0.81, 0.81), 
      breaks=c(-0.7, -0.5, 0, 0.5, 0.7))  +
    coord_flip() +
    theme(axis.text.y = element_text(
      colour = class_colors_ward_morecl))
  
  #Return results --------------------------------------------------------------
  return(list(
    env_dd_melt = env_dd_melt
    , env_dd_dep_cor = env_dd_dep_cor
    , env_dd_dep_cor_avg_morecl = env_dd_dep_cor_avg_morecl
    , excluded_deps = excluded_deps 
    , var_cor_byclass = var_cor_byclass
    , p_cophcor_avg = p_cophcor_avg
    , p_scree_avg =p_scree_avg
    , p_cophcor_ward = p_cophcor_ward
    , p_scree_ward =p_scree_ward
    , p_cluster_boxplot_ward_lesscl = p_cluster_boxplot_ward_lesscl
    , p_cluster_boxplot_ward_morecl = p_cluster_boxplot_ward_morecl
    , p_cluster_boxplot_avg_lesscl = p_cluster_boxplot_avg_lesscl
    , p_cluster_boxplot_avg_morecl = p_cluster_boxplot_avg_morecl
    , env_ddratio_corheatmap_avg_morecl = env_ddratio_corheatmap_avg_morecl
    , env_ddratio_corheatmap_ward_morecl = env_ddratio_corheatmap_ward_morecl
  ))
}


#-------------------------- build env-dd models ---------------------------------
# in_envdd_multivar_analysis=tar_read(envdd_multivar_analysis)
# in_drainage_density_summary=tar_read(drainage_density_summary)
# in_bvdep_inters_gdb_path=tar_read(bvdep_inters_gdb_path)

#Autocorrelation lecture/lab: https://www.emilyburchfield.org/courses/gsa/areal_data_lab
#Spatial regression lecture/lab: https://www.emilyburchfield.org/courses/gsa/spatial_regression_lab
#Other class on spatial regression: https://chrismgentry.github.io/Spatial-Regression/

build_mods_envdd <- function(in_envdd_multivar_analysis,
                             in_drainage_density_summary,
                             in_bvdep_inters_gdb_path) {
  
  #Merge all input non-spatial data
  env_dd_melt_gclass <- merge(
    in_envdd_multivar_analysis$env_dd_melt, #BV-level data on drainage density ratio and socio-env stats
    in_envdd_multivar_analysis$env_dd_dep_cor_avg_morecl[!duplicated(INSEE_DEP), #class of each bv
                                                      c('INSEE_DEP', 'gclass'),
                                                      with=F],
    by='INSEE_DEP') %>%
    merge(in_envdd_multivar_analysis$env_dd_dep_cor[ #Spearman correlation between variables and drainage density ratio
      ,c('INSEE_DEP', 'description', 'cor'), with=F],
      by=c('INSEE_DEP', 'description')
    ) %>%
    merge(in_drainage_density_summary$nets_stats_merged_bv[ #drainage length of ddt nets and bdtopo
      , c('total_length_bv_ddtnets', 'bdtopo', 'UID_BV'), with=F],
      by='UID_BV') %>%
    .[bv_area>10 & bdtopo >500,] #Only keep basins with a minimum area of 10 km2 and at least  500 m of bd topo
  
  #Read polygons of sub-basins
  bvdep_inters_vect <- vect(dirname(in_bvdep_inters_gdb_path),
                            layer=basename(in_bvdep_inters_gdb_path))
  
  #Pre-format data for analysis
  get_dat_formod <- function(in_dt_melt, 
                             in_envdd_multivar_analysis, 
                             in_bvdep_inters_vect,
                             in_gclass) {
    
    #Subset data for the departments in the class
    env_dd_melt_sub <-in_dt_melt[gclass==in_gclass,]
    
    
    #Cast environmental variables
    id_vars = c('INSEE_DEP', 'NOM', 'gclass', 'UID_BV', 'bv_area',
                'ddt_to_bdtopo_ddratio_ce', 'ddt_to_bdtopo_ddratio_ceind',
                'total_length_bv_ddtnets', 'bdtopo')
    formula_string <- paste0(
      paste(id_vars, collapse='+'),
      '~variable')
    env_dd_cast_sub <- dcast(env_dd_melt_sub, 
                             as.formula(formula_string)
                             , value.var='value')
    
    #Merge with spatial data
    bvdep_env_vect <- merge(in_bvdep_inters_vect,
                            env_dd_cast_sub, 
                            by='UID_BV',
                            all.x=F)
    
    #Transform irrigation variables
    ww_cols = grep("vww_mk_syr", names(env_dd_cast_sub), value=T)
    ww_cols_log = paste0(ww_cols, '_log')
    env_dd_cast_sub[, (ww_cols_log) := sapply(.SD, function(x) log10(x+10),
                                               simplify=F), 
                    .SDcols = ww_cols]
    env_dd_cast_sub[, (ww_cols) := NULL]
    
    env_dd_melt_sub <- melt(env_dd_cast_sub, id.vars = id_vars) %>%
      merge(in_dt_melt[gclass==in_gclass, c('UID_BV', 'cor', 'variable')],
            by=c('UID_BV', 'variable'))
    
    #Get correlation among predictor variables for the class
    var_cor_sub <- in_envdd_multivar_analysis$var_cor_byclass[[as.character(in_gclass)]]
    
    #Make correlation heatmap among predictor variables
    var_heatmap <- ggcorrplot(var_cor_sub, #method = "circle", 
                              #hc.order = TRUE, hc.method = 'average',
                              type = "upper", lab=T, lab_size =3,
                              digits=1, insig='blank',
                              outline.color = "white")
    
    #Plot relationship between predictor variables and response variable
    p <- ggplot(env_dd_melt_sub,
                aes(x=value,
                    y=ddt_to_bdtopo_ddratio_ceind)) +
      geom_point(aes(size=bv_area,
                     color=factor(INSEE_DEP)),
                 alpha=1/3) +
      geom_smooth(aes(weight=bv_area,
                      color=factor(INSEE_DEP)),
        method='gam',
        #family='lognormal',
        formula = y ~ s(x, bs = "cs", fx = TRUE, k =3),
        se=F)+
      geom_smooth(aes(weight=bv_area),
                  method='gam',
                  #family='lognormal',
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k =3),
                  se=F,
                  color='black')+
      scale_x_continuous(name=unique(env_dd_melt_sub$description)) +
      scale_y_continuous(name='Ratio of drainage density:
                         length(watercourses + unclassified)/length(BD TOPO)') +
      scale_size_continuous(name="Sub-basin area<br>(km<sup>2</sup>)",
                            breaks=c(20, 50, 100, 250, 500)) +
      # scale_color_brewer(name='rho',
      #                       palette='RdBu', 
      #                       limits=c(-0.81, 0.81),
      #                       breaks=c(-0.8, -0.5, 0, 0.5, 0.8)) +
      coord_cartesian(ylim=c(0, 
                             min(2,
                                 max(env_dd_melt_sub$ddt_to_bdtopo_ddratio_ceind)+0.1)
      ), 
      expand=FALSE) +
      facet_wrap(~variable,  #+paste('rho =', round(cor,2)
                 scales='free_x') +
      theme_classic() +
      theme(panel.grid.minor=element_blank(),
            strip.background = element_rect(fill="gray95",
                                            color="gray95",
                                            linewidth=NULL),
            legend.title = ggtext::element_markdown()
      )
    
    return(list(
      dat = env_dd_melt_sub,
      dat_cast = env_dd_cast_sub,
      dat_sp = bvdep_env_vect,
      id_vars = id_vars,
      var_cor = var_cor_sub,
      var_heatmap = var_heatmap,
      ddratio_env_plot = p
    ))
  }
  
  # in_mod <- cl9_mod4
  # in_dat_for_mod <- dat_for_mod_cl9
  # plot_nbs=T
  # removed_index=NULL

  postprocess_model <- function(in_mod, in_dat_for_mod, 
                                spatial_analysis = F,
                                plot_nbs=F, removed_index=NULL)  {
    
    in_dat <- in_dat_for_mod$dat_cast
    if (!is.null(removed_index)) {
      in_dat <- in_dat[!removed_index,]
    }
    in_dat_sp <- in_dat_for_mod$dat_sp[
      !(seq_along(in_dat_for_mod$dat_sp$UID_BV) %in% removed_index),]  
    
    id_vars <- in_dat_for_mod$id_vars

    smr <- summary(in_mod)
    #dat_copy<- copy(in_dat) 
    mod_name <- paste(names(in_mod$model)[2:length(names(in_mod$model))], 
                      collapse='_') %>%
      gsub("_[(]weights[)]", "", .)
    res_name <- paste('residuals', mod_name, sep="_")

    #Check distributions of residuals
    nsp_diag <- gg_diagnose(in_mod)
    
    #Check correlations of other variables with residuals
    in_dat[, (res_name) := residuals(in_mod)]
    
    #Plot residual correlations with other variables
    resid_env_p<- ggplot(melt(in_dat, 
                              id.vars=c(id_vars, 
                                        res_name)),
           aes(x=value, y=get(res_name))) +
      geom_point(aes(color=factor(INSEE_DEP),
                     size=bv_area), alpha=1/3) +
      geom_smooth(method='lm', se=F,
                  aes(color=factor(INSEE_DEP),
                      weight=bv_area)) +
      geom_smooth(method='lm', color='black', se=F,
                  aes(weight=bv_area)) +
      facet_wrap(~variable, scales='free_x') +
      theme_bw()
    
    #Check spatial autocorrelation of residuals
    if (spatial_analysis) {
      resids_sp <- merge(in_dat_sp,
                         in_dat[, c('UID_BV', res_name), with=F],
                         by='UID_BV')
      resids_sp_p <- ggplot(resids_sp) +
        geom_spatvector(aes_string(fill=res_name)) +
        scale_fill_distiller(palette='Spectral')
      
      #Compute k nearest neighbor list 
      id <- resids_sp$UID_BV
      coords <- coordinates(as(resids_sp, 'Spatial'))
      dat_nb4 <- knn2nb(knearneigh(coords, k = 4), row.names = id)
      if (plot_nbs) {
        plot(as(resids_sp, 'Spatial'))
        plot(dat_nb4, coords, add=T, main = "KNN = 4 Neighborhood")
      }
      
      
      #Assign weight to neighborhood based on idw (power 2) - globally standardized
      wlist <- nb2listwdist(neighbours=dat_nb4, 
                            x=as(resids_sp, 'Spatial'), type="idw", style="C", 
                            alpha = 2, dmax = NULL, longlat = NULL, zero.policy=NULL)
      #nb2listw(dat_nb4, style = "B", zero.policy = T)
      
      #Run moran's test
      moran_lm <- lm.morantest(in_mod, wlist)
      
      #Lagrange multiplier diagnostic tests for spatial dependence
      #Test for error dependence (LMerr) and 
      # test for missing spatially lagged dependent variable (LMlag)
      LMtests <- lm.LMtests(in_mod, wlist, test = "all")
      
      #Run Spatial error model
      sem <- spatialreg::errorsarlm(formula = in_mod$call$formula, 
                                    data = in_dat,
                                    listw = wlist, quiet = T)
      smr_sem <- summary(sem)
      
      # resids_sp$fitted_sem <- sem$fitted.values
      # spplot(resids_sp, "fitted_sem", main = "Trend")
      # resids_sp$resid_sem <- sem$residuals
      # spplot(resids_sp, "resid_sem", main = "Residuals")
      
      #Run spatial lag model
      lagm <- spatialreg::lagsarlm(formula = in_mod$call$formula, 
                                   data = in_dat,
                                   listw = wlist,
                                   zero.policy = T)
      smr_lagm <- summary(lagm)
      
      # resids_sp$resid_lagm <- lagm$residuals
      # spplot(resids_sp, "resid_lagm", main = "Residuals")
      
      return(list(
        smr=smr,
        nsp_diag=nsp_diag,
        resids_name = res_name,
        resid_env_p = resid_env_p,
        resids_sp = resids_sp,
        resids_sp_p = resids_sp_p,
        wlist = wlist,
        moran_lm = moran_lm,
        LMtests = LMtests,
        sem = sem,
        smr_sem = smr_sem,
        lagm = lagm,
        smr_lagm = smr_lagm
      ))
    } else {
      return(list(
        smr=smr,
        nsp_diag=nsp_diag,
        resids_name = res_name,
        resid_env_p = resid_env_p,
        resids_sp = NULL,
        resids_sp_p = NULL,
        wlist = NULL,
        moran_lm = NULL,
        LMtests = NULL,
        sem = NULL,
        smr_sem = NULL,
        lagm = NULL,
        smr_lagm = NULL
      ))
    }
  }
  ##
  #Class 1 model - Savoie ------------------------------------------------------
  print('Selecting model for class 1')
  dat_for_mod_cl1 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=1) 
  
  dat_for_mod_cl1$ddratio_env_plot
  
  ggplot(dat_for_mod_cl1$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl1$dat_cast,
         aes(x=slo_dg_sav, y=ddt_to_bdtopo_ddratio_ceind)) +
    geom_point() +
    geom_smooth(method='lm')
  
  #Model 1: dd ratio ~ slope
  cl1_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~slo_dg_sav,
                 data=dat_for_mod_cl1$dat_cast)
  
  cl1_mod1_diagnostics <- postprocess_model(in_mod = cl1_mod1,
                                            in_dat = dat_for_mod_cl1,
                                            spatial_analysis = T)
  cl1_mod1_diagnostics$smr
  cl1_mod1_diagnostics$resids_sp_p
  cl1_mod1_diagnostics$LMtests #lag model test is significant
  plot(cl1_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ slope + agriculture
  cl1_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind~slo_dg_sav+agr_pc_sse, 
                 data=dat_for_mod_cl1$dat_cast)
  
  cl1_mod2_diagnostics <- postprocess_model(in_mod = cl1_mod2,
                                            in_dat = dat_for_mod_cl1,
                                            spatial_analysis = F)
  cl1_mod2_diagnostics$smr #slope is not significant, agriculture is

  
  #Model 3: dd ratio ~ agriculture
  cl1_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind~agr_pc_sse, 
                 data=dat_for_mod_cl1$dat_cast)

  cl1_mod3_diagnostics <- postprocess_model(in_mod = cl1_mod3,
                                            in_dat = dat_for_mod_cl1,
                                            spatial_analysis = T)
  cl1_mod3_diagnostics$smr
  cl1_mod3_diagnostics$LMtests #Both lag and error models tests are significant
  cl1_mod3_diagnostics$smr_sem
  cl1_mod3_diagnostics$smr_lagm
  
  #Pseudo-R2 for lagm
  cl1_mod3_pseudoR2 <- cor(cl1_mod3_diagnostics$lagm$y, 
                           cl1_mod3_diagnostics$lagm$fitted.values)^2
  
  #Plot other variables against residuals of spatial regression
  dat_for_mod_cl1$dat_cast$mod3_lagm_residuals <- cl1_mod3_diagnostics$lagm$residuals
  cl1_mod3_lagm_residuals_env_p <- ggplot(
    melt(dat_for_mod_cl1$dat_cast, id.vars=c(dat_for_mod_cl1$id_vars, 'mod3_lagm_residuals')),
    aes(x=value, y=mod3_lagm_residuals)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~variable, scales='free')
  #No visible correlation
  
  #Plot actual drainage length against slope and agriculture for bdtopo and ddt net
  (ggplot(
    melt(dat_for_mod_cl1$dat_cast, 
         id.vars=c('UID_BV', 'slo_dg_sav'),
         measure.vars = c('bdtopo', 'total_length_bv_ddtnets')),
    aes(x=slo_dg_sav, y=value)) +
    geom_point() +
    geom_smooth(method='gam') +
    facet_wrap(~variable))/
    (ggplot(
      melt(dat_for_mod_cl1$dat_cast, 
           id.vars=c('UID_BV', 'agr_pc_sse'),
           measure.vars = c('bdtopo', 'total_length_bv_ddtnets')),
      aes(x=agr_pc_sse, y=value)) +
       geom_point() +
       geom_smooth(method='gam') +
       facet_wrap(~variable))
  
  cl1_chosen_model <- list(cl1_mod3, cl1_mod3_diagnostics)
  #Agricultural area is more strongly correlated to drainage density ratio
  #but slope is also strongly related. Other correlated include aridity, etc.
  #These all show a spatial gradient from east to west of the department
  #but it isn't so much a result of a differential classification of watercourses
  #against a uniform distribution from bdtopo as much as a much higher drainage
  #density in the higher slope-lower agriculture-wetter part of the department 
  #in bdtopo — most first-order and several second-order streams were removed
  #in those areas. Probably considered mountain gullies.
 
  #Class 2 model - Loire-et-Cher -----------------------------------------------
  print('Selecting model for class 2')
  
  dat_for_mod_cl2 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=2) 
  
  dat_for_mod_cl2$ddratio_env_plot

  ggplot(dat_for_mod_cl2$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl2$dat_cast, 
         aes(x=wcr_pc_sse, y=ddt_to_bdtopo_ddratio_ceind)) +
    geom_point() +
    geom_smooth(method='lm')
  
  #Model 1: dd ratio ~ winter crops
  cl2_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse,
                 data=dat_for_mod_cl2$dat_cast)
  
  cl2_mod1_diagnostics <- postprocess_model(in_mod = cl2_mod1,
                                            in_dat = dat_for_mod_cl2,
                                            spatial_analysis = F)
  cl2_mod1_diagnostics$smr
  plot(cl2_mod1_diagnostics$nsp_diag)
  
  #Model 2: dd ratio ~ winter crops after removing outlier (0 ce or ind)
  dat_for_mod_cl2$dat_cast <- dat_for_mod_cl2$dat_cast[
    ddt_to_bdtopo_ddratio_ceind>0,]
  dat_for_mod_cl2$dat_sp <- dat_for_mod_cl2$dat_sp[
    dat_for_mod_cl2$dat_sp$ddt_to_bdtopo_ddratio_ceind>0,]
  
  cl2_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse,
                 data=dat_for_mod_cl2$dat_cast)
  
  cl2_mod2_diagnostics <- postprocess_model(in_mod = cl2_mod2,
                                            in_dat = dat_for_mod_cl2,
                                            spatial_analysis = T)
  
  cl2_mod2_diagnostics$smr
  plot(cl2_mod2_diagnostics$nsp_diag)
  plot(cl2_mod2_diagnostics$resid_env_p)
  #mod2_diagnostics$resids_sp_p

  cl2_mod2_diagnostics$LMtests
  
  #Model 3: dd ratio ~ predicted intermittency after removing outlier (0 ce or ind)
  cl2_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind~irs_pc_sav,
                 data=dat_for_mod_cl2$dat_cast)
  
  cl2_mod3_diagnostics <- postprocess_model(in_mod = cl2_mod3,
                                            in_dat = dat_for_mod_cl2)
  
  cl2_mod3_diagnostics$smr
  plot(cl2_mod3_diagnostics$nsp_diag)
  
  cl2_chosen_model <- list(cl2_mod2, cl2_mod2_diagnostics)
  #Winter crops is the main explanation for differences in drainage density across
  #the department. No residual correlation with other variables
  #No residual spatial autocorrelation.
  
  #Class 3 model - Ain, Ardennes, Drome, Haute-Pyrennees -----------------------
  print('Selecting model for class 3')
  
  dat_for_mod_cl3 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=3) 
  
  dat_for_mod_cl3$ddratio_env_plot
  check <- unique(dat_for_mod_cl3$dat, by= c('variable', 'INSEE_DEP'))
  
  ggplot(dat_for_mod_cl3$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl3$dat_cast, 
         aes(x=sqrt(ppc_pk_sav), y=ddt_to_bdtopo_ddratio_ceind)) +
    geom_point() +
    geom_smooth(method='lm')
  
  dat_for_mod_cl3$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl3$dat_cast[, wcr_pc_sse_sqrt := sqrt(wcr_pc_sse)]
  dat_for_mod_cl3$dat_cast[, ppc_pk_sav_sqrt := sqrt(ppc_pk_sav)]
  
  #Model 0: null model - dd ratio ~ dep
  cl3_mod0 <- lm(ddt_to_bdtopo_ddratio_ceind~INSEE_DEP,
                 data=dat_for_mod_cl3$dat_cast,
                 weights=dat_for_mod_cl3$dat_cast$bv_area)
  summary(cl3_mod0)
  
  #Model 1: dd ratio ~ winter crops + dep
  cl3_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt + INSEE_DEP,
                 data=dat_for_mod_cl3$dat_cast,
                 weights=dat_for_mod_cl3$dat_cast$bv_area)

  cl3_mod1_diagnostics <- postprocess_model(in_mod = cl3_mod1,
                                        in_dat_for_mod = dat_for_mod_cl3)
  cl3_mod1_diagnostics$smr
  plot(cl3_mod1_diagnostics$nsp_diag)
  plot(cl3_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ winter crops + dep + orchards
  cl3_mod2 <- lm(
    ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt + orc_pc_sse + INSEE_DEP,
    data=dat_for_mod_cl3$dat_cast,
    weights=dat_for_mod_cl3$dat_cast$bv_area)
  
  cl3_mod2_diagnostics <- postprocess_model(in_mod = cl3_mod2,
                                        in_dat_for_mod = dat_for_mod_cl3)
  cl3_mod2_diagnostics$smr
  plot(cl3_mod2_diagnostics$nsp_diag)
  plot(cl3_mod2_diagnostics$resid_env_p)
  
  #Model 3: dd ratio ~ winter crops + dep + orchards + vineyards
  cl3_mod3 <- lm(
    ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt + orc_pc_sse + vny_pc_sse +
      INSEE_DEP,
    data = dat_for_mod_cl3$dat_cast,
    weights = dat_for_mod_cl3$dat_cast$bv_area)
  
  cl3_mod3_diagnostics <- postprocess_model(in_mod = cl3_mod3,
                                        in_dat_for_mod = dat_for_mod_cl3)
  cl3_mod3_diagnostics$smr
  plot(cl3_mod3_diagnostics$nsp_diag)
  plot(cl3_mod3_diagnostics$resid_env_p)
  
  #Model 4: dd ratio  ~ winter crops*dep + orchards + vineyards
  cl3_mod4 <- lm(
    ddt_to_bdtopo_ddratio_ceind ~ wcr_pc_sse_sqrt*INSEE_DEP + orc_pc_sse + vny_pc_sse,
    data = dat_for_mod_cl3$dat_cast,
    weights = dat_for_mod_cl3$dat_cast$bv_area)
  summary(cl3_mod4)
  
  cl3_mod4_diagnostics <- postprocess_model(in_mod = cl3_mod4,
                                        in_dat_for_mod = dat_for_mod_cl3)
  cl3_mod4_diagnostics$smr
  plot(cl3_mod4_diagnostics$nsp_diag)
  plot(cl3_mod4_diagnostics$resid_env_p)
  
  #Model 5: dd ratio  ~ winter crops*dep + orchards + vineyards + ire_pc_sse*dep
  cl3_mod5 <- lm(
    ddt_to_bdtopo_ddratio_ceind ~ 
      wcr_pc_sse_sqrt*INSEE_DEP +
      orc_pc_sse + vny_pc_sse +
      ire_pc_sse*INSEE_DEP,
    data = dat_for_mod_cl3$dat_cast,
    weights = dat_for_mod_cl3$dat_cast$bv_area)

  cl3_mod5_diagnostics <- postprocess_model(in_mod = cl3_mod5,
                                        in_dat_for_mod = dat_for_mod_cl3,
                                        spatial_analysis=T)
  vif(cl3_mod5)
  cl3_mod5_diagnostics$smr
  plot(cl3_mod5_diagnostics$nsp_diag)
  plot(cl3_mod5_diagnostics$resid_env_p)
  cl3_mod5_diagnostics$resids_sp_p
  cl3_mod5_diagnostics$moran_lm
  
  #Model 6: dd ratio  ~ winter crops*dep + orchards + vineyards + scr_pc_sse*dep
  cl3_mod6 <- lm(
    ddt_to_bdtopo_ddratio_ceind ~ 
      wcr_pc_sse_sqrt*INSEE_DEP +
      orc_pc_sse + vny_pc_sse +
      scr_pc_sse*INSEE_DEP,
    data = dat_for_mod_cl3$dat_cast,
    weights = dat_for_mod_cl3$dat_cast$bv_area)
  vif(cl3_mod6)
  
  cl3_mod6_diagnostics <- postprocess_model(in_mod = cl3_mod6,
                                        in_dat_for_mod = dat_for_mod_cl3,
                                        spatial_analysis=F)
  cl3_mod6_diagnostics$smr
  plot(cl3_mod6_diagnostics$nsp_diag)
  plot(cl3_mod6_diagnostics$resid_env_p)
  #cl3_mod6_diagnostics$resids_sp_p
  
  #mod 7
  cl3_mod7 <- lm(
    ddt_to_bdtopo_ddratio_ceind ~ 
      wcr_pc_sse_sqrt*INSEE_DEP +
      orc_pc_sse + vny_pc_sse +
      ire_pc_sse*INSEE_DEP +
      ppc_pk_sav_sqrt,
    data = dat_for_mod_cl3$dat_cast,
    weights = dat_for_mod_cl3$dat_cast$bv_area)
  
  summary(cl3_mod7)

  cl3_chosen_model <- list(cl3_mod5, cl3_mod5_diagnostics)
  #The chosen model is the fifth:
  # ddt_to_bdtopo_ddratio_ceind ~ wcr_pc_sse_sqrt*INSEE_DEP +
  #                               orc_pc_sse + vny_pc_sse +
  #                               ire_pc_sse*INSEE_DEP
  # All agriculturally driven. Summer crops are also correlated but 
  # autocorrelated with winter crops, so kept winter crops.
  # Spatial autocorrelation test is non-significant but there are clusteres
  # of substantial residuals (in southern Ain notably)
  
  #Class 4 model - 15 departments ----------------------------------------------
  print('Selecting model for class 4')
  
  dat_for_mod_cl4 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=4) 
  
  dat_for_mod_cl4$ddratio_env_plot
  check <- dat_for_mod_cl4$dat[, mean(cor), by= c('variable')]
  
  ggplot(dat_for_mod_cl4$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl4$dat_cast, 
         aes(x=sqrt(wcr_pc_sse), y=ddt_to_bdtopo_ddratio_ceind,
             color=INSEE_DEP)) +
    geom_point() +
    geom_smooth(method='lm', se=F)
  
  dat_for_mod_cl4$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl4$dat_cast[, wcr_pc_sse_sqrt := sqrt(wcr_pc_sse)]
  dat_for_mod_cl4$dat_cast[, ppc_pk_sav_sqrt := sqrt(ppc_pk_sav)]
  
  #Model 0: null model - dd ratio ~ dep
  cl4_mod0 <- lm(ddt_to_bdtopo_ddratio_ceind~INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  summary(cl4_mod0)
  
  #Model 1: dd ratio ~ winter crops + dep
  cl4_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt + INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  
  cl4_mod1_diagnostics <- postprocess_model(in_mod = cl4_mod1,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod1_diagnostics$smr
  plot(cl4_mod1_diagnostics$nsp_diag)
  plot(cl4_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ winter crops*dep
  cl4_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt*INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  
  cl4_mod2_diagnostics <- postprocess_model(in_mod = cl4_mod2,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod2_diagnostics$smr
  plot(cl4_mod2_diagnostics$nsp_diag)
  plot(cl4_mod2_diagnostics$resid_env_p)
  
  #Model 3: dd ratio ~ mean annual aridity + dep
  cl4_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind~ari_ix_syr + INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  
  cl4_mod3_diagnostics <- postprocess_model(in_mod = cl4_mod3,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod3_diagnostics$smr
  plot(cl4_mod3_diagnostics$nsp_diag)
  plot(cl4_mod3_diagnostics$resid_env_p)
  
  #Model 4: dd ratio ~ mean annual aridity*dep
  cl4_mod4 <- lm(ddt_to_bdtopo_ddratio_ceind~ari_ix_syr*INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  
  cl4_mod4_diagnostics <- postprocess_model(in_mod = cl4_mod4,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod4_diagnostics$smr
  plot(cl4_mod4_diagnostics$nsp_diag)
  plot(cl4_mod4_diagnostics$resid_env_p)
  
  #Model 5: dd ratio ~ wintercrop*dep + mean annual aridity*dep
  cl4_mod5 <- lm(ddt_to_bdtopo_ddratio_ceind~ari_ix_syr*INSEE_DEP + 
                   wcr_pc_sse_sqrt*INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)

  cl4_mod5_diagnostics <- postprocess_model(in_mod = cl4_mod5,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod5_diagnostics$smr
  plot(cl4_mod5_diagnostics$nsp_diag)
  plot(cl4_mod5_diagnostics$resid_env_p)
  
  #Model 6: dd ratio ~ wintercrop*dep + predicted percentage intermittency*dep
  cl4_mod6 <- lm(ddt_to_bdtopo_ddratio_ceind~irs_pc_sav*INSEE_DEP + 
                   wcr_pc_sse_sqrt*INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  
  cl4_mod6_diagnostics <- postprocess_model(in_mod = cl4_mod6,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod6_diagnostics$smr
  plot(cl4_mod6_diagnostics$nsp_diag)
  plot(cl4_mod6_diagnostics$resid_env_p)
  cl4_mod6_diagnostics$resids_sp_p + 
    scale_fill_distiller(palette="Spectral",
                         limits=c(-0.4, 0.4))
  cl4_mod6_diagnostics$moran_lm
  cl4_mod6_diagnostics$LMtests
  
  #Model 7: dd ratio ~ wintercrop*dep + predicted percentage intermittency*dep +
  #                     summercrop
  cl4_mod7 <- lm(ddt_to_bdtopo_ddratio_ceind~irs_pc_sav*INSEE_DEP + 
                   wcr_pc_sse_sqrt*INSEE_DEP +
                   scr_pc_sse*INSEE_DEP,
                 data=dat_for_mod_cl4$dat_cast,
                 weights=dat_for_mod_cl4$dat_cast$bv_area)
  
  cl4_mod7_diagnostics <- postprocess_model(in_mod = cl4_mod7,
                                        in_dat_for_mod = dat_for_mod_cl4)
  cl4_mod7_diagnostics$smr
  plot(cl4_mod7_diagnostics$nsp_diag)
  plot(cl4_mod7_diagnostics$resid_env_p)
  #Too much becomes insignificant
  
  cl4_chosen_model <- list(cl4_mod6, cl4_mod6_diagnostics)
  
  #Class 5 model - 60 departments ----------------------------------------------
  print('Selecting model for class 5')
  
  dat_for_mod_cl5 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=5) 
  
  dat_for_mod_cl5$dat_cast[
    is.na(ari_ix_ssu), 
    ari_ix_ssu := mean(dat_for_mod_cl5$dat_cast$ari_ix_ssu, na.rm=T)]
  
  dat_for_mod_cl5$ddratio_env_plot
  check <- dat_for_mod_cl5$dat[, mean(cor), by= c('variable')]
  
  ggplot(dat_for_mod_cl5$dat_cast, aes(x=sqrt(ddt_to_bdtopo_ddratio_ceind))) +
    geom_histogram() 
  ggplot(dat_for_mod_cl5$dat_cast, 
         aes(x=sqrt(wcr_pc_sse), y=sqrt(ddt_to_bdtopo_ddratio_ceind),
             color=INSEE_DEP)) +
    geom_point() +
    geom_smooth(method='lm', se=F)
  
  dat_for_mod_cl5$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl5$dat_cast[, wcr_pc_sse_sqrt := sqrt(wcr_pc_sse)]
  dat_for_mod_cl5$dat_cast[, ppc_pk_sav_sqrt := sqrt(ppc_pk_sav)]
  
  dat_for_mod_cl5$dat_cast[duplicated(ddt_to_bdtopo_ddratio_ceind),]
  
  #Null model 
  cl5_mod0 <- lm(ddt_to_bdtopo_ddratio_ceind~INSEE_DEP,
                 data=dat_for_mod_cl5$dat_cast,
                 weights=dat_for_mod_cl5$dat_cast$bv_area)
  summary(cl5_mod0)
  
  #Model 1: dd ratio ~ winter crops + dep
  cl5_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt + INSEE_DEP,
                 data=dat_for_mod_cl5$dat_cast,
                 weights=dat_for_mod_cl5$dat_cast$bv_area)
  summary(cl5_mod1)
  
  #Model 2: dd ratio ~ winter crops*dep
  cl5_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt*INSEE_DEP,
                 data=dat_for_mod_cl5$dat_cast,
                 weights=dat_for_mod_cl5$dat_cast$bv_area)
  summary(cl5_mod2)
  
  
  #Model 3: dd ratio ~ winter crops*dep + summer aridity
  cl5_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt*INSEE_DEP + 
                   ari_ix_ssu,
                 data=dat_for_mod_cl5$dat_cast,
                 weights=dat_for_mod_cl5$dat_cast$bv_area)
  summary(cl5_mod3)

  cl5_mod3_diagnostics <- postprocess_model(in_mod = cl5_mod3,
                                        in_dat_for_mod = dat_for_mod_cl5,
                                        spatial_analysis = F)
  cl5_mod3_diagnostics$smr
  plot(cl5_mod3_diagnostics$nsp_diag)
  plot(cl5_mod3_diagnostics$resid_env_p)
  
  #Model 4: dd ratio ~ winter crops*dep + summer aridity*dep
  cl5_mod4 <- lm(ddt_to_bdtopo_ddratio_ceind~wcr_pc_sse_sqrt*INSEE_DEP + 
                   ari_ix_ssu*INSEE_DEP,
                 data=dat_for_mod_cl5$dat_cast,
                 weights=dat_for_mod_cl5$dat_cast$bv_area)
  summary(cl5_mod4)
  
  cl5_chosen_model <- list(cl5_mod3, cl5_mod3_diagnostics)
  #############NMOD 3 is chosen. need to finish diagnostics, but launch it when got time
  
  
  #Class 6 model - Landes, Gironde, Indre --------------------------------------
  print('Selecting model for class 6')
  
  dat_for_mod_cl6 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=6) 
  
  ## determine the column names that contain NA values
  nm <- names(dat_for_mod_cl6$dat_cast)[colSums(is.na(dat_for_mod_cl6$dat_cast)) != 0]
  ## replace with the mean - by 'id'
  dat_for_mod_cl6$dat_cast[, (nm) := lapply(nm, function(x) {
    x <- get(x)
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }), by = 'INSEE_DEP']
  
  dat_for_mod_cl6$ddratio_env_plot
  check <- dat_for_mod_cl6$dat[, mean(cor), by= c('variable')]
  
  ggplot(dat_for_mod_cl6$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl6$dat_cast, 
         aes(x=sqrt(pst_pc_sse), y=ddt_to_bdtopo_ddratio_ceind,
             color=INSEE_DEP)) +
    geom_point() +
    geom_smooth(method='lm', se=F)
  
  dat_for_mod_cl6$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl6$dat_cast[, pst_pc_sse_sqrt := sqrt(pst_pc_sse)]
  #dat_for_mod_cl6$dat_cast[, ppc_pk_sav_sqrt := sqrt(ppc_pk_sav)]
  
  #Model 0: null model - dd ratio ~ dep
  cl6_mod0 <- lm(ddt_to_bdtopo_ddratio_ceind~INSEE_DEP,
                 data=dat_for_mod_cl6$dat_cast,
                 weights=dat_for_mod_cl6$dat_cast$bv_area)
  summary(cl6_mod0)
  
  #Model 1: dd ratio ~ pasture + dep
  cl6_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~pst_pc_sse_sqrt + INSEE_DEP,
                 data=dat_for_mod_cl6$dat_cast,
                 weights=dat_for_mod_cl6$dat_cast$bv_area)
  
  cl6_mod1_diagnostics <- postprocess_model(in_mod = cl6_mod1,
                                        in_dat_for_mod = dat_for_mod_cl6)
  cl6_mod1_diagnostics$smr
  plot(cl6_mod1_diagnostics$nsp_diag)
  plot(cl6_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ pasture + irs_pc_sav*dep
  cl6_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind~pst_pc_sse_sqrt + 
                   irs_pc_sav*INSEE_DEP,
                 data=dat_for_mod_cl6$dat_cast,
                 weights=dat_for_mod_cl6$dat_cast$bv_area)
  
  cl6_mod2_diagnostics <- postprocess_model(in_mod = cl6_mod2,
                                        in_dat_for_mod = dat_for_mod_cl6)
  cl6_mod2_diagnostics$smr
  plot(cl6_mod2_diagnostics$nsp_diag)
  plot(cl6_mod2_diagnostics$resid_env_p)
  
  #Model 3: dd ratio ~ pasture + irs_pc_sav*dep
  cl6_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind~pst_pc_sse_sqrt*INSEE_DEP + 
                   irs_pc_sav*INSEE_DEP,
                 data=dat_for_mod_cl6$dat_cast,
                 weights=dat_for_mod_cl6$dat_cast$bv_area)
  
  cl6_mod3_diagnostics <- postprocess_model(in_mod = cl6_mod3,
                                        in_dat_for_mod = dat_for_mod_cl6)
  cl6_mod3_diagnostics$smr
  plot(cl6_mod3_diagnostics$nsp_diag)
  plot(cl6_mod3_diagnostics$resid_env_p)
  
  #Model 4: dd ratio ~ pasture*dep + irs_pc_sav*dep + imperviousness
  cl6_mod4 <- lm(ddt_to_bdtopo_ddratio_ceind~pst_pc_sse_sqrt*INSEE_DEP + 
                   irs_pc_sav*INSEE_DEP + imp_pc_sse,
                 data=dat_for_mod_cl6$dat_cast[!72,], #Basedon cook's distance outlier detection
                 weights=dat_for_mod_cl6$dat_cast[!72, bv_area])
  
  cl6_mod4_diagnostics <- postprocess_model(in_mod = cl6_mod4,
                                        in_dat_for_mod = dat_for_mod_cl6,
                                        removed_index = 72)
  cl6_mod4_diagnostics$smr
  plot(cl6_mod4_diagnostics$nsp_diag)
  plot(cl6_mod4_diagnostics$resid_env_p)
  cl6_mod4_diagnostics$resids_sp_p
  cl6_mod4_diagnostics$moran_lm
  
  #Model 5: dd ratio ~ pasture*dep + irs_pc_sav*dep + imperviousness + summer aridity
  cl6_mod5 <- lm(ddt_to_bdtopo_ddratio_ceind~pst_pc_sse_sqrt*INSEE_DEP + 
                   irs_pc_sav*INSEE_DEP + imp_pc_sse + ari_ix_ssu,
                 data=dat_for_mod_cl6$dat_cast[!72,], #Basedon cook's distance outlier detection
                 weights=dat_for_mod_cl6$dat_cast[!72, bv_area])
  
  cl6_mod5_diagnostics <- postprocess_model(in_mod = cl6_mod5,
                                        in_dat_for_mod = dat_for_mod_cl6,
                                        removed_index = 72)
  cl6_mod5_diagnostics$smr
  plot(cl6_mod5_diagnostics$nsp_diag)
  plot(cl6_mod5_diagnostics$resid_env_p)
  cl6_mod5_diagnostics$resids_sp_p
  cl6_mod5_diagnostics$moran_lm
  
  cl6_chosen_model <- list(cl6_mod5, cl6_mod5_diagnostics)
  #Model 5 is the chosen model
  
  #Class 7 model - Seine-Maritime, Charente-Maritime ---------------------------
  print('Selecting model for class 7')
  
  dat_for_mod_cl7 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=7) 
  
  dat_for_mod_cl7$ddratio_env_plot + geom_smooth(method='lm', se=F)
  check <- dat_for_mod_cl7$dat[, mean(cor), by= c('variable')]
  
  ggplot(dat_for_mod_cl7$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl7$dat_cast, 
         aes(x=ari_ix_syr, y=ddt_to_bdtopo_ddratio_ceind,
             color=factor(INSEE_DEP) )) +
    geom_point() +
    geom_smooth(method='lm', se=F, aes(weight=bv_area))
  
  dat_for_mod_cl7$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl7$dat_cast[, ppc_pk_sav_sqrt := sqrt(ppc_pk_sav)]

  ## determine the column names that contain NA values
  nm <- names(dat_for_mod_cl7$dat_cast)[
    colSums(is.na(dat_for_mod_cl7$dat_cast)) != 0]
  ## replace with the mean - by 'id'
  dat_for_mod_cl7$dat_cast[, (nm) := lapply(nm, function(x) {
    x <- get(x)
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }), by = 'INSEE_DEP']
  
  dat_for_mod_cl7$dat_cast[is.infinite(bar_bk_ssu_TOTAL), 
                           bar_bk_ssu_TOTAL := 0]
  
  #Model 0: null model - dd ratio ~ dep
  cl7_mod0 <- lm(ddt_to_bdtopo_ddratio_ceind~INSEE_DEP,
                 data=dat_for_mod_cl7$dat_cast,
                 weights=dat_for_mod_cl7$dat_cast$bv_area)
  summary(cl7_mod0)
  
  #Model 1: dd ratio ~ annual aridity*dep
  cl7_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ari_ix_syr*INSEE_DEP,
                 data=dat_for_mod_cl7$dat_cast,
                 weights=dat_for_mod_cl7$dat_cast$bv_area)
  
  cl7_mod1_diagnostics <- postprocess_model(in_mod = cl7_mod1,
                                        in_dat_for_mod = dat_for_mod_cl7,
                                        spatial_analysis = T)
  cl7_mod1_diagnostics$smr
  cl7_mod1_diagnostics$resids_sp_p
  plot(cl7_mod1_diagnostics$nsp_diag)
  plot(cl7_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ manual aridity*dep  + AWC*dep
  cl7_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ari_ix_syr*INSEE_DEP + 
                   awc_mm_sav*INSEE_DEP,
                 data=dat_for_mod_cl7$dat_cast,
                 weights=dat_for_mod_cl7$dat_cast$bv_area)
  
  cl7_mod2_diagnostics <- postprocess_model(
    in_mod = cl7_mod2,
    in_dat_for_mod = dat_for_mod_cl7,
    spatial_analysis = T
  )
  
  cl7_mod2_diagnostics$smr
  cl7_mod2_diagnostics$resids_sp_p
  plot(cl7_mod2_diagnostics$nsp_diag)
  plot(cl7_mod2_diagnostics$resid_env_p)
  
  #Model 3: dd ratio ~ manual aridity*dep  + AWC*dep + population density
  cl7_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ari_ix_syr*INSEE_DEP + 
                   awc_mm_sav*INSEE_DEP + ppc_pk_sav_sqrt ,
                 data=dat_for_mod_cl7$dat_cast,
                 weights=dat_for_mod_cl7$dat_cast$bv_area)
  
  cl7_mod3_diagnostics <- postprocess_model(
    in_mod = cl7_mod3,
    in_dat_for_mod = dat_for_mod_cl7,
    spatial_analysis = T
  )
  
  cl7_mod3_diagnostics$smr
  cl7_mod3_diagnostics$resids_sp_p
  plot(cl7_mod3_diagnostics$nsp_diag)
  plot(cl7_mod3_diagnostics$resid_env_p)
  cl7_mod3_diagnostics$moran_lm
  cl7_mod3_diagnostics$LMtests
  
  #Model 4: dd ratio ~ manual aridity*dep  + AWC*dep + population density +
  #                     pst_pc_sse
  cl7_mod4 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ari_ix_syr*INSEE_DEP + 
                   awc_mm_sav*INSEE_DEP + ppc_pk_sav_sqrt + pst_pc_sse,
                 data=dat_for_mod_cl7$dat_cast,
                 weights=dat_for_mod_cl7$dat_cast$bv_area)
  
  cl7_mod4_diagnostics <- postprocess_model(
    in_mod = cl7_mod4,
    in_dat_for_mod = dat_for_mod_cl7,
    spatial_analysis = F
  )
  
  cl7_mod4_diagnostics$smr

  
  #Model 5: dd ratio ~ manual aridity*dep  + pasture*dep + population density 
  cl7_mod5 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ari_ix_syr*INSEE_DEP + 
                   pst_pc_sse*INSEE_DEP + ppc_pk_sav_sqrt,
                 data=dat_for_mod_cl7$dat_cast,
                 weights=dat_for_mod_cl7$dat_cast$bv_area)
  
  cl7_mod5_diagnostics <- postprocess_model(
    in_mod = cl7_mod5,
    in_dat_for_mod = dat_for_mod_cl7,
    spatial_analysis = T
  )
  
  cl7_mod5_diagnostics$smr
  cl7_mod5_diagnostics$resids_sp_p
  plot(cl7_mod5_diagnostics$nsp_diag)
  plot(cl7_mod5_diagnostics$resid_env_p)
  cl7_mod5_diagnostics$moran_lm
  cl7_mod5_diagnostics$LMtests
  
  cl7_chosen_model <- list(cl7_mod3, cl7_mod3_diagnostics)
  #Model 3 and Model 5 are two potential models
  #Model 5 which includes pasture is less prefered, albeit higher explanatory power
  #because the direction of the relationship with pasture is opposite between the departments
  #and stronger residual spatial autocorrelation
  
  #Class 8 model - Yvelines ----------------------------------------------------
  print('Selecting model for class 8')
  
  dat_for_mod_cl8 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=8) 
  
  dat_for_mod_cl8$ddratio_env_plot
  check <- dat_for_mod_cl8$dat[, mean(cor), by= c('variable')]
  
  ggplot(dat_for_mod_cl8$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 

  
  dat_for_mod_cl8$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl8$dat_cast[, ppc_pk_sav_sqrt := sqrt(ppc_pk_sav)]
  dat_for_mod_cl8$dat_cast[, bar_bk_ssu_TOTAL_sqrt := sqrt(bar_bk_ssu_TOTAL)]
  
  ggplot(dat_for_mod_cl8$dat_cast, 
         aes(x= bar_bk_ssu_TOTAL_sqrt, y=ddt_to_bdtopo_ddratio_ceind,
             color=INSEE_DEP, size=bv_area)) +
    geom_point() +
    geom_smooth(method='lm', se=F)

  #Model 1: dd ratio ~ winter crops + dep
  cl8_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind~bar_bk_ssu_TOTAL_sqrt,
                 data=dat_for_mod_cl8$dat_cast,
                 weights=dat_for_mod_cl8$dat_cast$bv_area)
  
  cl8_mod1_diagnostics <- postprocess_model(in_mod = cl8_mod1,
                                        in_dat_for_mod = dat_for_mod_cl8)
  cl8_mod1_diagnostics$smr
  plot(cl8_mod1_diagnostics$nsp_diag)
  plot(cl8_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ winter crops + dep
  cl8_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind~bar_bk_ssu_TOTAL_sqrt + slo_dg_sav,
                 data=dat_for_mod_cl8$dat_cast,
                 weights=dat_for_mod_cl8$dat_cast$bv_area)
  
  cl8_mod2_diagnostics <- postprocess_model(in_mod = cl8_mod2,
                                        in_dat_for_mod = dat_for_mod_cl8,
                                        spatial_analysis = T)
  cl8_mod2_diagnostics$smr
  plot(cl8_mod2_diagnostics$nsp_diag)
  plot(cl8_mod2_diagnostics$resid_env_p)
  plot(cl8_mod2_diagnostics$resids_sp_p)
  cl8_mod2_diagnostics$moran_lm

  #Choose mod 2
  cl8_chosen_model <- list(cl8_mod2, cl8_mod2_diagnostics)
  
  #Class 9 model - Nord, Bouches-du-Rhones -------------------------------------
  print('Selecting model for class 9')
  
  dat_for_mod_cl9 <- get_dat_formod(
    in_dt_melt=env_dd_melt_gclass, 
    in_envdd_multivar_analysis=in_envdd_multivar_analysis, 
    in_bvdep_inters_vect = bvdep_inters_vect,
    in_gclass=9) 
  
  dat_for_mod_cl9$ddratio_env_plot
  check <- dat_for_mod_cl9$dat[, mean(cor), by= c('variable')]
  
  #Exclude the four sub-basins with ratios over 3.5
  dat_for_mod_cl9$dat_cast <- dat_for_mod_cl9$dat_cast[
    ddt_to_bdtopo_ddratio_ceind < 3.5,]
  
  ggplot(dat_for_mod_cl9$dat_cast, aes(x=ddt_to_bdtopo_ddratio_ceind)) +
    geom_histogram() 
  ggplot(dat_for_mod_cl9$dat_cast, 
         aes(x=sqrt(ire_pc_sse), y=ddt_to_bdtopo_ddratio_ceind,
             color=factor(INSEE_DEP))) +
    geom_point() +
    geom_smooth(method='lm', se=F, aes(weight=bv_area))
  
  dat_for_mod_cl9$dat_cast[, INSEE_DEP := as.factor(INSEE_DEP)]
  dat_for_mod_cl9$dat_cast[, ire_pc_sse_sqrt := sqrt(ire_pc_sse)]
  
  #Model 0: null model - dd ratio ~ dep
  cl9_mod0 <- lm(ddt_to_bdtopo_ddratio_ceind~INSEE_DEP,
                 data=dat_for_mod_cl9$dat_cast,
                 weights=dat_for_mod_cl9$dat_cast$bv_area)
  summary(cl9_mod0)
  
  #Model 1: dd ratio ~ irrigation*dep
  cl9_mod1 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ire_pc_sse_sqrt*INSEE_DEP,
                 data=dat_for_mod_cl9$dat_cast,
                 weights=dat_for_mod_cl9$dat_cast$bv_area)
  
  cl9_mod1_diagnostics <- postprocess_model(in_mod = cl9_mod1,
                                        in_dat_for_mod = dat_for_mod_cl9)
  cl9_mod1_diagnostics$smr
  plot(cl9_mod1_diagnostics$nsp_diag)
  plot(cl9_mod1_diagnostics$resid_env_p)
  
  #Model 2: dd ratio ~ irrigatoin*dep + winter crops
  cl9_mod2 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ire_pc_sse_sqrt*INSEE_DEP +
                   wcr_pc_sse,
                 data=dat_for_mod_cl9$dat_cast,
                 weights=dat_for_mod_cl9$dat_cast$bv_area)
  
  cl9_mod2_diagnostics <- postprocess_model(in_mod = cl9_mod2,
                                        in_dat_for_mod = dat_for_mod_cl9,
                                        spatial_analysis = T)
  cl9_mod2_diagnostics$smr
  plot(cl9_mod2_diagnostics$nsp_diag)
  plot(cl9_mod2_diagnostics$resid_env_p)
  cl9_mod2_diagnostics$moran_lm
  cl9_mod2_diagnostics$LMtests
  
  cl9_mod2_diagnostics$smr_sem
  cl9_mod2_pseudoR2 <- cor(cl9_mod2_diagnostics$sem$y, 
                           cl9_mod2_diagnostics$sem$fitted.values)^2
  
  #Model 3: dd ratio ~ irrigatoin*dep + pasture
  cl9_mod3 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ire_pc_sse_sqrt*INSEE_DEP +
                   pst_pc_sse,
                 data=dat_for_mod_cl9$dat_cast,
                 weights=dat_for_mod_cl9$dat_cast$bv_area)
  
  cl9_mod3_diagnostics <- postprocess_model(in_mod = cl9_mod3,
                                        in_dat_for_mod = dat_for_mod_cl9)
  cl9_mod3_diagnostics$smr

  #Model 4: dd ratio ~ irrigatoin*dep + winter crops + imperviousness
  cl9_mod4 <- lm(ddt_to_bdtopo_ddratio_ceind ~ ire_pc_sse_sqrt*INSEE_DEP +
                   wcr_pc_sse + imp_pc_sse,
                 data=dat_for_mod_cl9$dat_cast,
                 weights=dat_for_mod_cl9$dat_cast$bv_area)
  
  dat_for_mod_cl9$dat_cast[, which(is.na(wcr_pc_sse))]
  
  cl9_mod4_diagnostics <- postprocess_model(in_mod = cl9_mod4,
                                        in_dat_for_mod = dat_for_mod_cl9,
                                        spatial_analysis=T,
                                        plot_nbs = T)
  cl9_mod4_diagnostics$smr
  vif(cl9_mod4)
  vif(cl9_mod2)
  plot(cl9_mod4_diagnostics$nsp_diag)
  plot(cl9_mod4_diagnostics$resid_env_p)
  cl9_mod4_diagnostics$moran_lm
  cl9_mod4_diagnostics$LMtests
  
  cl9_mod4_diagnostics$smr_sem
  cl9_mod4_pseudoR2 <- cor(cl9_mod4_diagnostics$sem$y, 
                           cl9_mod4_diagnostics$sem$fitted.values)^2
  
  #Model 2 and 4 are both good candidates but mod4 has hiher explanatory power.
  #Model 4 displays a stronger residual spatial autocorrelation. 
  #VIF of mod4 is only slightly higher than mod2.
  #Several areas of Nort have very low drainage density in BD Topo and have been 
  #completed with other sources by DDT, so large differences in ratio
  cl9_chosen_model <- list(cl9_mod4, cl9_mod4_diagnostics)
  
  
  #Return ----------------------------------------------------------------------
  return(list(
   cl1 = cl1_chosen_model,
   cl2 = cl2_chosen_model,
   cl3 = cl3_chosen_model,
   cl4 = cl4_chosen_model,
   cl5 = cl5_chosen_model,
   cl6 = cl6_chosen_model,
   cl7 = cl7_chosen_model,
   cl8 = cl8_chosen_model,
   cl9 = cl9_chosen_model
  ))
}

#-------------------------- tabulate env_dd models -----------------------------
in_mods_envdd <- tar_read(mods_envdd)

tabulate_mods <- function(in_mods_envdd) {
  
  
}

#-------------------------- analyze vulnerable waters --------------------------
# 
# in_drainage_density_summary = tar_read(drainage_density_summary)
# in_ddtnets_strahler = tar_read(ddtnets_strahler)
# in_ddtnets_bvinters = tar_read(ddtnets_bvinters)
# in_bdtopo_strahler = tar_read(bdtopo_strahler)
# in_bdtopo_bvinters = tar_read(bdtopo_bvinters)

analyze_vulnerable_waters <- function(in_drainage_density_summary,
                                      in_bvdep_inters_gdb_path,
                                      in_ddtnets_strahler,
                                      in_bdtopo_strahler,
                                      in_bdtopo_bvinters
    ) {
   names(in_bdtopo_bvinters)
   names(in_bdtopo_strahler)
   
   ddtnets_dat <- merge(in_ddtnets_bvinters,
                        in_ddtnets_strahler[, c('UID_CE', 'strahler'), with=F])
  
   bdtopo_dat <- merge(in_bdtopo_bvinters,
                       in_bdtopo_strahler[, c('ID', 'strahler'), with=F],
                       by='ID')
   
   
}

#-------------------------- plotmap_envdd_cors --------------------------------------
# in_envdd_multivar_analysis = tar_read(envdd_multivar_analysis)
# in_env_dd_merged_bv <- tar_read(env_dd_merged_bv)
# in_bvdep_inters_gdb_path <- tar_read(bvdep_inters_gdb_path)
# in_ddtnets_path <-  tar_read(ddtnets_path)
# in_deps_path = tar_read(deps_shp_path)

plotmap_envdd_cors <- function(in_envdd_multivar_analysis,
                               in_env_dd_merged_bv,
                               in_bvdep_inters_gdb_path,
                               in_ddtnets_path,
                               in_deps_path) {
  
  #Join environmental and drainage density attributes to basin vector
  bvdep_inters_vect <- vect(dirname(in_bvdep_inters_gdb_path),
                            layer=basename(in_bvdep_inters_gdb_path))
  
  bvdep_env_vect <- merge(bvdep_inters_vect,
                          in_env_dd_merged_bv, 
                          by=c('UID_BV',"INSEE_DEP", "PFAF_ID08", "PFAF_ID09"),
                          all.x=T)
  
  #Read in french departments 
  deps_vect <-  vect(in_deps_path) %>%
    terra::simplifyGeom(tolerance=1000,
                        preserveTopology=T,
                        makeValid=T)
  
  #Make an overall map of French departments
  dep_map_all <- ggplot(deps_vect[deps_vect$INSEE_REG != 94]) + 
    geom_spatvector(fill='white', color='grey') + 
    theme_minimal()
  
  #Subset correlation data table to only keep those > 0.5 and get associated
  #drainage density and environmental data
  env_dd_cor_o05 <- in_envdd_multivar_analysis$env_dd_dep_cor[
    abs(cor)>=0.5,] %>%
    merge(in_envdd_multivar_analysis$env_dd_melt, .,
          by=c('NOM', 'INSEE_DEP', 'description')) 
  
  
  
  #Function to map classified drainage network and environmental variable
  map_env_ddtnet_dep <- function(in_dep_num,
                                 in_dep_name,
                                 in_var_descr,
                                 in_driver,
                                 in_ddtnets_path,
                                 in_bvdep_env_vect,
                                 in_deps_vect,
                                 in_dep_map_all
  ) {
    
    dep_netlyr <- paste0('D', in_dep_num, '_', gsub('-', '_', in_dep_name))
    
    SQLquery <- paste0("SELECT * FROM ",
                       basename(in_ddtnets_path),
                       " WHERE orig_layer = '", dep_netlyr, "'")
    
    type_stand_levels <- c("Cours d'eau", "Indéterminé",
                           "Non cours d'eau", "Inexistant")
    type_stand_labels <- c("Watercourse", "Unclassified",
                           "Non-watercourse", "Inexistant")
    ddtnet_dep <- vect(
      x = dirname(in_ddtnets_path),
      layer = basename(in_ddtnets_path),
      query = SQLquery
    ) 
    ddtnet_dep$type_stand <- factor(ddtnet_dep$type_stand, 
                                    levels=type_stand_levels)
    ddtnet_dep <- ddtnet_dep[ddtnet_dep$type_stand != "Hors département",]
    #Subset basins for the department
    bvdep_env_sub <- in_bvdep_env_vect[
      as.numeric(in_bvdep_env_vect$INSEE_DEP)==in_dep_num,]
    
    #Make a map of French departments with the highlighted department
    deps_vect_sub <- in_deps_vect[as.numeric(in_deps_vect$INSEE_DEP)==in_dep_num,]
    
    dep_map_sub <- dep_map_all + 
      geom_spatvector(data=deps_vect_sub,
                      fill='black', color='black') +
      theme_classic() + 
      coord_sf(expand = FALSE) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    #Plot
    main_map <- ggplot(bvdep_env_sub) +
      tidyterra::geom_spatvector(aes(fill=get(as.character(in_driver))), 
                                 alpha=0.75) +
      tidyterra::geom_spatvector(data=ddtnet_dep,
                                 aes(color=type_stand),
                                 key_glyph = 'timeseries') +
      tidyterra::scale_fill_whitebox_c(name=in_var_descr, 
                                       palette='arid') +
      scale_color_manual(
        name = 'Watercourse status',
        values=c("#0b5da2ff", "#138f60ff", #https://thenode.biologists.com/data-visualization-with-flying-colors/research/
                 "#c94905ff", "#de8e08ff"),  #cours d'eau, indéterminé, non cours d'eau, inexistant
        labels=type_stand_labels,
        guide = "none") + 
      #guides(color = guide_legend(override.aes = list(linewidth = 1.3))) +
      coord_sf(expand = FALSE) +
      ggspatial::annotation_scale(style='ticks') +
      theme_classic() + 
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal"
      )
    
    inset_layout <- "
    AAAB
    AAA#
    AAA#"
    
    out_map <-  main_map + 
      dep_map_sub +
      plot_layout(design=inset_layout) +
      plot_annotation(in_dep_name)
    
    return(out_map)
  }
  
  scatter_env_ddtnet <-  function(in_dt_driver) {
    p <- ggplot(in_dt_driver,
                aes(x=value,
                    y=ddt_to_bdtopo_ddratio_ceind)) +
      geom_point(aes(size=bv_area, 
                     color=cor)) +
      geom_smooth(#aes(weight=bv_area),
        method='gam',
        #family='lognormal',
        formula = y ~ s(x, bs = "cs", fx = TRUE, k =3),
        color='black') +
      scale_x_continuous(name=unique(in_dt_driver$description)) +
      scale_y_continuous(name='Ratio of drainage density:
                         length(watercourses + unclassified)/length(BD TOPO)') +
      scale_size_continuous(name="Sub-basin area<br>(km<sup>2</sup>)",
                            breaks=c(20, 50, 100, 250, 500)) +
      scale_color_distiller(name='rho',
                            palette='RdBu', 
                            limits=c(-0.81, 0.81),
                            breaks=c(-0.8, -0.5, 0, 0.5, 0.8)) +
      coord_cartesian(ylim=c(0, 
                             min(2,
                                 max(in_dt_driver$ddt_to_bdtopo_ddratio_ceind)+0.1)
      ), 
      expand=FALSE) +
      facet_wrap(~NOM+paste('rho =', round(cor,2)), 
                 scales='free_x') +
      theme_classic() +
      theme(panel.grid.minor=element_blank(),
            strip.background = element_rect(fill="gray95",
                                            color="gray95",
                                            linewidth=NULL),
            legend.title = ggtext::element_markdown()
      )
    return(p)
  } 
  
  driver_dd_plots <- lapply(
    unique(env_dd_cor_o05$variable), function(driver) {
      print(paste("Creating plot for", driver))
      
      dt_driver <- env_dd_cor_o05[variable==driver,]
      dt_driver[, NOM := factor(NOM, levels=unique(dt_driver$NOM[order(cor)]))]
      
      #----------------------- CREATE MAPS -------------------------------------
      if (min(dt_driver$cor)<0) {
        min_dep_dt <- dt_driver[cor==min(cor),]
        dep_num <- unique(min_dep_dt$INSEE_DEP)
        dep_name <- unique(min_dep_dt$NOM)
        var_descr <- unique(min_dep_dt$description)
        
        p_map_min <- map_env_ddtnet_dep(in_dep_num = dep_num,
                                        in_dep_name = dep_name,
                                        in_var_descr = var_descr,
                                        in_driver = driver,
                                        in_ddtnets_path = in_ddtnets_path,
                                        in_bvdep_env_vect = bvdep_env_vect,
                                        in_deps_vect = deps_vect,
                                        in_dep_map_all = dep_map_all
        )
      } else {
        p_map_min <- plot_spacer()
      }
      
      if (max(dt_driver$cor)>0) {
        min_dep_dt <- dt_driver[cor==max(cor),]
        dep_num <- unique(min_dep_dt$INSEE_DEP)
        dep_name <- unique(min_dep_dt$NOM)
        var_descr <- unique(min_dep_dt$description)
        
        p_map_max <- map_env_ddtnet_dep(in_dep_num = dep_num,
                                        in_dep_name = dep_name,
                                        in_var_descr = var_descr,
                                        in_driver = driver,
                                        in_ddtnets_path = in_ddtnets_path,
                                        in_bvdep_env_vect = bvdep_env_vect,
                                        in_deps_vect = deps_vect,
                                        in_dep_map_all = dep_map_all
        )
      } else {
        p_map_max <- plot_spacer()
      }
      
      p_scatter <- scatter_env_ddtnet(in_dt_driver=dt_driver)
      
      final_layout <- "
      AABB
      AACC
      "
      
      final_p <- p_scatter + p_map_min + p_map_max +
        plot_layout(design=final_layout)
      return(final_p)
    }
  )
  
  return(driver_dd_plots)
}


######################################################################
#Plot the distribution of ratios by department
#Histogram of number of BVs and their size (color) by department
