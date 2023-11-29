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

imput_refids_ddtnets <- function(in_ddtnets_path,
                                 in_ddtnets_bdtopo_polyinters,
                                 in_ddtnets_carthage_polyinters) {
  ddtnet <- vect(
    x = dirname(in_ddtnets_path),
    layer = basename(in_ddtnets_path),
    what='attributes'
  )

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
    .[,  
      max_dev := max(sapply(.SD, function(x) {abs(1-x)})),
      .SDcols = c('inters_to_bdtopo_ratio', 
                  'inters_to_ddt_ratio', 
                  'ddt_to_bdtopo_ratio'),
      by=c('UID_CE', 'ID_bdtopo')]
   
  ddtnet_bdtopo_nodupliddt <-  ddtnet_bdtopo[
    order(max_dev),
    .SD[!duplicated(UID_CE),] #Keep the BD Topo record with the highest intersect with the DDT line
  ]
  
  check <- ddtnet_bdtopo[max_dev < 0.15 & 
                    is.na(id),]

  ddtnet_bdtopo_nodupliddt[UID_CE==855158,]
  
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
# in_ddtnets_bvinters <- tar_read(ddtnets_bvinters)
# in_bdtopo_bvinters <- tar_read(bdtopo_bvinters)
# in_carthage_bvinters <- tar_read(carthage_bvinters)

format_ddtnets_bvinters <- function(in_ddtnets_bvinters,
                                    in_bdtopo_bvinters,
                                    in_carthage_bvinters) {
  #  names(in_ddtnets_bvinters)
  # summary(in_ddtnets_bvinters)
  # in_ddtnets_bvinters[geom_Length < 0.01, .N]
  # unique(stringr::str_to_lower(in_ddtnets_bvinters$regime))
  # unique(stringr::str_to_lower(in_ddtnets_bvinterss$regime2))
  
  #----- Format regime
  #Get data from bd topo
  in_ddtnets_bvinters <- merge(
    in_ddtnets_bvinters,
    in_bdtopo_bvinters[!duplicated(ID), .(ID, REGIME)],
    by.x='id', by.y='ID', all.x=T) %>%
    merge(in_carthage_bvinters[!duplicated(CODE_HYDRO) & CODE_HYDRO != '',
                               .(CODE_HYDRO, ETAT)],
          by.x='code_hydro', by.y='CODE_HYDRO', all.x=T) %>%
    setnames(c('REGIME','ETAT'), c('regime_bdtopo', 'regime_carthage')) 
  
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
  
  in_ddtnets_bvinters[, regime_formatted := 
                        fcase(regime_carthage=='Permanent', 'perennial',
                              regime_carthage=='Intermittent', 'intermittent',
                              default='undetermined')]
  
  in_ddtnets_bvinters[!is.na(regime_bdtopo) &  
                        regime_bdtopo == 'Permanent',
                      regime_formatted := 'perennial']
  in_ddtnets_bvinters[!is.na(regime_bdtopo) &  
                        regime_bdtopo =='Intermittent',
                      regime_formatted := 'intermittent']
  in_ddtnets_bvinters[!is.na(regime) &  regime %in% perrenial_termlist,
                      regime_formatted := 'perennial']
  in_ddtnets_bvinters[!is.na(regime) &  regime %in% intermittent_termlist,
                      regime_formatted := 'intermittent']
  in_ddtnets_bvinters[!is.na(regime2) &  (regime2 %in% perrenial_termlist),
                      regime_formatted := 'perennial']
  in_ddtnets_bvinters[!is.na(regime2) &  (regime2 %in% intermittent_termlist),
                      regime_formatted := 'intermittent']
  
  #Remove lines from a departmental layer outside of that department
  in_ddtnets_bvinters[, orig_dep := as.integer(str_extract(orig_layer, "[0-9]{1,2}"))]
  
  in_ddtnets_bvinters <- in_ddtnets_bvinters[(orig_dep == INSEE_DEP) |
                                               (INSEE_DEP %in% c(92, 93 ,94)),]
  
  #For the section of the watercourse that is actually within the department
  #Re-assign "Hors département" to "Indéterminé"
  in_ddtnets_bvinters[type_stand == "Hors département", 
                      type_stand := 'Indéterminé'] 
  #Only 88 actually labeled as "Hors département"
  
  #Summarize length of lines by attributes
  bv_stats <- in_ddtnets_bvinters[
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
#-------------------------- summarize_drainage_density ---------------------------
# 
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
  
  
  #Compute statistics for HydroBASINS level 8-----------------------------------
  nets_stats_melt_b8 <-nets_stats_melt[, list(
    length_ddtnets_ce = sum(length_bv_ce_ddtnets, na.rm=T),
    length_ddtnets_ind = sum(length_bv_ind_ddtnets, na.rm=T),
    length_others = sum(value, na.rm=T),
    b8_area = sum(bv_area)
  ), by=c('PFAF_ID08', 'INSEE_DEP', 'NOM_DEP', 'variable')] %>%
    .[, length_ddtnets_ceind := length_ddtnets_ce + length_ddtnets_ind]
  
  #Compute statistics for departments ------------------------------------------
  #Compute IRES representativeness at the department level
  int_nce_stats_dep <-   in_ddtnets_stats[, list(
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
    ,
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
  
  return(list(
    nets_stats_merged_bv=nets_stats_merged_bv,
    nets_stats_melt=nets_stats_melt,
    nets_stats_melt_b8=nets_stats_melt_b8,
    nets_stats_melt_dep=nets_stats_melt_dep
  ))
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


#-------------------------- Summarize drainage density and deviations at the dep level ---------------------------
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

#-------------------------- Multivariate analysis env-dd for bvs across departments --------
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
    .[n_bvs>10,] 
  
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
  env_dd_dep_gowdist <- daisy(env_dd_dep_cormat, 
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
                         round(cophcor_avg, 2))) +
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
  classcol<- c("#176c93","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#7a5614","#6baed6","#00441b") #9 classes with darker color (base blue-green from Colorbrewer2 not distinguishable on printed report and ppt)
  classcol_temporal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#666666','#a65628')
  
  
  #Make table of gauge classes and good looking dendogram
  env_dd_dendo_avg_5cl <-prettydend(hclus_out = env_dd_dep_hclust_avg, 
                                    kclass=5, colors=classcol,
                                    classnames= NULL)
  env_dd_dendo_avg_8cl <-prettydend(hclus_out = env_dd_dep_hclust_avg, 
                                    kclass=8, colors=classcol,
                                    classnames= NULL)
  p_dendo_avg_5cl <- env_dd_dendo_avg_5cl[[2]]
  p_dendo_avg_8cl <- env_dd_dendo_avg_8cl[[2]]
  
  
  env_dd_dep_cor_avg_cl5 <- merge(env_dd_dep_cor, env_dd_dendo_avg_5cl[[1]],
                                  by.x='NOM', by.y='ID')
  env_dd_dep_cor_avg_cl8 <- merge(env_dd_dep_cor, env_dd_dendo_avg_8cl[[1]],
                                  by.x='NOM', by.y='ID')
  
  p_cluster_boxplot_avg5 <- ggplot(
    env_dd_dep_cor_avg_cl5, 
    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  
  p_cluster_boxplot_avg8 <- ggplot(
    env_dd_dep_cor_avg_cl8, 
    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  
  #Make table of gauge classes and good looking dendogram
  env_dd_dendo_ward_5cl <-prettydend(hclus_out = env_dd_dep_hclust_ward, 
                                     kclass=5, colors=classcol,
                                     classnames= NULL)
  env_dd_dendo_ward_8cl <-prettydend(hclus_out = env_dd_dep_hclust_ward, 
                                     kclass=8, colors=classcol,
                                     classnames= NULL)
  p_dendo_ward_5cl <- env_dd_dendo_ward_5cl[[2]]
  p_dendo_ward_8cl <- env_dd_dendo_ward_8cl[[2]]
  
  env_dd_dep_cor_ward_cl5 <- merge(env_dd_dep_cor, env_dd_dendo_ward_5cl[[1]],
                                   by.x='NOM', by.y='ID')
  
  p_cluster_boxplot_ward5 <- ggplot(env_dd_dep_cor_ward_cl5, 
                                    aes(x=factor(gclass), y=cor, color=factor(gclass))) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0) +
    facet_wrap(~description)
  
  env_dd_dep_cor_ward_cl8 <- merge(env_dd_dep_cor, env_dd_dendo_ward_8cl[[1]],
                                   by.x='NOM', by.y='ID')
  
  p_cluster_boxplot_ward8 <- ggplot(env_dd_dep_cor_ward_cl8, 
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
  
  #Heatmap of correlation between drainage density ratio and variables----------
  colnames(env_dd_dep_cormat) <- 
    gsub("gw", "gw",
         gsub("surface water", "sw",
              gsub("water withdrawals", "ww", colnames(env_dd_dep_cormat))))
  
  max(env_dd_dep_cormat, na.rm=T)
  min(env_dd_dep_cormat, na.rm=T)
  
  env_dd_dep_cormat[abs(env_dd_dep_cormat)<0.3] <- NA
  
  env_dd_dep_cormat_avg8cl <- env_dd_dep_cormat[env_dd_dep_hclust_avg$order,]
  #as.data.table(env_dd_dendo_avg_8cl[[1]])[order(ID),order(gclass)],]
  
  class_colors_avg_8cl <- classcol[as.data.table(
    env_dd_dendo_avg_8cl[[1]])[order(gclass, ID),gclass]]
  
  env_ddratio_corheatmap_avg8cl <- 
    ggcorrplot(env_dd_dep_cormat_avg8cl, #method = "circle", 
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
    theme(axis.text.y = element_text(
      colour = class_colors_avg_8cl))
  
  
  #Plot heatmap of env-dd correlations, clustered
  env_dd_dep_cormat_ward8cl <- env_dd_dep_cormat[env_dd_dep_hclust_ward$order,]
  
  class_colors_ward_8cl <- classcol[as.data.table(
    env_dd_dendo_ward_8cl[[1]])[order(gclass, ID),gclass]]
  
  env_ddratio_corheatmap_ward8cl <- 
    ggcorrplot(env_dd_dep_cormat_ward8cl, #method = "circle", 
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
      colour = class_colors_ward_8cl))
  
  
  return(list(
    env_dd_melt = env_dd_melt
    , env_dd_dep_cor = env_dd_dep_cor
    , excluded_deps = excluded_deps 
    , p_cophcor_avg = p_cophcor_avg
    , p_scree_avg =p_scree_avg
    , p_cophcor_ward = p_cophcor_ward
    , p_scree_ward =p_scree_ward
    , p_cluster_boxplot_ward5 = p_cluster_boxplot_ward5
    , p_cluster_boxplot_ward8 = p_cluster_boxplot_ward8
    , p_cluster_boxplot_avg5 = p_cluster_boxplot_avg5
    , p_cluster_boxplot_avg8 = p_cluster_boxplot_avg8
    , env_ddratio_corheatmap_avg8cl = env_ddratio_corheatmap_avg8cl
    , env_ddratio_corheatmap_ward8cl = env_ddratio_corheatmap_ward8cl
  ))
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
