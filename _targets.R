source("R/cartoCE_packages.R")
source("R/cartoCE_functions.R")

rootdir = rprojroot::find_root(has_dir('src'))
datdir = file.path(rootdir, 'data')
resdir = file.path(rootdir, 'results')

tar_option_set(format = "qs")

#--- Data repositories --------------------------------------------------------
statsgdb = file.path(resdir, 'env_stats.gdb')

############################# Define targets plan ##############################
list(
  #------------------------------- Read files ----------------------------------
  #Read in metadata
  tar_target(ddt_metadata_path, file.path(datdir, 'metadonnes_cartographie_cours_deau_20231106.xlsx')), #format='file'
  tar_target(ddt_nets_colnas_path, file.path(resdir, 'cartos_loi_eau_colNAs.csv'), format='file') ,
  #Path to actual river network
  tar_target(ddtnets_path, file.path(resdir, 'analysis_outputs.gdb', 'carto_loi_eau_fr')),
  #Path to departments
  tar_target(deps_shp_path, file.path(datdir, 'données_auxiliaires', 'admin_express',
                                  'ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2023-10-16',
                                  'ADMIN-EXPRESS', '1_DONNEES_LIVRAISON_2023-10-16',
                                  'ADE_3-2_SHP_LAMB93_FXX', 'DEPARTEMENT.shp'),
             format='file'),
  
  #Establish file paths (to be reactive to file updates)
  tar_target(ddtnets_bdtopo_polyinters_path, file.path(resdir, 'carto_loi_eau_bdtopo_inters_tab.csv'), format='file'),   #Path to spatial intersection of DDT river networks with 1-m polygons around BD TOPO and BD Carthage river networks
  tar_target(ddtnets_carthage_polyinters_path, file.path(resdir, 'carto_loi_eau_carthage_inters_tab.csv'), format='file'),
  
  tar_target(bvdep_inters_gdb_path, file.path(resdir, 'preprocessing_ancillary_data.gdb', 'BV_hybas0809_depsinters')),
  tar_target(bvdep_inters_csv_path, file.path(resdir, 'BV_hybas0809_depsinters.csv'), format='file'),
  
  tar_target(varnames_path, file.path(datdir, 'variable_names_metadata.csv'), format='file'),
  
  tar_target(bcae_bvinters_path, file.path(resdir, "bcae_fr_bvinters.csv"),format = 'file'), #BCAE
  tar_target(bdtopo_bvinters_path, file.path(resdir, "bdtopo2015_fr_bvinters.csv"),format = 'file'), #BDTOPO
  tar_target(rht_bvinters_path, file.path(resdir, "rht_lbt93_bvinters.csv"),format = 'file'), #RHT
  tar_target(carthage_bvinters_path, file.path(resdir, "TRONCON_HYDROGRAPHIQUE_bvinters.csv"  ),format = 'file'), #Carthage
  tar_target(ddtnets_bvinters_path, file.path(resdir, "carto_loi_eau_fr_bvinters.csv"),format = 'file'), #DDT harmonized networks
  
  tar_target(amber_bvinters_path, file.path(resdir, "barriers_amber_bvinters.csv"),format = 'file'), #barriers
  tar_target(bdforet_bvinters_path, file.path(resdir, "bdforet_fr_bvinters.csv"),format = 'file'), #forest type
  tar_target(bdhaies_bvinters_path, file.path(resdir, "bdhaies_bvinters.csv"),format = 'file'), #hedges
  tar_target(comirrig_bvinters_path, file.path(resdir, "com_irrig_bvinters.csv"),format = 'file'), #commune-based irrigation
  tar_target(bdcharm_bvinters_path, file.path(resdir, "GEO050K_HARM_merge_bvinters.csv"),format = 'file'), #lithology
  tar_target(snelder_bvinters_path, file.path(resdir, "snelder_ires_bvinters.csv"),format = 'file'),
  tar_target(bnpe_bvinters_path, file.path(resdir, "withdrawals_bnpe_proj_bvinters.csv"),format = 'file'), #withdrawals
  tar_target(bnpe_timeseries_path, file.path(datdir, 'données_auxiliaires','bnpe', 'bnpe_chroniques.csv'), format = 'file'),
  tar_target(bnpe_withdrpts_path, file.path(datdir, 'données_auxiliaires','bnpe', 'bnpe_prelevements.csv'), format = 'file'),
  tar_target(bnpe_ouvrages_path, file.path(datdir, 'données_auxiliaires','bnpe', 'bnpe_ouvrages.csv'), format = 'file'),
  
  tar_target(onde_ddtnets_spjoin_path, file.path(resdir, "onde_carto_loi_eau_spjoin.csv"),format = 'file'),
  tar_target(onde_stations_bvinters_path, file.path(resdir, "onde_stations_bvinters.csv"),format = 'file'), #stations of intermittency observation
  
  tar_target(fish_ddtnets_spjoin_path, file.path(resdir, "fish_pop_aspe_carto_loi_eau_spjoin.csv"), format = 'file'), 
  tar_target(fish_pop_bvinters_path, file.path(resdir, "fish_pop_bvinters.csv"), format = 'file'), #stations of intermittency observation
  tar_target(fish_data_tablelist, file.path(datdir, 'données_auxiliaires', 'aspe', 'raw_data', 'raw_data', 'csv', 
                                            c('operation.csv', 'station.csv', 'operation_ipr.csv', 'operation_ipr_plus.csv', 'point_prelevement.csv'))),
  tar_target(hydrobio_ddtnets_spjoin_path, file.path(resdir, "hydrobio_stations_naiade_carto_loi_eau_spjoin.csv"),format = 'file'),
  tar_target(hydrobio_stations_bvinters_path, file.path(resdir, "hydrobio_stations_bvinters.csv"),format = 'file'), #stations of intermittency observation
  
  #Read in DDT metadata
  tar_target(metadata_sources, read_xlsx(path = ddt_metadata_path, sheet="Sources")),
  tar_target(
    metadata_nets, 
    read_xlsx(path = ddt_metadata_path, sheet="Métadonnées_réseau_SIG",
              col_types=c('numeric', rep('text', 4), 'numeric', rep('text', 23),
                          'numeric', 'text', 'date', 'date', 'text', 'date',
                          'text', 'text'
              ))),
  tar_target(metadata_websites, read_xlsx(path = ddt_metadata_path, sheet="Données_sites_DDT")),
  tar_target(ddt_nets_colnas, fread(ddt_nets_colnas_path)),
  
  #"skip", "guess", "logical", "numeric", "date", "text" or "list"
  
  #Path to spatial intersection of DDT river networks with 1-m polygons around BD TOPO and BD Carthage river networks
  tar_target(ddtnets_bdtopo_polyinters, fread(ddtnets_bdtopo_polyinters_path)),
  tar_target(ddtnets_carthage_polyinters, fread(ddtnets_carthage_polyinters_path)),
  
  #Read in units of analysis csv
  tar_target(bvdep_inters_tab, fread(bvdep_inters_csv_path)),
  
  #Read in variable names for display
  tar_target(varnames, fread(varnames_path)),
  
  #Read in csvs of networks
  tar_target(bcae_bvinters, fread(bcae_bvinters_path)), #BCAE
  tar_target(bdtopo_bvinters, fread(bdtopo_bvinters_path)), #BDTOPO
  tar_target(rht_bvinters, fread(rht_bvinters_path)), #RHT
  tar_target(carthage_bvinters, fread(carthage_bvinters_path)), #Carthage
  tar_target(ddtnets_bvinters, fread(ddtnets_bvinters_path)), #DDT harmonized networks
  
  #Read in csvs of environmental files
  tar_target(amber_bvinters, fread(amber_bvinters_path)), #barriers
  tar_target(bdforet_bvinters, fread(bdforet_bvinters_path)), #forest type
  tar_target(bdhaies_bvinters, fread(bdhaies_bvinters_path)), #hedges
  tar_target(comirrig_bvinters, fread(comirrig_bvinters_path)), #commune-based irrigation
  tar_target(bdcharm_bvinters, fread(bdcharm_bvinters_path)), #lithology
  tar_target(snelder_bvinters, fread(snelder_bvinters_path)),
  tar_target(bnpe_bvinters, fread(bnpe_bvinters_path)), #withdrawals
  tar_target(bnpe_timeseries, fread(bnpe_timeseries_path)),
  tar_target(bnpe_withdrpts, fread(bnpe_withdrpts_path)),
  tar_target(bnpe_ouvrages, fread(bnpe_ouvrages_path)),
  
  #Read in csvs of point data
  tar_target(onde_ddtnets_spjoin, fread(onde_ddtnets_spjoin_path)), 
  tar_target(onde_stations_bvinters, fread(onde_stations_bvinters_path)), #stations of intermittency observation
  
  tar_target(fish_ddtnets_spjoin, fread(fish_ddtnets_spjoin_path, encoding='Latin-1')),
  ##### HAVE TO REMOVE DOUBLE QUOTES FROM THIS FILE BELOW FOR IT TO BE READ######
  tar_target(fish_pop_bvinters, fread(fish_pop_bvinters_path, encoding='Latin-1')), #stations of intermittency observation

  
  tar_target(hydrobio_ddtnets_spjoin, fread(hydrobio_ddtnets_spjoin_path)), 
  ##### HAVE TO REMOVE DOUBLE QUOTES FROM THIS FILE BELOW FOR IT TO BE READ######
  tar_target(hydrobio_stations_bvinters, fread(hydrobio_stations_bvinters_path)), #stations of intermittency observation
  
  #Read and merge gdb tables
  tar_target(
    env_gdbtabs,
    lapply(c('ari_ix_ssu','ari_ix_syr', 'ppc_in_sav'
             ,'awc_mm_sav0_5' ,'awc_mm_sav5_15' ,'awc_mm_sav15_30'
             ,'awc_mm_sav30_60','awc_mm_sav60_100','awc_mm_sav100_200'
             ,'cly_pc_sav0_5','cly_pc_sav5_15','cly_pc_sav15_30'
             ,'cly_pc_sav30_60','cly_pc_sav60_100','cly_pc_sav100_200'
             ,'slt_pc_sav0_5','slt_pc_sav5_15','slt_pc_sav15_30'
             ,'slt_pc_sav30_60','slt_pc_sav60_100','slt_pc_sav100_200'
             ,'snd_pc_sav0_5','snd_pc_sav5_15','snd_pc_sav15_30'
             ,'snd_pc_sav30_60','snd_pc_sav60_100','snd_pc_sav100_200'
             ,'lc_pc_s19','lc_pc_s20','lc_pc_s21','slo_dg_sav'),
           function(lyr) {
             as.data.table(st_read(statsgdb, layer=lyr)) %>%
               setnames(
                 old = names(.)[-(names(.)=='UID_BV')],
                 new = paste(names(.)[-(names(.)=='UID_BV')],lyr,sep='_')
               )
           }) %>% 
      Reduce(function(x, y) merge(x, y, by="UID_BV", all.x=T, all.y=T), .)
  )
  ,
  #------------------------------- Format environmental data -------------------
  tar_target(
    barriers_formatted,
    format_amber(amber_bvinters)
  ),
  tar_target(
    forest_formatted,
    format_bdforet(bdforet_bvinters)
  ),
  tar_target(
    irrig_formatted,
    format_irrig(comirrig_bvinters)
  ),
  tar_target(
    lithology_formatted,
    format_bdcharm(bdcharm_bvinters)
  ),
  tar_target(
    ires_formatted,
    format_snelder(snelder_bvinters)
  ),
  tar_target(
    withdrawals_formatted,
    format_bnpe(bnpe_bvinters, bnpe_timeseries, bnpe_ouvrages)
  ),
  
  tar_target(
    env_bv_dt,
    compile_all_env(in_bvdep_inters = bvdep_inters_tab,
                    in_envlist =   list(
                      env_gdbtabs=env_gdbtabs
                      , barriers_formatted=barriers_formatted
                      , lithology_formatted=lithology_formatted
                      , forest_formatted=forest_formatted
                      , ires_formatted=ires_formatted
                      , withdrawals_formatted=withdrawals_formatted
                      , irrig_formatted=irrig_formatted
                    ))
  ),
  
  #------------------------------- Format networks data ------------------------
  tar_target(
    metadata_nets_formatted,
    format_metadata_nets(metadata_nets)
  ),
  
  #Match segments in the DDT networks to those in BD TOPO or BD Carthage 
  #based on outputs from spatial intersection
  tar_target(
    ddtnets_refids_imputed,
    impute_refids_ddtnets(in_ddtnets_path = ddtnets_path,
                          in_ddtnets_bdtopo_polyinters = ddtnets_bdtopo_polyinters,
                          in_ddtnets_carthage_polyinters = ddtnets_carthage_polyinters)
    

  ),
  
  tar_target(
    ddtnets_bvinters_stats,
    format_ddtnets_bvinters(in_ddtnets_bvinters = ddtnets_bvinters,
                            in_bdtopo_bvinters = bdtopo_bvinters,
                            in_carthage_bvinters = carthage_bvinters,
                            in_ddtnets_refids_imputed = ddtnets_refids_imputed)
  ),
  
  tar_target(
    ddtnets_dep_plots,
    plot_ddtnet_dep(ddtnets_bvinters_stats$dep_stats)
  ),
  
  #Format the other networks
  tar_target(
    carthage_bvinters_stats,
    format_carthage(in_carthage_bvinters = carthage_bvinters) 
  ),
  
  tar_target(
    bcae_bvinters_stats,
    format_bcae(in_bcae_bvinters = bcae_bvinters) 
  ),
  
  tar_target(
    bdtopo_bvinters_stats,
    format_bdtopo(in_bdtopo_bvinters = bdtopo_bvinters) 
  ),
  
  tar_target(
    rht_bvinters_stats,
    format_rht(in_rht_bvinters = rht_bvinters) 
  ),
  
  #----------- Evaluate coverage of point-based monitoring networks ------------
  tar_target(
    onde_coverage,
    evaluate_onde_coverage(in_onde_ddtnets_spjoin=onde_ddtnets_spjoin,
                           in_onde_stations_bvinters=onde_stations_bvinters,
                           in_ddtnets_path=ddtnets_path)
  ),
  
  tar_target(
    fish_coverage,
    evaluate_fish_coverage(in_fish_ddtnets_spjoin=fish_ddtnets_spjoin,
                           in_fish_pop_bvinters=fish_pop_bvinters,
                           in_fish_data_tablelist=fish_data_tablelist,
                           in_ddtnets_path=ddtnets_path)
  ),
  
  tar_target(
    hydrobio_coverage,
    evaluate_hydrobio_coverage(in_hydrobio_ddtnets_spjoin=hydrobio_ddtnets_spjoin,
                               in_hydrobio_stations_bvinters=hydrobio_stations_bvinters,
                               in_ddtnets_path=ddtnets_path)
  ),
  #------------------------------- analyze data --------------------------------
  tar_target(
    drainage_density_summary,
    summarize_drainage_density(
      in_ddtnets_stats = ddtnets_bvinters_stats$bv_stats,
      in_othernets_statlist = list(
        carthage = carthage_bvinters_stats,
        bcae = bcae_bvinters_stats,
        bdtopo = bdtopo_bvinters_stats,
        rht = rht_bvinters_stats),
      outdir = resdir,
      in_bvdep_inters = bvdep_inters_tab
    )
  ),
  
  tar_target(
    env_dd_merged_bv,
    merge_env_dd_bv(in_drainage_density_summary=drainage_density_summary,
                    in_env_bv_dt=env_bv_dt)
  ),
  
  tar_target(
    env_dd_merged_dep,
    merge_env_dd_dep (in_drainage_density_summary=drainage_density_summary,
                      in_env_bv_dt=env_bv_dt)
  ),
  
  tar_target(
    envdd_dep_plots,
    plot_envdd_dep(in_drainage_density_summary=drainage_density_summary,
                   in_env_dd_merged_dep = env_dd_merged_dep,
                   in_varnames=varnames)
  ),
  
  tar_target(
    env_dd_merged_bv_tab,
    fwrite(env_dd_merged_bv,
           file.path(resdir, 'env_dd_merged_bv.csv')),
    format = 'file'
  ),
  
  tar_target(
    env_dd_merged_dep_tab,
    fwrite(env_dd_merged_dep,
           file.path(resdir, 'env_dd_merged_dep.csv')),
    format = 'file'
  ),
  
  tar_target(
    envdd_multivar_analysis,
    corclus_envdd_bv(in_env_dd_merged_bv=env_dd_merged_bv,
                     in_varnames=varnames,
                     in_bvdep_inters=bvdep_inters_tab)
  )
  #,
  #
  # tar_target(
  #   envdd_plot_correlations,
  #   plotmap_envdd_cors(in_envdd_multivar_analysis=envdd_multivar_analysis,
  #                      in_env_dd_merged_bv=env_dd_merged_bv,
  #                      in_bvdep_inters_gdb_path=bvdep_inters_gdb_path,
  #                      in_ddtnets_path=ddtnets_path,
  #                      in_deps_path=deps_shp_path)
  # ),
  # 
  # tar_target(
  #   output_plots,
  #   lapply(seq(1, length(envdd_plot_correlations)), function(plot_i) {
  #     ggsave(paste0("envdd_plot_correlatons", plot_i, ".png"), 
  #            envdd_plot_correlations[[plot_i]],
  #            width = 180, height=185, units='mm', dpi=300
  #     )
  #   })
  #   ),
  #   
  #   tar_target(
  #     output_plots2,
  #     ggsave(paste0("envdd_dep_plots.png"), 
  #            envdd_dep_plots$dd_scatter_dep,
  #            width = 180, height=180, units='mm', dpi=300
  #     )
  #   ),
  # 
  # tar_target(
  #   output_plots3,
  #   ggsave(paste0("ddratio_bars_dep.png"), 
  #          envdd_dep_plots$ddratio_bars_dep,
  #          width = 180, height=180, units='mm', dpi=300
  #   )
  # ),
  # 
  # tar_target(
  #   output_plots4,
  #   ggsave(paste0("env_lengthratio_bdtopo.png"), 
  #          envdd_dep_plots$env_lengthratio_bdtopo,
  #          width = 180, height=180, units='mm', dpi=300
  #   )
  # ),
  # 
  # tar_target(
  #   output_plots5,
  #   ggsave(paste0("env_ddratio_corheatmap_avg8cl.png"), 
  #          envdd_multivar_analysis$env_ddratio_corheatmap_avg8cl,
  #          width = 250, height=300, units='mm', dpi=300
  #   )
  # )
  
)