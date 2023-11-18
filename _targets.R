source("R/cartoCE_packages.R")
source("R/cartoCE_functions.R")

rootdir = rprojroot::find_root(has_dir('src'))
datdir = file.path(rootdir, 'data')
resdir = file.path(rootdir, 'results')

tar_option_set(format = "qs")

#--- Data repositories --------------------------------------------------------
statsgdb = file.path(resdir, 'env_stats.gdb')
list.files(resdir)

############################# Define targets plan ##############################
list(
  #------------------------------- Read files ----------------------------------
  #Read in metadata
  tar_target(ddt_metadata_path,
             file.path(datdir, 
                       'metadonnes_cartographie_cours_deau_20231106.xlsx')
  ) #format='file'
  ,
  tar_target(ddt_nets_colnas_path,
             file.path(resdir, 
                       'cartos_loi_eau_colNAs.csv'),
             format='file'
  ) 
  ,
  
  #Establish csv file paths (to be reactive to file updates)
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
  tar_target(onde_stations_bvinters_path, file.path(resdir, "onde_stations_bvinters.csv"),format = 'file'), #stations of intermittency observation
  tar_target(snelder_bvinters_path, file.path(resdir, "snelder_ires_bvinters.csv"),format = 'file'),
  tar_target(bnpe_bvinters_path, file.path(resdir, "withdrawals_bnpe_proj_bvinters.csv"),format = 'file'), #withdrawals
  tar_target(bnpe_timeseries_path, file.path(datdir, 'données_auxiliaires','bnpe', 'bnpe_chroniques.csv')),
  
  #Read in DDT metadata
  tar_target(metadata_sources, read_xlsx(path = ddt_metadata_path, sheet="Sources")),
  tar_target(
    metadata_nets, 
    read_xlsx(path = ddt_metadata_path, sheet="Métadonnées_réseau_SIG",
              col_types=c('numeric', rep('text', 4), 'numeric', rep('text', 21),
                          'numeric', 'text', 'date', 'date', 'text', 'date',
                          'text', 'text'
              ))),
  tar_target(metadata_websites, read_xlsx(path = ddt_metadata_path, sheet="Données_sites_DDT")),
  tar_target(ddt_nets_colnas, fread(ddt_nets_colnas_path)),
  
  #"skip", "guess", "logical", "numeric", "date", "text" or "list"
  
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
  tar_target(onde_stations_bvinters, fread(onde_stations_bvinters_path)), #stations of intermittency observation
  tar_target(snelder_bvinters, fread(snelder_bvinters_path)),
  tar_target(bnpe_bvinters, fread(bnpe_bvinters_path)), #withdrawals
  tar_target(bnpe_timeseries, fread(bnpe_timeseries_path)),
  
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
      Reduce(function(x, y) merge(x, y, by="UID_BV"), .)
  )
  ,
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
    format_bnpe(bnpe_bvinters, bnpe_timeseries)
  ),
  

  tar_target(
    metadata_nets_formatted,
    format_metadata_nets(metadata_nets)
  ),
  
  tar_target(
    ddtnets_bvinters_stats,
    format_ddtnets_bvinters(in_ddtnets_bvinters = ddtnets_bvinters)
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
  
  tar_target(
    drainage_density_analysis,
    analyze_drainage_density(
      in_dt_list = list(
        ddtnets = ddtnets_bvinters_stats$bv_stats,
        carthage = carthage_bvinters_stats,
        bcae = bcae_bvinters_stats,
        bdtopo = bdtopo_bvinters_stats,
        rht = rht_bvinters_stats),
      outdir = resdir
    )
  ),
  
  tar_target(
    drainage_density_plots,
    plot_drainage_density(in_drainage_density_analysis=drainage_density_analysis)
  )
  
  #------------------------------- Format statistics ---------------------------
  #amber_bvinters[, .N, by=c('LabelAtlas', 'UID_BV')]
  
)




