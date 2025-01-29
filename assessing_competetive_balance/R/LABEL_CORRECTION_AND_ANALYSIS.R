#### SCRIPT FOR LABEL SWITCHING CORRECTION AND ANALYSIS

# paths for elements
#folder_path<-paste0("Inference_results//Season_",season,"//")
#after_object<-paste0("Season_",season,"_", (S-burn_in_level)/1000 ,"k_seed",my_seed)

# In case one wants to load it separately

# burn_in_level<-50000
# S<-250000
#
# MCMC<-"AE_GS_MK_corrected"
# Championship<-"Premiere"
# my_seed<-1909
#
# load(paste0("Inference_results//mcmc_Premier_Season_",season,
# "//WS_Premier_Season_", season, "_", (S-burn_in_level)/1000,
# "k_seed_",my_seed,".RData"))

label_correction <- function(Z_seq_burned, K_max, True_K_seq_burned, O, K_seq) {
  # parameters to feed into label correction algorithm
  SS<-dim(Z_seq_burned)[1]
  Knumb<-rep(max(K_seq), SS)

  ####################################################################################
  ## ANALYSIS AND PLOTS


  # Plotting Traceplot for K as in Nobile and Fearnside
  ##########################################################
  Noisy_True_K<-True_K_seq_burned
  for(ii in 1:SS){
    Noisy_True_K[ii]<-Noisy_True_K[ii]+runif(1,min=0.1, max=0.9)
  }
  K_maxmax<-max(True_K_seq_burned)
  K_minmin<-min(True_K_seq_burned)
  #Plot directly the points
  ##########################################################


  # Print Results Table
  ##########################################################
  Point_Table<-mapvalues(O, from=c(1,2,3), to=c(3,1,0))
  Reversed_Table<-mapvalues(Point_Table, from=c(3,0), to=c(0,3))
  # Italian tip: "Tabellone" means scoreboard or finale ranking table :)
  Tabellone<-as.matrix(rowSums(Point_Table, na.rm = TRUE)+
                         colSums(Reversed_Table, na.rm = TRUE))

  Ordered_Tabellone<-cbind(sort(Tabellone, decreasing = TRUE))
  rownames(Ordered_Tabellone)<-rownames(Tabellone)[order(Tabellone, decreasing = TRUE)]
  Tabellone_table<-xtable(Ordered_Tabellone, digits = 0)
  ##########################################################


  # Mode, needed for estimated Heatmap
  ##########################################################
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  K_estimated = Mode(True_K_seq_burned)

  list(K_estimated = K_estimated,
       table = Ordered_Tabellone,
       Noisy_True_K = Noisy_True_K,
       K_maxmax = K_maxmax,
       K_minmin = K_minmin)
}

