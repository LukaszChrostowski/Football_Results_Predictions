####################################################################################
## MODELS SEPARATION AFTER LABEL SWITCH
O <- ConvFun(ContTable_1920)
N <- nrow(O)
y <- to_adjacency(O, N)
model <- mcmc(season = "1920", N = N, y = y, O = O)
Z_seq_burned <- model$Z_seq_burned
SS <- dim(Z_seq_burned)[1]
Knumb <- rep(max(model$K_seq), SS)
# timing the algorithm
start_time_undo <- Sys.time()
# unlist(apply(Z_seq_burned, 1, function(x) length(unique(x))))
# object output
Undone_Z_results <- collpcm.undo.label.switching(Z_seq_burned, Knumb) #to fix
end_time_undo <- Sys.time()
end_time_undo-start_time_undo

# Relabelled clusters
Undone_Z_seq<-Undone_Z_results$relab

# Record no. clusters for each draw
how_many_clust_labels = apply(Undone_Z_seq, MARGIN = 1, FUN = function(x){ # TO FIX
length(unique(x))
})
## Max no. of clusters obtained
K_max_seq = max(how_many_clust_labels)

# joining
Z_seq_burned_label_undone = cbind(Undone_Z_seq, how_many_clust_labels)

# SEPARATION
how_many_draws_k = rep(NA, K_max_seq)
for (kk in 1:K_max_seq){
 selected_Z_lab = Z_seq_burned_label_undone[Z_seq_burned_label_undone[,ncol(Z_seq_burned_label_undone)]==kk,]
if (is.matrix(selected_Z_lab)){
 assign(paste0("Z_seq_burned_K", kk, "undone"), selected_Z_lab[,1:(ncol(selected_Z_lab)-1)])
how_many_draws_k[kk] = dim(selected_Z_lab)[1]
  }else{
   assign(paste0("Z_seq_burned_K", kk, "undone"), selected_Z_lab[-length(selected_Z_lab)])
  how_many_draws_k[kk] = 1
}
}


####################################################################################
## ANALYSIS AND PLOTS

## POSTERIOR OF K TO FIX
##########################################################
posterior_k = rbind(how_many_draws_k/(S-burn_in_level)*100)
colnames(posterior_k) = 1:K_max_seq
print("Model percentages")
posterior_k
##########################################################


# Extracting allocation probabilities: P(Z|K)
# Write Summary table of posterior allocations for Teams
########################################################## TO FIX
library(xtable)

for (kk in 1:K_max_seq){
  selected_Z_lab = get(paste0("Z_seq_burned_K", kk, "undone"))
  if (is.matrix(selected_Z_lab)){
    not_null = dim(selected_Z_lab)[1]!=0
    n_teams = ncol(selected_Z_lab)
  }else{
    not_null = is.vector(selected_Z_lab)
    n_teams = length(selected_Z_lab)
  }
  if (not_null){
    s_k = how_many_draws_k[kk]
    my_levs_current = unique(as.vector(selected_Z_lab))
    if (length(my_levs_current)!=kk){
      # at times, the clusterings might differ
      max_kk = max(length(my_levs_current), kk)
      current_alloc_matrix =  matrix(NA, n_teams, max_kk)
    }else{
      current_alloc_matrix =  matrix(NA, n_teams, kk)
    }
    for (tt in 1:n_teams){
      if (is.vector(selected_Z_lab)){
        current_alloc_matrix[tt,] = table(factor(selected_Z_lab[tt], levels = my_levs_current))/s_k*100
      }else{
        current_alloc_matrix[tt,] = table(factor(selected_Z_lab[,tt], levels = my_levs_current))/s_k*100
      }
    }
    assign(paste0("Allocation_prob_Model", kk),current_alloc_matrix)
    current_cluster_percentages = t(current_alloc_matrix)
    colnames(current_cluster_percentages)<-colnames(O)
    rownames(current_cluster_percentages)<- paste0("Cluster ", 1:length(my_levs_current))
    # Save matrix for each cluster model
    assign(paste0("Cluster_Percentages_Model", kk),current_cluster_percentages)

    summary_table<-xtable(current_cluster_percentages,
                          caption = paste0("Season ", substr(season, start = 1, stop = 2), "/",
                                           substr(season, start = 3, stop = 4), " for model with K = ", kk))
    # Save Latex Table
    print.xtable(summary_table, type="latex", caption.placement = 'top',
                 file=paste0(folder_path,"Summary_table_K",kk,"_",after_object,".txt"))
  }
}

## Stacked Plot: of posterior allocations for each team in each cluster TO FIX
##########################################################
library(RColorBrewer)

coul = brewer.pal(K_max_seq, "Set1")

for (kk in 2:K_max_seq){
  current_clust_model = get(paste0("Cluster_Percentages_Model", kk))
  dim_k = dim(current_clust_model)[1]
  rev_clust<-matrix(NA,dim_k, N)
  for(i in 1:dim_k){
    rev_clust[i,]<-rev(current_clust_model[i,])*100
  }
  table_clusters<-as.table(rev_clust)
  colnames(table_clusters)<-rev(colnames(O))

  # order table according to final table (Tabellone)
  Team_Names = cbind(colnames(Results), rownames(Results))
  rownames(Team_Names) = Team_Names[,2]
  # ordered_team_names (according to Tabellone)
  Team_Names_ordered = Team_Names[rownames(Ordered_Tabellone),]

  # ordered stacked table according to tabellone
  ordered_stacked_table = table_clusters[,rev(Team_Names_ordered[,1])]

  pdf(paste0(folder_path,"StackedPlot_K",kk,after_object,".pdf"),width = 7, height=20, paper="a4")
  coul_kk = coul[1:dim_k]
  barplot(ordered_stacked_table,col=coul_kk, horiz = TRUE, border="white", ylab="Teams", cex.names=0.8)
  mtext(side=3,"Posterior allocation probabilities",line=2,cex=1.8)
  mtext(side=3,paste("K =", kk),line=0,cex=1.5)
  dev.off()
}
################

# FUNCTION FROM MCMC_main.R
# VISUALIZE AND SAVE PERMUTED MATCH GRID AFTER ESTIMATING K AND Z
#################################################################################### available after fix toFix.R
if (K_estimated>1){
  Winner_label = Team_Names[rownames(Ordered_Tabellone)[1],][1]
  # select cluster percentages
  Cluster_percentages = get(paste0("Cluster_Percentages_Model",
                                   K_estimated))

  Top_block = as.numeric(which.max(Cluster_percentages[,Winner_label]))

  # Team is in topblock if the posterior allocation is >=0.5
  Top_block_Teams = as.numeric(which(Cluster_percentages[Top_block,]>50))
  how_many_top = length(Top_block_Teams)

  all = 1:(dim(O)[1])
  the_others <- all[!all %in% Top_block_Teams]

  new_block_order = c(Top_block_Teams, the_others)
  Reordered_O = O[new_block_order,]
  Final_O = Reordered_O[,new_block_order]
}

