library(readxl)
library(igraph)
library(RCy3)
library(stats)
library(tidyverse)
#library(ggcorrplot)

spring_2223 <- read_xlsx("data/Projecten_SPRING 2022-2023.xlsx")
spring_2223_opl <- spring_2223[3:31]=="x"
spring_2223_opl <- !is.na(spring_2223_opl)

autumn_2223 <- read_xlsx("data/Projecten_AUTUMN 2022-2023.xlsx")
autumn_2223_opl <- autumn_2223[3:29]>=1
autumn_2223_opl <- !is.na(autumn_2223_opl)

# start Cytoscape (check Batch-file after update Cytoscape)
system("Cytoscape.bat")
source("CytoscapeStyles.R")

graph_heat_sss <- function(data_sss) {
  # based on https://stackoverflow.com/questions/54629654/co-occurence-networks-using-presence-absence-data-r
  # Generate co-occurrence matrix with crossproduct
  co_mat <- t(data_sss) %*% data_sss
  # Set diagonal values to 0
  diag(co_mat) <- 0
  # Assign dim names
  dimnames(co_mat) <- list(colnames(data_sss), colnames(data_sss))
  # Create graph from adjacency matrix
  # ! edge weights are equal to frequency of co-occurrence
  g <- graph_from_adjacency_matrix(co_mat, mode = "upper", weighted = TRUE)
  # Assign nodes weight equal to species frequency
  g <- set.vertex.attribute(g, "v_weight", value = colSums(data_sss))
  # plot number of studyprograms
  x <- rownames_to_column(data.frame(degree(g)))
  colnames(x) <- c("opleiding", "aantal")
  ggplot(x, aes(opleiding,aantal)) + geom_bar(stat = "identity")
  ggsave(paste0("export/",deparse(substitute(data_sss)),"_aantal_samenwerking.jpg"), width = 8, height = 6)
  # network to cytoscape
  createNetworkFromIgraph(g,deparse(substitute(data_sss)))
  setVisualStyle("GreyNodesLabel")
  layoutNetwork("kamada-kawai")
  setEdgeLineWidthMapping('weight', c(min(E(g)$weight),mean(E(g)$weight),max(E(g)$weight)),c(1,3,5), style.name = "GreyNodesLabel")
  setNodeSizeMapping("v_weight", sizes=c(1,mean(colSums(data_sss)),max(colSums(data_sss))),style.name = "GreyNodesLabel")
  # heatmap cooperations
  jpeg(file=paste0("export/",deparse(substitute(data_sss)),"_heatmap.jpg"))
  heatmap(co_mat)
  #heatmap(co_mat, Colv = NA, Rowv = NA)
  dev.off()
  g
}

g_spring_2223_opl <- graph_heat_sss(spring_2223_opl)

g_autumn_2223_opl <- graph_heat_sss(autumn_2223_opl)

#manual saving networks from Cytoscape, because layout with attributes is not available in RCy3
