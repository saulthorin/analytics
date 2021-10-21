#get node summary stats in dataframes for each node in igraph objects

#' Calculates basic node summary statistics
#' @import igraph
#' @import stats
#' @param c an igraph object
#'
#' @return a data.frame of node summary statistics
#' @export network_summary
#'
#' @examples
#' adj_matrix <- round(matrix(data = rep(runif(100, min = 0, max = 1)), 10,10),1)
#' diag(adj_matrix) <- 0
#' sample_igraph <- igraph::graph_from_adjacency_matrix(adj_matrix)
#' network_summary_table <- network_summary(sample_igraph)
#' print(network_summary_table)
#'

network_summary <- function(c){
  node_count <- igraph::vcount(c)
  edge_count <- igraph::ecount(c)
  Density <- igraph::graph.density(c)
  Avg.InDegree <- mean(igraph::degree(c, mode = "in"))
  Sd.InDegree <- stats::sd(igraph::degree(c, mode = "in"))
  Avg.OutDegree <- mean(igraph::degree(c, mode = "out"))
  Sd.OutDegree <- stats::sd(igraph::degree(c, mode = "out"))
  Avg.Btw.Centrality <- mean(igraph::betweenness(c))
  Sd.Btw.Centrality <- stats::sd(igraph::betweenness(c))
  Avg.Closeness.Centrality <- mean(igraph::closeness(c))
  Sd.Closeness.Centrality <- stats::sd(igraph::closeness(c))
  Avg.Google.Pagerank <- mean(igraph::page.rank(c)$vector)
  Sd.Google.Pagerank <- stats::sd(igraph::page.rank(c)$vector)


  f <- data.frame(
    "node_count"=node_count,
    "edge_count"=edge_count,
    "Density"=Density,
    "Avg.InDegree"=Avg.InDegree,
    "Sd.InDegree"=Sd.InDegree,
    "Avg.OutDegree"=Avg.OutDegree,
    "Sd.OutDegree"=Sd.OutDegree,
    "Avg.Btw.Centrality"=Avg.Btw.Centrality,
    "Sd.Btw.Centrality"=Sd.Btw.Centrality,
    "Avg.Closeness.Centrality"=Avg.Closeness.Centrality,
    "Sd.Closeness.Centrality"=Sd.Closeness.Centrality,
    "Avg.Google.Pagerank"=Avg.Google.Pagerank,
    "Sd.Google.Pagerank"=Sd.Google.Pagerank
  )
  return(f)
}


