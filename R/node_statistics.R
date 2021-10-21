  #
#' A wrapper function to more quickly calculate common node statistics on an igraph object
#'
#' @param c an igraph object
#' @import igraph
#' @return a dataframe containing one row for each node of an igraph with centrality measures
#' @export node_statistics
#'
#' @examples
#' adj_matrix <- round(matrix(data = rep(runif(100, min = 0, max = 1)), 10,10),1)
#' diag(adj_matrix) <- 0
#' sample_igraph <- igraph::graph_from_adjacency_matrix(adj_matrix)
#' node_statistics_df <- node_statistics(sample_igraph)
#' print(node_statistics)
#'
#'
#'
#'
#'
#'


node_statistics <- function(c){

  InDegree <- igraph::degree(c, mode = "in")
  OutDegree <- igraph::degree(c, mode = "out")
  Total.Degree <- igraph::degree(c, mode = "total")
  Btw.Centrality <- igraph::betweenness(c)
  Closeness.Centrality <- igraph::closeness(c)
  Google_Pagerank <- igraph::page.rank(c)$vector

  g <- data.frame(

    "InDegree"=InDegree,
    "OutDegree"=OutDegree,
    "Total.Degree"=Total.Degree,
    "Btw.Centrality"=Btw.Centrality,
    "Closeness.Centrality"=Closeness.Centrality,
    "Google_Pagerank"=Google_Pagerank

  )

  return(g)

}
