#' Visualize Data Connections
#'
#' Visualize the common columns between multiple data frames as a graph. 
#' This function uses the output from the \code{\link{find_ids}} function. 
#' 
#' @param df_list A list of data frames to analyze
#'
#' @return An undirected graph showing the common variables between the data frames
#'
#' @import igraph
#' @importFrom dplyr filter select distinct group_by mutate ungroup
#' @export
#' 
#' @examples 
#' # Load example data
#' df1 <- data.frame(ID1 = 1:5, Name = c("John", "Mary", "Bob", "Alice", "Tom"), Age = c(23, 34, 29, 19, 41))
#' df2 <- data.frame(ID1 = 3:7, Age = c(25, 32, 46, 18, 57), Gender = c("M", "F", "M", "F", "M"))
#' df3 <- data.frame(ID2 = 2:6, Gender = c("M", "F", "M", "F", "M"), City = c("NYC", "LA", "Chicago", "Houston", "Miami"))
#' df4 <- data.frame(ID2 = 2:6, Gender = c("M", "F", "M", "F", "M"), City = c("NYC", "LA", "Chicago", "Houston", "Miami"))
#'
#' # Save data as list
#' df_list <- list(df1, df2, df3, df4)
#'
#' # Create plot
#' visualize_find_ids(df_list)
#'
#' @seealso \code{\link{find_ids}}
#' @keywords data, visualization, graph, igraph
#' @export
# Save function
visualize_find_ids <- function(df_list) {
  # Get the results from the find_ids function
  results <- find_ids(df_list)
  
  # Debugging print statement
  print(unique(results$Data_Frame))
  
  # Create an empty graph object
  g <- graph.empty(n = length(unique(results$Data_Frame)))
  
  # Add the nodes (data frames) to the graph
  V(g)$name <- as.character(unique(results$Data_Frame))
  
  # Add the edges (common variables) to the graph
  edges <- results %>%
    filter(Common == "Yes") %>%
    select(Data_Frame, Variable) %>%
    distinct() %>%
    group_by(Variable) %>%
    mutate(n = n()) %>%
    filter(n > 1 | is.na(n)) %>%
    ungroup() %>%
    select(-n)
  
  g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)
  
  # Set the layout of the graph
  layout <- layout_with_kk(g)
  
  # Plot the graph
  plot(g, layout = layout, vertex.label.cex = 1.2, edge.label.cex = 1.2,
       vertex.color = "white", vertex.frame.color = "black", vertex.shape = "circle",
       edge.arrow.size = 0.5)
}