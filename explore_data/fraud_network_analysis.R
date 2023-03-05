#' Simulate and visualize customer transaction network
#' 
#' This function simulates a customer transaction network and visualizes it using visNetwork. 
#' It shows how to detects potential fraudsters using network centrality measures.
#' 
#' @import igraph
#' @import visNetwork
#' 
#' @return A visualization of the customer transaction network, along with a list of potential fraudsters.
#' 
#' @examples
#' simulate_and_visualize_customer_network()
#' 
#' @export
# load required packages
library(igraph)
library(visNetwork)

# set seed for reproducibility
set.seed(123)

# simulate data
n <- 100 # number of customers
m <- 200 # number of transactions
fraud_rate <- 0.1 # proportion of fraudulent transactions
fraudsters <- sample(1:n, round(fraud_rate*n)) # randomly select fraudsters

# create customer network
g <- erdos.renyi.game(n, p = 0.05) # create random graph
V(g)$label <- 1:n # assign customer IDs as labels
E(g)$weight <- 0 # set initial transaction weights to zero

# simulate transactions
# simulate transactions
for(i in 1:m) {
  sender <- sample(1:n, 1) # randomly select sender
  recipients <- neighbors(g, sender) # find neighbors of sender
  if(length(recipients) > 3) { # if sender has more than 3 neighbors, randomly select 3
    recipients <- sample(recipients, 3)
  }
  E(g)[sender %--% recipients]$weight <- E(g)[sender %--% recipients]$weight + 1 # update transaction weights
  if(sender %in% fraudsters) { # if sender is a fraudster
    for(recipient in recipients) { # increase weight of transactions with each recipient
      E(g)[sender %--% recipient]$weight <- E(g)[sender %--% recipient]$weight + 1 
    }
  }
}

# set edge weights to a minimum value of 1
E(g)$weight[E(g)$weight == 0] <- 1

# visualize network
plot(g, layout = layout_with_kk, vertex.label.color = "black", vertex.size = 10, vertex.label.cex = 0.8, 
     vertex.color = ifelse(V(g)$label %in% fraudsters, "red", "gray"), edge.width = log(E(g)$weight), 
     main = "Customer Transaction Network")

# convert igraph object to visNetwork object
g_vis <- toVisNetworkData(g)

# visualize network
visNetwork(g_vis$nodes, g_vis$edges) %>%
  visEdges(arrows = "to", smooth = TRUE, width = "weight") %>%
  visNodes(shape = "circle", color = list(background = ifelse(V(g)$label %in% fraudsters, "red", "gray")), 
           size = 10, font = list(size = 16)) %>%
  visLayout(randomSeed = 123) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE, algorithm = "hierarchical"))


# detect fraudsters using network centrality measures
fraud_centrality <- betweenness(g) # calculate betweenness centrality
fraud_threshold <- quantile(fraud_centrality, 0.95) # set threshold based on top 5% of centrality scores
fraudsters_detected <- V(g)$label[fraud_centrality >= fraud_threshold] # identify customers with high centrality scores

# print results
cat("The following customers were identified as potential fraudsters based on network centrality:\n")
cat(fraudsters_detected, sep = ", ")
