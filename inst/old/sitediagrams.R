library(DiagrammeR)


#freshwater site=============================================================#
y <- c(rev(rep(1:8, each = 2)), 3, -5) - c(rep(seq(0.0, 3.5, by = 0.5), each = 2), 0, 0)
x <- c(rep(c(-1,3), 8), 1, 1)

fwcolors <- sapply(c(1,4,4,1,1,4,1,4,1,1,2,2,2,2,2,2,3,3), function(x) viridis::viridis(4)[x])
fwnodes <- create_nodes(nodes = c(c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11, 14, 13, 16, 15), "Boardwalk", "Road"),
                      height = c(rep(1,16), 12, 1),
                      width = c(rep(1,16), 1, 8),
											style = "filled",	
                      x = x, y = y,
                      color = fwcolors,
                      shape = c(rep("circle", 16), "rectangle", "rectangle")
                      )

fwgraph <- create_graph(nodes_df = fwnodes, graph_attrs = c("layout = neato"))
render_graph(fwgraph)

#brackish site================================================================#

y <- c(rev(rep(1:8, each = 2)), 3, 3) - c(rep(seq(0.0, 3.5, by = 0.5), each = 2), 0, 0)
x <- c(rep(c(-1,3), 7), -2.5, -1, 1, -6)

bwcolors <- sapply(c(2,2,2,2,2,2,4,1,1,1,1,4,1,4,1,4,3,3), function(x) viridis::viridis(4)[x])
bwnodes <- create_nodes(nodes = c(rev(c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11, 14, 13, 16, 15)), "Boardwalk", "Road"),
                        height = c(rep(1,16), 12, 7),
                        width = c(rep(1,16), 1, 0.1),
                        x = x, y = y,
												style = "filled",
                        color = bwcolors,
                        shape = c(rep("circle", 16), "rectangle", "polygon"),
                        skew = c(rep(0,16), 0, -3)
)
#distortion = c(rep(0,16), 0, -1)

bwgraph <- create_graph(nodes_df = bwnodes, graph_attrs = c("layout = neato"))

render_graph(bwgraph)



#==================================================================#

#edges <- create_edges(from = c(1:16, "Boardwalk"), to = c(rep("Boardwalk", 16), "Road"), relationship = "given_to")



