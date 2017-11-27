library(igraph)
library(libSBML)
library(rsbml)
library(shiny)
#library(intergraph)
#library(GGally)
#library(ggplot2)
#library(sna)
#library(rsconnect)
library(visNetwork)
sbml_model = rsbml_read("data/toycon.xml")
data = rsbml_graph((sbml_model))
toycon_graph = igraph.from.graphNEL(data)
visdata <- toVisNetworkData(toycon_graph)
visdata$nodes$group = rep("Metabolite",length(visdata$nodes$id))
visdata$nodes$group[which(grepl("R",visdata$nodes$id))] = "Reaction"
visdata$edges$width = log10(visdata$edges$weight)
visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
  visLegend() %>%
  visGroups(groupname = "Metabolite", color = color_metabolite, shape = "circle") %>%
  visGroups(groupname = "Reaction", color = color_reaction, shape = "square")
