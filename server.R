#Loading libraries
library(igraph)
library(libSBML)
library(rsbml)
library(shiny)
library(intergraph)
library(GGally)
library(ggplot2)
library(sna)

shinyServer(function(input, output) {
  path_to_file = reactive({
    input$file$datapath
  })
  observeEvent(input$update, {
    output$graph = renderPlot({
      #reading SBML files
      sbml_model = rsbml_read(path_to_file())
      toycon_graph = igraph.from.graphNEL(rsbml_graph(rsbml_read(path_to_file())))
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      #Setting colors according to node class
      colors = rep(isolate(input$color_metabolites), length(names))
      colors[grep("R", names)] = isolate(input$color_reactions)
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite_name = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
          compartment = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@compartment
          metabolite = paste(metabolite_name, compartment, sep = "_")
          edges_names[i] = metabolite
        }
        else{
          if (any(names(sbml_model@model@reactions) == as.character(names[i]))) {
            reaction_name = sbml_model@model@reactions[[which(names(sbml_model@model@reactions) == as.character(names[i]))]]@name
            edges_names[i] = reaction_name
          }
          else{
            edges_names[i] = "NoName"
          }
        }
      }
      
      if (isolate(input$weighting) == "none") {
        edgesize = 0.25
      }
      #Weighting edges
      else{
        weights_edges = rep(0,length(unlist(net$val)[which(names(unlist(net$val))=="vertex.names")]))
        if (isolate(input$weighting) == "w") {
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = weights_edges
          edgesize = log10(weights_edges) + 0.25
        }
        if (isolate(input$weighting) == "gimme"){
          system("bash gimme_wrapper.sh")
          fluxes = read.csv("gimme_fluxes.csv",header = F, stringsAsFactors = F)         
          reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val))=="vertex.names")][which(grepl("^R",unlist(net$val)[which(names(unlist(net$val))=="vertex.names")]))])
          for (i in seq(1,dim(fluxes)[1],1)){
            weights_edges[which(as.vector(unlist(net$val)) == reactions_names[i])] = fluxes[which(fluxes[,1] == reactions_names[i]),2]
          }
          edgesize = weights_edges #produces some NAs. Look into!
          #edgesize = log10(weights_edges) + 0.25
        }
      }
      #Plotting graph
      ggnet2(
        net,
        label = edges_names,
        arrow.gap = 0.035,
        arrow.size = 8,
        layout.exp = 0.1,
        color = colors,
        edge.size = edgesize
      )
      
    })
    
  })
})
