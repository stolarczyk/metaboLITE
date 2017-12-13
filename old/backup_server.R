#Loading libraries
library(igraph)
library(libSBML)
library(rsbml)
library(shiny)
library(intergraph)
library(GGally)
library(ggplot2)
library(sna)
library(rsconnect)

shinyServer(function(input, output) {
  path_to_file = reactive({
    input$file$datapath
  })
  observeEvent(input$update, {
    output$graph = renderPlot({
      #reading SBML files
      sbml_model = rsbml_read(path_to_file())
      data = rsbml_graph(rsbml_read(path_to_file()))
      
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      #Setting colors according to node class
      color_reactions = isolate(input$color_reactions)
      color_metabolites = isolate(input$color_metabolites)
      net %v% "type" = ifelse(grepl("R",names), "Reaction", "Metabolite")
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
        #test=unlist(net$val)[which(names(unlist(net$val))=="vertex.names")]
        test=unlist(net$val)
        #weights_edges = rep(0,length(test))
        weights_edges=c()
        if (isolate(input$weighting) == "stoichiometry") {
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = weights_edges
          edgesize = log10(weights_edges) + 0.25
          edgesize[which(!is.finite(edgesize))] = 0.25
        }
        if (isolate(input$weighting) == "gimme"){
          system("bash gimme_wrapper.sh")
          fluxes = read.csv("gimme_fluxes.csv",header = F, stringsAsFactors = F)
          fluxes = fluxes[-which(grepl("\\+",fluxes[,1]) | grepl("\\-",fluxes[,1])),]
          ndata = names(data@edgeData)
          for (i in seq(1,dim(fluxes)[1])){
            hits = which(grepl(fluxes[i,1],ndata))
            for (j in hits){
              data@edgeData@data[[j]]$weight = fluxes[i,2]
            }
          }
          toycon_graph = igraph.from.graphNEL(data)
          net = asNetwork(toycon_graph)
          net %v% "type" = ifelse(grepl("R",names), "Reaction", "Metabolite")
          reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val))=="vertex.names")][which(grepl("^R",unlist(net$val)[which(names(unlist(net$val))=="vertex.names")]))])
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = log10(weights_edges+abs(min(weights_edges))+1)+0.1
        }
      }
      #Plotting graph
      ggnet2(
        net,
        label = edges_names,
        arrow.gap = 0.035,
        arrow.size = 8,
        layout.exp = 0.1,
        #color = colors,
        edge.size = edgesize,
        shape = "type",
        shape.palette = c("Reaction" = 15, "Metabolite" = 19),
        color = "type",
        color.palette = c("Reaction" = color_reactions, "Metabolite" = color_metabolites) 
      )
    })
  })
})
