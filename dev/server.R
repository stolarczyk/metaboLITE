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
library(visNetwork)
library(xtable)


shinyServer(function(input, output) {
  path_to_file = reactive({
    input$file$datapath
  })
  observeEvent(input$update, {
    output$graph = renderVisNetwork({
      #reading SBML files
      sbml_model = rsbml_read(path_to_file())
      data = rsbml_graph((sbml_model))
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      visdata$edges$width = 2
      visdata$edges$length = 150
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      #Setting colors according to node class
      color_reaction = isolate(input$color_reactions)
      color_metabolite = isolate(input$color_metabolites)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
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
      names_dict = rbind(edges_names,names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      if (isolate(input$weighting) == "none") {
        edgesize = 1
        output$fluxes = renderTable({
        })
      }
      #Weighting edges
      else{
        test = unlist(net$val)
        weights_edges = c()
        if (isolate(input$weighting) == "stoichiometry") {
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = weights_edges
          edgesize = log10(weights_edges) + 0.25
          edgesize[which(!is.finite(edgesize))] = 0.25
          visdata$edges$width = edgesize
          output$fluxes = renderTable({
          })
        }
        if (isolate(input$weighting) == "gimme") {
          system("bash ../gimme_wrapper.sh")
          fluxes = read.csv(
            "data/gimme_fluxes.csv",
            header = F,
            stringsAsFactors = F
          )
          fluxes = fluxes[-which(grepl("\\+", fluxes[, 1]) |
                                   grepl("\\-", fluxes[, 1])), ]
          fluxes_output = fluxes
          colnames(fluxes_output) = c("Edge", "Flux")
          for (i in seq(1,dim(names_dict)[2],by = 1)){
            if(any(which(fluxes_output[,1] == names_dict[2,i])))
            fluxes_output[which(fluxes_output[,1] == names_dict[2,i]),1] = names_dict[1,i]
          }
          
          output$fluxes = renderTable({
            fluxes_output
          },caption = "GIMME fluxes",
          caption.placement = getOption("xtable.caption.placement", "top"), 
          caption.width = getOption("xtable.caption.width", NULL))
          ndata = names(data@edgeData)
          for (i in seq(1, dim(fluxes)[1])) {
            hits = which(grepl(fluxes[i, 1], ndata))
            for (j in hits) {
              data@edgeData@data[[j]]$weight = fluxes[i, 2]
            }
          }
          toycon_graph = igraph.from.graphNEL(data)
          net = asNetwork(toycon_graph)
          net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
          reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                              "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                         "vertex.names")]))])
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = log10(weights_edges + abs(min(weights_edges)) + 1) +
            0.1
          visdata$edges$width = edgesize
        }
      }
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(stepX = 75,
                  stepY = 100,
                  width = 0.1) %>%
        visOptions(highlightNearest = TRUE) %>% #,nodesIdSelection = list(enabled = TRUE,selected = "M_m04c")) %>%
        visEdges(color = "black") %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = "circle") %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = "box")
    })
  })
})
