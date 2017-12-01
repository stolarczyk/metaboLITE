#Loading libraries
library(igraph)
library(libSBML)
library(rsbml)
library(shiny)
library(sna)
library(intergraph)
library(GGally)
library(ggplot2)
library(rsconnect)
library(visNetwork)
library(xtable)
library(dplyr)

shinyServer(function(input, output) {
  hideTab(inputId = "tabs", target = "Change media")
  hideTab(inputId = "tabs", target = "KO reactions")
  path_to_file = reactive({
    input$file$datapath
  })
  observeEvent(input$update, {
    ptf = path_to_file()
    sbml_model = rsbml_read(path_to_file())
    data = rsbml_graph((sbml_model))
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$edges$width = 2
    visdata$edges$length = 150
    #visdata$edges$arrows = c("from", "to")
    net = asNetwork(toycon_graph)
    names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "tomato"
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
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    output$graph = renderVisNetwork({
      #reading SBML files
      if (isolate(input$weighting) == "none") {
        edgesize = 1
        output$fluxes = renderTable({
          
        })
        command = paste("bash check_flux_wrapper.sh", path_to_file())
        system(command = command)
        fluxes = read.table("data/flux.txt")
        flux = as.character(read.table("data/flux.txt"))
        output$text_flux = renderText({
          paste("<br/>", "<b>Flux: ", flux, "</b>", "<br/>")
        })
      }
      #Weighting edges
      else{
        command = paste("bash check_flux_wrapper.sh", path_to_file())
        system(command = command)
        fluxes = read.table("data/flux.txt")
        flux = as.character(read.table("data/flux.txt"))
        output$text_flux = renderText({
          paste("<br/>", "<b>Flux: ", flux, "</b>", "<br/>")
        })
        test = unlist(net$val)
        weights_edges = c()
        if (isolate(input$weighting) == "stoichiometry") {
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = weights_edges
          edgesize = log(weights_edges) + 0.25
          edgesize[which(!is.finite(edgesize))] = 0.25
          visdata$edges$width = edgesize
          output$fluxes = renderTable({
            
          })
        }
        if (isolate(input$weighting) == "gimme") {
          command = paste("bash check_flux_wrapper.sh", path_to_file())
          system(command = command)
          fluxes = read.table("data/flux.txt")
          flux = as.character(read.table("data/flux.txt"))
          output$text_flux = renderText({
            paste("<br/>", "<b>Flux: ", flux, "</b>", "<br/>", "<br/>")
          })
          command = paste("bash gimme_wrapper.sh", path_to_file())
          system(command = command)
          fluxes = read.csv(
            "data/gimme_fluxes.csv",
            header = F,
            stringsAsFactors = F
          )
          fluxes = fluxes[-which(grepl("\\+", fluxes[, 1]) |
                                   grepl("\\-", fluxes[, 1])), ]
          fluxes_output = fluxes
          colnames(fluxes_output) = c("Reaction", "Flux")
          for (i in seq(1, dim(names_dict)[2], by = 1)) {
            #Mapping nodes IDs to names for table displaying purposes
            if (any(which(fluxes_output[, 1] == names_dict[2, i])))
              fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
          }
          output$fluxes = renderTable({
            fluxes_output
          }, caption = "GIMME fluxes",
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
          edgesize = log(abs(weights_edges)) + 1
          visdata$edges$width = edgesize
        }
        if (isolate(input$weighting) == "gimmestoichiometry") {
          command = paste("bash check_flux_wrapper.sh", path_to_file())
          system(command = command)
          fluxes = read.table("data/flux.txt")
          flux = as.character(read.table("data/flux.txt"))
          output$text_flux = renderText({
            paste("<br/>", "<b>Flux: ", flux, "</b>", "<br/>", "<br/>")
          })
          weights_edges = c()
          command = paste("bash gimme_wrapper.sh", path_to_file())
          system(command = command)
          fluxes = read.csv(
            "data/gimme_fluxes.csv",
            header = F,
            stringsAsFactors = F
          )
          to_del = which(grepl("\\+", fluxes[, 1]) |
                           grepl("\\-", fluxes[, 1]))
          fluxes = fluxes[-to_del, ]
          fluxes_output = fluxes
          colnames(fluxes_output) = c("Reaction", "Flux")
          for (i in seq(1, dim(names_dict)[2], by = 1)) {
            #Mapping nodes IDs to names for table displaying purposes
            if (any(which(fluxes_output[, 1] == names_dict[2, i])))
              fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
          }
          
          ndata = names(data@edgeData)
          for (i in seq(1, dim(fluxes)[1])) {
            hits = which(grepl(fluxes[i, 1], ndata))
            for (j in hits) {
              data@edgeData@data[[j]]$flux = fluxes[i, 2]
            }
          }
          edges_df = dplyr::mutate(visdata$edges, name = paste(from, to, sep = "|"))
          for (i in seq(1, length(data@edgeData@data))) {
            hit = which(edges_df[, 6] == ndata[i])
            data@edgeData@data[[i]]$stoi = edges_df[hit, 3]
          }
          for (i in seq(1, length(data@edgeData@data))) {
            data@edgeData@data[[i]]$weight = data@edgeData@data[[i]]$flux * data@edgeData@data[[i]]$stoi
          }
          new_df = data.frame()
          for (i in seq(1, length(data@edgeData@data))) {
            df1 = as.data.frame(data@edgeData@data[[i]])
            df1$reaction = ndata[i]
            new_df = merge(new_df, df1, all = T)
          }
          new_df = new_df[which(grepl("^R", new_df$reaction)), ]
          new_df$metabolite = sapply(new_df$reaction, function(x)
            strsplit(x, split = "\\|")[[1]][2])
          new_df$reaction = sapply(new_df$reaction, function(x)
            strsplit(x, split = "\\|")[[1]][1])
          new_df$reaction = sapply(new_df$reaction, function(x)
            names_dict[1, which(names_dict[2, ] == x)])
          new_df$metabolite = sapply(new_df$metabolite, function(x)
            names_dict[1, which(names_dict[2, ] == x)])
          new_df = new_df[, c(4, 5, 3, 2, 1)]
          
          output$fluxes = renderTable({
            new_df
          }, caption = "GIMME fluxes & stoichiometry",
          caption.placement = getOption("xtable.caption.placement", "top"),
          caption.width = getOption("xtable.caption.width", NULL))
          
          toycon_graph = igraph.from.graphNEL(data)
          net = asNetwork(toycon_graph)
          net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
          reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                              "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                         "vertex.names")]))])
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          visdata$edges$width = log(abs(weights_edges))
        }
      }
      
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(stepX = 75,
                  stepY = 100,
                  width = 0.1) %>%
        visOptions(highlightNearest = TRUE) %>%
        visEdges(color = "black",arrows = "from") %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = "circle") %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = "box") %>% 
        visLayout(randomSeed = 123) 
    })
    output$change_media = renderUI(actionButton(
      inputId = "change_media",
      label = "Change media",style='padding:10px;'
    ))
    output$ko_rxn = renderUI(actionButton(
      inputId = "ko_rxn",
      label = "KO reaction",style='padding:10px;'
    ))
    
    #Prepare select input dropdown menu of reactions to constrain in media types
    observeEvent(input$change_media, {
      showTab(inputId = "tabs", target = "Change media")
        choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_E", names(sbml_model@model@reactions)))])
        names(choices_list) = sapply(choices_list, function(x)
          names_dict[1, which(names_dict[2, ] == x)])
        output$pick_rxn = renderUI(selectInput(
          inputId = "pick_rxn",
          label = "Pick reaction:",
          choices = choices_list,width = "200px"
        ))
        output$lbound = renderUI(textInput(inputId = "lbound", label = "Select the lower bound:", placeholder = "Integer number", width = "200px"))
        output$ubound = renderUI(textInput(inputId = "ubound", label = "Select the upper bound:", placeholder = "Integer number", width = "200px"))
        output$button_apply_media = renderUI(actionButton(inputId = "apply_media", label = "Apply",style='padding:10px;'))
    })
    observeEvent(input$apply_media,{
        reaction = (input$pick_rxn)
        reaction_ID = strsplit(reaction,split = "_")[[1]][2]
        command = paste("bash change_bounds_wrapper.sh", "/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml" , reaction_ID, input$lbound, input$ubound)
        system(command = command)
        flux = as.character(read.table("data/flux_bounds.txt"))
        output$text_flux_media = renderText({
          paste("<br/>", "<b>Flux: ", flux, "</b>", "<br/>", "<br/>")
      })
    })
    ###HERE
    observeEvent(input$ko_rxn, {
      showTab(inputId = "tabs", target = "KO reactions")
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_", names(sbml_model@model@reactions)))])
      names(choices_list) = sapply(choices_list, function(x)
        names_dict[1, which(names_dict[2, ] == x)])
      output$pick_ko_rxn = renderUI(selectInput(
        inputId = "pick_ko_rxn",
        label = "Pick a reaction to KO:",
        choices = choices_list,width = "200px"
      ))
      output$button_apply_ko = renderUI(actionButton(inputId = "apply_ko", label = "Apply",style='padding:10px;'))
    })
    observeEvent(input$apply_ko,{
      reaction = (input$pick_ko_rxn)
      reaction_ID = strsplit(reaction,split = "_")[[1]][2]
      command = paste("bash ko_rxn_wrapper.sh", "/home/mstolarczyk/Uczelnia/UVA/shinyapp/dev/data/toycon.xml" , reaction_ID)
      system(command = command)
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "tomato"
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      flux = read.csv("data/flux_ko.txt",header = F,as.is = T)
      path_ko = as.character(flux[2])
      output$text_flux_ko = renderText({
        paste("<br/>", "<b>Flux: ", as.character(flux[1]), "</b>", "<br/>", "<br/>")
      })
      sbml_model_ko = rsbml_read(path_ko)
      data_ko = rsbml_graph((sbml_model_ko))
      toycon_graph_ko = igraph.from.graphNEL(data_ko)
      visdata_ko <- toVisNetworkData(toycon_graph_ko)
      visdata_ko$nodes$group = rep("Metabolite", length(visdata_ko$nodes$id))
      visdata_ko$nodes$group[which(grepl("R", visdata_ko$nodes$id))] = "Reaction"
      visdata_ko$edges$width = 2
      visdata_ko$edges$length = 150
      #visdata$edges$arrows = c("from", "to")
      net_ko = asNetwork(toycon_graph_ko)
      names_ko = unlist(net_ko$val)[seq(2, length(unlist(net_ko$val)), 2)]
      
      #Setting colors according to node class
      color_reaction_ko = "lightblue"
      color_metabolite_ko = "tomato"
      net_ko %v% "type" = ifelse(grepl("R", names_ko), "Reaction", "Metabolite")
      edges_names_ko = names_ko
      #Setting names
      for (i in seq(1, length(names_ko))) {
        if (any(names(sbml_model_ko@model@species) == as.character(names_ko[i]))) {
          metabolite_name_ko = sbml_model_ko@model@species[[which(names(sbml_model@model@species) == as.character(names_ko[i]))]]@name
          compartment_ko = sbml_model_ko@model@species[[which(names(sbml_model_ko@model@species) == as.character(names_ko[i]))]]@compartment
          metabolite_ko = paste(metabolite_name_ko, compartment_ko, sep = "_")
          edges_names_ko[i] = metabolite_ko
        }
        else{
          if (any(names(sbml_model_ko@model@reactions) == as.character(names_ko[i]))) {
            reaction_name_ko = sbml_model_ko@model@reactions[[which(names(sbml_model_ko@model@reactions) == as.character(names_ko[i]))]]@name
            edges_names_ko[i] = reaction_name_ko
          }
          else{
            edges_names_ko[i] = "NoName"
          }
        }
      }
      names_dict_ko = rbind(edges_names_ko, names_ko) #Names and IDs dictionary
      visdata_ko$nodes$label = as.vector(edges_names_ko)
      
      output$graph_ko = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata_ko$nodes, edges = visdata_ko$edges) %>%
          visLegend(stepX = 75,
                    stepY = 100,
                    width = 0.1) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "from") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite_ko,
                    shape = "circle") %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction_ko,
                    shape = "box") %>%
          visLayout(randomSeed = 123) 
      })
    })
  })
})

