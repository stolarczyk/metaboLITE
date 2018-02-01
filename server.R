
# LOADING LIBRARIES -------------------------------------------------------
library(igraph)
library(rsbml)
library(shiny)
library(intergraph)
library(ggplot2)
library(visNetwork)
library(xtable)
library(dplyr)
library(sna)
library(shinyBS)
library(rPython)
library(shinythemes)

# FUNCTIONS ---------------------------------------------------------------


fill_blank <- function(x, len) {
  len_ori = nchar(x)
  len_diff = len - len_ori
  len_add = len_diff / 2
  len_add_left = ceiling(len_add)
  len_add_right = len_diff - len_add_left
  add_left = paste(rep(" ", len_add_left), collapse = "")
  add_right = paste(rep(" ", len_add_right), collapse = "")
  result = paste(add_left, x, add_right, sep = "")
  return(result)
}


shinyServer(function(input, output, session) {
  working_dir = getwd()
  path = "/data/toycon.xml"
  if( .Platform$OS.type == "windows" ){
    path = gsub("\\\\", "/", path)
  }
  model_file_path = paste(working_dir,path,sep = "")
  hideTab(inputId = "tabs", target = "Change media")
  hideTab(inputId = "tabs", target = "KO reactions")
  
  # VISUALIZATION UPDATE/LAUNCH APP -----------------------------------------
  observeEvent(input$update, ignoreNULL = F , {
    working_dir = getwd()
    path = "/data/toycon.xml"
    if( .Platform$OS.type == "windows" ){
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir,path,sep = "")
    path="/data/model_var.RData"
    if( .Platform$OS.type == "windows" ){
      path = gsub("\\\\", "/", path)
    }
    load(paste(working_dir,path,sep = ""))
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
    color_reaction = "lightblue"
    color_metabolite = "tomato"
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    #Setting names
    for (i in seq(1, length(names))) {
      if (any(names(sbml_model@model@species) == as.character(names[i]))) {
        metabolite_name = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
        compartment = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@compartment
        metabolite = paste(metabolite_name, compartment, sep = " ")
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
    edges_names = sapply(edges_names, function(x)
      fill_blank(x, max(nchar(edges_names))))
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    
    output$graph = renderVisNetwork({
      #reading SBML files
      if (isolate(input$weighting) == "none") {
        edgesize = 0.75
        output$fluxes = renderTable({
          
        })
        python.assign("model_file_path",model_file_path)
        path = "/scripts/check_flux.py"
        if( .Platform$OS.type == "windows" ){
          path = gsub("\\\\", "/", path)
        }
        python.load(paste(working_dir,path,sep = ""))
        flux = python.get(var.name = "flux")
        output$text_flux = renderText({
          paste("<br/>", "<b>Objective value: ", flux, "</b>", "<br/>")
        })
      }
      #Weighting edges
      else{
        python.assign("model_file_path",model_file_path)
        path = "/scripts/check_flux.py"
        if( .Platform$OS.type == "windows" ){
          path = gsub("\\\\", "/", path)
        }
        python.load(paste(working_dir,path,sep = ""))
        flux = python.get(var.name = "flux")
        output$text_flux = renderText({
          paste("<br/>",
                "<br/>",
                "<b>Objective value: ",
                flux,
                "</b>",
                "<br/>",
                "<br/>")
        })
        
        test = unlist(net$val)
        weights_edges = c()
        if (isolate(input$weighting) == "stoichiometry") {
          ndata = names(data@edgeData)
          edges_df = dplyr::mutate(visdata$edges, name = paste(from, to, sep = "|"))
          for (i in seq(1, length(data@edgeData@data))) {
            hit = which(edges_df[, 6] == ndata[i])
            data@edgeData@data[[i]]$stoi = edges_df[hit, 3]
          }
          for (i in seq(1, length(data@edgeData@data))) {
            data@edgeData@data[[i]]$weight = data@edgeData@data[[i]]$stoi
          }
          new_df = data.frame()
          for (i in seq(1, length(data@edgeData@data))) {
            df1 = as.data.frame(data@edgeData@data[[i]])
            df1$reaction = ndata[i]
            new_df = merge(new_df, df1, all = T)
          }
          selection = union(which(grepl("^R_", new_df$reaction)), which(grepl("\\|R_E", new_df$reaction)))
          new_df = new_df[selection,]
          new_df$metabolite = sapply(new_df$reaction, function(x)
            strsplit(x, split = "\\|")[[1]][2])
          new_df$reaction = sapply(new_df$reaction, function(x)
            strsplit(x, split = "\\|")[[1]][1])
          rotate = which(grepl("M_", new_df$reaction))
          cache = new_df[rotate, "reaction"]
          new_df[rotate, "reaction"] = new_df[rotate, "metabolite"]
          new_df[rotate, "metabolite"] = cache
          new_df$reaction = sapply(new_df$reaction, function(x)
            names_dict[1, which(names_dict[2,] == x)])
          new_df$metabolite = sapply(new_df$metabolite, function(x)
            names_dict[1, which(names_dict[2,] == x)])
          new_df = new_df[, c(3, 4, 2)]
          new_df$stoi = as.character(new_df$stoi)
          
          output$fluxes = renderTable({
            new_df
          }, caption = "Stoichiometry",
          caption.placement = getOption("xtable.caption.placement", "top"),
          caption.width = getOption("xtable.caption.width", NULL))
          
          toycon_graph = igraph.from.graphNEL(data)
          net = asNetwork(toycon_graph)
          net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
          reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                              "vertex.names")][which(grepl("^R_", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                          "vertex.names")]))])
          weights_edges = c()
          for (i in seq(1, length(net$mel))) {
            weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
          }
          edgesize = log(abs(weights_edges)) + 1
          visdata$edges$width = edgesize
          visdata$edges$title = paste("Stoichiometric coefficient: ",ceiling(weights_edges))
        }
      }
      path="data/textbooky_coords.csv"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      #Emphasize main reactions
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "25px arial"
      
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(stepX = 75,
                  stepY = 100,
                  width = 0.1) %>%
        visOptions(highlightNearest = TRUE) %>%
        visEdges(
          color = "black",
          arrows = "to",
          smooth = list(
            enabled = TRUE,
            type = "vertical",
            roundness = 0.1,
            forceDirection = "vertical"
          )
        ) %>%
        # visNodes(fixed = T) %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = "circle") %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = "box") %>%
        visLayout(randomSeed = 1) %>%
        visPhysics(barnesHut = list(
          springLength = 200,
          springConstant = 0,
          gravitationalConstant = 0
        ))
    })
    output$change_media = renderUI(
      popify(
        bsButton(inputId = "change_media",
                 label = "Change media"),
        title = "Creates the \"Change media\" tab",
        content = "Simulate model growth media changes by manipulating the exchange reactions bounds",
        placement = "right",
        trigger = "hover"
      )
    )
    
    output$ko_rxn = renderUI(
      popify(
        bsButton(inputId = "ko_rxn",
                 label = "KO reaction"),
        title = "Creates the \"Reaction KO\" tab",
        content = "Simulate reaction Knockouts (KOs) and visualize the model",
        placement = "right",
        trigger = "hover"
      )
    )
    
    #Prepare select input dropdown menu of reactions to constrain in media types

    # SHOW CHANGE MEDIA TAB ---------------------------------------------------
    observeEvent(input$change_media, {
      showTab(inputId = "tabs", target = "Change media")
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_E", names(sbml_model@model@reactions)))])
      names(choices_list) = sapply(choices_list, function(x)
        names_dict[1, which(names_dict[2,] == x)])
      output$pick_rxn = renderUI(
        selectInput(
          inputId = "pick_rxn",
          label = "Pick reaction:",
          choices = choices_list,
          width = "200px"
        )
      )
      output$range = renderUI(
        sliderInput(inputId = "range",min = -1000,
                    max = 1000,
                    label = "Select the exchange limts:",
                    value = c(-1000,1000),
                    step = 10,
                    round = TRUE,
                    ticks = TRUE,
                    width = "300px")
      )
      output$button_apply_media = renderUI(
        popify(
          bsButton(inputId = "apply_media",
                   label = "Constrain"),
          title = "Constrain selected reaction",
          content = "The media changes are performed by constraining the exchange reactions in the model during the FBA simulation",
          placement = "right",
          trigger = "hover"
        )
      )
    })

    # APPLY MEDIA1 ------------------------------------------------------------
    observeEvent(input$media1, {
      media_type = "media1"
      python.assign("media_type", media_type)
      python.assign("model_file_path",model_file_path)
      path="/scripts/run_media.py"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir,path,sep = ""))
      flux = python.get(var.name = "flux")
      fluxes = python.get(var.name = "fluxes")
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      for (i in seq(1, dim(fluxes_output)[1])) {
        hits = which(grepl(fluxes_output[i, 1], ndata))
        for (j in hits) {
          data@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
        }
      }
      
      
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      output$text_flux_media = renderText({
        paste("<br/>",
              "<b>Objective value: ",
              as.character(flux),
              "</b>",
              "<br/>")
      })
      
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      dashed = rep(FALSE, length(weights_edges))
      dashed[which(weights_edges == 0)] = TRUE 
      visdata$edges$dashes = dashed
      edgesize = log(abs(weights_edges)) + 1
      visdata$edges$width = edgesize
      visdata$edges$length = 150
      visdata$edges$title = paste("Flux: ",ceiling(weights_edges))
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
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, max(nchar(edges_names))))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      path="data/textbooky_coords.csv"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      set1 = rownames(visdata$nodes)
      set2 = rownames(coords)
      deleted_rxn = setdiff(set2, set1)
      if (length(deleted_rxn) > 0) {
        coords_deleted_rxn = coords[-(which(rownames(coords) == deleted_rxn)),]
      }
      else{
        coords_deleted_rxn = coords
      }
      visdata$nodes = cbind(visdata$nodes, coords_deleted_rxn)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "25px arial"
      output$graph_media = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
          visLegend(stepX = 75,
                    stepY = 100,
                    width = 0.1) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "to") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite,
                    shape = "circle") %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction,
                    shape = "box") %>%
          visPhysics(barnesHut = list(
            springLength = 200,
            springConstant = 0,
            gravitationalConstant = 0
          )) %>%
          visLayout(randomSeed = 1)
      })
    })

    # APPLY MEDIA2 ------------------------------------------------------------
    observeEvent(input$media2, {
      media_type = "media2"
      python.assign("media_type", media_type)
      python.assign("model_file_path",model_file_path)
      path="/scripts/run_media.py"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir,path,sep = ""))
      flux = python.get(var.name = "flux")
      fluxes = python.get(var.name = "fluxes")
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      for (i in seq(1, dim(fluxes_output)[1])) {
        hits = which(grepl(fluxes_output[i, 1], ndata))
        for (j in hits) {
          data@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
        }
      }
      
      
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      output$text_flux_media = renderText({
        paste("<br/>",
              "<b>Objective value: ",
              as.character(flux),
              "</b>",
              "<br/>")
      })
      
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      dashed = rep(FALSE, length(weights_edges))
      dashed[which(weights_edges == 0)] = TRUE 
      visdata$edges$dashes = dashed
      edgesize = log(abs(weights_edges)) + 1
      visdata$edges$width = edgesize
      visdata$edges$length = 150
      visdata$edges$title = paste("Flux: ",ceiling(weights_edges))
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
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, max(nchar(edges_names))))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      path="data/textbooky_coords.csv"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      set1 = rownames(visdata$nodes)
      set2 = rownames(coords)
      deleted_rxn = setdiff(set2, set1)
      if (length(deleted_rxn) > 0) {
        coords_deleted_rxn = coords[-(which(rownames(coords) == deleted_rxn)),]
      }
      else{
        coords_deleted_rxn = coords
      }
      visdata$nodes = cbind(visdata$nodes, coords_deleted_rxn)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "25px arial"
      output$graph_media = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
          visLegend(stepX = 75,
                    stepY = 100,
                    width = 0.1) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "to") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite,
                    shape = "circle") %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction,
                    shape = "box") %>%
          visPhysics(barnesHut = list(
            springLength = 200,
            springConstant = 0,
            gravitationalConstant = 0
          )) %>%
          visLayout(randomSeed = 1)
      })
    })

    # APPLY MEDIA3 ------------------------------------------------------------
    observeEvent(input$media3, {
      media_type = "media3"
      python.assign("media_type", media_type)
      python.assign("model_file_path",model_file_path)
      path="/scripts/run_media.py"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir,path,sep = ""))
      flux = python.get(var.name = "flux")
      fluxes = python.get(var.name = "fluxes")
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      for (i in seq(1, dim(fluxes_output)[1])) {
        hits = which(grepl(fluxes_output[i, 1], ndata))
        for (j in hits) {
          data@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
        }
      }
      
      
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      output$text_flux_media = renderText({
        paste("<br/>",
              "<b>Objective value: ",
              as.character(flux),
              "</b>",
              "<br/>")
      })
      
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      dashed = rep(FALSE, length(weights_edges))
      dashed[which(weights_edges == 0)] = TRUE 
      visdata$edges$dashes = dashed
      edgesize = log(abs(weights_edges)) + 1
      visdata$edges$width = edgesize
      visdata$edges$length = 150
      visdata$edges$title = paste("Flux: ",ceiling(weights_edges))
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
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, max(nchar(edges_names))))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      path="data/textbooky_coords.csv"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      set1 = rownames(visdata$nodes)
      set2 = rownames(coords)
      deleted_rxn = setdiff(set2, set1)
      if (length(deleted_rxn) > 0) {
        coords_deleted_rxn = coords[-(which(rownames(coords) == deleted_rxn)),]
      }
      else{
        coords_deleted_rxn = coords
      }
      visdata$nodes = cbind(visdata$nodes, coords_deleted_rxn)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "25px arial"
      output$graph_media = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
          visLegend(stepX = 75,
                    stepY = 100,
                    width = 0.1) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "to") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite,
                    shape = "circle") %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction,
                    shape = "box") %>%
          visPhysics(barnesHut = list(
            springLength = 200,
            springConstant = 0,
            gravitationalConstant = 0
          )) %>%
          visLayout(randomSeed = 1)
      })
    })
    
    # APPLY MEDIA -------------------------------------------------------------
    observeEvent(input$apply_media, {
      lb = input$range[1]
      ub = input$range[2]
      # lb = input$lbound
      # ub = input$ubound
      reaction = (input$pick_rxn)
      reaction_ID = strsplit(reaction, split = "_")[[1]][2]
      if (lb < ub) {
        python.assign("lb", lb)
        python.assign("ub", ub)
        python.assign("reaction_ID", reaction_ID)
        python.assign("model_file_path",model_file_path)
        path="/scripts/change_bounds.py"
        if( .Platform$OS.type == "windows" ){
          path = gsub("\\\\", "/", path)
        }
        python.load(paste(working_dir,path,sep = ""))
        flux = python.get(var.name = "flux")
        fluxes = python.get(var.name = "fluxes")
        fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
        fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
        rownames(fluxes_output) = c()
        colnames(fluxes_output) = c("Reaction", "Flux")
        
        data = rsbml_graph((sbml_model))
        ndata = names(data@edgeData)
        for (i in seq(1, dim(fluxes_output)[1])) {
          hits = which(grepl(fluxes_output[i, 1], ndata))
          for (j in hits) {
            data@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
          }
        }
        
        
        for (i in seq(1, dim(names_dict)[2], by = 1)) {
          #Mapping nodes IDs to names for table displaying purposes
          if (any(which(fluxes_output[, 1] == names_dict[2, i])))
            fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
        }
        
        net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
        edges_names = names
        output$text_flux_media = renderText({
          paste("<br/>",
                "<b>Objective value: ",
                as.character(flux),
                "</b>",
                "<br/>")
        })
        
        toycon_graph = igraph.from.graphNEL(data)
        net = asNetwork(toycon_graph)
        net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
        reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                            "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                       "vertex.names")]))])
        
        toycon_graph = igraph.from.graphNEL(data)
        visdata <- toVisNetworkData(toycon_graph)
        visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
        visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
        weights_edges = c()
        for (i in seq(1, length(net$mel))) {
          weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
        }
        dashed = rep(FALSE, length(weights_edges))
        dashed[which(weights_edges == 0)] = TRUE 
        visdata$edges$dashes = dashed
        edgesize = log(abs(weights_edges)) + 1
        visdata$edges$width = edgesize
        visdata$edges$length = 150
        visdata$edges$title = paste("Flux: ",ceiling(weights_edges))
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
        edges_names = sapply(edges_names, function(x)
          fill_blank(x, max(nchar(
            edges_names
          ))))
        names_dict = rbind(edges_names, names) #Names and IDs dictionary
        visdata$nodes$label = as.vector(edges_names)
        path="data/textbooky_coords.csv"
        if( .Platform$OS.type == "windows" ){
          path = gsub("\\\\", "/", path)
        }
        coords = read.csv(path)
        set1 = rownames(visdata$nodes)
        set2 = rownames(coords)
        deleted_rxn = setdiff(set2, set1)
        if (length(deleted_rxn) > 0) {
          coords_deleted_rxn = coords[-(which(rownames(coords) == deleted_rxn)),]
        }
        else{
          coords_deleted_rxn = coords
        }
        visdata$nodes = cbind(visdata$nodes, coords_deleted_rxn)
        visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "25px arial"
        visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "25px arial"
        visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "25px arial"
        visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "25px arial"
        output$graph_media = renderVisNetwork({
          #Plotting graph
          visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
            visLegend(stepX = 75,
                      stepY = 100,
                      width = 0.1) %>%
            visOptions(highlightNearest = TRUE) %>%
            visEdges(color = "black", arrows = "to") %>%
            visGroups(groupname = "Metabolite",
                      color = color_metabolite,
                      shape = "circle") %>%
            visGroups(groupname = "Reaction",
                      color = color_reaction,
                      shape = "box") %>%
            visPhysics(
              barnesHut = list(
                springLength = 200,
                springConstant = 0,
                gravitationalConstant = 0
              )
            ) %>%
            visLayout(randomSeed = 1)
        })
      } else{
        showNotification(
          HTML("Wrong bounds values"),
          duration = 2,
          type = "error",
          session = session
        )
        showNotification(
          HTML("Lower bound must be greater than upper bound"),
          duration = 2,
          session = session
        )
      }
      
      
      #VISUALIZEHERE
      
      output$media1 = renderUI({
        popify(
          bsButton(inputId = "media1",
                   label = "Glucose free media"),
          title = "Apply glucose free media",
          content = "<b>glucose exchange bounds:</b><br> lower = 0, upper = 0 <br>",
          placement = "right",
          trigger = "hover"
        )
      })
      output$text_media = renderText({
        paste("<br/>", "<b>Use predefined media: ", "</b>", "<br/>")
      })
      output$media2 = renderUI({
        popify(
          bsButton(inputId = "media2",
                   label = "Microaerophilic media"),
          title = "Apply microaerophilic media",
          content = "<b>O2 exchange bounds:</b><br>lower = -10, upper = 10",
          placement = "right",
          trigger = "hover"
        )
      })
      output$media3 = renderUI({
        popify(
          bsButton(inputId = "media3",
                   label = "Lactate rich media"),
          title = "Apply lactate rich media",
          content = "<b>lactate exchange bounds:</b><br> lower = -700, upper = 700 <br>",
          placement = "right",
          trigger = "hover"
        )
      })
    })
    observeEvent(input$ko_rxn, {
      showTab(inputId = "tabs", target = "KO reactions")
      updateTabsetPanel(session, "tabs",
                        selected = "ko")
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_", names(sbml_model@model@reactions)))])
      names(choices_list) = sapply(choices_list, function(x)
        names_dict[1, which(names_dict[2,] == x)])
      output$pick_ko_rxn = renderUI(
        selectInput(
          inputId = "pick_ko_rxn",
          label = "Pick a reaction to KO:",
          choices = choices_list,
          width = "200px"
        )
      )
      output$button_apply_ko = renderUI({
        popify(
          bsButton(inputId = "apply_ko",
                   label = "Knockout"),
          title = "Knocksout the reaction picked above",
          content = "The reaction knockout can correspond to complete enzyme inhibition that catalyzes the reaction in question",
          placement = "right",
          trigger = "hover"
        )
      })
    })
    
    # KO RESET ----------------------------------------------------------------
    observeEvent(input$reset, {
      reaction_ID = "Reset"
      python.assign("reaction_ID", reaction_ID)
      python.assign("model_file_path",model_file_path)
      path="/scripts/ko_rxn.py"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir,path,sep = ""))
      flux = python.get(var.name = "flux")
      path_removed = python.get(var.name = "path_removed")
      path_ko = path_removed
      fluxes = python.get(var.name = "fluxes")
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      for (i in seq(1, dim(fluxes_output)[1])) {
        hits = which(grepl(fluxes_output[i, 1], ndata))
        for (j in hits) {
          data@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
        }
      }
      
      
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
      output$fluxes_ko = renderTable({
        fluxes_output
      }, width = "250", caption = "Fluxes without any KOs",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      output$text_flux_media = renderText({
        paste("<br/>",
              "<b>Objective value: ",
              as.character(flux),
              "</b>",
              "<br/>")
      })
      
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      dashed = rep(FALSE, length(weights_edges))
      dashed[which(weights_edges == 0)] = TRUE 
      visdata$edges$dashes = dashed
      edgesize = log(abs(weights_edges)) + 1
      visdata$edges$width = edgesize
      visdata$edges$length = 150
      visdata$edges$title = paste("Flux: ",ceiling(weights_edges))
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
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, max(nchar(edges_names))))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      path="data/textbooky_coords.csv"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      set1 = rownames(visdata$nodes)
      set2 = rownames(coords)
      deleted_rxn = setdiff(set2, set1)
      if (length(deleted_rxn) > 0) {
        coords_deleted_rxn = coords[-(which(rownames(coords) == deleted_rxn)),]
      }
      else{
        coords_deleted_rxn = coords
      }
      visdata$nodes = cbind(visdata$nodes, coords_deleted_rxn)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "25px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "25px arial"
      output$graph_ko = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
          visLegend(stepX = 75,
                    stepY = 100,
                    width = 0.1) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "to") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite,
                    shape = "circle") %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction,
                    shape = "box") %>%
          visPhysics(barnesHut = list(
            springLength = 200,
            springConstant = 0,
            gravitationalConstant = 0
          )) %>%
          visLayout(randomSeed = 1)
      })
    })

    
    # APPLY KO ----------------------------------------------------------------
    observeEvent(input$apply_ko, {
      output$reset_ko = renderUI(
        popify(
          bsButton(inputId = "reset",
                   label = "Reset"),
          title = "Reset model",
          content = "Brings the model back to its original state",
          placement = "right",
          trigger = "hover"
        )
      )
      
      reaction = (input$pick_ko_rxn)
      reaction_ID = strsplit(reaction, split = "_")[[1]][2]
      python.assign("reaction_ID", reaction_ID)
      python.assign("model_file_path",model_file_path)
      path="/scripts/ko_rxn.py"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir,path,sep = ""))
      flux = python.get(var.name = "flux")
      path_removed = python.get(var.name = "path_removed")
      path_ko = path_removed
      fluxes = python.get(var.name = "fluxes")
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      
      sbml_model_ko = rsbml_read(path_ko)
      data_ko = rsbml_graph((sbml_model_ko))
      ndata = names(data_ko@edgeData)
      for (i in seq(1, dim(fluxes_output)[1])) {
        hits = which(grepl(fluxes_output[i, 1], ndata))
        for (j in hits) {
          data_ko@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
        }
      }
      
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
      output$fluxes_ko = renderTable({
        fluxes_output
      }, width = "250", caption = paste("Fluxes after the KO of", names_dict[1, which(names_dict[2, ] == paste("R_", reaction_ID, sep = ""))]),
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      
      output$text_flux_ko = renderText({
        paste(
          "<br/>",
          "<br/>",
          "<b>Objective value: ",
          as.character(flux),
          "</b>",
          "<br/>",
          "<br/>"
        )
      })
      
      toycon_graph_ko = igraph.from.graphNEL(data_ko)
      visdata_ko <- toVisNetworkData(toycon_graph_ko)
      visdata_ko$nodes$group = rep("Metabolite", length(visdata_ko$nodes$id))
      visdata_ko$nodes$group[which(grepl("R", visdata_ko$nodes$id))] = "Reaction"
      visdata_ko$edges$length = 150
      net_ko = asNetwork(toycon_graph_ko)
      names_ko = unlist(net_ko$val)[seq(2, length(unlist(net_ko$val)), 2)]
      net_ko %v% "type" = ifelse(grepl("R", names_ko), "Reaction", "Metabolite")
      
      weights_edges = c()
      for (i in seq(1, length(net_ko$mel))) {
        weights_edges = append(weights_edges, net_ko$mel[[i]][[3]][[2]])
      }
      dashed = rep(FALSE, length(weights_edges))
      dashed[which(weights_edges == 0)] = TRUE 
      visdata_ko$edges$dashes = dashed
      edgesize = log(abs(weights_edges)) + 1
      visdata_ko$edges$width = edgesize
      visdata_ko$edges$title = paste("Flux: ",ceiling(weights_edges))
      
      #Setting colors according to node class
      color_reaction_ko = "lightblue"
      color_metabolite_ko = "tomato"
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
      edges_names_ko = sapply(edges_names_ko, function(x)
        fill_blank(x, max(nchar(
          edges_names_ko
        ))))
      names_dict_ko = rbind(edges_names_ko, names_ko) #Names and IDs dictionary
      visdata_ko$nodes$label = as.vector(edges_names_ko)
      path="data/textbooky_coords.csv"
      if( .Platform$OS.type == "windows" ){
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      set1 = rownames(visdata_ko$nodes)
      set2 = rownames(coords)
      deleted_rxn = setdiff(set2, set1)
      if (length(deleted_rxn) > 0) {
        coords_deleted_rxn = coords[-(which(rownames(coords) == deleted_rxn)),]
      }
      else{
        coords_deleted_rxn = coords
      }
      visdata_ko$nodes = cbind(visdata_ko$nodes, coords_deleted_rxn)
      #Emphasize main reactions
      visdata_ko$nodes[which(grepl("glycolysis", names_dict_ko[1, ])), "font"] = "25px arial"
      visdata_ko$nodes[which(grepl("respiration", names_dict_ko[1, ])), "font"] = "25px arial"
      visdata_ko$nodes[which(grepl("synthase", names_dict_ko[1, ])), "font"] = "25px arial"
      visdata_ko$nodes[which(grepl("demand", names_dict_ko[1, ])), "font"] = "25px arial"
      output$graph_ko = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata_ko$nodes, edges = visdata_ko$edges) %>%
          visLegend(stepX = 75,
                    stepY = 100,
                    width = 0.1) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "to") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite_ko,
                    shape = "circle") %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction_ko,
                    shape = "box") %>%
          visPhysics(barnesHut = list(
            springLength = 200,
            springConstant = 0,
            gravitationalConstant = 0
          )) %>%
          visLayout(randomSeed = 1)
      })
    })
  })
  
  session$onSessionEnded(function() {
    setwd("data")
    file.remove(list.files(pattern = "flux*"))
    file.remove(list.files(pattern = "*removed*"))
    stopApp()
  })
})
