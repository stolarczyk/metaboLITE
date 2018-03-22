

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
library(markdown)

# FUNCTIONS ---------------------------------------------------------------

fill_blank <- function(x, len) {
  if (nchar(x) > len) {
    result = x
  } else{
    len_ori = nchar(x)
    len_diff = len - len_ori
    len_add = len_diff / 2
    len_add_left = ceiling(len_add)
    len_add_right = len_diff - len_add_left
    add_left = paste(rep(" ", len_add_left), collapse = "")
    add_right = paste(rep(" ", len_add_right), collapse = "")
    result = paste(add_left, x, add_right, sep = "")
  }
  return(result)
}

add_dups_new_layout <- function(visdata) {
  visdata$nodes[23, ] = c("M_m01c1", "M_m01c1")#ADP
  visdata$nodes[28, ] = c("M_m01c2", "M_m01c2")#ADP
  visdata$nodes[22, ] = c("M_m02c1", "M_m02c1")#ATP
  visdata$nodes[21, ] = c("M_m06c1", "M_m06c1")#H2O
  visdata$nodes[20, ] = c("M_m09c1", "M_m09c1")#Pi
  visdata$nodes[24, ] = c("R_E41", "R_E41")#H2O exchange
  visdata$nodes[25, ] = c("M_m05c1", "M_m05c1")#H
  visdata$nodes[26, ] = c("M_m05m1", "M_m05m1")#H
  visdata$nodes[27, ] = c("M_m09c2", "M_m09c2")#Pi
  
  rownames(visdata$nodes) = visdata$nodes[, 1]
  
  visdata$edges[15, 1] = "M_m09c1"
  visdata$edges[30, 2] = "M_m09c2"
  visdata$edges[8, 1] = "M_m06c1"
  visdata$edges[28, 2] = "M_m06c1"
  visdata$edges[31, ] = c("M_m06c1", "R_E41", "1")
  visdata$edges[16, 1] = "M_m02c1"
  visdata$edges[26, 2] = "M_m02c1"
  visdata$edges[12, 1] = "M_m01c1"
  visdata$edges[29, 2] = "M_m01c2"
  visdata$edges[17, 1] = "M_m05c1"
  visdata$edges[27, 2] = "M_m05m1"
  return(visdata)
}

shinyServer(function(input, output, session) {
  hideTab(inputId = "tabs", target = "change_media")
  hideTab(inputId = "tabs", target = "ko_reactions")
  hideTab(inputId = "tabs", target = "simulate_expression_changes")
  working_dir = getwd()
  path = "/data/toycon.xml"
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  model_file_path = paste(working_dir, path, sep = "")
  

  
  # VISUALIZATION UPDATE/LAUNCH APP -----------------------------------------
  
  observeEvent(input$update, ignoreNULL = F , {
    working_dir = getwd()
    path = "/data/toycon.xml"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = "/data/model_var.RData"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = ""))
    toycon = readRDS(paste(working_dir, "/data/toycon1.rda", sep = ""))
    data = rsbml_graph((sbml_model))
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    visdata = add_dups_new_layout(visdata)
    
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (nchar(names[i]) < 6) {
        names[i] = substr(names[i], 1, 4)
      } else{
        names[i] = substr(names[i], 1, 6)
      }
      if (any(names(sbml_model@model@species) == as.character(names[i]))) {
        metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
    
    #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
    edges_names = sapply(edges_names, function(x)
      fill_blank(x, 7))
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    
    output$graph = renderVisNetwork({
      if (isolate(input$weighting) == "none") {
        edgesize = 0.75
        output$fluxes = renderTable({
          
        })
        #binds the R variable to Python variable
        python.assign("model_file_path", model_file_path)
        path = "/scripts/check_flux.py"
        if (.Platform$OS.type == "windows") {
          path = gsub("\\\\", "/", path)
        }
        #runs the script specified by the path variable
        python.load(paste(working_dir, path, sep = ""))
        #gets the python variable after the script execution
        flux = python.get(var.name = "flux")
        output$text_flux = renderText({
          paste("<br/>", "<b>Objective value: ", flux, "</b>", "<br/>")
        })
      }
      #Weighting edges
      else{
        python.assign("model_file_path", model_file_path)
        path = "/scripts/check_flux.py"
        if (.Platform$OS.type == "windows") {
          path = gsub("\\\\", "/", path)
        }
        python.load(paste(working_dir, path, sep = ""))
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
        #The code block below executes if user selects the log2(stoichiometry) weighting scheme
        if (isolate(input$weighting) == "stoichiometry") {
          #Necessary transformations for the table displaying purposes
          ndata = names(data@edgeData)
          edges_df = dplyr::mutate(visdata_ori$edges, name = paste(from, to, sep = "|"))
          for (i in seq(1, length(data@edgeData@data))) {
            hit = which(edges_df$name == ndata[i])
            data@edgeData@data[[i]]$stoi = edges_df[hit, 3]
          }
          #Rewrites the coefficients to edge's weight slot
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
            names_dict[1, which(names_dict[2,] == x)[1]])
          new_df$metabolite = sapply(new_df$metabolite, function(x)
            names_dict[1, which(names_dict[2,] == x)[1]])
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
          #Transfer missing weights
          from_idx = which(substr(visdata$edges[dim(visdata$edges)[1], 1][1], 1, nchar(visdata$edges[dim(visdata$edges)[1], 1][1]) -
                                    1) == visdata$edges[, 1])
          to_idx = which(substr(visdata$edges[dim(visdata$edges)[1], 2][1], 1, nchar(visdata$edges[dim(visdata$edges)[1], 2][1]) -
                                  1) == visdata$edges[, 2])
          if (from_idx == to_idx) {
            visdata$edges[dim(visdata$edges)[1], 3] = visdata$edges[from_idx, 3]
          }
          weights_edges = as.numeric(visdata$edges$weight)
          edgesize = log(abs(weights_edges)) + 1
          visdata$edges$width = edgesize
          visdata$edges$title = paste("Stoichiometric coefficient: ",
                                      round(weights_edges))
        }
      }
      #Read the saved coordinates for the graph dispalying purpose
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      #Emphasize main reactions
      visdata$nodes[which(grepl("^glycolysis$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^respiration$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
      
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

    output$text_main = renderText({
      paste("<u><b>Launch tabs with following functionalities: ",
            "</b></u>")
    })
    output$text_vis = renderText({
      paste("<u><b>Visualize the metabolic network: ", "</b></u>")
    })
    
    addPopover(
      session = session,
      id = "apply_media_popover",
      title = "Constrain selected reaction",
      content = "The media changes are performed by constraining the exchange reactions in the model during the FBA simulation",
      placement = "right",
      trigger = "click",
      options = list(container = "body")
  )
    
    addPopover(
      session = session,
      id = "pick_rxn_ko_popover",
      title = "Knocksout the reaction picked above",
      content = "The reaction knockout can correspond to complete enzyme inhibition that catalyzes the reaction in question",
      placement = "right",
      trigger = "click",
      options = list(container = "body")
  )
    
    addPopover(
      session = session,
      id = "media1",
      title = "Apply glucose free media",
      content = "<b>glucose exchange bounds:</b><br> lower = 0, upper = 0 <br>",
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
    addPopover(
      session = session,
      id = "media2",
      title = "Apply microaerophilic media",
      content = "<b>O2 exchange bounds:</b><br>lower = -10, upper = 10",
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
    
    addPopover(
      session = session,
      id = "media3",
      title = "Apply lactate rich media",
      content = "<b>lactate exchange bounds:</b><br> lower = -700, upper = 700 <br>",
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )

    
    addPopover(
      session = session,
      id = "expression_popover",
      title = "Adjusts the gene expression level",
      content = "The pseudo-expression level scale (0 - 1) corresponds to \"no expression\" and \"maximal overexpression\", respectively. It directly influences the flux that is carried by the reaction catalyzed by the enzyme encoded by the gene in question",
      placement = "right",
      trigger = "click",
      options = list(container = "body")
    )
    
    observeEvent(input$tabs_popover, ignoreInit = T, {
      updateNavbarPage(session = session,
                       inputId = "tabs",
                       selected = "help")
    })
    
    updateNavbarPage(session = session,
                     inputId = "tabs",
                     selected = "visualize")
    
    
    
    # SHOW CHANGE MEDIA TAB ---------------------------------------------------
    observeEvent(input$change_media, ignoreInit = T, {
      #Prepare the list of reactions to constrain
      showTab(inputId = "tabs", target = "change_media")
      updateNavbarPage(session = session,
                       inputId = "tabs",
                       selected = "change_media")
      # choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_E", names(sbml_model@model@reactions)))])
      # names(choices_list) = sapply(choices_list, function(x)
      #   names_dict[1, which(names_dict[2,] == x)[1]])
      

      #render th text to display in the UI
      output$text_media = renderText({
        paste("<u><b>Use predefined media: ", "</b></u>")
      })


    })
    
    # SHOW SIMULATE EXPR TAB --------------------------------------------------
    
    observeEvent(input$simulate_expr, ignoreInit = T, {
      #Show the tab in the app
      showTab(inputId = "tabs", target = "simulate_expression_changes")
      updateNavbarPage(session = session,
                       inputId = "tabs",
                       selected = "simulate_expression_changes")
      
      #Prepare the choices list of the reactions/genes which expression can be adjusted
      choices_list_expr = as.list(toycon@react_name)
      names(choices_list_expr) = paste(toycon@allGenes, toycon@react_name, sep = ": ")
      #render the selection list for the UI
      output$pick_expr_gene = renderUI(
        selectInput(
          inputId = "pick_expr_gene",
          label = NULL,
          choices = choices_list_expr,
          width = "200px"
        )
      )
      #render the apply button for the UI
      output$button_apply_expr = renderUI({
        bsButton(inputId = "apply_expr",
                 label = "Adjust")
      })
      #Render the selection slider for the expression level adjustment
      output$expr = renderUI(
        sliderInput(
          inputId = "expr",
          min = 0,
          max = 1,
          label = NULL,
          value = 0.5,
          step = 0.1,
          round = TRUE,
          ticks = F,
          width = "300px"
        )
      )
    })
    
    # APPLY MEDIA1 ------------------------------------------------------------
    observeEvent(input$media1, {
      #Define the type of media and assigne to the python variable
      media_type = "media1"
      python.assign("media_type", media_type)
      python.assign("model_file_path", model_file_path)
      path = "/scripts/run_media.py"
      #change the path in case of widnowsOS
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      #run the script changing media(rxn bounds)
      python.load(paste(working_dir, path, sep = ""))
      #Get the resulting flux and fluxes
      flux = python.get(var.name = "flux")
      fluxes = python.get(var.name = "fluxes")
      #Transfor result for the presentation
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      #Read the sbml model
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      #Write the fluxes to the data structure for the later use
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
      #render the tex for UI
      output$text_flux_media = renderText({
        paste("<br/>",
              "<b>Objective value: ",
              as.character(flux),
              "</b>",
              "<br/>")
      })
      
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      #Set the type of the node depending on its name
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      #Assign proper names
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      #transform the data to visNetwork format
      visdata_ori <- toVisNetworkData(toycon_graph)
      #preserve the original model
      visdata = visdata_ori
      visdata_ori$nodes$group = rep("Metabolite", length(visdata_ori$nodes$id))
      visdata_ori$nodes$group[which(grepl("R", visdata_ori$nodes$id))] = "Reaction"
      
      #Adding duplicate metabolites/reactions
      visdata = add_dups_new_layout(visdata)
      
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      #Get the weights from the net object
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      #calculate the thickness of each edge
      edgesize = log(abs(as.numeric(visdata$edges$weight))) + 1
      #assign the thickness values to the slot
      visdata$edges$width = edgesize
      #dash the edges of the graph that carry 0 flux
      dashed = rep(FALSE, dim(visdata$edges)[1])
      dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
      visdata$edges$dashes = dashed
      #Add data for the popup titles for the edges
      visdata$edges$title = paste("Flux: ", round(as.numeric(visdata$edges$weight)))
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      
      #Setting colors according to the node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      names = rownames(visdata$nodes)
      edges_names = names
      #Setting proper names names
      for (i in seq(1, length(names))) {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
      #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      
      #Read the saved coordinates for the graph dispalying purpose
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      #Empasise the main reactions
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
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
    #For the inline comments check out the APPLY MEDIA1 section above
    observeEvent(input$media2, {
      media_type = "media2"
      python.assign("media_type", media_type)
      python.assign("model_file_path", model_file_path)
      path = "/scripts/run_media.py"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir, path, sep = ""))
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
      visdata_ori <- toVisNetworkData(toycon_graph)
      visdata = visdata_ori
      visdata_ori$nodes$group = rep("Metabolite", length(visdata_ori$nodes$id))
      visdata_ori$nodes$group[which(grepl("R", visdata_ori$nodes$id))] = "Reaction"
      
      #Adding duplicate metabolites/reactions
      visdata = add_dups_new_layout(visdata)
      
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      
      
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      edgesize = log(abs(as.numeric(visdata$edges$weight))) + 1
      visdata$edges$width = edgesize
      #visdata = transfer_visdata_weights(visdata,visdata_ori)
      
      
      dashed = rep(FALSE, dim(visdata$edges)[1])
      dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
      visdata$edges$dashes = dashed
      visdata$edges$title = paste("Flux: ", round(as.numeric(visdata$edges$weight)))
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
      #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
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
    #For the inline comments check out the APPLY MEDIA1 section above
    observeEvent(input$media3, {
      media_type = "media3"
      python.assign("media_type", media_type)
      python.assign("model_file_path", model_file_path)
      path = "/scripts/run_media.py"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      python.load(paste(working_dir, path, sep = ""))
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
      visdata_ori <- toVisNetworkData(toycon_graph)
      visdata = visdata_ori
      visdata_ori$nodes$group = rep("Metabolite", length(visdata_ori$nodes$id))
      visdata_ori$nodes$group[which(grepl("R", visdata_ori$nodes$id))] = "Reaction"
      
      #Adding duplicate metabolites/reactions
      visdata = add_dups_new_layout(visdata)
      
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      
      
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      edgesize = log(abs(as.numeric(visdata$edges$weight))) + 1
      visdata$edges$width = edgesize
      #visdata = transfer_visdata_weights(visdata,visdata_ori)
      
      
      dashed = rep(FALSE, dim(visdata$edges)[1])
      dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
      visdata$edges$dashes = dashed
      visdata$edges$title = paste("Flux: ", round(as.numeric(visdata$edges$weight)))
      #visdata$edges$arrows = c("from", "to")
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
      #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
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
    
    observeEvent(input$media_custom, {
      
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_E", names(sbml_model@model@reactions)))])
      names(choices_list) = sapply(choices_list, function(x)
        names_dict[1, which(names_dict[2,] == x)[1]])
      
      #render the UI select component
      output$pick_rxn = renderUI(
        selectInput(
          inputId = "pick_rxn",
          label = "Pick reaction:",
          choices = choices_list,
          width = "200px"
        )
      )
      
      #render th text to display in the UI
      output$text_own = renderText({
        paste("<u><b>Adjust the exchange limits yourself: ",
              "</b></u>")
      })
      
      
      #render the flux range limiting slider for the UI
      output$range = renderUI(
        sliderInput(
          inputId = "range",
          min = -1000,
          max = 1000,
          label = "Select the exchange limts:",
          value = c(-1000, 1000),
          step = 10,
          round = TRUE,
          ticks = TRUE,
          width = "300px"
        )
      )
      
      
      output$range_help = renderUI(
        actionLink(inputId = "range_popover","",icon=icon("question-circle-o"))
      )

      #render the button applying the changes
      output$button_apply_media = renderUI(
        bsButton(inputId = "apply_media",
                 label = "Constrain")
      )

      addPopover(
        session = session,
        id = "range_help",
        title = "Technical information",
        content = "This slider adjusts the upper and lower bound, which define the maximum and minimum allowable fluxes of the reactions.",
        placement = "right",
        trigger = "click",
        options = list(container = "body")
      )
    }
    )
    # APPLY MEDIA -------------------------------------------------------------
    observeEvent(input$apply_media, {
      #get the bouds to be applied
      lb = input$range[1]
      ub = input$range[2]
      #get the reaction to be constrained
      reaction = (input$pick_rxn)
      reaction_ID = strsplit(reaction, split = "_")[[1]][2]
      if (lb < ub) {
        #use Python to change the bouds and perform the FBA
        python.assign("lb", lb)
        python.assign("ub", ub)
        python.assign("reaction_ID", reaction_ID)
        python.assign("model_file_path", model_file_path)
        path = "/scripts/change_bounds.py"
        if (.Platform$OS.type == "windows") {
          path = gsub("\\\\", "/", path)
        }
        python.load(paste(working_dir, path, sep = ""))
        #Get the results
        flux = python.get(var.name = "flux")
        fluxes = python.get(var.name = "fluxes")
        fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
        fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
        rownames(fluxes_output) = c()
        colnames(fluxes_output) = c("Reaction", "Flux")
        #Import fluxes into the data object for further use
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
        #Render UI text
        output$text_flux_media = renderText({
          paste("<br/>",
                "<b>Objective value: ",
                as.character(flux),
                "</b>",
                "<br/>")
        })
        
        toycon_graph = igraph.from.graphNEL(data)
        net = asNetwork(toycon_graph)
        #Set the type of the node depending on its name
        net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
        #Assign proper names
        reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                            "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                       "vertex.names")]))])
        #transform the data to visNetwork format
        toycon_graph = igraph.from.graphNEL(data)
        visdata_ori <- toVisNetworkData(toycon_graph)
        #preserve the original model
        visdata = visdata_ori
        visdata_ori$nodes$group = rep("Metabolite", length(visdata_ori$nodes$id))
        visdata_ori$nodes$group[which(grepl("R", visdata_ori$nodes$id))] = "Reaction"
        
        #Adding duplicate metabolites/reactions
        visdata = add_dups_new_layout(visdata)
        
        visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
        visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
        
        #Get the weights from the net object
        weights_edges = c()
        for (i in seq(1, length(net$mel))) {
          weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
        }
        #calculate the thickness of each edge
        edgesize = log(abs(as.numeric(visdata$edges$weight))) + 1
        #assign the thickness values to the slot
        visdata$edges$width = edgesize
        #dash the edges of the graph that carry 0 flux
        dashed = rep(FALSE, dim(visdata$edges)[1])
        dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
        visdata$edges$dashes = dashed
        #Add data for the popup titles for the edges
        visdata$edges$title = paste("Flux: ", round(as.numeric(visdata$edges$weight)))
        net = asNetwork(toycon_graph)
        names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
        
        #Setting colors according to node class
        color_reaction = "lightblue"
        color_metabolite = "lightsalmon"
        names = rownames(visdata$nodes)
        net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
        edges_names = names
        #Setting names
        for (i in seq(1, length(names))) {
          if (nchar(names[i]) < 6) {
            names[i] = substr(names[i], 1, 4)
          } else{
            names[i] = substr(names[i], 1, 6)
          }
          if (any(names(sbml_model@model@species) == as.character(names[i]))) {
            metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
        #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
        edges_names = sapply(edges_names, function(x)
          fill_blank(x, 7))
        names_dict = rbind(edges_names, names) #Names and IDs dictionary
        visdata$nodes$label = as.vector(edges_names)
        names_dict = rbind(edges_names, names) #Names and IDs dictionary
        path = "data/textbooky_coords.csv"
        if (.Platform$OS.type == "windows") {
          path = gsub("\\\\", "/", path)
        }
        coords = read.csv(path)
        #Empasise the main reactions
        visdata$nodes = cbind(visdata$nodes, coords)
        visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
        #Emphasize main metabolites
        visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
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
        #Ifthe lower bound is not lower thn te upper one - display a poper notification
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
    })
    
    # SHOW KO REACTION TAB ----------------------------------------------------
    
    observeEvent(input$ko_rxn, ignoreInit = T, {
      showTab(inputId = "tabs", target = "ko_reactions")
      updateNavbarPage(session = session,
                       inputId = "tabs",
                       selected = "ko_reactions")
      updateTabsetPanel(session, "tabs",
                        selected = "ko")
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_", names(sbml_model@model@reactions)))])
      names(choices_list) = sapply(choices_list, function(x)
        names_dict[1, which(names_dict[2,] == x)[1]])
      output$pick_ko_rxn = renderUI(
        selectInput(
          inputId = "pick_ko_rxn",
          label = NULL,
          choices = choices_list,
          width = "200px"
        )
      )
      output$button_apply_ko = renderUI({
          bsButton(inputId = "apply_ko",
                   label = "Knockout")
      })
    })
    
    
    # KO RESET ----------------------------------------------------------------
    observeEvent(input$reset, {
      #assign R variable to python variable
      python.assign("model_file_path", model_file_path)
      path = "/scripts/reset_ko.py"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      #run the script resetting the model
      python.load(paste(working_dir, path, sep = ""))
      #Get the resulting flux and fluxes
      flux = python.get(var.name = "flux")
      fluxes = python.get(var.name = "fluxes")
      #Transform result for the presentation
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      #Read the sbml model
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      #Write the fluxes to the data structure for the later use
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
      #render the objective flux text output for UI
      output$text_flux_media = renderText({
        paste("<br/>",
              "<b>Objective value: ",
              as.character(flux),
              "</b>",
              "<br/>")
      })
      #render the fluxes table for the UI
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
      toycon = readRDS(paste(working_dir, "/data/toycon1.rda", sep = ""))
      path = "/data/model_var.RData"
      load(paste(working_dir, path, sep = ""))
      toycon_graph = igraph.from.graphNEL(data)
      #transform the data to visNetwork format
      visdata <- toVisNetworkData(toycon_graph)
      net = asNetwork(toycon_graph)
      #Set the type of the node depending on its name
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      #Assign proper names
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      #Adding duplicate metabolites/reactions
      visdata = add_dups_new_layout(visdata)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      #Get the weights from the net object
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      #calculate the thickness of each edge
      edgesize = log(abs(as.numeric(visdata$edges$weight))) + 1
      #assign the thickness values to the slot
      visdata$edges$width = edgesize
      #dash the edges of the graph that carry 0 flux
      dashed = rep(FALSE, dim(visdata$edges)[1])
      dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
      visdata$edges$dashes = dashed
      #Add data for the popup titles for the edges
      visdata$edges$title = paste("Flux: ", round(as.numeric(visdata$edges$weight)))
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
      #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      #Empasise the main reactions
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
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
      #get the reaction name to be KOed and assign its ID to the Python variable
      reaction = (input$pick_ko_rxn)
      reaction_ID = strsplit(reaction, split = "_")[[1]][2]
      python.assign("reaction_ID", reaction_ID)
      python.assign("model_file_path", model_file_path)
      path = "/scripts/ko_rxn.py"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      #run the Python script performing the KO
      python.load(paste(working_dir, path, sep = ""))
      #get the results
      flux = python.get(var.name = "flux")
      fluxes = python.get(var.name = "fluxes")
      fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
      rownames(fluxes_output) = c()
      colnames(fluxes_output) = c("Reaction", "Flux")
      fluxes = fluxes_output
      
      
      
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
      #render UI table to display the fluxes in the model with missing reaction
      output$fluxes_ko = renderTable({
        fluxes_output
      }, width = "250", caption = paste("Fluxes after the KO of", names_dict[1, which(names_dict[2, ] == paste("R_", reaction_ID, sep = ""))]),
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      #render text with objective value
      output$text_flux_ko = renderText({
        paste(
          "<br/>",
          "<b>Objective value: ",
          as.character(flux),
          "</b>",
          "<br/>"
        )
      })
      
      working_dir = getwd()
      path = "/data/toycon.xml"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/x`", path)
      }
      model_file_path = paste(working_dir, path, sep = "")
      path = "/data/model_var.RData"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      
      #read the model in
      load(paste(working_dir, path, sep = ""))
      toycon = readRDS(paste(working_dir, "/data/toycon1.rda", sep = ""))
      data = rsbml_graph((sbml_model))
      ndata = names(data@edgeData)
      #conver the model to visNetwork format
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      
      #Adding duplicate metabolites/reactions
      visdata = add_dups_new_layout(visdata)
      
      #delete the KOed reactions
      visdata$nodes = visdata$nodes[-which(visdata$nodes$id == reaction), ]
      visdata$edges = visdata$edges[-which(visdata$edges$from == reaction |
                                             visdata$edges$to == reaction), ]
      
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      visdata$edges$length = 150
      net = asNetwork(toycon_graph)
      
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
      #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      output$graph_ko = renderVisNetwork({
        #reading SBML files
        
        path = "data/textbooky_coords.csv"
        if (.Platform$OS.type == "windows") {
          path = gsub("\\\\", "/", path)
        }
        #Read the saved coordinates for the graph dispalying purpose
        coords = read.csv(path)
        coords = coords[-which(rownames(coords) == reaction), ]
        visdata$nodes = cbind(visdata$nodes, coords)
        #Adjust the coorfinates of the network after the deaction KO
        for (i in seq(1, dim(fluxes)[1])) {
          hits = which(
            grepl(as.character(fluxes[i, 1]), visdata$edges$from) |
              grepl(as.character(fluxes[i, 1]), visdata$edges$to)
          )
          for (j in hits) {
            visdata$edges$weight[j] = as.numeric(fluxes[i, 2])
          }
        }
        #Get the weights from the net object
        weights_edges = as.numeric(visdata$edges$weight)
        edgesize = log(abs(weights_edges)) + 1
        visdata$edges$width = edgesize
        visdata$edges$title = paste("Flux: ",
                                    round(weights_edges))
        dashed = rep(FALSE, dim(visdata$edges)[1])
        dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
        visdata$edges$dashes = dashed
        #Emphasize main reactions
        visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
        #Emphasize main metabolites
        visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
        visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
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
    })
    
    
    # APPLY GENE EXPRESSION ---------------------------------------------------
    observeEvent(input$apply_expr, {
      #get the gene/reaction expression of which will be adjusted
      reaction_name = (input$pick_expr_gene)
      reaction_ID = toycon@react_id[which(toycon@react_name == reaction_name)]
      #get the bounds of the expression level and sssign to the Python variable
      bound = input$expr
      python.assign("bound", bound)
      python.assign("reaction_ID", reaction_ID)
      python.assign("model_file_path", model_file_path)
      path = "/scripts/simulate_expression.py"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      #run the Python script performind the expression adjustment
      python.load(paste(working_dir, path, sep = ""))
      #get the results
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
      #Assign proper names
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                     "vertex.names")]))])
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      #Adding duplicate metabolites/reactions
      visdata = add_dups_new_layout(visdata)
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      
      weights_edges = c()
      for (i in seq(1, length(net$mel))) {
        weights_edges = append(weights_edges, net$mel[[i]][[3]][[2]])
      }
      #Get the weights from the net object
      edgesize = log(abs(as.numeric(visdata$edges$weight))) + 1
      visdata$edges$width = edgesize
      #dash the edges of the graph that carry 0 flux
      dashed = rep(FALSE, dim(visdata$edges)[1])
      dashed[which(round(as.numeric(visdata$edges$weight)) == 0)] = TRUE
      visdata$edges$dashes = dashed
      #Add data for the popup titles for the edges
      visdata$edges$title = paste("Flux: ", round(as.numeric(visdata$edges$weight)))
      net = asNetwork(toycon_graph)
      names = unlist(net$val)[seq(2, length(unlist(net$val)), 2)]
      
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
        if (any(names(sbml_model@model@species) == as.character(names[i]))) {
          metabolite = sbml_model@model@species[[which(names(sbml_model@model@species) == as.character(names[i]))]]@name
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
      #Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
      
      output$graph_expr = renderVisNetwork({
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
  })
  session$onSessionEnded(function() {
    #set dir
    setwd("data")
    #remove files which were produced
    file.remove(list.files(pattern = "*removed*"))
    stopApp()
  })
})
