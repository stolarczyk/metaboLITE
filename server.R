




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
library(htmlwidgets)

# FUNCTIONS ---------------------------------------------------------------

fill_blank <- function(x, len) {
  #This function is used to make the metabolite name strings the same length. It fills the strings with spaces so the final lengths of each metabolite equals the length of the longest one
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
  #This function is used to duplicate the metabolites in order to acheve the textbooky look of the network
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

exclude_trans_exchange <- function(visdata) {
  visdata$nodes = visdata$nodes[-grep("port", visdata$nodes[, 2]), ]
  visdata$nodes = visdata$nodes[-which(grepl("_e$", rownames(visdata$nodes), perl = T) ==
                                         TRUE), ]
  return(visdata)
}

show_basic_network <-
  function(model_name = "toycon",
           weighting = "none",
           exclude = F) {
    #This function is used to show a basic network when the tabs are first launched in order to help the user to decide what to do
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    edgesize = 0.75
    #Read the saved coordinates for the graph dispalying purpose
    path = "data/textbooky_coords.csv"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    coords = read.csv(path)
    if (model_name == "toycon") {
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
    }
    
    if (weighting == TRUE) {
      #Necessary transformations for the table displaying purposes
      ndata = names(data@edgeData)
      edges_df = dplyr::mutate(visdata_ori$edges, name = paste(from, to, sep = "|"))
      for (i in seq(1, length(data@edgeData@data))) {
        hit = which(edges_df$name == ndata[i])
        data@edgeData@data[[i]]$coefficient = edges_df[hit, 3]
      }
      #Rewrites the coefficients to edge's weight slot
      for (i in seq(1, length(data@edgeData@data))) {
        data@edgeData@data[[i]]$weight = data@edgeData@data[[i]]$coefficient
      }
      toycon_graph = igraph.from.graphNEL(data)
      net = asNetwork(toycon_graph)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                          "vertex.names")][which(grepl("^R_", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                      "vertex.names")]))])
      if (model_name == "toycon") {
        visdata1 = visdata
      } else{
        toycon_graph1 = igraph.from.graphNEL(data)
        visdata1 <- toVisNetworkData(toycon_graph1)
      }
      weights_edges = as.numeric(visdata1$edges$weight)
      edgesize = log(abs(weights_edges)) + 1
      visdata$edges$width = edgesize
      visdata$edges$title = paste("Stoichiometric coefficient: ", round(weights_edges))
    }
    shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
    shape_reactions = ifelse(model_name == "toycon", "box", "square")
    lnodes <-
      data.frame(
        label = c(
          "Cytosolic metabolite",
          "Mitochondrial metabolite",
          "External metabolite",
          "Reaction"
        ),
        shape = c("dot", "dot", "dot", "box"),
        color = c("lightsalmon", "red", "indianred", "lightblue"),
        title = "Informations"
      )
    
    ledges <- data.frame(
      color = c("black", "black"),
      label = c("flux", "no flux"),
      arrows = c("to", "to"),
      dashes = c(F, T)
    )
    
    if (exclude == T & model_name != "toycon") {
      visdata = exclude_trans_exchange(visdata)
    }
    
    #Plotting graph
    visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
      visLegend(
        position = "right",
        stepX = 100,
        stepY = 75,
        width = 0.2,
        useGroups = F,
        addNodes = lnodes,
        zoom = T,
        addEdges = ledges
      ) %>%
      visOptions(highlightNearest = T) %>%
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
                shape = shape_metabolites) %>%
      visGroups(groupname = "Reaction",
                color = color_reaction,
                shape = shape_reactions) %>%
      visGroups(groupname = "Metabolite mitochondria",
                color = color_metabolite_mitochondria,
                shape = shape_metabolites) %>%
      visGroups(groupname = "Metabolite external",
                color = color_metabolite_external,
                shape = shape_metabolites) %>%
      visLayout(randomSeed = 1) %>%
      visPhysics(
        solver = "hierarchicalRepulsion",
        hierarchicalRepulsion = list(
          nodeDistance = 40,
          springLength = 10,
          springConstant = 0,
          damping = 0.1
        )
      )
  }

get_coefficients_DF <- function(model_name = "toycon") {
  working_dir = getwd()
  path = paste("/data/", model_name, ".xml", sep = "")
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  model_file_path = paste(working_dir, path, sep = "")
  path = paste("/data/", model_name, "_var.RData", sep = "")
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  
  load(paste(working_dir, path, sep = "")) #formal class SBML object
  toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
  data = rsbml_graph(sbml_model)
  toycon_graph = igraph.from.graphNEL(data)
  visdata <- toVisNetworkData(toycon_graph)
  visdata_ori = visdata
  
  #Adding duplicate metabolites/reactions
  if (model_name == "toycon") {
    visdata = add_dups_new_layout(visdata)
  }
  visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
  visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
  visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
  visdata$edges$width = 2
  visdata$edges$length = 150
  net = asNetwork(toycon_graph)
  
  #Setting colors according to node class
  color_reaction = "lightblue"
  color_metabolite = "lightsalmon"
  color_metabolite_mitochondria = "red"
  color_metabolite_external = "indianred"
  color_reaction_objective = "lightgreen"
  names = rownames(visdata$nodes)
  net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
  edges_names = names
  
  #Setting proper nodes names
  for (i in seq(1, length(names))) {
    if (model_name == "toycon") {
      if (nchar(names[i]) < 6) {
        names[i] = substr(names[i], 1, 4)
      } else{
        names[i] = substr(names[i], 1, 6)
      }
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
  if (model_name == "toycon") {
    edges_names = sapply(edges_names, function(x)
      fill_blank(x, 7))
  }
  names_dict = rbind(edges_names, names) #Names and IDs dictionary
  visdata$nodes$label = as.vector(edges_names)
  edgesize = 0.75
  #Read the saved coordinates for the graph dispalying purpose
  path = "data/textbooky_coords.csv"
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  coords = read.csv(path)
  if (model_name == "toycon") {
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
  }
  #Necessary transformations for the table displaying purposes
  ndata = names(data@edgeData)
  edges_df = dplyr::mutate(visdata_ori$edges, name = paste(from, to, sep = "|"))
  for (i in seq(1, length(data@edgeData@data))) {
    hit = which(edges_df$name == ndata[i])
    data@edgeData@data[[i]]$coefficient = edges_df[hit, 3]
  }
  #Rewrites the coefficients to edge's weight slot
  for (i in seq(1, length(data@edgeData@data))) {
    data@edgeData@data[[i]]$weight = data@edgeData@data[[i]]$coefficient
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
  names(new_df) = c("Reaction", "Metabolite", "Coefficient")
  return(new_df)
}

check_flux <- function(model = "toycon") {
  # this function gets the objective value when provided with a model name
  # rPython package is used for the R Python connectivity, cobrapy is used for FBA calculations
  working_dir = getwd()
  path = paste("/data/", model, ".xml", sep = "")
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  model_file_path = paste(working_dir, path, sep = "")
  python.assign("model_file_path", model_file_path)
  path = "/scripts/check_flux.py"
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  #runs the script specified by the path variable
  python.load(paste(working_dir, path, sep = ""))
  #gets the python variable after the script execution
  flux = python.get(var.name = "flux")
  return(flux)
}

create_GPR_df <- function(model) {
  model@gpr[which(model@gpr == "")] = "No associated genes"
  tab = data.frame(Genes = model@gpr, Reaction = model@react_name)
  return(tab)
}

get_model_stats <- function(model_name) {
  working_dir = getwd()
  toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
  if (model_name == "toycon") {
    compartments = c("c", "m")
  } else{
    compartments = toycon@mod_compart
  }
  num_compartments = length(compartments)
  num_reactions = toycon@react_num
  num_metabolites = toycon@met_num
  num_genes = length(toycon@allGenes)
  return(t(
    data.frame(
      # Name = model_name,
      Reactions = num_reactions,
      Metabolites = num_metabolites,
      Genes = num_genes,
      Compartments = num_compartments
    )
  ))
}

get_reactionIDs_names <- function(model_name) {
  working_dir = getwd()
  path = paste("/data/", model_name, "_var.RData", sep = "")
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  load(paste(working_dir, path, sep = "")) #formal class SBML object
  choices_list = list()
  names_choices = c()
  for (i in seq_len(length(sbml_model@model@reactions))) {
    choices_list[[i]] = sbml_model@model@reactions[[i]]@id
    names_choices = append(names_choices, sbml_model@model@reactions[[i]]@name)
  }
  names(choices_list) = names_choices
  return(choices_list)
}


# SERVER ------------------------------------------------------------------


shinyServer(function(input, output, session) {
  working_dir = getwd()
  path = "/data/toycon.xml"
  if (.Platform$OS.type == "windows") {
    path = gsub("\\\\", "/", path)
  }
  model_file_path = paste(working_dir, path, sep = "")
  
  output$model_stats = DT::renderDataTable({
    model1 = get_model_stats("toycon")
    model2 = get_model_stats("ecoli")
    data.frame(cbind(model1, model2))
  }, selection = "single", rownames = T, colnames = c("iSIM", "Ecoli core"), options = list(dom = 't'))
  
  # VISUALIZATION UPDATE/LAUNCH APP -----------------------------------------
  
  observeEvent(input$update, {
    showNotification(id = "graphInteravtive",ui = "The graph is interactive, play with it!",duration = 10,closeButton = TRUE,type = "default")
    model_name = isolate(input$pick_model)
    weighting = isolate(input$weighting)
    if (model_name != "toycon") {
      exclude = isolate(input$exclude)
    } else{
      exclude = F
    }
    
    if (weighting == TRUE) {
      output$fluxes = DT::renderDataTable({
        get_coefficients_DF(model_name = model_name)
      }, selection = "single", options = list(pageLength = 10), rownames = FALSE)
    } else{
      output$fluxes = DT::renderDataTable({
        # intentionally left empty - clears the populated table
      })
    }
    
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    names = rownames(visdata$nodes)
    net = asNetwork(toycon_graph)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    
    output$graph = renderVisNetwork({
      show_basic_network(model_name = model_name,
                         weighting = weighting,
                         exclude = exclude)
    })
    
    observe({
      s = input$fluxes_rows_selected
      df = get_coefficients_DF(model_name = model_name)
      rxn_name = df[s, 1]
      rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
      visNetworkProxy("graph") %>%
        visSelectNodes(id = rxn_id)
    })
    
    
    updateNavbarPage(session = session,
                     inputId = "tabs",
                     selected = "visualize")
    
  })
  
  observeEvent(input$tabs_popover, ignoreInit = T, {
    updateNavbarPage(session = session,
                     inputId = "tabs",
                     selected = "help")
  })
  
  # SHOW CHANGE MEDIA TAB ---------------------------------------------------
  observeEvent(input$change_media,
               ignoreInit = F,
               {
                 removeTab(inputId = "tabs",
                           target = "change_media",
                           session = getDefaultReactiveDomain())
                 model_name = (input$pick_model)
                 if (model_name != "toycon") {
                   exclude = isolate(input$exclude)
                 } else{
                   exclude = F
                 }
                 
                 objective = isolate(input$select_objective)
                 ori_objective = objective
                 
                 output$fluxes_media = DT::renderDataTable({
                   # intentionally left empty - clears the populated table
                 })
                 output$fluxes_media1 = DT::renderDataTable({
                   # intentionally left empty - clears the populated table
                 })
                 output$text_flux_media = renderText({
                   # intentionally left empty - clears the field
                 })
                 output$text_objective_media = renderText({
                   # intentionally left empty - clears the field
                 })
                 
                 #Prepare the list of reactions to constrain
                 insertTab(
                   inputId = "tabs",
                   target = "help",
                   tabPanel(
                     "Change media",
                     value = "change_media",
                     sidebarPanel(
                       conditionalPanel(
                         condition = "input.pick_model == 'toycon'",
                         h3("Experiment setup"),
                         br(),
                         br(),
                         fluidRow(class = "myRowText",
                                  column(
                                    8, HTML("<u><b>Use predefined media: </b></u>")
                                  ),
                                  column(
                                    1,
                                    offset = 0,
                                    popify(
                                      actionLink("apply_media_popover", "", icon = icon("question-circle")),
                                      title = "Grow the organism in different media conditions",
                                      content = "Media changing can be easily simulated by manipulating the exchange reactions fluxes",
                                      placement = "right",
                                      trigger = "focus",
                                      options = list(container = "body")
                                    )
                                  )),
                         fluidRow(class = "myRowButton", column(
                           6,
                           popify(
                             bsButton(
                               inputId = "media1",
                               block = T,
                               label = "Glucose free media"
                             ),
                             title = "Apply glucose free media",
                             content = "Simulate organism growth in media containing lactate, oxygen, carbon dioxide, water, but no glucose (the import of this metabolite is inhibited by changing <b>the lower and upper bound of glucose exchange to 0</b>). Does the organism produce ATP in this condition?",
                             trigger = "hover",
                             placement = "right",
                             options = list(container = "body")
                           )
                         )),
                         fluidRow(class = "myRowButton", column(
                           6,
                           popify(
                             bsButton(
                               inputId = "media2",
                               block = T,
                               label = "Microaerophilic media"
                             ),
                             title = "Apply microaerophilic media",
                             content = "Simulate organism growth in media containing lactate, glucose, carbon dioxide, water, but limited amount of oxygen (the import of this metabolite is inhibited by changing <b>the lower and upper bound of oxygen exchange to -10 and 10, respevtively</b>). Does the organism produce ATP in this condition?",
                             trigger = "hover",
                             placement = "right",
                             options = list(container = "body")
                           )
                         )),
                         fluidRow(class = "myRowButton", column(
                           6,
                           popify(
                             bsButton(
                               inputId = "media3",
                               block = T,
                               label = "Lactate rich media"
                             ),
                             title = "Apply lactate rich media",
                             content = "Simulate organism growth in media containing oxygen, carbon dioxide, water and high concentration of lactate (the import of this metabolite is constrained by changing <b>the lower and upper bound of lactate exchange to -700 and 700, respectively</b>). Does the organism produce ATP in this condition?",
                             trigger = "hover",
                             placement = "right",
                             options = list(container = "body")
                           )
                         )),
                         uiOutput("line"),
                         div(style = "vertical-align:top; width: 80%;height: 30px", htmlOutput("text_own"))
                       ),
                       conditionalPanel(
                         condition = "input.pick_model == 'ecoli'",
                         h3("Experiment setup"),
                         br(),
                         br(),
                         HTML(
                           "<font color='#808080'>To simulate growth conditions of your choice select any exchange reaction and change its flux limits below</font>"
                         ),
                         br(),
                         br()
                       ),
                       uiOutput("pick_rxn"),
                       fluidRow(
                         class = "myRowButton",
                         column(10, uiOutput("range")),
                         column(1, offset = 0, uiOutput("range_help"))
                       ),
                       uiOutput("button_apply_media"),
                       hr(),
                       br(),
                       h3("Results"),
                       br(),
                       br(),
                       fluidRow(class = "myRowButton", column(
                         5, HTML("<u><b>Biological objective: </b></u>")
                       ), column(
                         5, offset = 0, htmlOutput("text_objective_media")
                       )),
                       fluidRow(
                         class = "myRowButton",
                         column(5, HTML("<u><b>Objective value: </b></u>")),
                         column(1, offset = 0, htmlOutput("text_flux_media")),
                         column(
                           1,
                           offset = 1,
                           popify(
                             actionLink("flux_popover_media", "", icon = icon("question-circle")),
                             title = "Objective value",
                             content = "It represents flux through the reaction that is a biological objective of the model.",
                             placement = "right",
                             trigger = "focus",
                             options = list(container = "body")
                           )
                         )
                       ),
                       br(),
                       DT::dataTableOutput('fluxes_media'),
                       DT::dataTableOutput('fluxes_media1')
                     ),
                     mainPanel(visNetworkOutput("graph_media", height = "1200"), width = 8)
                   )
                 )
                 updateNavbarPage(session = session,
                                  inputId = "tabs",
                                  selected = "change_media")
                 
                 output$graph_media = renderVisNetwork({
                   show_basic_network(model_name, exclude = exclude)
                 })
                 
                 #render th text to display in the UI
                 output$text_media = renderText({
                   paste("<u><b>Use predefined media: ", "</b></u>")
                 })
                 
                 
                 
               })
  
  # SHOW SIMULATE EXPR TAB --------------------------------------------------
  
  observeEvent(input$simulate_expr,
               ignoreInit = T,
               {
                 removeTab(inputId = "tabs",
                           target = "simulate_expression_changes",
                           session = getDefaultReactiveDomain())
                 model_name = isolate(input$pick_model)
                 if (model_name != "toycon") {
                   exclude = isolate(input$exclude)
                 } else{
                   exclude = F
                 }
                 
                 objective = isolate(input$select_objective)
                 ori_objective = objective
                 
                 output$fluxes_expr = DT::renderDataTable({
                   # intentionally left empty - clears the populated table
                 })
                 output$fluxes_expr1 = DT::renderDataTable({
                   # intentionally left empty - clears the populated table
                 })
                 output$text_flux_expr = renderText({
                   # intentionally left empty - clears the field
                 })
                 output$text_objective_expr = renderText({
                   # intentionally left empty - clears the field
                 })
                 path = paste("/data/", model_name, ".xml", sep = "")
                 if (.Platform$OS.type == "windows") {
                   path = gsub("\\\\", "/", path)
                 }
                 model_file_path = paste(working_dir, path, sep = "")
                 path = paste("/data/", model_name, "_var.RData", sep = "")
                 if (.Platform$OS.type == "windows") {
                   path = gsub("\\\\", "/", path)
                 }
                 
                 load(paste(working_dir, path, sep = "")) #formal class SBML object
                 toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
                 
                 #Show the tab in the app
                 insertTab(
                   inputId = "tabs",
                   target = "help",
                   tabPanel(
                     "Simulate gene expression changes",
                     value = "simulate_expression_changes",
                     sidebarPanel(
                       h3("Experiment setup"),
                       br(),
                       br(),
                       fluidRow(class = "myRowText",
                                column(
                                  7, HTML("<b>Change expression of a gene:</b>")
                                ),
                                column(
                                  1,
                                  offset = 0,
                                  popify(
                                    actionLink("expression_popover", "", icon = icon("question-circle")),
                                    title = "Adjusts the gene expression level",
                                    content = "The expression level scale (0 - 1) corresponds to \"no expression\" and \"maximum overexpression\", respectively.",
                                    placement = "right",
                                    trigger = "focus",
                                    options = list(container = "body")
                                  )
                                )),
                       fluidRow(
                         class = "myRowButton",
                         column(7, uiOutput("pick_expr_gene")),
                         column(1, offset = 0, actionLink("show_gpr_expr", "", icon = icon("table")))
                       ),
                       fluidRow(class = "myRowText", column(
                         7, HTML("<b>Select the gene expression level:</b>")
                       )),
                       uiOutput("expr"),
                       uiOutput("button_apply_expr"),
                       hr(),
                       br(),
                       h3("Results"),
                       br(),
                       br(),
                       fluidRow(class = "myRowButton", column(
                         5, HTML("<u><b>Biological objective: </b></u>")
                       ), column(
                         5, offset = 0, htmlOutput("text_objective_expr")
                       )),
                       fluidRow(
                         class = "myRowButton",
                         column(5, HTML("<u><b>Objective value: </b></u>")),
                         column(1, htmlOutput("text_flux_expr")),
                         column(
                           1,
                           offset = 1,
                           popify(
                             actionLink("flux_popover_expr", "", icon = icon("question-circle")),
                             title = "Objective value",
                             content = "It represents flux through the reaction that is a biological objective of the model.",
                             placement = "right",
                             trigger = "focus",
                             options = list(container = "body")
                           )
                         )
                       ),
                       DT::dataTableOutput('fluxes_expr'),
                       DT::dataTableOutput('fluxes_expr1'),
                       bsModal(
                         id = "modal_expr",
                         title = "Gene - reaction associations lookup table",
                         trigger = "show_gpr_expr",
                         size = "large",
                         DT::dataTableOutput("gpr_expr")
                       ),
                       width = 4
                     ),
                     mainPanel(visNetworkOutput("graph_expr", height = "1200"), width = 8)
                   )
                 )
                 updateNavbarPage(session = session,
                                  inputId = "tabs",
                                  selected = "simulate_expression_changes")
                 
                 output$graph_expr = renderVisNetwork({
                   show_basic_network(model_name, exclude = exclude)
                 })
                 output$fluxes_expr = DT::renderDataTable({
                   
                 })
                 
                 model_name = isolate(input$pick_model)
                 working_dir = getwd()
                 path = paste("/data/", model_name, ".xml", sep = "")
                 if (.Platform$OS.type == "windows") {
                   path = gsub("\\\\", "/", path)
                 }
                 model_file_path = paste(working_dir, path, sep = "")
                 path = paste("/data/", model_name, "_var.RData", sep = "")
                 if (.Platform$OS.type == "windows") {
                   path = gsub("\\\\", "/", path)
                 }
                 
                 load(paste(working_dir, path, sep = "")) #formal class SBML object
                 toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
                 data = rsbml_graph(sbml_model)
                 toycon_graph = igraph.from.graphNEL(data)
                 visdata <- toVisNetworkData(toycon_graph)
                 visdata_ori = visdata
                 
                 #Adding duplicate metabolites/reactions
                 if (model_name == "toycon") {
                   visdata = add_dups_new_layout(visdata)
                 }
                 visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
                 visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
                 visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
                 visdata$nodes$group[which(grepl(
                   perl = T,
                   pattern = "^M\\S*e$",
                   x = visdata$nodes$id
                 ))] = "Metabolite external"
                 visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
                 
                 visdata$edges$width = 2
                 visdata$edges$length = 150
                 net = asNetwork(toycon_graph)
                 
                 #Setting colors according to node class
                 color_reaction = "lightblue"
                 color_metabolite = "lightsalmon"
                 color_metabolite_mitochondria = "red"
                 color_metabolite_external = "indianred"
                 color_reaction_objective = "lightgreen"
                 names = rownames(visdata$nodes)
                 net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
                 edges_names = names
                 
                 #Setting proper nodes names
                 for (i in seq(1, length(names))) {
                   if (model_name == "toycon") {
                     if (nchar(names[i]) < 6) {
                       names[i] = substr(names[i], 1, 4)
                     } else{
                       names[i] = substr(names[i], 1, 6)
                     }
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
                 if (model_name == "toycon") {
                   edges_names = sapply(edges_names, function(x)
                     fill_blank(x, 7))
                 }
                 names_dict = rbind(edges_names, names) #Names and IDs dictionary
                 choices_list_expr = as.list(toycon@allGenes)
                 names(choices_list_expr) = toycon@allGenes
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
                            label = "Run FBA")
                 })
                 output$gpr_expr = DT::renderDataTable({
                   create_GPR_df(toycon)
                 }, rownames = FALSE)
                 #Render the selection slider for the expression level adjustment
                 output$expr = renderUI(
                   sliderInput(
                     inputId = "expr",
                     min = 0,
                     max = 1,
                     label = div(
                       style = 'width:350px;',
                       div(style = 'float:left;font-weight:normal;', 'no expression'),
                       div(style = 'float:right;font-weight:normal;', 'maximum overexpression')
                     ),
                     value = 0.5,
                     step = 0.1,
                     round = TRUE,
                     ticks = F,
                     width = "350px"
                   )
                 )
               })
  
  # APPLY MEDIA1 ------------------------------------------------------------
  observeEvent(input$media1, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    ori_objective = objective
    objective = strsplit(objective, split = "_")[[1]][2]
    #Define the type of media and assigne to the python variable
    media_type = "media1"
    
    python.assign("media_type", media_type)
    python.assign("objective", objective)
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
    
    working_dir = getwd()
    path = "/data/toycon.xml"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = "/data/toycon_var.RData"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = ""))
    toycon = readRDS(paste(working_dir, "/data/toycon.rda", sep = ""))
    data = rsbml_graph((sbml_model))
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    visdata = add_dups_new_layout(visdata)
    
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
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
    
    #render the text for UI
    output$text_flux_media = renderText({
      paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
    })
    names_mapper = get_reactionIDs_names(model_name = model_name)
    objective_name = names(which(names_mapper == ori_objective))
    output$text_objective_media = renderText({
      paste("<b>", as.character(objective_name), "</b>")
    })
    
    toycon_graph = igraph.from.graphNEL(data)
    net = asNetwork(toycon_graph)
    #Set the type of the node depending on its name
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    #Assign proper names
    reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                        "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                   "vertex.names")]))])
    
    if (model_name == "toycon") {
      output$fluxes_media = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10, selection = "single"), caption =
        "Reaction fluxes after change to glucose free media", rownames = FALSE)
    } else{
      output$fluxes_media1 = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10, selection = "single"), caption =
        "Reaction fluxes after change to glucose free media", rownames = FALSE)
    }
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
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
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
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
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
    
    shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
    shape_reactions = ifelse(model_name == "toycon", "box", "square")
    
    lnodes <-
      data.frame(
        label = c(
          "Cytosolic metabolite",
          "Mitochondrial metabolite",
          "External metabolite",
          "Reaction",
          "Biological objective"
        ),
        shape = c("dot", "dot", "dot", "box", "box"),
        color = c(
          "lightsalmon",
          "red",
          "indianred",
          "lightblue",
          "lightgreen"
        ),
        title = "Informations"
      )
    
    ledges <- data.frame(
      color = c("black", "black"),
      label = c("flux", "no flux"),
      arrows = c("to", "to"),
      dashes = c(F, T)
    )
    
    output$graph_media = renderVisNetwork({
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(
          position = "right",
          stepX = 100,
          stepY = 75,
          width = 0.2,
          useGroups = F,
          addNodes = lnodes,
          zoom = T,
          addEdges = ledges
        ) %>%
        visOptions(highlightNearest = TRUE) %>%
        visEdges(color = "black", arrows = "to") %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = "circle") %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = "box") %>%
        visGroups(groupname = "Metabolite mitochondria",
                  color = color_metabolite_mitochondria,
                  shape = "circle") %>%
        visGroups(groupname = "Metabolite external",
                  color = color_metabolite_external,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Biological objective",
                  color = color_reaction_objective,
                  shape = shape_reactions) %>%
        visPhysics(
          solver = "hierarchicalRepulsion",
          hierarchicalRepulsion = list(
            nodeDistance = 40,
            springLength = 10,
            springConstant = 0,
            damping = 0.1
          )
        ) %>%
        visLayout(randomSeed = 1)
    })
    
    if (model_name == "toycon") {
      observe({
        df = fluxes_output
        s = input$fluxes_media_rows_selected
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id)
      }, priority = 0, autoDestroy = T)
    } else{
      observe({
        df1 = fluxes_output
        s1 = input$fluxes_media1_rows_selected
        rxn_name = df1[s1, 1]
        rxn_id1 = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id1)
      }, priority = 0, autoDestroy = T)
    }
  })
  # APPLY MEDIA2 ------------------------------------------------------------
  #For the inline comments check out the APPLY MEDIA1 section above
  observeEvent(input$media2, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    objective = strsplit(objective, split = "_")[[1]][2]
    media_type = "media2"
    python.assign("objective", objective)
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
    
    working_dir = getwd()
    path = "/data/toycon.xml"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = "/data/toycon_var.RData"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = ""))
    toycon = readRDS(paste(working_dir, "/data/toycon.rda", sep = ""))
    data = rsbml_graph((sbml_model))
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    visdata = add_dups_new_layout(visdata)
    
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
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
    
    data = rsbml_graph((sbml_model))
    ndata = names(data@edgeData)
    for (i in seq(1, dim(fluxes_output)[1])) {
      hits = which(grepl(fluxes_output[i, 1], ndata))
      for (j in hits) {
        data@edgeData@data[[j]]$weight = as.numeric(fluxes_output[i, 2])
      }
    }
    
    #render UI table to display the fluxes in the model with missing reaction
    if (model_name == "toycon") {
      output$fluxes_media = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10, selection = "single"), caption =
        "Reaction fluxes after change to glucose free media", rownames = FALSE)
    } else{
      output$fluxes_media1 = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10, selection = "single"), caption =
        "Reaction fluxes after change to glucose free media", rownames = FALSE)
    }
    for (i in seq(1, dim(names_dict)[2], by = 1)) {
      #Mapping nodes IDs to names for table displaying purposes
      if (any(which(fluxes_output[, 1] == names_dict[2, i])))
        fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
    }
    #render the text for UI
    output$text_flux_media = renderText({
      paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
    })
    names_mapper = get_reactionIDs_names(model_name = model_name)
    objective_name = names(which(names_mapper == ori_objective))
    output$text_objective_media = renderText({
      paste("<b>", as.character(objective_name), "</b>")
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
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    
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
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
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
    
    shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
    shape_reactions = ifelse(model_name == "toycon", "box", "square")
    
    lnodes <-
      data.frame(
        label = c(
          "Cytosolic metabolite",
          "Mitochondrial metabolite",
          "External metabolite",
          "Reaction",
          "Biological objective"
        ),
        shape = c("dot", "dot", "dot", "box", "box"),
        color = c(
          "lightsalmon",
          "red",
          "indianred",
          "lightblue",
          "lightgreen"
        ),
        title = "Informations"
      )
    
    ledges <- data.frame(
      color = c("black", "black"),
      label = c("flux", "no flux"),
      arrows = c("to", "to"),
      dashes = c(F, T)
    )
    output$graph_media = renderVisNetwork({
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(
          position = "right",
          stepX = 100,
          stepY = 75,
          width = 0.2,
          useGroups = F,
          addNodes = lnodes,
          zoom = T,
          addEdges = ledges
        ) %>%
        visOptions(highlightNearest = TRUE) %>%
        visEdges(color = "black", arrows = "to") %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = "circle") %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = "box") %>%
        visGroups(groupname = "Metabolite mitochondria",
                  color = color_metabolite_mitochondria,
                  shape = "circle") %>%
        visGroups(groupname = "Metabolite external",
                  color = color_metabolite_external,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Biological objective",
                  color = color_reaction_objective,
                  shape = shape_reactions) %>%
        visPhysics(
          solver = "hierarchicalRepulsion",
          hierarchicalRepulsion = list(
            nodeDistance = 40,
            springLength = 10,
            springConstant = 0,
            damping = 0.1
          )
        ) %>%
        visLayout(randomSeed = 1)
    })
    
    if (model_name == "toycon") {
      observe({
        df = fluxes_output
        s = input$fluxes_media_rows_selected
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id)
      }, priority = 0, autoDestroy = T)
    } else{
      observe({
        df1 = fluxes_output
        s1 = input$fluxes_media1_rows_selected
        rxn_name = df1[s1, 1]
        rxn_id1 = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id1)
      }, priority = 0, autoDestroy = T)
    }
    
  })
  
  # APPLY MEDIA3 ------------------------------------------------------------
  #For the inline comments check out the APPLY MEDIA1 section above
  observeEvent(input$media3, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    objective = strsplit(objective, split = "_")[[1]][2]
    media_type = "media3"
    python.assign("objective", objective)
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
    
    
    working_dir = getwd()
    path = "/data/toycon.xml"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = "/data/toycon_var.RData"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = ""))
    toycon = readRDS(paste(working_dir, "/data/toycon.rda", sep = ""))
    data = rsbml_graph((sbml_model))
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    visdata = add_dups_new_layout(visdata)
    
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
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
    #render the text for UI
    output$text_flux_media = renderText({
      paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
    })
    names_mapper = get_reactionIDs_names(model_name = model_name)
    objective_name = names(which(names_mapper == ori_objective))
    output$text_objective_media = renderText({
      paste("<b>", as.character(objective_name), "</b>")
    })
    
    #render UI table to display the fluxes in the model with missing reaction
    
    if (model_name == "toycon") {
      output$fluxes_media = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10, selection = "single"), caption =
        "Reaction fluxes after change to glucose free media", rownames = FALSE)
    } else{
      output$fluxes_media1 = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10, selection = "single"), caption =
        "Reaction fluxes after change to glucose free media", rownames = FALSE)
    }
    
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
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    
    
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
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
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
    
    shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
    shape_reactions = ifelse(model_name == "toycon", "box", "square")
    lnodes <-
      data.frame(
        label = c(
          "Cytosolic metabolite",
          "Mitochondrial metabolite",
          "External metabolite",
          "Reaction",
          "Biological objective"
        ),
        shape = c("dot", "dot", "dot", "box", "box"),
        color = c(
          "lightsalmon",
          "red",
          "indianred",
          "lightblue",
          "lightgreen"
        ),
        title = "Informations"
      )
    
    ledges <- data.frame(
      color = c("black", "black"),
      label = c("flux", "no flux"),
      arrows = c("to", "to"),
      dashes = c(F, T)
    )
    output$graph_media = renderVisNetwork({
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(
          position = "right",
          stepX = 100,
          stepY = 75,
          width = 0.2,
          useGroups = F,
          addNodes = lnodes,
          zoom = T,
          addEdges = ledges
        ) %>%
        visOptions(highlightNearest = TRUE) %>%
        visEdges(color = "black", arrows = "to") %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = "circle") %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = "box") %>%
        visGroups(groupname = "Metabolite mitochondria",
                  color = color_metabolite_mitochondria,
                  shape = "circle") %>%
        visGroups(groupname = "Metabolite external",
                  color = color_metabolite_external,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Biological objective",
                  color = color_reaction_objective,
                  shape = shape_reactions) %>%
        visPhysics(
          solver = "hierarchicalRepulsion",
          hierarchicalRepulsion = list(
            nodeDistance = 40,
            springLength = 10,
            springConstant = 0,
            damping = 0.1
          )
        ) %>%
        visLayout(randomSeed = 1)
    })
    
    if (model_name == "toycon") {
      observe({
        df = fluxes_output
        s = input$fluxes_media_rows_selected
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id)
      }, priority = 0, autoDestroy = T)
    } else{
      observe({
        df1 = fluxes_output
        s1 = input$fluxes_media1_rows_selected
        rxn_name = df1[s1, 1]
        rxn_id1 = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id1)
      }, priority = 0, autoDestroy = T)
    }
    
  })
  # APPLY CUSTOM MEDIA ------------------------------------------------------------
  observeEvent(input$change_media, {
    output$fluxes_media = DT::renderDataTable({
      
    })
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    if (model_name == "toycon") {
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_E", names(sbml_model@model@reactions)))])
    } else{
      choices_list = as.list(names(sbml_model@model@reactions)[which(grepl("^R_EX", names(sbml_model@model@reactions)))])
      
    }
    names(choices_list) = sapply(choices_list, function(x)
      names_dict[1, which(names_dict[2,] == x)[1]])
    
    # render the UI select component
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
      paste("<u><b>Manipulate media component concentrations: ",
            "</b></u>")
    })
    
    output$line = renderUI(hr())
    output$line1 = renderUI(hr())
    
    #render the flux range limiting slider for the UI
    output$range = renderUI(
      sliderInput(
        inputId = "range",
        min = -1000,
        max = 1000,
        label = "Select the exchange limits:",
        value = c(-1000, 1000),
        step = 10,
        round = TRUE,
        ticks = TRUE,
        width = "300px"
      )
    )
    
    
    output$range_help = renderUI(
      popify(
        actionLink(
          inputId = "range_help",
          "",
          icon = icon("question-circle")
        ),
        title = "Technical information",
        content = "This slider adjusts the upper and lower bound, which define the maximum and minimum allowable fluxes of the reactions.",
        placement = "right",
        trigger = "focus",
        options = list(container = "body")
      )
    )
    
    #render the button applying the changes
    output$button_apply_media = renderUI(bsButton(inputId = "apply_media",
                                                  label = "Run"))
    
    
  })
  
  # APPLY MEDIA -------------------------------------------------------------
  observeEvent(input$apply_media, priority = 1, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    if (model_name != "toycon") {
      exclude = isolate(input$exclude)
      objective = paste(strsplit(objective, split = "_")[[1]][-1], collapse = "_")
    } else{
      exclude = F
      objective = strsplit(objective, split = "_")[[1]][2]
    }
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    #get the bouds to be applied
    lb = input$range[1]
    ub = input$range[2]
    #get the reaction to be constrained
    reaction = (input$pick_rxn)
    if (model_name == "toycon") {
      reaction_ID = strsplit(reaction, split = "_")[[1]][2]
    } else{
      reaction_ID = gsub(", ", "_", toString(strsplit(reaction, split = "_")[[1]][-1]))
    }
    if (lb < ub) {
      #use Python to change the bouds and perform the FBA
      python.assign("lb", lb)
      python.assign("ub", ub)
      python.assign("objective", objective)
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
      
      working_dir = getwd()
      path = paste("/data/", model_name, ".xml", sep = "")
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      model_file_path = paste(working_dir, path, sep = "")
      path = paste("/data/", model_name, "_var.RData", sep = "")
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      
      load(paste(working_dir, path, sep = "")) #formal class SBML object
      toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
      data = rsbml_graph(sbml_model)
      toycon_graph = igraph.from.graphNEL(data)
      visdata <- toVisNetworkData(toycon_graph)
      
      #Adding duplicate metabolites/reactions
      if (model_name == "toycon") {
        visdata = add_dups_new_layout(visdata)
      }
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
      visdata$nodes$group[which(grepl(
        perl = T,
        pattern = "^M\\S*e$",
        x = visdata$nodes$id
      ))] = "Metabolite external"
      visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
      
      visdata$edges$width = 2
      visdata$edges$length = 150
      net = asNetwork(toycon_graph)
      
      #Setting colors according to node class
      color_reaction = "lightblue"
      color_metabolite = "lightsalmon"
      color_metabolite_mitochondria = "red"
      color_metabolite_external = "indianred"
      color_reaction_objective = "lightgreen"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      
      #Setting proper nodes names
      for (i in seq(1, length(names))) {
        if (model_name == "toycon") {
          if (nchar(names[i]) < 6) {
            names[i] = substr(names[i], 1, 4)
          } else{
            names[i] = substr(names[i], 1, 6)
          }
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
      if (model_name == "toycon") {
        edges_names = sapply(edges_names, function(x)
          fill_blank(x, 7))
      }
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      
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
      #render the text for UI
      output$text_flux_media = renderText({
        paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
      })
      names_mapper = get_reactionIDs_names(model_name = model_name)
      objective_name = names(which(names_mapper == ori_objective))
      output$text_objective_media = renderText({
        paste("<b>", as.character(objective_name), "</b>")
      })
      
      if (model_name == "toycon") {
        output$fluxes_media = DT::renderDataTable({
          fluxes_output
        }, options = list(pageLength = 10), selection = "single", caption =
          "Reaction fluxes after change to custom growth media", rownames = FALSE)
      } else{
        output$fluxes_media1 = DT::renderDataTable({
          fluxes_output
        }, options = list(pageLength = 10), selection = "single", caption =
          "Reaction fluxes after change to custom growth media", rownames = FALSE)
        
      }
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
      if (model_name == "toycon") {
        visdata = add_dups_new_layout(visdata)
      }
      
      visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
      visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
      visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
      visdata$nodes$group[which(grepl(
        perl = T,
        pattern = "^M\\S*e$",
        x = visdata$nodes$id
      ))] = "Metabolite external"
      visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
      
      
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
      color_metabolite_mitochondria = "red"
      color_metabolite_external = "indianred"
      color_reaction_objective = "lightgreen"
      names = rownames(visdata$nodes)
      net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
      edges_names = names
      #Setting names
      for (i in seq(1, length(names))) {
        if (model_name == "toycon") {
          if (nchar(names[i]) < 6) {
            names[i] = substr(names[i], 1, 4)
          } else{
            names[i] = substr(names[i], 1, 6)
          }
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
      if (model_name == "toycon") {
        edges_names = sapply(edges_names, function(x)
          fill_blank(x, 7))
      }
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      visdata$nodes$label = as.vector(edges_names)
      names_dict = rbind(edges_names, names) #Names and IDs dictionary
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      coords = read.csv(path)
      #Empasise the main reactions
      if (model_name == "toycon") {
        visdata$nodes = cbind(visdata$nodes, coords)
      }
      visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
      #Emphasize main metabolites
      visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
      visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
      
      shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
      shape_reactions = ifelse(model_name == "toycon", "box", "square")
      
      lnodes <-
        data.frame(
          label = c(
            "Cytosolic metabolite",
            "Mitochondrial metabolite",
            "External metabolite",
            "Reaction",
            "Biological objective"
          ),
          shape = c("dot", "dot", "dot", "box", "box")
          ,
          color = c(
            "lightsalmon",
            "red",
            "indianred",
            "lightblue",
            "lightgreen"
          ),
          title = "Informations"
        )
      
      ledges <- data.frame(
        color = c("black", "black"),
        label = c("flux", "no flux"),
        arrows = c("to", "to"),
        dashes = c(F, T)
      )
      
      
      output$graph_media = renderVisNetwork({
        #Plotting graph
        visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
          visLegend(
            position = "right",
            stepX = 100,
            stepY = 75,
            width = 0.2,
            useGroups = F,
            addNodes = lnodes,
            zoom = T,
            addEdges = ledges
          ) %>%
          visOptions(highlightNearest = TRUE) %>%
          visEdges(color = "black", arrows = "to") %>%
          visGroups(groupname = "Metabolite",
                    color = color_metabolite,
                    shape = shape_metabolites) %>%
          visGroups(groupname = "Reaction",
                    color = color_reaction,
                    shape = shape_reactions) %>%
          visGroups(groupname = "Metabolite mitochondria",
                    color = color_metabolite_mitochondria,
                    shape = shape_metabolites) %>%
          visGroups(groupname = "Metabolite external",
                    color = color_metabolite_external,
                    shape = shape_metabolites) %>%
          visGroups(groupname = "Biological objective",
                    color = color_reaction_objective,
                    shape = shape_reactions) %>%
          visPhysics(
            solver = "hierarchicalRepulsion",
            hierarchicalRepulsion = list(
              nodeDistance = 40,
              springLength = 10,
              springConstant = 0,
              damping = 0.1
            )
          ) %>%
          visLayout(randomSeed = 1)
        
      })
      if (model_name == "toycon") {
        observe({
          df = fluxes_output
          s = input$fluxes_media_rows_selected
          rxn_name = df[s, 1]
          rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
          visNetworkProxy("graph_media") %>%
            visSelectNodes(id = rxn_id)
        }, priority = 0, autoDestroy = T)
      } else{
        observe({
          df1 = fluxes_output
          s1 = input$fluxes_media1_rows_selected
          rxn_name = df1[s1, 1]
          rxn_id1 = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
          visNetworkProxy("graph_media") %>%
            visSelectNodes(id = rxn_id1)
        }, priority = 0, autoDestroy = T)
      }
      
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
    removeTab(inputId = "tabs",
              target = "ko_reactions",
              session = getDefaultReactiveDomain())
    
    model_name = isolate(input$pick_model)
    if (model_name != "toycon") {
      exclude = isolate(input$exclude)
    } else{
      exclude = F
    }
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    output$fluxes_ko = DT::renderDataTable({
      # intentionally left empty - clears the populated table
    })
    output$fluxes_ko1 = DT::renderDataTable({
      # intentionally left empty - clears the populated table
    })
    output$text_flux_ko = renderText({
      # intentionally left empty - clears the field
    })
    output$text_objective_ko = renderText({
      # intentionally left empty - clears the field
    })
    
    insertTab(
      inputId = "tabs",
      target = "help",
      tabPanel(
        "Simulate gene knockout",
        value = "ko_reactions",
        sidebarPanel(
          h3("Experiment setup"),
          br(),
          br(),
          fluidRow(class = "myRowText",
                   column(
                     7, HTML("<b><u>Pick a gene to knock out:</u></b>")
                   ),
                   column(
                     1,
                     offset = 0,
                     popify(
                       actionLink("pick_rxn_ko_popover", "", icon = icon("question-circle")),
                       title = "Knocksout the reaction picked above",
                       content = "The gene knockout results in the network deprived of the reaction that was catalyzed by the enzyme coded by this gene",
                       placement = "right",
                       trigger = "focus",
                       options = list(container = "body")
                     )
                   )),
          fluidRow(
            class = "myRowButton",
            column(7, uiOutput("pick_ko_rxn")),
            column(1, offset = 0, actionLink("show_gpr_ko", "", icon = icon("table")))
          ),
          div(style = "vertical-align:top; width: 30%;height: 60px", uiOutput("button_apply_ko")),
          div(style = "vertical-align:top; width: 30%;height: 60px", uiOutput("reset_ko")),
          hr(),
          br(),
          h3("Results"),
          br(),
          br(),
          fluidRow(class = "myRowButton", column(
            5, HTML("<u><b>Biological objective: </b></u>")
          ), column(
            5, offset = 0, htmlOutput("text_objective_ko")
          )),
          fluidRow(
            class = "myRowButton",
            column(5, HTML("<u><b>Objective value: </b></u>")),
            column(1, htmlOutput("text_flux_ko")),
            column(
              1,
              offset = 1,
              popify(
                actionLink("flux_popover_ko", "", icon = icon("question-circle")),
                title = "Objective value",
                content = "It represents flux through the reaction that is a biological objective of the model.",
                placement = "right",
                trigger = "focus",
                options = list(container = "body")
              )
            )
          ),
          DT::dataTableOutput('fluxes_ko'),
          DT::dataTableOutput('fluxes_ko1'),
          bsModal(
            id = "modal_ko",
            title = "Gene - reaction associations lookup table",
            trigger = "show_gpr_ko",
            size = "large",
            DT::dataTableOutput("gpr_ko")
          )
        ),
        mainPanel(visNetworkOutput("graph_ko", height = "1200"), width = 8)
        
      )
    )
    
    updateNavbarPage(session = session,
                     inputId = "tabs",
                     selected = "ko_reactions")
    updateTabsetPanel(session, "tabs",
                      selected = "ko")
    
    output$graph_ko = renderVisNetwork({
      show_basic_network(model_name = model_name, exclude = exclude)
    })
    
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    
    output$gpr_ko = DT::renderDataTable({
      create_GPR_df(toycon)
    }, rownames = FALSE)
    
    if (model_name == "toycon") {
      choices_list_ko = as.list(toycon@react_name)
      names(choices_list_ko) = toycon@allGenes
    } else{
      unique_genes = unique(unlist(toycon@genes))
      unique_genes = unique_genes[which(unique_genes != "")]
      choices_list_ko = as.list(unique_genes)
      names(unique_genes) = unique_genes
    }
    
    output$pick_ko_rxn = renderUI(
      selectInput(
        inputId = "pick_ko_rxn",
        label = NULL,
        choices = choices_list_ko,
        width = "200px"
      )
    )
    output$button_apply_ko = renderUI({
      bsButton(inputId = "apply_ko",
               label = "Knockout",
               block = T)
    })
    
    
  })
  
  
  # KO RESET ----------------------------------------------------------------
  observeEvent(input$reset, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    ori_objective = objective
    if (model_name != "toycon") {
      exclude = isolate(input$exclude)
      objective = paste(strsplit(objective, split = "_")[[1]][-1], collapse = "_")
    } else{
      exclude = F
      objective = strsplit(objective, split = "_")[[1]][2]
    }
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    
    python.assign("model_file_path", model_file_path)
    path = "/scripts/reset_ko.py"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    python.assign("objective", objective)
    python.assign("model_file_path", model_file_path)
    python.load(paste(working_dir, path, sep = ""))
    #Get the results
    flux = python.get(var.name = "flux")
    fluxes = python.get(var.name = "fluxes")
    fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
    fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
    rownames(fluxes_output) = c()
    colnames(fluxes_output) = c("Reaction", "Flux")
    
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    # Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    
    # Make the names equal length (7 is the max length of matabolite name) for the displaying purposes. this way the sizes of the metabolite nodes are all equal
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    
    
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
    #render the text for UI
    output$text_flux_ko = renderText({
      paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
    })
    names_mapper = get_reactionIDs_names(model_name = model_name)
    objective_name = names(which(names_mapper == ori_objective))
    output$text_objective_ko = renderText({
      paste("<b>", as.character(objective_name), "</b>")
    })
    
    if (model_name == "toycon") {
      output$fluxes_ko = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10), selection = "single", caption =
        "Reaction fluxes after change to custom growth media", rownames = FALSE)
    } else{
      output$fluxes_ko1 = DT::renderDataTable({
        fluxes_output
      }, options = list(pageLength = 10), selection = "single", caption =
        "Reaction fluxes after change to custom growth media", rownames = FALSE)
    }
    
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
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
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
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    #Setting names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      visdata$nodes = cbind(visdata$nodes, coords)
    }
    visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
    #Emphasize main metabolites
    visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
    
    shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
    shape_reactions = ifelse(model_name == "toycon", "box", "square")
    
    if (exclude == T & model_name != "toycon") {
      visdata = exclude_trans_exchange(visdata)
    }
    
    lnodes <-
      data.frame(
        label = c(
          "Cytosolic metabolite",
          "Mitochondrial metabolite",
          "External metabolite",
          "Reaction",
          "Biological objective"
        ),
        shape = c("dot", "dot", "dot", "box", "box")
        ,
        color = c(
          "lightsalmon",
          "red",
          "indianred",
          "lightblue",
          "lightgreen"
        ),
        title = "Informations"
      )
    
    ledges <- data.frame(
      color = c("black", "black"),
      label = c("flux", "no flux"),
      arrows = c("to", "to"),
      dashes = c(F, T)
    )
    output$graph_ko = renderVisNetwork({
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(
          position = "right",
          stepX = 100,
          stepY = 75,
          width = 0.2,
          useGroups = F,
          addNodes = lnodes,
          zoom = T,
          addEdges = ledges
        ) %>%
        visOptions(highlightNearest = TRUE) %>%
        visEdges(color = "black", arrows = "to") %>%
        visGroups(groupname = "Metabolite",
                  color = color_metabolite,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = shape_reactions) %>%
        visGroups(groupname = "Metabolite mitochondria",
                  color = color_metabolite_mitochondria,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Metabolite external",
                  color = color_metabolite_external,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Biological objective",
                  color = color_reaction_objective,
                  shape = shape_reactions) %>%
        visPhysics(
          solver = "hierarchicalRepulsion",
          hierarchicalRepulsion = list(
            nodeDistance = 40,
            springLength = 10,
            springConstant = 0,
            damping = 0.1
          )
        ) %>%
        visLayout(randomSeed = 1)
      
    })
    if (model_name == "toycon") {
      observe({
        df = fluxes_output
        s = input$fluxes_media_rows_selected
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id)
      }, priority = 0, autoDestroy = T)
    } else{
      observe({
        df1 = fluxes_output
        s1 = input$fluxes_media1_rows_selected
        rxn_name = df1[s1, 1]
        rxn_id1 = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_media") %>%
          visSelectNodes(id = rxn_id1)
      }, priority = 0, autoDestroy = T)
    }
  })
  
  
  
  
  # APPLY KO ----------------------------------------------------------------
  observeEvent(input$apply_ko, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    
    if (model_name != "toycon") {
      exclude = isolate(input$exclude)
      objective = paste(strsplit(objective, split = "_")[[1]][-1], collapse = "_")
    } else{
      exclude = F
      objective = strsplit(objective, split = "_")[[1]][2]
    }
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    edgesize = 0.75
    #Read the saved coordinates for the graph dispalying purpose
    path = "data/textbooky_coords.csv"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    coords = read.csv(path)
    if (model_name == "toycon") {
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
    }
    
    
    #get the reaction name to be KOed and assign its ID to the Python variable
    reaction_name = (input$pick_ko_rxn)
    if (model_name == "toycon") {
      reaction_ID = toycon@react_id[which(toycon@react_name == reaction_name)]
    } else{
      reaction_ID = reaction_name
    }
    python.assign("reaction_ID", reaction_ID)
    python.assign("objective", objective)
    python.assign("model_file_path", model_file_path)
    if (model_name == "toycon") {
      path = "/scripts/ko_rxn.py"
    } else{
      path = "/scripts/ko_gene.py"
    }
    
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    #run the Python script performing the KO
    python.load(paste(working_dir, path, sep = ""))
    #get the results
    flux = python.get(var.name = "flux")
    fluxes = python.get(var.name = "fluxes")
    fluxes_output = t(rbind(t(names(fluxes)), t(fluxes)))
    if (model_name == "toycon") {
      fluxes_output[, 1] = paste("R_", fluxes_output[, 1], sep = "")
    }
    rownames(fluxes_output) = c()
    colnames(fluxes_output) = c("Reaction", "Flux")
    fluxes = fluxes_output
    
    
    if (model_name == "toycon") {
      for (i in seq(1, dim(names_dict)[2], by = 1)) {
        #Mapping nodes IDs to names for table displaying purposes
        if (any(which(fluxes_output[, 1] == names_dict[2, i])))
          fluxes_output[which(fluxes_output[, 1] == names_dict[2, i]), 1] = names_dict[1, i]
      }
    } else{
      for (i in seq_len(dim(fluxes_output)[1])) {
        name = toycon@react_name[which(gsub("\\(e\\)", "_e", toycon@react_id) == fluxes_output[i, 1])]
        
        fluxes_output[i, 1] = name
      }
    }
    #render UI table to display the fluxes in the model with missing reaction
    if (model_name == "toycon") {
      output$fluxes_ko = DT::renderDataTable({
        fluxes_output
      }, selection = "single", options = list(pageLength = 10), rownames = FALSE)
    } else{
      output$fluxes_ko1 = DT::renderDataTable({
        fluxes_output
      }, selection = "single", options = list(pageLength = 10), rownames = FALSE)
      
    }
    #render the text for UI
    output$text_flux_ko = renderText({
      paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
    })
    names_mapper = get_reactionIDs_names(model_name = model_name)
    objective_name = names(which(names_mapper == ori_objective))
    output$text_objective_ko = renderText({
      paste("<b>", as.character(objective_name), "</b>")
    })
    
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
      #delete the KOed reactions
      reaction = paste("R_", reaction_ID, sep = "")
      visdata$nodes = visdata$nodes[-which(visdata$nodes$id == reaction), ]
      visdata$edges = visdata$edges[-which(visdata$edges$from == reaction |
                                             visdata$edges$to == reaction), ]
    }
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    #Setting names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    output$graph_ko = renderVisNetwork({
      path = "data/textbooky_coords.csv"
      if (.Platform$OS.type == "windows") {
        path = gsub("\\\\", "/", path)
      }
      #Read the saved coordinates for the graph dispalying purpose
      if (model_name == "toycon") {
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
      } else{
        #Adjust the coorfinates of the network after the deaction KO
        fluxes[, 1] = paste("R_", fluxes[, 1], sep = "")
        for (i in seq(1, dim(fluxes)[1])) {
          hits = which(
            grepl(as.character(fluxes[i, 1]), visdata$edges$from) |
              grepl(as.character(fluxes[i, 1]), visdata$edges$to)
          )
          for (j in hits) {
            visdata$edges$weight[j] = as.numeric(fluxes[i, 2])
          }
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
      
      shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
      shape_reactions = ifelse(model_name == "toycon", "box", "square")
      
      if (exclude == T & model_name != "toycon") {
        visdata = exclude_trans_exchange(visdata)
      }
      
      lnodes <-
        data.frame(
          label = c(
            "Cytoslic metabolite",
            "Mitochondrial metabolite",
            "External metabolite",
            "Reaction",
            "Biological objective"
          ),
          shape = c("dot", "dot", "dot", "box", "box"),
          color = c(
            "lightsalmon",
            "red",
            "indianred",
            "lightblue",
            "lightgreen"
          ),
          title = "Informations"
        )
      
      ledges <- data.frame(
        color = c("black", "black"),
        label = c("flux", "no flux"),
        arrows = c("to", "to"),
        dashes = c(F, T)
      )
      #Plotting graph
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(
          position = "right",
          stepX = 100,
          stepY = 75,
          width = 0.2,
          useGroups = F,
          addNodes = lnodes,
          zoom = T,
          addEdges = ledges
        ) %>%
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
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = shape_reactions) %>%
        visGroups(groupname = "Metabolite mitochondria",
                  color = color_metabolite_mitochondria,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Metabolite external",
                  color = color_metabolite_external,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Biological objective",
                  color = color_reaction_objective,
                  shape = shape_reactions) %>%
        visLayout(randomSeed = 1) %>%
        visPhysics(
          solver = "hierarchicalRepulsion",
          hierarchicalRepulsion = list(
            nodeDistance = 40,
            springLength = 10,
            springConstant = 0,
            damping = 0.1
          )
        )
      
    })
    
    if (model_name == "toycon") {
      observe({
        s = input$fluxes_ko_rows_selected
        df = fluxes_output
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_ko") %>%
          visSelectNodes(id = rxn_id)
      })
    } else{
      observe({
        s = input$fluxes_ko1_rows_selected
        df = fluxes_output
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_ko") %>%
          visSelectNodes(id = rxn_id)
      })
    }
    
    output$reset_ko = renderUI(
      popify(
        bsButton(
          inputId = "reset",
          label = "Reset",
          block = T
        ),
        title = "Reset model",
        content = "Brings the model back to its original state",
        placement = "right",
        trigger = "hover"
      )
    )
  })
  
  
  # APPLY GENE EXPRESSION ---------------------------------------------------
  observeEvent(input$apply_expr, {
    model_name = isolate(input$pick_model)
    objective = isolate(input$select_objective)
    ori_objective = objective
    rxn_names_list = get_reactionIDs_names(model_name = model_name)
    ori_objective_name = names(which(rxn_names_list == ori_objective))
    
    if (model_name != "toycon") {
      exclude = isolate(input$exclude)
      objective = paste(strsplit(objective, split = "_")[[1]][-1], collapse = "_")
    } else{
      exclude = F
      objective = strsplit(objective, split = "_")[[1]][2]
    }
    working_dir = getwd()
    path = paste("/data/", model_name, ".xml", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    model_file_path = paste(working_dir, path, sep = "")
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    toycon = readRDS(paste(working_dir, "/data/", model_name, ".rda", sep = "")) #formal class modelorg object
    data = rsbml_graph(sbml_model)
    toycon_graph = igraph.from.graphNEL(data)
    visdata <- toVisNetworkData(toycon_graph)
    visdata_ori = visdata
    
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
    visdata$edges$width = 2
    visdata$edges$length = 150
    net = asNetwork(toycon_graph)
    
    #Setting colors according to node class
    color_reaction = "lightblue"
    color_metabolite = "lightsalmon"
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    
    #Setting proper nodes names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    
    #get the gene/reaction expression of which will be adjusted
    gene_name = (input$pick_expr_gene)
    gene_ID = gene_name
    #get the bounds of the expression level and assign to the Python variable
    bound = input$expr
    python.assign("objective", objective)
    python.assign("bound", bound)
    python.assign("gene_ID", gene_ID)
    python.assign("model_file_path", model_file_path)
    if (model_name == "toycon") {
      path = "/scripts/simulate_expression_toycon.py"
    } else{
      path = "/scripts/simulate_expression.py"
    }
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
    
    #render the fluxes table for the UI
    if (model_name == "toycon") {
      output$fluxes_expr = DT::renderDataTable({
        fluxes_output
      }, selection = "single", options = list(pageLength = 10), rownames = FALSE)
    } else{
      output$fluxes_expr1 = DT::renderDataTable({
        fluxes_output
      }, selection = "single", options = list(pageLength = 10), rownames = FALSE)
    }
    
    #render the text for UI
    output$text_flux_expr = renderText({
      paste("<b>", format(round(flux, digits = 2), nsmall = 2), "</b>")
    })
    names_mapper = get_reactionIDs_names(model_name = model_name)
    objective_name = names(which(names_mapper == ori_objective))
    output$text_objective_expr = renderText({
      paste("<b>", as.character(objective_name), "</b>")
    })
    
    toycon_graph = igraph.from.graphNEL(data)
    net = asNetwork(toycon_graph)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    #Assign proper names
    reactions_names = as.vector(unlist(net$val)[which(names(unlist(net$val)) ==
                                                        "vertex.names")][which(grepl("^R", unlist(net$val)[which(names(unlist(net$val)) ==
                                                                                                                   "vertex.names")]))])
    visdata <- toVisNetworkData(toycon_graph)
    #Adding duplicate metabolites/reactions
    if (model_name == "toycon") {
      visdata = add_dups_new_layout(visdata)
    }
    visdata$nodes$group = rep("Metabolite", length(visdata$nodes$id))
    visdata$nodes$group[which(grepl("R", visdata$nodes$id))] = "Reaction"
    visdata$nodes$group[which(grepl("m\\d*$", visdata$nodes$id))] = "Metabolite mitochondria"
    visdata$nodes$group[which(grepl(
      perl = T,
      pattern = "^M\\S*e$",
      x = visdata$nodes$id
    ))] = "Metabolite external"
    visdata$nodes$group[which(visdata$nodes$id == ori_objective)] = "Biological objective"
    
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
    color_metabolite_mitochondria = "red"
    color_metabolite_external = "indianred"
    color_reaction_objective = "lightgreen"
    names = rownames(visdata$nodes)
    net %v% "type" = ifelse(grepl("R", names), "Reaction", "Metabolite")
    edges_names = names
    #Setting names
    for (i in seq(1, length(names))) {
      if (model_name == "toycon") {
        if (nchar(names[i]) < 6) {
          names[i] = substr(names[i], 1, 4)
        } else{
          names[i] = substr(names[i], 1, 6)
        }
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
    if (model_name == "toycon") {
      edges_names = sapply(edges_names, function(x)
        fill_blank(x, 7))
    }
    names_dict = rbind(edges_names, names) #Names and IDs dictionary
    visdata$nodes$label = as.vector(edges_names)
    path = "data/textbooky_coords.csv"
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    if (model_name == "toycon") {
      coords = read.csv(path)
      visdata$nodes = cbind(visdata$nodes, coords)
    }
    visdata$nodes[which(grepl("glycolysis", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("respiration", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("synthase", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("demand", names_dict[1, ])), "font"] = "20px arial"
    #Emphasize main metabolites
    visdata$nodes[which(grepl("^lactate$", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("^glucose$", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("ATP", names_dict[1, ])), "font"] = "20px arial"
    visdata$nodes[which(grepl("ADP", names_dict[1, ])), "font"] = "20px arial"
    
    shape_metabolites = ifelse(model_name == "toycon", "circle", "dot")
    shape_reactions = ifelse(model_name == "toycon", "box", "square")
    
    if (exclude == T & model_name != "toycon") {
      visdata = exclude_trans_exchange(visdata)
    }
    
    lnodes <-
      data.frame(
        label = c(
          "Cytosolic metabolite",
          "Mitochondrial metabolite",
          "External metabolite",
          "Reaction",
          "Biological objective"
        ),
        shape = c("dot", "dot", "dot", "box", "box"),
        color = c(
          "lightsalmon",
          "red",
          "indianred",
          "lightblue",
          "lightgreen"
        ),
        title = "Informations"
      )
    
    ledges <- data.frame(
      color = c("black", "black"),
      label = c("flux", "no flux"),
      arrows = c("to", "to"),
      dashes = c(F, T)
    )
    #Plotting graph
    output$graph_expr = renderVisNetwork({
      visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
        visLegend(
          position = "right",
          stepX = 100,
          stepY = 75,
          width = 0.2,
          useGroups = F,
          addNodes = lnodes,
          zoom = T,
          addEdges = ledges
        ) %>%
        visOptions(highlightNearest = T) %>%
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
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Reaction",
                  color = color_reaction,
                  shape = shape_reactions) %>%
        visGroups(groupname = "Metabolite mitochondria",
                  color = color_metabolite_mitochondria,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Metabolite external",
                  color = color_metabolite_external,
                  shape = shape_metabolites) %>%
        visGroups(groupname = "Biological objective",
                  color = color_reaction_objective,
                  shape = shape_reactions) %>%
        visLayout(randomSeed = 1) %>%
        visPhysics(
          solver = "hierarchicalRepulsion",
          hierarchicalRepulsion = list(
            nodeDistance = 40,
            springLength = 10,
            springConstant = 0,
            damping = 0.1
          )
        )
    })
    
    if (model_name == "toycon") {
      observe({
        df = fluxes_output
        s = input$fluxes_expr_rows_selected
        rxn_name = df[s, 1]
        rxn_id = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_expr") %>%
          visSelectNodes(id = rxn_id)
      }, priority = 0, autoDestroy = T)
    } else{
      observe({
        df1 = fluxes_output
        s1 = input$fluxes_expr1_rows_selected
        rxn_name = df1[s1, 1]
        rxn_id1 = visdata$nodes$id[which(visdata$nodes$label == rxn_name)]
        visNetworkProxy("graph_expr") %>%
          visSelectNodes(id = rxn_id1)
      }, priority = 0, autoDestroy = T)
    }
  })
  
  # MODEL CHANGE ------------------------------------------------------------
  
  observeEvent(input$pick_model, ignoreInit = F, {
    removeTab(inputId = "tabs",
              target = "ko_reactions",
              session = getDefaultReactiveDomain())
    removeTab(inputId = "tabs",
              target = "simulate_expression_changes",
              session = getDefaultReactiveDomain())
    removeTab(inputId = "tabs",
              target = "change_media",
              session = getDefaultReactiveDomain())
    if (input$pick_model != "toycon") {
      insertUI(selector = "#placeholder",
               ui = tags$div(
                 id = "exclude",
                 checkboxInput(
                   inputId = "exclude",
                   label = "Do not visualize the exchange and transport reactions",
                   value = F,
                   width = "55%"
                 )
               ))
      
    } else{
      removeUI(selector = "#exclude")
    }
    model_name = input$pick_model
    working_dir = getwd()
    path = paste("/data/", model_name, "_var.RData", sep = "")
    if (.Platform$OS.type == "windows") {
      path = gsub("\\\\", "/", path)
    }
    load(paste(working_dir, path, sep = "")) #formal class SBML object
    choices_list = list()
    names_choices = c()
    for (i in seq_len(length(sbml_model@model@reactions))) {
      choices_list[[i]] = sbml_model@model@reactions[[i]]@id
      names_choices = append(names_choices, sbml_model@model@reactions[[i]]@name)
    }
    names(choices_list) = names_choices
    if (model_name == "toycon") {
      ori_objective = "R_R4"
    } else{
      ori_objective = "R_BIOMASS_Ecoli_core_w_GAM"
    }
    
    removeUI(selector = "#select_objective")
    insertUI(selector = "#placeholder1",
             ui = tags$div(
               id = "select_objective",
               selectInput(
                 inputId = "select_objective",
                 label = NULL,
                 choices = choices_list,
                 selected = ori_objective,
                 width = "80%"
               )
             ))
  })
  
  observeEvent(input$select_objective, ignoreInit = T, {
    removeTab(inputId = "tabs",
              target = "ko_reactions",
              session = getDefaultReactiveDomain())
    removeTab(inputId = "tabs",
              target = "simulate_expression_changes",
              session = getDefaultReactiveDomain())
    removeTab(inputId = "tabs",
              target = "change_media",
              session = getDefaultReactiveDomain())
  })
  
  
  observeEvent(input$exclude, {
    removeTab(inputId = "tabs",
              target = "ko_reactions",
              session = getDefaultReactiveDomain())
    removeTab(inputId = "tabs",
              target = "simulate_expression_changes",
              session = getDefaultReactiveDomain())
    removeTab(inputId = "tabs",
              target = "change_media",
              session = getDefaultReactiveDomain())
  })
  
  
  session$onSessionEnded(function() {
    #set dir
    setwd("data")
    #remove files which were produced
    file.remove(list.files(pattern = "*removed*"))
    stopApp()
  })
})
