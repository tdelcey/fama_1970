build_dynamic_networks_bb <- function(
  nodes,
  directed_edges,
  source_id,
  target_id,
  time_variable = NULL,
  time_window = NULL,
  backbone_method = c("statistical", "structured"),
  statistical_method = c("sdsm", "fdsm", "fixedfill", "fixedfrow", "fixedcol"),
  alpha = alpha,
  coupling_measure = c(
    "coupling_angle",
    "coupling_strength",
    "coupling_similarity"
  ),
  edges_threshold = 1,
  overlapping_window = FALSE,
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...,
  verbose = TRUE
) {
  size <- node_size <- N <- method <- NULL

  # Making sure the table is a datatable
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)

  # Checking the methods
  backbone_methods = c("statistical", "structured")

  coupling_measures <- c(
    "coupling_angle",
    "coupling_strength",
    "coupling_similarity"
  )

  statistical_methods <- c("sdsm", "fdsm", "fixedfill", "fixedfrow", "fixedcol")

  if (length(backbone_method) > 1) {
    cli::cli_abort(
      c(
        "You did not choose any method for extracting the backbone. You have to choose between: ",
        "*" = "\"statistical\";",
        "*" = "\"structured\"."
      )
    )
  }

  if (!backbone_method %in% backbone_methods) {
    cli::cli_abort(
      c(
        "You did not choose any method for extracting the backbone. You have to choose between: ",
        "*" = "\"statistical\";",
        "*" = "\"structured\";"
      )
    )
  }

  # check various setting for the structured methods

  if (backbone_method == "structured") {
    # Checking various problems: lacking method,
    if (length(coupling_measure) > 1) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }

    if (!coupling_measure %in% coupling_measures) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }
  }

  # check various setting for the statistical methods
  if (backbone_method == "statistical") {
    # check if a model is given
    if (length(statistical_method) > 1) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedfrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }

    if (!statistical_method %in% statistical_methods) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedfrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }

    # check if alpha is given
    if (is.null(alpha)) {
      cli::cli_abort(
        "For statistical backbone extraction, you have to choose a signifiance level alpha."
      )
    }
  }

  # warning if the source_id is not unique
  if (
    nodes[, .N, source_id, env = list(source_id = source_id)][N > 1, .N] > 0
  ) {
    cli::cli_alert_warning(
      "Some identifiers in your column {.field {source_id}} in your nodes table are not unique. You need only one row per node."
    )
  }

  # check settings for intertemporal networks
  if (!is.null(time_window) & is.null(time_variable)) {
    cli::cli_abort(
      "You cannot have a {.emph time_window} if you don't give any column with a temporal variable. Put a column in {.emph time_variable} or remove the {.emph time_window}."
    )
  }

  # VERBOSE

  if (verbose == TRUE) {
    if (length(statistical_method > 0)) {
      cli::cli_alert_info(paste(
        "We extract the network backbone using the",
        backbone_method,
        "method."
      ))
    }

    if (keep_singleton == FALSE) {
      cli::cli_alert_info(
        "Keep_singleton == FALSE: removing the nodes that are alone with no edge. \n\n"
      )
    }
  }

  # CHECKING THE DATA

  # NODES
  nodes_coupling <- data.table::copy(nodes)
  nodes_coupling[,
    source_id := as.character(source_id),
    env = list(source_id = source_id)
  ]

  if (is.null(time_variable)) {
    time_variable <- "fake_column"
    nodes_coupling[,
      time_variable := 1,
      env = list(time_variable = time_variable)
    ]
  }

  if (
    !target_id %in% colnames(nodes_coupling) &
      compute_size == TRUE
  ) {
    cli::cli_abort(
      "You don't have the column {.field {target_id}} in your nodes table. Set {.emph compute_size} to {.val FALSE}."
    )
  }

  if (compute_size == TRUE) {
    nodes_coupling[,
      target_id := as.character(target_id),
      env = list(target_id = target_id)
    ]
  }

  # EDGES

  edges <- data.table::copy(directed_edges)
  edges <- edges[, .SD, .SDcols = c(source_id, target_id)] # we keep only the columns we need
  edges[,
    c(source_id, target_id) := lapply(.SD, as.character),
    .SDcols = c(source_id, target_id)
  ] # we need to have character columns

  ######################### Dynamics networks *********************

  # define the time window
  nodes_coupling <- nodes_coupling[
    order(time_variable),
    env = list(time_variable = time_variable)
  ]
  nodes_coupling[,
    time_variable := as.integer(time_variable),
    env = list(time_variable = time_variable)
  ]

  first_year <- nodes_coupling[,
    min(as.integer(time_variable)),
    env = list(time_variable = time_variable)
  ]
  last_year <- nodes_coupling[,
    max(as.integer(time_variable)),
    env = list(time_variable = time_variable)
  ]

  if (!is.null(time_window)) {
    if (last_year - first_year + 1 < time_window) {
      cli::cli_alert_warning(
        "Your time window is larger than the number of distinct values of {.field {time_variable}}"
      )
    }
  }

  if (is.null(time_window)) {
    all_years <- first_year
    time_window <- last_year - first_year + 1
  } else {
    if (overlapping_window == TRUE) {
      last_year <- last_year - time_window + 1
      all_years <- first_year:last_year
    } else {
      all_years <- seq(first_year, last_year, by = time_window)
      if (all_years[length(all_years)] + (time_window - 1) > last_year) {
        cli::cli_warn(
          "Your last network is shorter than the other(s) because the cutting by time window does not give a round count.
                The last time unity in your data is {.val {last_year}}, but the upper limit of your last time window is
                {.val {all_years[length(all_years)] + (time_window - 1)}}."
        )
      }
    }
  }

  # Prepare our list
  tbl_coup_list <- list()

  for (year in all_years) {
    nodes_of_the_year <- nodes_coupling[
      time_variable >= year &
        time_variable < (year + time_window),
      env = list(time_variable = time_variable, year = year)
    ]

    if (time_variable != "fake_column") {
      nodes_of_the_year[,
        time_window := paste0(year, "-", year + time_window - 1),
        env = list(year = year)
      ]

      if (verbose == TRUE) {
        cli::cli_h1(
          "Generation of the network for the {.val {year}}-{.val {year + time_window - 1}} time window."
        )
      }
    } else {
      nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
    }

    edges_of_the_year <- edges[
      source_id %in% nodes_of_the_year[, source_id],
      env = list(source_id = source_id)
    ]

    # size of nodes
    if (compute_size == TRUE) {
      nb_cit <- edges_of_the_year[
        source_id %in% nodes_of_the_year[, source_id],
        .N,
        target_id,
        env = list(source_id = source_id, target_id = target_id)
      ]

      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"

      if ("node_size" %in% colnames(nodes_coupling) == TRUE) {
        cli::cli_warn(
          "You already have a column name {.field node_size}. The content of the column will be replaced."
        )
      }

      nodes_of_the_year <- data.table::merge.data.table(
        nodes_of_the_year,
        nb_cit,
        by = target_id,
        all.x = TRUE
      )

      nodes_of_the_year[is.na(node_size), node_size := 0]
    }

    # backbone

    if (backbone_method == "statistical") {
      # prepare backbone function
      backbone_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(backbone::sdsm),
            rlang::expr(backbone::fdsm),
            rlang::expr(backbone::fixedfrow),
            rlang::expr(backbone::fixedcol),
            rlang::expr(backbone::fixedfill)
          ),
          method = c("sdsm", "fdsm", "fixedfrow", "fixedcol", "fixedfill")
        )

      backbone_functions <- backbone_functions[method == statistical_method][[
        "biblio_function"
      ]][[1]]

      # Evaluate the expression and catch internal errors to backbone package

      tryCatch(
        {
          # using backbone with edgelist is simpler but lead to error in backbone function
          edges_of_the_year <-
            rlang::expr((!!backbone_functions)(
              B = as.data.frame(edges_of_the_year),
              alpha = rlang::inject(alpha)
            )) %>%
            eval() %>%
            as.data.table()
        },
        error = function(e) {
          stop(
            "The backbone function failed with an error. Read the backbone documentation for more information. Error message: ",
            e$message
          )
        }
      )
    }

    # coupling
    if (backbone_method == "structured") {
      biblio_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(biblionetwork::biblio_coupling),
            rlang::expr(biblionetwork::coupling_strength),
            rlang::expr(biblionetwork::coupling_similarity)
          ),
          method = c(
            "coupling_angle",
            "coupling_strength",
            "coupling_similarity"
          )
        )

      biblio_function <- biblio_functions[method == coupling_measure][[
        "biblio_function"
      ]][[1]]

      # evaluate the expression and catch internal errors to biblionetwork package

      tryCatch(
        {
          edges_of_the_year <-
            rlang::expr((!!biblio_function)(
              dt = edges_of_the_year,
              source = rlang::inject(source_id),
              ref = rlang::inject(target_id),
              weight_threshold = rlang::inject(edges_threshold)
            )) %>%
            eval()
        },
        error = function(e) {
          stop(
            "The coupling function failed with an error. Read the biblionetwork documentation for more information. Error message: ",
            e$message
          )
        }
      )
    }

    edges_of_the_year[, source_id := from]
    edges_of_the_year[, target_it := to]

    # remove nodes with no edges
    if (keep_singleton == FALSE) {
      nodes_of_the_year <- nodes_of_the_year[
        source_id %in%
          edges_of_the_year$from |
          source_id %in% edges_of_the_year$to,
        env = list(source_id = source_id)
      ]
    }

    # make tbl
    if (length(all_years) == 1) {
      tbl_coup_list <- tidygraph::tbl_graph(
        nodes = nodes_of_the_year,
        edges = edges_of_the_year,
        directed = FALSE,
        node_key = source_id
      )
    } else {
      tbl_coup_list[[paste0(year, "-", year + time_window - 1)]] <-
        tidygraph::tbl_graph(
          nodes = nodes_of_the_year,
          edges = edges_of_the_year,
          directed = FALSE,
          node_key = source_id
        )
    }
  }

  if (filter_components == TRUE) {
    if (verbose == TRUE) {
      if (!is.null(min_share)) {
        cli::cli_alert_info(
          "We keep the components with a share of at least {min_share}."
        )
      }

      if (!is.null(nb_components)) {
        cli::cli_alert_info(
          "We keep the top {nb_components} largest component(s)."
        )
      }
    }

    tbl_coup_list <- filter_components_dynamic(tbl_coup_list, ...)
  }
  return(tbl_coup_list)
}


###### custom filter components

filter_components_dynamic <- function(
  graphs,
  nb_components = NULL,
  min_share = NULL,
  keep_component_columns = FALSE,
  verbose = FALSE
) {
  # Helper function to apply filtering to a single graph
  filter_single_graph <- function(graph) {
    # checking

    if (
      !is.null(min_share) &
        !is.null(nb_components) |
        is.null(min_share) & is.null(nb_components)
    ) {
      cli::cli_abort(
        "You must specify either {.field nb_components} or {.field min_share}."
      )
    }

    graph <- graph %N>%
      dplyr::mutate(
        components_att = tidygraph::group_components(type = "weak")
      ) %>%
      dplyr::group_by(components_att) %>%
      dplyr::mutate(size_components = dplyr::n()) %>%
      dplyr::ungroup()

    # Summarize component sizes
    component_sizes <- graph %N>%
      as.data.frame() %>%
      dplyr::count(components_att, name = "size") %>%
      dplyr::mutate(share = size / sum(size)) %>%
      dplyr::arrange(desc(size)) %>%
      dplyr::mutate(cum_share = cumsum(share), rank = dplyr::row_number())

    # Decide how many components to keep
    if (!is.null(min_share)) {
      selected_components <- component_sizes %>%
        dplyr::filter(share >= min_share) %>%
        dplyr::pull(components_att)

      if (verbose) {
        cli::cli_alert_info(
          "Keeping components with share of at least {min_share}."
        )
      }
    } else if (!is.null(nb_components)) {
      selected_components <- component_sizes %>%
        dplyr::slice_head(n = nb_components) %>%
        dplyr::pull(components_att)

      if (verbose) {
        cli::cli_alert_info(
          "Keeping top {nb_components} largest component(s)."
        )
      }
    } else {
      cli::cli_abort(
        "You must specify either {.field nb_components} or {.field min_share}."
      )
    }

    # Filter graph
    graph <- graph %N>%
      dplyr::filter(components_att %in% selected_components)

    # Clean up columns if needed
    if (!keep_component_columns) {
      graph <- graph %>% dplyr::select(-components_att, -size_components)
    }

    return(graph)
  }

  # Apply to list or single graph
  if (inherits(graphs, "list")) {
    graphs <- lapply(graphs, filter_single_graph)
  } else if (inherits(graphs, "tbl_graph")) {
    graphs <- filter_single_graph(graphs)
  } else {
    cli::cli_abort(
      "Your {.field graphs} object must be a {.cls tbl_graph} or a list of {.cls tbl_graph}."
    )
  }

  return(graphs)
}

add_node_roles <- function(
  graph,
  module_col = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
) {
  # Check if the module column exists in the node data
  if (
    !module_col %in%
      names(graph %>% tidygraph::activate(nodes) %>% dplyr::as_tibble())
  ) {
    cli::cli_abort("Column {.field {module_col}} is missing from node data.")
  }

  # use name col as variable in dplyr
  module_sym <- rlang::sym(module_col)

  nodes_tbl <- graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(.node_id = dplyr::row_number()) %>%
    dplyr::select(.node_id, module = !!module_sym)

  edges_tbl <- graph %>%
    tidygraph::activate(edges) %>%
    dplyr::as_tibble()

  # checking for from and to columns in edges data
  from_col <- if ("from" %in% names(edges_tbl)) {
    "from"
  } else if (".from" %in% names(edges_tbl)) {
    ".from"
  } else {
    cli::cli_abort("No {.field from} or {.field .from} column found in edges.")
  }

  to_col <- if ("to" %in% names(edges_tbl)) {
    "to"
  } else if (".to" %in% names(edges_tbl)) {
    ".to"
  } else {
    cli::cli_abort("No {.field to} or {.field .to} column found in edges.")
  }

  edges_tbl <- edges_tbl %>%
    dplyr::mutate(
      .from = as.integer(.data[[from_col]]),
      .to = as.integer(.data[[to_col]])
    )

  # checking for weight column in edges data
  edges_tbl$..weight <- edges_tbl[[weight_col]]

  # join nodes id to edges to get module information for both ends of the edges
  edges_tbl <- edges_tbl %>%
    dplyr::left_join(nodes_tbl, by = c(".from" = ".node_id")) %>%
    dplyr::rename(module_from = module) %>%
    dplyr::left_join(nodes_tbl, by = c(".to" = ".node_id")) %>%
    dplyr::rename(module_to = module)

  # long format of edges to compute indicators
  edges_long <- dplyr::bind_rows(
    edges_tbl %>%
      dplyr::transmute(
        node_id = .from,
        module_from = module_from,
        module_to = module_to,
        w = ..weight
      ),
    edges_tbl %>%
      dplyr::transmute(
        node_id = .to,
        module_from = module_to,
        module_to = module_from,
        w = ..weight
      )
  )

  # for each node, compute k_i
  # that is the total number of edges of node i
  k_i <- edges_long %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(k_i = sum(w), .groups = "drop")

  # for each node and each module, compute k_is
  # that is the number of edges from node i to nodes in module s
  k_is <- edges_long %>%
    dplyr::group_by(node_id, module_to) %>%
    dplyr::summarise(k_is = sum(w), .groups = "drop")

  # compute ki_s for the same module (kappa)
  kappa <- edges_long %>%
    dplyr::filter(module_to == module_from) %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(kappa = sum(w), .groups = "drop")

  # join everything to nodes table
  roles_tbl <- nodes_tbl %>%
    dplyr::left_join(k_i, by = c(".node_id" = "node_id")) %>%
    dplyr::left_join(kappa, by = c(".node_id" = "node_id")) %>%
    dplyr::mutate(
      k_i = tidyr::replace_na(k_i, 0),
      kappa = tidyr::replace_na(kappa, 0)
    )

  # compute the participation coefficient for each node
  participation_tbl <- k_is %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(
      participation = 1 - sum((k_is / sum(k_is))^2),
      .groups = "drop"
    )

  # join participation coefficient to roles table
  roles_tbl <- roles_tbl %>%
    dplyr::left_join(participation_tbl, by = c(".node_id" = "node_id")) %>%
    # if a node has no edges, its participation coefficient is 0
    dplyr::mutate(participation = tidyr::replace_na(participation, 0))

  # we now compute the within-module degree z-score for each node, which is the number of edges within the same module (kappa) standardized by the mean and standard deviation of kappa for all nodes in the same module
  roles_tbl <- roles_tbl %>%
    dplyr::group_by(module) %>%
    dplyr::mutate(
      kappa_mean = mean(kappa, na.rm = TRUE),
      kappa_sd = stats::sd(kappa, na.rm = TRUE),
      within_module_z = dplyr::if_else(
        kappa_sd == 0,
        0,
        (kappa - kappa_mean) / kappa_sd
      )
    ) %>%
    dplyr::ungroup()

  # we can now classify the nodes into 7 roles following Guimerà and Amaral (2005)
  roles_tbl <- roles_tbl %>%
    dplyr::mutate(
      role_ga = dplyr::case_when(
        # classify hubs first
        within_module_z >= z_threshold &
          participation < 0.30 ~ "R5 provincial hub",
        within_module_z >= z_threshold &
          participation < 0.75 ~ "R6 connector hub",
        within_module_z >= z_threshold ~ "R7 kinless hub",
        # the rest of the nodes are non-hubs and are classified
        # into 4 roles based on their participation coefficient
        participation < 0.05 ~ "R1 ultra-peripheral",
        participation < 0.62 ~ "R2 peripheral",
        participation < 0.80 ~ "R3 non-hub connector",
        TRUE ~ "R4 non-hub kinless"
      ),
      # compute dummy variable to compute average indicators for each cluster later
      is_central = within_module_z >= z_threshold,
      is_connector = participation >= 0.62
    ) %>%
    # select only the columns we need to join back to the graph
    dplyr::select(
      .node_id,
      within_module_degree = kappa,
      within_module_z,
      participation_coeff = participation,
      role_ga,
      is_central,
      is_connector
    )

  # final join to the graph
  graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(.node_id = dplyr::row_number()) %>%
    dplyr::left_join(roles_tbl, by = ".node_id") %>%
    dplyr::select(-.node_id)
}
