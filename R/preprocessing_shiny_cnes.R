.preprocessing(
  param_list = param_list,
  par_fun = "parent")


# Internal function, which is the "real" preprocessing() function insider the use of sink
# (this workaround was used in order to manage final sink() in those cases
# in which return() is used inside the function.)
# TODO: manage also errors (.preprocessing inside a trycatch; in case of errors, stop
# passing the error message)
.preprocessing <- function(param_list = NULL,
                           parallel = TRUE,
                           use_python = TRUE,
                           tmpdir = NA,
                           rmtmp = TRUE,
                           par_fun = "parent") {
  # Starting execution
  print_message(
    type = "message",
    date = TRUE,
    "Demarrage de la session"
  )
}
  
##### 1. Read / import parameters #####

# Import param_list, if provided
pm_list <- if (is(param_list, "character")) {
  # load json parameter file
  jsonlite::fromJSON(param_list)
  # TODO check package version and parameter names
} else if (is(param_list, "list")) {
  param_list
  # TODO check parameter names
} else {
  list("pkg_version" = packageVersion("shinycnes"))
}

# La tu choisis a partir de quoi tu importes tes parametres

## Check consistency of parameters
# TODO work in progress
pm <- check_param_list(pm_list, type = "error", correct = TRUE)

# convert from GeoJSON to sf
if (is(pm$extent, "character") | is(pm$extent, "geojson")) {
  pm$extent <- st_read(pm$extent, quiet = TRUE)
} else if (is(pm$extent, "Spatial")) {
  pm$extent <- st_as_sf(pm$extent)
}

# Passage du format GeoJson (donnees spatialisees) a du shapefile

# check parallel (workaround) = est-ce que les calculs s'effectuent sur plusieurs cores en même temps ?
# TODO add parallel to the GUI, and threat as a normal parameter
if (is.null(pm$parallel)) {
  pm$parallel <- parallel
}

# si y a rien d'indique dans pm parallel --> tu le fais en parallele

# define and create tmpdir = stocker des donnees temporaires
if (is.na(tmpdir)) {
  # if outformat is VRT, set as a subdirectory of path_out
  tmpdir <- if (pm$outformat == "VRT" &
                !all(is.na(pm[c(
                  "path_data",
                  "path_out",
                  "path_rgb",
                  "path_indices",
                  "path_tiles",
                  "path_translate",
                  "path_mosaic",
                  "path_merged"
                )]))) {
    # use path_out if it is not NA, otherwise path_indices, otherwise path_tiles, otherwise path_merged
    main_dir <-
      unlist(pm[c(
        "path_data",
        "path_out",
        "path_rgb",
        "path_indices",
        "path_tiles",
        "path_translate",
        "path_mosaic",
        "path_merged"
      )])[!is.na(pm[c(
        "path_data",
        "path_out",
        "path_rgb",
        "path_indices",
        "path_tiles",
        "path_translate",
        "path_mosaic",
        "path_merged"
      )])][1] # Pourquoi le [1] ?
    dir.create(main_dir, showWarnings = FALSE)
    file.path(main_dir, ".vrt") # construction du chemin a partir de main_dir cree precedemment 
  } else {
    tempfile(pattern = "shinycnes_") # retourne une liste de caracteres qui peuvent etre utilises comme noms pour des fichiers temporaires avec pattern qui donne la partie initiale du nom (?)
  }
}
if (pm$outformat == "VRT") {
  rmtmp <- FALSE # force not to remove intermediate files
}

dir.create(tmpdir, showWarnings = FALSE)

# Temporay execution
print_message(
  type = "message",
  date = TRUE,
  "Une direction temporaire a ete creee."
)

# internal parameters --> a chaque fois, si le chemin existe on l'utilise sinon on le cree dans tmpdir
paths <- c()
paths["data"] <- if (!is.na(pm$path_data)) {
  pm$path_data
} else {
  file.path(tmpdir, "data")
}
paths["out"] <- if (!is.na(pm$path_out)) {
  pm$path_out
} else {
  file.path(tmpdir, "out")
}
paths["translate"] <- if (!is.na(pm$path_translate)) {
  pm$path_translate
} else {
  file.path(tmpdir, "translate")
}
paths["rgb"] <- if (!is.na(pm$path_rgb)) {
  pm$path_rgb
} else {
  file.path(tmpdir, "rgb")
}
paths["indices"] <- if (!is.na(pm$path_indices)) {
  pm$path_indices
} else {
  file.path(tmpdir, "indices")
}
paths["tiles"] <- if (!is.na(pm$path_tiles)) {
  pm$path_tiles
} else {
  file.path(tmpdir, "tiles")
}
paths["merged"] <- if (!is.na(pm$path_merged)) {
  pm$path_merged
} else {
  file.path(tmpdir, "merged")
}
paths["mosaic"] <- if (!is.na(pm$path_mosaic)) {
  pm$path_mosaic
} else {
  file.path(tmpdir, "mosaic")
}
paths["warped"] <- if (!is.na(pm$path_warped)) {
  pm$path_warped
} else {
  file.path(tmpdir, "warped")
}

# check that output parent directories exist, and create required paths 
# pm = perception mapping
parent_paths <- sapply(
  pm[c(
    "path_project",
    "path_data",
    "path_tiles",
    "path_merged",
    "path_mosaic",
    "path_translate",
    "path_out",
    "path_rgb",
    "path_warped",
    "path_indices"
  )],
  function(x) {
    if (is.na(x)) {
      NA
    } else {
      dirname(x)
    }
  }
) %>% unique() %>% na.omit() %>% as.character() 
# iris %>% head () = head (iris)

# Check path execution
print_message(
  type = "message",
  date = TRUE,
  "Repertoires de travail crees avec succes."
)

paths_exist <- sapply(parent_paths, file.exists) # on regarde si tout est ok au niveau des parent_paths
if (any(!paths_exist)) { # any = au moins un truc parmi tout ce qui suit est verifie, ici si au moins un des chemins manque
  print_message(
    type = "error",
    if (sum(!paths_exist) == 1) {
      "La direction de sortie suivante n'existe pas."
    } else {
      "Les directions de sortie suivantes n'existent pas."
    },
    paste(names(paths_exist[!paths_exist]), collapse = "\n"),
    if (sum(!paths_exist) == 1) {
      ".\nVeuillez la creer avant de continuer."
    } else {
      ".\nVeuillez les creer avant de continuer."
    },
  )
} # end if

if (pm$product == "theia") {
  sapply(
    pm[c(
      "path_project",
      "path_data",
      "path_tiles",
      "path_merged",
      "path_translate",
      "path_out",
      "path_mosaic",
      "path_rgb",
      "path_warped",
      "path_indices"
    )],
    function(x) {
      if (is.na(x)) {
        NA
      } else {
        dir.create(x, recursive = FALSE, showWarnings = FALSE)
      }
    }
  )
}

# check output format
gdal_formats <-
  fromJSON(system.file("extdata", "gdal_formats.json", package = "Inventaire_XY")) # conversion du format JSON vers un objet R
sel_driver <- gdal_formats[gdal_formats$name == pm$outformat, ]
if (is.null(pm$rgb_outformat)) {
  pm$rgb_outformat <- pm$outformat
} # to avoid errors
if (is.null(pm$rgb_compression)) {
  pm$rgb_compression <- pm$compression
} # to avoid errors
sel_rgb_driver <-
  gdal_formats[gdal_formats$name == pm$rgb_outformat, ]

# if (is.null(py_to_r(sel_driver))) {
if (nrow(sel_driver) == 0) {
  print_message(
    type = "error",
    "Le format \"",
    pm$outformat,
    "\" n'est pas reconnu; ",
    "veuillez utiliser l'un des formats supportes par notre installation GDAL.\n\n",
    "Pour les lister, utilisez la commande suivante :\n",
    "gdalUtils::gdalinfo(formats=TRUE)\n\n",
    "Pour rechercher un format specifique, utilisez :\n",
    "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
  )
}
if (nrow(sel_rgb_driver) == 0) {
  print_message(
    type = "error",
    "Le format \"",
    pm$rgb_outformat,
    "\" n'est pas reconnu; ",
    "veuillez utiliser l'un des formats supportes par notre installation GDAL.\n\n",
    "Pour les lister, utilisez la commande suivante :\n",
    "gdalUtils::gdalinfo(formats=TRUE)\n\n",
    "Pour rechercher un format specifique, utilisez :\n",
    "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
  )
}
# define output extension
main_ext <- sel_driver[1, "ext"]

##### 2. THEIA Part (define output formats) #####

# if product is required, define output formats
if (pm$product == "theia") {
  ## Define output formats
  if (!anyNA(pm$list_prods)) {
    out_ext <- main_ext
    out_outformat <- pm$outformat
  } else {
    out_ext <- "vrt"
    out_outformat <- "VRT"
  }
  if (!is.na(pm$path_data)) {
    tiles_ext <- main_ext
    tiles_outformat <- pm$outformat
  } else {
    tiles_ext <- "vrt"
    tiles_outformat <- "VRT"
  }
  if (!is.na(pm$path_tiles)) {
    tiles_ext <- main_ext
    tiles_outformat <- pm$outformat
  } else {
    tiles_ext <- "vrt"
    tiles_outformat <- "VRT"
  }
  if (!is.na(pm$path_mosaic)) {
    mosaic_ext <- main_ext
    mosaic_outformat <- pm$outformat
  } else {
    mosaic_ext <- "vrt"
    mosaic_outformat <- "VRT"
  }
  if (!is.na(pm$path_translate)) {
    translate_ext <- main_ext
    translate_outformat <- pm$outformat
  } else {
    translate_ext <- "vrt"
    translate_outformat <- "VRT"
  }
  if (!is.na(pm$path_merged)) {
    merged_ext <- main_ext
    merged_outformat <- pm$outformat
  } else {
    merged_ext <- "vrt"
    merged_outformat <- "VRT"
  }
  if (is.na(pm$mask_type)) {
    warped_ext <- out_ext
    warped_outformat <- out_outformat
  } else {
    warped_ext <- "vrt"
    warped_outformat <- "VRT"
  }
  if (pm$index_source %in% pm$list_prods) {
    sr_masked_ext <- main_ext
    sr_masked_outformat <- pm$outformat
  } else {
    sr_masked_ext <- "vrt"
    sr_masked_outformat <- "VRT"
  }
}

##### 3. Find PEPS or THEIA and compute the names of required files #####
for (dummy in TRUE) { # dummy prend la valeur 0 ou 1 pour indiquer que quelque chose est vrai ou faux
  # dummy cycle, created only to allow "break" from this part    
  cnes_lists <- list()
  cnes_lists_downloaded <- list()
  
  if (pm$online == TRUE) {
    if (pm$product == "theia") {
      print_message(
        type = "message",
        date = TRUE,
        "Nous recherchons des produits THEIA disponibles sur theia-land"
      )
    }
  }
    } # else {
    #   print_message(
    #     type = "message",
    #     date = TRUE,
    #     i18n$t("Searching for available PEPS products on peps")
    #   )
    # }
    
    # if online mode, retrieve list with cnes_list() basing on parameters
    if (pm$product == "theia") {
      api <- pm$apitheia
    } # else {
    #   api <- pm$apipeps
    
    if ("sentinel2" %in% pm$theiacollection) { # si S2 est present dans pm$theiacollection
      # list of THEIA needed for required Sentinel2 level
      cnes_lists <- cnes_list(
        spatial_extent = pm$extent,
        time_interval = pm$timewindow,
        time_period = pm$timeperiod,
        level = pm$theiaplatformsentinellevel,
        platform = pm$theiaplatformsentinel,
        maxcloud = pm$max_mask,
        collection = pm$theiacollection,
        api = api
      )
    } else {
    # if offline mode, read the PEPS or THEIA product list from folders and filter
    cnes_lists <- list.dirs(path = paths["data"])
  } # end of if
  
  cnes_list <-
    stringr::str_extract(
      grep("SENTINEL2A", cnes_lists, value = TRUE), # recherche l'expression dans cnes_lists
      def_regex$tile$regex # ?
    )
  rm(cnes_lists)
  
  # compute names for required files
  print_message(type = "message", date = TRUE, "Compilation des noms de sortie")
  
  cnes2names <- compute_cnes2_paths( # TO REPLACE
    pm = pm,
    cnes_list = cnes_list,
    force_tiles = TRUE
  )
  
  # if cnes_list is empty, exit
  if (length(cnes_list) == 0) {
    if (pm$product == "theia") {
      print_message(
        type = "message",
        date = TRUE,
        "Aucun produit THEIA ne correspond aux parametres selectionnes ",
        "(les parametres sont peut-etre trop restrictifs, ",
        "ou l'acces a theia-land est peut-etre indisponible."
      )
    } # else {
    #   print_message(
    #     type = "message",
    #     date = TRUE,
    #     i18n$t("No PEPS products found with the parameters set "),
    #     i18n$t("(the searching parameters may be too restrictive, "),
    #     i18n$t("or the Peps Access could be unavailable).")
    #   )
    # }
    break # a quoi ca correspond ?
  }
  
  # If cnes_list is empty, exit
  if (length(cnes_list) == 0) {
    if (pm$product == "theia") {
      print_message(
        type = "message",
        date = TRUE,
        if (pm$online == FALSE) {
          paste0(
            "Aucun produit THEIA correspondant a ces parametres n'a ete trouve sur le disque local;\n "
            ,
            "veuillez les telecharger ou choisir une plage spatiale/temporelle differente.\n"
            ,
            "Execution stoppee."
          )
        } else {
          paste0("Aucun produit THEIA ne correspondant aux criteres n'est trouve (1).")
        }
      )
    } # else {
    #   print_message(
    #     type = "message",
    #     date = TRUE,
    #     if (pm$online == FALSE) {
    #       paste0(
    #         "No PEPS products which match the settings were found locally;\n ",
    #         "please download them or set different spatial/temporal extents.\n",
    #         "Execution halted."
    #       )
    #     } else {
    #       paste0("No PEPS products matching the settings were found (1).")
    #     }
    #   )
    # }
    break
  } else {
    if (pm$product == "theia") {
      print_message(
        type = "message",
        date = TRUE,
        paste0(
          length(cnes_list),
          "\u2000Les produits THEIA sont trouvables sur theia-land."
        )
      )
    } # else {
    #   print_message(
    #     type = "message",
    #     date = TRUE,
    #     paste0(
    #       length(cnes_list),
    #       i18n$t("\u2000PEPS products are found on PEPS Hub.")
    #     )
    #   )
    # }
  }
  
  # Check if processing is needed
  if (all(sapply(cnes2names[c(
    "indices_names_new", "rgb_names_new", "out_names_new", "masked_names_new",
    "warped_names_new", "merged_names_new", "tiles_names_new", "mosaic_names_new"
  )], length) == 0)) {
    print_message(
      type = "message",
      date = TRUE,
      "Tous les fichiers de sortie necessaires existent deja; rien a faire.\n ",
      "Pour proceder a nouveau, lancez theia2r() avec l'argument \"overwrite\" = TRUE\n ",
      "ou specifiez une direction de sortie differente."
    )
    return(invisible(NULL)) # pourquoi cette fin de commande ?
  }
  
  
  
  ##### 4. Download required THEIA #####
  # TODO implement ovwerite/skip
  # (now it skips, but analysing each single file)
  
  if (pm$online == TRUE) {
    print_message(
      type = "message",
      date = TRUE,
        "Le telechargement des niveaux requis des produits THEIA Sentinel2 a commence."
    )
    
    cnes_lists_downloaded[["sentinel2"]] <- product_download(
      spatial_extent = pm$extent,
      time_interval = pm$timewindow,
      time_period = pm$timeperiod,
      level = pm$theiaplatformsentinellevel,
      platform = pm$theiaplatformsentinel,
      maxcloud = pm$max_mask,
      collection = pm$theiacollection,
      writedir = pm$path_data,
      api = api
    )
    
    cnes_list_downloaded <-
      stringr::str_extract(
        grep("already exists", cnes_lists_downloaded[[1]], value = TRUE), # est-ce que ca se change (grep recherche le terme)
        def_regex$tile$regex
      )
    rm(cnes_lists_downloaded)
    
    # If cnes_list is empty, exit
    if (length(cnes_list_downloaded) == 0) {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list),
            i18n$t("\u2000Les produits THEIA ont ete telecharges sur theia-land.")
          )
        )
      # } else {
      #   print_message(
      #     type = "message",
      #     date = TRUE,
      #     paste0(
      #       length(cnes_list),
      #       i18n$t("\u2000PEPS products have been downloaded on PEPS Hub.")
      #     )
      #   )
      # }
    } else {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list_downloaded),
            "\u2000Les produits THEIA existent deja et n'ont pas ete telecharges."
          )
        )
      # } else {
      #   print_message(
      #     type = "message",
      #     date = TRUE,
      #     paste0(
      #       length(cnes_list_downloaded),
      #       i18n$t("\u2000PEPS products already exist and have not been downloaded.")
      #     )
      #   )
      # }
    }
    
    print_message(
      type = "message",
      date = TRUE,
      "Le telechargement des produits Sentinel2 est termine."
    )
    
    # unzip files
    zipfile <- c(paste0(cnes_list, ".zip"))
    for (i in 1:length(zipfile)) {
      tryCatch(if (!dir.exists(paste0(pm$path_data, "/", stringr::str_sub(file_path_sans_ext(zipfile[i]), end = -3)))) { # gere des conditions comme les warnings et les errors
        # list all files in zipfile
        li <- utils::unzip(
          zipfile = paste0(pm$path_data, "/", zipfile[i]),
          list = TRUE
        )
        # unzip just FRE_ and ALL.jpg files
        utils::unzip(
          zipfile = paste0(pm$path_data, "/", zipfile[i]),
          exdir = paste0(pm$path_data, "/", stringr::str_sub(file_path_sans_ext(zipfile[i]), end = -3)),
          files = li$Name[grep('FRE_|ALL\\.jpg', li$Name)],
          junkpaths = TRUE,
          overwrite = FALSE
        )
        print_message(
          type = "message",
          date = TRUE,
          paste("Le fichier zip", zipfile[i], "a ete decompresse avec succes.")
        )
      },
      error = function(e) {
        print_message(
          type = "message",
          date = TRUE,
          paste("Le fichier zip", zipfile[i], "n'a pas pu etre decompresse.")
        )
      },
      warning = function(w) {
        print_message(
          type = "message",
          date = TRUE,
          paste("Le fichier zip", zipfile[i], "n'a pas pu etre decompresse.")
        )
      }) # end of tryCatch
    } # end of for
    
    # copy cnes2names$tiles_names_exp in project/tiles
    # find the files that i want
    oldf <- list.files(
      file.path(
        paste0(pm$path_data, "/", stringr::str_sub(cnes_list, end = -3))
      ),
      "\\.tif",
      full.names = T
    )
    if (length(oldf) > 0) {
      print_message(
        type = "message",
        date = TRUE,
        paste(length(oldf), "Les fichiers sont copies vers le repertoire, cela peut prendre du temps ...")
      )
      newf <- file.path(
        pm$path_tiles,
        basename(gsub("_[A-Z]_V[0-9].[0-9]_", "_TILES_", oldf)) # pas bien compris ?
      )
      file.copy(
        from = oldf,
        to = newf,
        overwrite = FALSE,
        recursive = FALSE,
        copy.mode = TRUE
      )
    } # end of if
  } # end of if of online
  
  ### GDAL processing: convert THEIA, merge tiles, warp, mask and compute indices ###
  
  ##### 5. Mosaic by orbit #####
  cnes_lists_mosaic <- list()
  if (sum(!file.exists(nn(cnes2names$mosaic_names_new))) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      "Debut de la compilation des tuiles par date et par orbite."
    )
    
    dir.create(paths["mosaic"], recursive = FALSE, showWarnings = FALSE)
    
    cnes_lists_mosaic[["cnes"]] <- s2_mosaic(
      infiles = cnes2names$mosaic_names_new,
      cnes_list = cnes_list,
      tilesdir = paths["tiles"],
      outdir = paths["mosaic"],
      parallel = TRUE
    )
  }
  
  ##### 6. Translate by orbit #####
  cnes_lists_translate <- list()
  
  if (sum(!file.exists(nn(cnes2names$translate_names_new))) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      "Debut de la translation ..."
    )
    
    dir.create(paths["translate"], recursive = FALSE, showWarnings = FALSE)
    
    cnes_lists_translate[["cnes"]] <- s2_translate(
      infiles = cnes2names$translate_names_new,
      tilesdir = paths["tiles"],
      mosaicdir = paths["mosaic"],
      outdir = paths["translate"]
    )
  }
  
  ##### 7. Warpe #####
  cnes_lists_warped <- list()
  if (sum(!file.exists(nn(cnes2names$warped_names_new))) > 0) {
    ## Rescale, reproject ##
    if (pm$clip_on_extent == TRUE) {
      print_message(
        type = "message",
        date = TRUE,
        "Debut de l'edition de la geometrie (reprojeter, redimensionner)."
      )
      
      dir.create(paths["warped"], recursive = FALSE, showWarnings = FALSE)
      
      cnes_lists_warped[["cnes"]] <- s2_warped(
        infiles = cnes2names$warped_names_new,
        tilesdir = paths["tiles"],
        translatedir = paths["translate"],
        outdir = paths["warped"]
      )
    }
  }
  
  
  ##### 8. Merge #####
  cnes_lists_merged <- list()
  if (sum(!file.exists(nn(cnes2names$merged_names_new))) > 0) {
    ## Merge ##
    print_message(
      type = "message",
      date = TRUE,
      "Debut de l'edition de la geometrie (fusion de tuiles)."
    )
    
    dir.create(paths["merged"], recursive = FALSE, showWarnings = FALSE)
    
    cnes_lists_merged[["cnes"]] <- s2_merge(
      infiles = cnes2names$merged_names_new,
      extent = pm$extent,
      tilesdir = paths["tiles"],
      warpedir = paths["warped"],
      outdir = paths["merged"]
    )
  }
  
  # ##### 9. Apply mask #####
  # #   # FIXME understand if this should be done before warping (if so, how to manage virtual/physical files?)
  # #   # masked_names <- file.path(paths["out"],
  # #   #                           if(pm$path_subdirs==TRUE){basename(dirname(warped_names[!names_merged_exp_scl_idx]))}else{""},
  # #   #                           gsub(paste0(warped_ext,"$"),out_ext,basename(warped_names[!names_merged_exp_scl_idx])))
  # #
  # #   if (!is.na(pm$mask_type) & length(s2names$masked_names_new) > 0) {
  # #     print_message(
  # #       type = "message",
  # #       date = TRUE,
  # #       "Starting to apply cloud masks."
  # #     )
  # #
  # #     # index which is TRUE for SCL products, FALSE for others
  # #     names_warped_exp_scl_idx <- theia2r_getElements(s2names$warped_names_exp, format = "data.frame")$prod_type == "SCL"
  # #     names_warped_req_scl_idx <- theia2r_getElements(s2names$warped_names_req, format = "data.frame")$prod_type == "SCL"
  # #     # index which is TRUE for products to be atm. masked, FALSE for others
  # #     names_warped_tomask_idx <- if ("SCL" %in% pm$list_prods) {
  # #       names_warped_req_scl_idx > -1
  # #     } else {
  # #       !names_warped_req_scl_idx
  # #     }
  # #
  # #     # if SR outformat is different (because BOA was not required,
  # #     # but some indices are) launch s2_mask separately
  # #     masked_names_infiles <- if (pm$clip_on_extent == TRUE) {
  # #       s2names$warped_names_req[names_warped_tomask_idx & file.exists(s2names$warped_names_req)]
  # #     } else {
  # #       s2names$merged_names_req[names_merged_tomask_idx & file.exists(s2names$merged_names_req)]
  # #     }
  # #     masked_names_infiles_sr_idx <- any(!is.na(pm$list_indices)) &
  # #       !pm$index_source %in% pm$list_prods &
  # #       sapply(masked_names_infiles, function(x) {
  # #         theia2r_getElements(x)$prod_type == pm$index_source
  # #       })
  # #
  # #     masked_names_out_nsr <- if (length(masked_names_infiles[!masked_names_infiles_sr_idx]) > 0) {
  # #       trace_function(
  # #         s2_mask,
  # #         infiles = masked_names_infiles[!masked_names_infiles_sr_idx],
  # #         maskfiles = if (pm$clip_on_extent == TRUE) {
  # #           s2names$warped_names_exp[names_warped_exp_scl_idx]
  # #         } else {
  # #           s2names$merged_names_exp[names_merged_exp_scl_idx]
  # #         },
  # #         smooth = pm$mask_smooth,
  # #         buffer = pm$mask_buffer,
  # #         mask_type = pm$mask_type,
  # #         max_mask = pm$max_mask,
  # #         outdir = paths["out"],
  # #         tmpdir = file.path(tmpdir, "s2_mask"), rmtmp = rmtmp,
  # #         format = out_outformat,
  # #         compress = pm$compression,
  # #         subdirs = pm$path_subdirs,
  # #         overwrite = pm$overwrite,
  # #         parallel = pm$parallel,
  # #         .log_message = .log_message, .log_output = .log_output,
  # #         trace_files = s2names$out_names_new
  # #       )
  # #     } else {
  # #       character(0)
  # #     }
  # #     masked_names_out_sr <- if (length(masked_names_infiles[masked_names_infiles_sr_idx]) > 0) {
  # #       trace_function(
  # #         s2_mask,
  # #         infiles = masked_names_infiles[masked_names_infiles_sr_idx],
  # #         maskfiles = if (pm$clip_on_extent == TRUE) {
  # #           s2names$warped_names_exp[names_warped_exp_scl_idx]
  # #         } else {
  # #           s2names$merged_names_exp[names_merged_exp_scl_idx]
  # #         },
  # #         mask_type = pm$mask_type,
  # #         smooth = pm$mask_smooth,
  # #         buffer = pm$mask_buffer,
  # #         max_mask = pm$max_mask,
  # #         outdir = paths["out"],
  # #         tmpdir = file.path(tmpdir, "s2_mask"), rmtmp = rmtmp,
  # #         format = sr_masked_outformat,
  # #         compress = pm$compression,
  # #         subdirs = pm$path_subdirs,
  # #         overwrite = pm$overwrite,
  # #         parallel = pm$parallel,
  # #         .log_message = .log_message, .log_output = .log_output,
  # #         trace_files = s2names$out_names_new
  # #       )
  # #     } else {
  # #       character(0)
  # #     }
  # #     masked_names_out <- c(masked_names_out_nsr, masked_names_out_sr)
  # #     masked_names_notcreated <- c(
  # #       attr(masked_names_out_nsr, "toomasked"),
  # #       attr(masked_names_out_sr, "toomasked")
  # #     )
  # #   }
  # # } # end of gdal_warp and s2_mask IF cycle
  # #
  # #
  ##### 10. Create RGB products #####
  cnes_lists_rgb <- list()
  if (sum(!file.exists(nn(cnes2names$rgb_names_new))) > 0) {
    ## RGB ##
    print_message(
      type = "message",
      date = TRUE,
      "Debut de la compilation RGB."
    )
    
    dir.create(paths["rgb"], recursive = FALSE, showWarnings = FALSE)
    
    cnes_lists_rgb[["cnes"]] <- s2_rgb(
      infiles = cnes2names$rgb_names_new,
      project = pm$project_name,
      rgblist = pm$rgb_out,
      dates = pm$timewindow,
      extent = pm$extent,
      tilesdir = paths["tiles"],
      mosaicdir = paths["mosaic"],
      outdir = paths["rgb"],
      resolution = 10
    )
  }
  
  ##### 11. Compute spectral indices #####
  cnes_lists_indices <- list()
  if (sum(!file.exists(unlist(nn(cnes2names$indices_names_list)))) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      "Calcul des indices spectraux requis."
    )
    
    dir.create(paths["indices"], recursive = FALSE, showWarnings = FALSE)
    
    cnes_lists_indices[["cnes"]] <- s2_calcindices(
      infiles = cnes2names$indices_names_list,
      extent = pm$extent,
      indices = pm$list_indices,
      project = pm$project_name,
      dates = pm$timewindow,
      outdir = paths["indices"],
      subdirs = pm$path_subdirs,
      source = pm$index_source,
      format = pm$outformat,
      dataType = pm$index_datatype,
      compress = pm$compression,
      overwrite = pm$overwrite,
      parallel = TRUE,
    )
  }
  
  # ##### 12. create thumbnails #####
  # 
  # if (thumbnails == TRUE) {
  #   thumb_names_req <- names_out_created
  # 
  #   if (length(thumb_names_req) > 0) {
  #     print_message(
  #       type = "message",
  #       date = TRUE,
  #       "Generating thumbnails."
  #     )
  # 
  #     # define expected output names
  #     thumb_names_new <- file.path(
  #       dirname(thumb_names_req),
  #       "thumbnails",
  #       sapply(
  #         basename(thumb_names_req),
  #         function(x) {
  #           gsub(
  #             "\\..+$",
  #             if (theia2r_getElements(x)$prod_type %in% c("SCL")) {
  #               ".png"
  #             } else {
  #               ".jpg"
  #             },
  #             x
  #           )
  #         }
  #       )
  #     )
  # 
  #     thumb_names_out <- trace_function(
  #       s2_thumbnails,
  #       infiles = thumb_names_req,
  #       tmpdir = file.path(tmpdir, "s2_thumbnails"), rmtmp = rmtmp,
  #       trace_files = c(thumb_names_new, paste0(thumb_names_new, ".aux.xml"))
  #     )
  #   }
  # } # end of thumbnails IF cycle
  
  
  # ##### 13. remove temporary files #####
  # # if (rmtmp == TRUE) {
  # #   unlink(tmpdir, recursive = TRUE)
  # # }
  # #
  # # # check if some files were not created
  # # names_cloudcovered <- nn(c(
  # #   if (exists("masked_names_notcreated")) {
  # #     masked_names_notcreated
  # #   },
  # #   if (exists("indices_names_notcreated")) {
  # #     indices_names_notcreated
  # #   }
  # # ))
  # # names_missing <- names_out[!file.exists(nn(names_out))]
  # # names_missing <- names_missing[!names_missing %in% names_cloudcovered]
  # # names_cloudcovered <- names_cloudcovered[!grepl(tmpdir, names_cloudcovered, fixed = TRUE)]
  # #
  # # # Add attributes related to files not created
  # # attr(names_out_created, "cloudcovered") <- names_cloudcovered
  # # attr(names_out_created, "missing") <- names_missing
  # #
  # # # Note down the list of non created files (#ignorePath)
  # # # Sometimes not all the output files are correctly created:
  # # # the main reason is the cloud coverage higher than the maximum allowed
  # # # value (argument "max_mask"), but also some other unexpected reasons could
  # # # happen, i.e. because of old name SAFE products which do not include all the tiles.
  # # # To prevent to try to create these files every time the function is called
  # # # with the same parameter file, if param_list is a path, this list is noted
  # # # in two hidden files (one per file not created because of cloud coverage,
  # # # one other for all the other reasons) so to ignore them during next executions.
  # # # To try it again, delete the files or set overwrite = TRUE).
  # # if (length(names_missing) > 0) {
  # #   ignorelist_path <- gsub("\\.json$", "_ignorelist.txt", param_list)
  # #   if (is(param_list, "character")) {
  # #     write(names_missing, ignorelist_path, append = TRUE)
  # #   }
  # #   print_message(
  # #     type = "warning",
  # #     "Some files were not created:\n\"",
  # #     paste(names_missing, collapse = "\"\n\""), "\"",
  # #     if (is(param_list, "character")) {
  # #       paste0(
  # #         "\"\nThese files will be skipped during next executions ",
  # #         "from the current parameter file (\"", param_list, "\").\n",
  # #         "To try again to build them, remove the file \"",
  # #         ignorelist_path, "\"."
  # #       )
  # #     }
  # #   )
  # # }
  # # if (length(names_cloudcovered) > 0) {
  # #   cloudlist_path <- gsub("\\.json$", "_cloudlist.txt", param_list)
  # #   if (is(param_list, "character")) {
  # #     write(names_cloudcovered, cloudlist_path, append = TRUE)
  # #   }
  # #   print_message(
  # #     type = "message",
  # #     "Some files were not created ",
  # #     "because the cloud coverage was higher than \"max_mask\":\n\"",
  # #     paste(names_cloudcovered, collapse = "\"\n\""), "\"",
  # #     if (is(param_list, "character")) {
  # #       paste0(
  # #         "\"\nThe list of these files was written in a hidden file ",
  # #         "(\"", cloudlist_path, "\"), ",
  # #         "so to be skipped during next executions."
  # #       )
  # #     }
  # #   )
  # # }
  # 
  # 
  # 
  
} # end of dummy cycle
#### Exit ####
print_message(
  type = "message",
  date = TRUE,
  "Fin de la session de travail."
)

}