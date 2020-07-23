# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "s2product" ) # Name of the module
golem::add_module( name = "dendro" )
golem::add_module( name = "donneessup") 
golem::add_module( name = "outputoptions")
golem::add_module( name = "outputfiles")
golem::add_module( name = "pop")
golem::add_module( name = "cumul")
golem::add_module( name = "incertitude")
golem::add_module( name = "chargement_tuiles")
golem::add_module( name = "login_theia")
golem::add_module( name = "download")
golem::add_module( name = "print_message")
golem::add_module( name = "theia_download_python_module")
golem::add_module( name = "check_chemins")
golem::add_module( name = "check_liste_param")
golem::add_module( name = "check_gdal")
golem::add_module( name = "init")
golem::add_module( name = "map_update")
golem::add_module( name = "overlap")
golem::add_module( name = "map_creation")
golem::add_module( name = "forest_mode")
golem::add_module( name = "bbox_mode")
golem::add_module( name = "vectorfile_mode")
golem::add_module( name = "draw_mode")
golem::add_module( name = "presabs")
golem::add_module( name = "mask_module")
golem::add_module( name = "prevision_map")
golem::add_module( name = "helper")
golem::add_module( name = "paths_update")
golem::add_module( name = "theia_credentials")
golem::add_module( name = "all_correctly_set")
golem::add_module( name = "object_to_return")
golem::add_module( name = "param_list")
golem::add_module( name = "modal_dialog")

## 2.2 Add dependencies

usethis::use_package( "thinkr" ) # To call each time you need a new package

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("shinyLiDAR")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
