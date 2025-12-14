# Header -----------------------------------------------------------------------
# File name: set_up.R
# Folder/Module name: ./
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-24
# Editor/Creator: Akson Sam Varghese
# Description: File to set up the project
# ------------------------------------------------------------------------------


# Functions --------------------------------------------------------------------
read_config_file <- function(path='./config.yaml'){
  #' @description
  #' A function to read config file of the application
  #' 
  #' @param path string, default value
  #' 
  #' @example 
  #'  # read_config_file()
  #'  # read_config_file(path='./file/path')
  #' 
  #' @return config_file
  
  config_file <- yaml.load_file(path)
  return (config_file)
}

custom_install_pkg <- function(pkg_list){
  #' @description
  #' A function to install list of packages
  #' 
  #' @param pkg_list, list of packages to install
  #' 
  #' @example 
  #'  # custom_install_pkg(c('yaml'))
  #' 
  #' @return None
  
  options(install.packages.compile.from.source = "always")
  # check first if the packages are installed if not then install them
  install.packages(
    setdiff(pkg_list, rownames(installed.packages())), type="both")
  
}


# Execution --------------------------------------------------------------------

# install yaml for the fist time
custom_install_pkg(c('yaml'))


# Read the dependency list from config file and install the dependencies
config <- read_config_file()
package_list <- config$dependencies$pkg_list
custom_install_pkg(package_list)
