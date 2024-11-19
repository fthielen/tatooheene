# Load pacman and install if not installed. Pacman allows for mutliple packages to be easily installed and downloaded
if (require(pacman) == FALSE) {install.packages("pacman")}
if (require(DT) == FALSE) {install.packages("DT")}
if (require(cbsodataR) == FALSE){install.package("cbsodataR")}
if (require(lubridate) == FALSE){install.packages("lubridate")}
if(require(zoo) == FALSE) {install.packages("zoo")}

# Load and/or install all needed packages

pacman::p_load(
  here, # for easier path references
  kableExtra, # for nice HTML tables
  openxlsx, # to read and write Excel data
  downloadthis, # for download buttons
  tidyverse,
  DT,
  cbsodataR,
  lubridate,
  zoo)
