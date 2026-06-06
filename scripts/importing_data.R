#########################
# Purpose: importing and merging individual datasets into one dataframe
# Last updated: 05.06.2026
# Author: Pia Alina Brakstad Smit
#########################

library(tools)
library(tidyverse)
library(collapse)

## Flow below following: https://stackoverflow.com/questions/51490757/loading-in-multiple-rda-files-into-a-list-in-r

loading_rda <- function(file){
  e <- new.env()
  load(file, envir = e)
  as.list(e) 
}

folder <- "Utforsking/raw_data"
files <- list.files(folder, pattern = ".rda$")

data_files <- Map(loading_rda, file.path(folder, files))
names(data_files) <- file_path_sans_ext(files)

#-----------------------------------------------------------#

full_df <- data_files |> 
  unlist2d() |> 
  select(-'.id.2') |>
  rename("dataID" = '.id.1') |> 
  mutate(dataID = recode(dataID,
                         "Master_fra Sofie" = "master_sofie",
                         "codeco" = "cod_eco",
                         "codwinter" = "cod_winter",
                         "haddockeco" = "haddock_eco",
                         "haddockwin" = "haddock_winter"))
  
  

save(full_df, file = "Utforsking/data/full_dataframe.rda")
