#!!!! https://tex.stackexchange.com/questions/125274/error-font-ts1-zi4r-at-540-not-found !!!
library(tidyverse)

devtools::document()
devtools::build()

############

# pdf vignette bauen
pack <- "epidemos"
source <- paste0("C:/Users/Schliebs/OneDrive/github/packages/")

path <- paste0(source,pack) #getwd()#find.package(pack)#

file.remove(paste0(source,pack,"/",pack,".pdf"))
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

source("helpers/update_yaml.R")
update_yaml(pack, overwrite = T)

pkgdown::build_site()




