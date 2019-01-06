# install packages from GitHub

require(devtools)

devtools::install_github("BillPetti/baseballr")
devtools::install_github("BillPetti/ggallup")
devtools::install_github("aoles/shinyURL")
devtools::install_github('thomasp85/gganimate')
devtools::install_github('thomasp85/farver')
devtools::install_github("thomasp85/transformr")
devtools::install_github("thomasp85/patchwork")

# install from BioConductor

source("https://bioconductor.org/biocLite.R")
biocLite("BiocGenerics")
biocLite("EBImage")


# install local packages

install.packages("/Users/williampetti/myDBconnections", repos = NULL, type = "source")