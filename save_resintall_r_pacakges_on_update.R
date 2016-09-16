#### Save R packages for reinstall when upgrading R
#### Bill Petti
#### September 2016
#### code borrowed/adapted from https://www.datascienceriot.com/how-to-upgrade-r-without-losing-your-packages/kris/

# set workding directory where you want the list of packages exported to and where you will access an existing list for reinstall

setwd("/Users/williampetti/General-Code-Reference") # my directory, change to your particular directory

# create list of installed packages 

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

# load list and reinstall missing packages (this might take a while...)
# note that any package not installed via CRAN (e.g. GitHub) will not be resinstalled through this process

load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
system.time(install.packages(missing))
system.time(update.packages())

# check to see which packages are still missing

tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
still_missing <- setdiff(installedpkgs, installedpkgs.new)

# export still_missing in case you need to refer to it later for a manual reinstall

save(still_missing, file = paste0("still_missing_asof_", Sys.Date(), ".rda"))
