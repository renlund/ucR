devtools::document("ucR")  # create documentation
devtools::install_local("ucR") # install from directory
devtools::build("ucR") # build a tar.gz
help(package="ucR") # view documentation