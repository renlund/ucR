# install from tar.gz, example:
install.packages(pkgs="P:/Programming/package/ucR_0.1.tar.gz", repos=NULL, type="source")

# some devtools commands:
devtools::document("ucR")  # create documentation
devtools::install_local("ucR") # install from directory
devtools::build("ucR") # build a tar.gz
help(package="ucR") # view documentation


