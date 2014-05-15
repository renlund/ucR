#' @title Some instructions
#' @description Run \code{instr_ucR()} to get some very brief instructions on
#' how to contribute to the ucR package.
#' @export

instr_ucR <- function(){
   ucR_ascii <- "
      ___           ___           ___     
     /__/\\         /  /\\         /  /\\    
     \\  \\:\\       /  /:/        /  /::\\   
      \\  \\:\\     /  /:/        /  /:/\\:\\  
  ___  \\  \\:\\   /  /:/  ___   /  /:/~/:/  
 /__/\\  \\__\\:\\ /__/:/  /  /\\ /__/:/ /:/___
 \\  \\:\\ /  /:/ \\  \\:\\ /  /:/ \\  \\:\\/:::::/
  \\  \\:\\  /:/   \\  \\:\\  /:/   \\  \\::/~~~~ 
   \\  \\:\\/:/     \\  \\:\\/:/     \\  \\:\\     
    \\  \\::/       \\  \\::/       \\  \\:\\    
     \\__\\/         \\__\\/         \\__\\/        
"
   git_instr <- "
 To get the files for R package ucR
------------------------------------
Install git from 'http://git-scm.com/'.
Clone the repository (at 'P:/Programming/package/ucR/git') to 
some location on your computer, e.g. 'C:/R/ucR/' (a directory 
that should not exist - and need not be named 'ucR' for that 
matter).

After that you can add and improve the code. (Consider making 
useful commit messages.)

When you are content with the improvements:
 - consider changing the version number in 'DESCRIPTION'
 - push your changes back to the 'P:/' repository '
 - build the package and copy to 'P:/Programming/package/'
"
   package_instr <- "
 To work with the package
--------------------------
Examine the existing files for how to make 'roxygen comments' 
that can be used to make help files for functions. Add R 
functions to '/R', add '.rnw' vignettes to '/vignettes', 
arbitrary documentation to 'inst/doc' and test files to 
'inst/tests'. (For the latter, check out the 'testthat' package.)

Install devtools: 
> install.packages('devtools', dependencies=TRUE)
> library('devtools')

Set working directory to the parent directory of the package files.
E.g. if the package is at 'C:/R/ucR' then
> setwd('C:/R')
To install from these files directly
> install_local('ucR')
To build a 'tar.gz':
> build('ucR')

If you want to see if the documentation and vignettes are ok 
without installing
> document('ucR')
> build_vignettes('ucR')
The latter will make pdf:s of the rnw:s in '/vignettes' and 
put the pdf, source and a purl:ed file in 'inst/doc' that 
will be accessible through R help (once the package is installed 
and loaded)

To make an overall test if package is ok:
> check('ucR')
"
   
   cat(ucR_ascii)
   while(TRUE){
      cat("

  ---------------------------------
   Type
    - 'g' for git instructions
    - 'p' for package instructions
    - anything else to quit
  ---------------------------------
")
      input <- readline()
      if(input == 'p') 
         cat(package_instr)
      else if (input == 'g')
         cat(git_instr)
      else
         break
   }
}
