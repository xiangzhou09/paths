## declare as Global Variables a number of variables that
## are used in plot.paths()

## These variables are called inside aes() when calling
## ggplot() using non-standard evaluation. CRAN CHECK
## sees these variables as undefined global variables
## even though they are legal variables belonging to
## a data object.

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("est", "decomposition", "lower", "upper"))
}
