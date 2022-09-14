.onLoad <- function(libname, pkgname){
  packageStartupMessage("Dataset and scripts belonging to Schlicht, Santema, and Kempenaers 2022\n
'Timing of activity predicts extra-pair siring success independent\n
of age in male blue tits'\n
HOW TO USE:\r\n
To load the full dataset with the name 'x', type\n
> data(dataset)\r\n
A description of the dataset can be found at\n
> ?x\n
To open the code that runs the models and creates the figures, type \n
> file.edit(paste0(.libPaths(), '/ssam/models.r'))\n
To open the code that creates the supplementary figure (sensitivity analysis), type  \n
> file.edit(paste0(.libPaths(), '/ssam/Supplement_sensitivity analysis.R'))\n")
}
