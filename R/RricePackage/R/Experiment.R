#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Experiment",

  #attributes of the class
  representation = representation(name = "character", date = "character", locus = "vector")
)


#####################################################################
########################### Constructor #############################


Experiment <- function(name, date, locus){
  result <- new("Experiment", name, date, locus)
  return(result)
}


#####################################################################
############################ Methods ################################
