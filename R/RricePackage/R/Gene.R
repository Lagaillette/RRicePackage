#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Gene",

  #attributes of the class
  slots = list(uniquename = "character", locus = "data.frame")
)


#####################################################################
########################### Constructor #############################


Gene <- function(uniquename, locus){
  result <- new("Gene", uniquename = uniquename, locus = locus)
  return(result)
}


#####################################################################
############################ Methods ################################
