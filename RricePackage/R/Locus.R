#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Locus",

  #attributes of the class
  representation = representation(start = "numeric", end = "numeric", genes = "vector", chromosome = "numeric"),
  
  contains = c("character")
)


#####################################################################
########################### Constructor #############################


Locus <- function(start, end, genes, chromosome){
  result <- new("Locus", start, end, genes, chromosome)
  return(result)
}


#####################################################################
############################# Methods ###############################


