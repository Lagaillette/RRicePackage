#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Gene",

  #attributes of the class
  slots = list(uniquename = "factor", msU7name = "factor", fgeneshName = "factor", rappredname = "factor", fmin = "numeric", fmax = "numeric",contig = "factor",iricname = "factor", strand = "factor", description = "character")
)


#####################################################################
########################### Constructor #############################


Gene <- function(uniquename, msU7name, fgeneshName, rappredname, fmin, fmax,contig,iricname, strand, description){
  result <- new("Gene", uniquename, msU7name, fgeneshName, rappredname, fmin, fmax,contig,iricname, strand, description)
  return(result)
}


#####################################################################
############################ Methods ################################
