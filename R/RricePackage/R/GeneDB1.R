#####################################################################
############################## Class ################################

setClass(
  #name of the class
  "GeneDB1",
  
  #attributes of the classnumeric
  slots = list(msU7name = "character", fgeneshName = "character", rappredname = "character", fmin = "numeric", fmax = "numeric",contig = "character",iricname = "character", strand = "numeric", description = "character"),
  
  contains = "Gene"
)


#####################################################################
########################### Constructor #############################


GeneDB1 <- function(uniquename, locus, msU7name, fgeneshName, rappredname, fmin, fmax,contig,iricname, strand, description){
  result <- new("GeneDB1",uniquename = uniquename, locus = locus, msU7name = msU7name, fgeneshName = fgeneshName, rappredname = rappredname, fmin = fmin, fmax = fmax,contig = contig,iricname = iricname, strand = strand, description = description)
  return(result)
}


#####################################################################
############################ Methods ################################