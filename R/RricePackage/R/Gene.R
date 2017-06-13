#the class for the genes
setClass(
  #name of the class
  "Gene",

  #attributes of the class
  slots = list(uniquename = "factor", msU7name = "factor", fgeneshName = "factor", rappredname = "factor", fmin = "numeric", fmax = "numeric",contig = "factor",iricname = "factor", strand = "factor", description = "character")
)


#####################################################################
############################ Methods ################################
