#the class for the locus
setClass(
  #name of the class
  "Locus",

  #attributes of the class
  representation = representation(start = "numeric", end = "numeric", genes = "vector", chromosome = "numeric")

)


#####################################################################
############################ Methods ################################

