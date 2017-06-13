#the class for the experiments
setClass(
  #name of the class
  "Experiment",

  #attributes of the class
  representation = representation(name = "character", date = "character", locus = "vector")
)


#####################################################################
############################ Methods ################################
