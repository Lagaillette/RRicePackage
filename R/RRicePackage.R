#the class for the properties
setClass(
  #name of the class
  "prototype",
  
  #attributes of the class
  slots=list(description="character")
)

#the class for the genes
setClass(
  #name of the class
  "gene",
  
  #attributes of the class
  slots=list(start="numeric", end="numeric", name="character", strand="factor", properties="vector")
)

#the class for the chromosomes
setClass(
  #name of the class
  "chromosome",
  
  #attributes of the class
  slots=list(name="character")
)

#the class for the locus
setClass(
  #name of the class
  "locus",
  
  #attributes of the class
  slots=list(start="numeric", end="numeric", genes="vector", chromosome="chromosome")
)

#the class for the experiments
setClass(
  #name of the class
  "experiment",
  
  #attributes of the class
  slots=list(name="character", date="character", locus="locus")
)
