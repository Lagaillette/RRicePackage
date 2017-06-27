#' The Experiment class.
#'
#' Some details about this class and my plans for it in the body.
#'
#' @slot name The name of the experiment
#' @slot date The date of the experiment
#' @slot databases the databases which concern the object we are studying
#' @slot genes The list of genes we avec download.
#' @slot others The others attributes the user can add and delete
#' @name Experiment
#' @rdname Experiment-class
#' @docType classes
#' @exportClass Experiment
setClass(
  #name of the class
  "Experiment",
  
  #attributes of the class
  representation = representation(name = "character",
                                  date = "Date",
                                  databases = "list",
                                  genes = "list",
                                  others = "list")
)

#' The Gene class.
#'
#' A gene contains a lot of details that depend on the database it belongs. 
#' But every genes have informations in common. So we put all these informations
#' into a global class "gene" from which the others specific gene class will
#' inherit.
#' 
#' @slot id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot genes The list of genes we avec download.
#' @name Gene
#' @rdname Gene-class
#' @exportClass Gene
setClass(
  #name of the class
  "Gene",
  
  #attributes of the class
  slots = list(id = "character",
               uniquename = "character",
               locus = "data.frame",
               others = "list")
)


#' The Gene specific to the first database class.
#'
#' A gene contains a lot of details that depend on the database it belongs. 
#' But every gens have information in common. So we put all these informations
#' into a global class "gene" from which the others specific gene class 
#' inherit.
#'
#' @slot id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot genes The list of genes we avec download.
#' @slot msU7name an identicator
#' @slot fgeneshName an identicator
#' @slot rappredname an identicator
#' @slot fmin an identicator
#' @slot fmax an identicator
#' @slot contig an identicator
#' @slot iricname an identicator
#' @slot strand an identicator
#' @slot description a description
#' @name GeneDB1
#' @rdname GeneDB1-class
#' @exportClass Gene
setClass(
  #name of the class
  "GeneDB1",
  
  #attributes of the classnumeric
  slots = list(msU7name = "character",
               fgeneshName = "character",
               rappredname = "character",
               fmin = "numeric",
               fmax = "numeric",
               contig = "character",
               iricname = "character",
               strand = "numeric",
               description = "character"),
  
  contains = "Gene"
)