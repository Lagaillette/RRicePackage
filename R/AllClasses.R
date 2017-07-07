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
#' @docType class
#' @exportClass Experiment
setClass(
  ##name of the class
  "Experiment",

  ##attributes of the class
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
#' into a global class "Gene" from which the others specific genes classes will
#' inherit.
#'
#' @slot id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @name Gene
#' @rdname Gene-class
#' @exportClass Gene
setClass(
  ##name of the class
  "Gene",

  ##attributes of the class
  slots = list(id = "character",
               locus = "data.frame",
               others = "list")
)


setClass(
  ##name of the class
  "GeneDBtest",

  ##attributes of the classnumeric
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

#' The Gene specific to the first database class.
#'
#' This gene is specific to the database 1 that is the database "RAPDB". So it
#' contains all the informations we can have about one gene with the "RAPDB"
#' database.
#'
#' @slot id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot rapDBGeneNameSynonym an identificator
#' @slot rapDBGeneSymbolSynonym an identificator
#' @slot cgsnlGeneName an identificator
#' @slot cgsnlGeneSymbol an identificator
#' @slot oryzabaseGeneNameSynonym an identificator
#' @slot oryzabaseGeneSymbolSynonym an identificator
#' @slot description a description of the gene
#' @name GeneDB1
#' @rdname GeneDB1-class
#' @exportClass GeneDB1
setClass(
    ##name of the class
    "GeneDB1",

    ##attributes of the classnumeric
    slots = list(rapDBGeneNameSynonym = "character",
                 rapDBGeneSymbolSynonym = "character",
                 cgsnlGeneName = "character",
                 cgsnlGeneSymbol = "character",
                 oryzabaseGeneNameSynonym = "character",
                 oryzabaseGeneSymbolSynonym = "character",
                 position = "data.frame",
                 description = "character"),

    contains = "Gene"
)


#' The Gene  class specific to the third database.
#'
#' This gene is specific to the database 3 that is the database "Oryzabase".
#' So it contains all the informations we can have about one gene with the
#' "Oryzabase" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because there
#' are the same in spite of the different attributes they have.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot traitGeneId id
#' @slot cgsnlGeneSymbol id
#' @slot GeneSymbolSynonim id
#' @slot cgsnlSymbolSynonim id
#' @slot GeneNameSynonim id
#' @slot proteinName id
#' @slot allele id
#' @slot chromosomeNumber id
#' @slot explanation id
#' @slot traitClass id
#' @slot rapID id
#' @slot grameneId id
#' @slot arm id
#' @slot locate id
#' @slot geneOntology id
#' @slot traitOntology id
#' @slot plantOntology id
#' @name GeneDB3
#' @rdname GeneDB3-class
#' @exportClass GeneDB3
setClass(
    ##name of the class
    "GeneDB3",

    ##attributes of the classnumeric
    slots = list(traitGeneId = "character",
                 cgsnlGeneSymbol = "character",
                 GeneSymbolSynonim = "character",
                 cgsnlSymbolSynonim = "character",
                 GeneNameSynonim = "character",
                 proteinName = "character",
                 allele = "character",
                 chromosomeNumber = "numeric",
                 explanation = "character",
                 traitClass = "character",
                 rapID = "character",
                 grameneId = "character",
                 arm = "character",
                 locate = "character",
                 geneOntology = "character",
                 traitOntology = "character",
                 plantOntology = "character"
    ),

    contains = "Gene"
)
