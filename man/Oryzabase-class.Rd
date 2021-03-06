% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{Oryzabase-class}
\alias{Oryzabase-class}
\title{An S4 class to represent the third database Gene.}
\description{
This gene is specific to the database 3 that is the database "Oryzabase".
So it contains all the informations we can have about one gene with the
"Oryzabase" database.
}
\section{Slots}{

\describe{
\item{\code{id}}{The id of the gene. Many genes can have the same id because 
there are the same in spite of the different attributes they have.}

\item{\code{genesIDs}}{the list of diferent ids it have we will use with the 
databases.}

\item{\code{locus}}{The locus the gene belongs.}

\item{\code{others}}{The others attributes the user want to add or delete.}

\item{\code{traitGeneId}}{id}

\item{\code{cgsnlGeneSymbol}}{id}

\item{\code{GeneSymbolSynonim}}{id}

\item{\code{cgsnlSymbolSynonim}}{id}

\item{\code{GeneNameSynonim}}{id}

\item{\code{proteinName}}{id}

\item{\code{allele}}{id}

\item{\code{chromosomeNumber}}{id}

\item{\code{explanation}}{id}

\item{\code{traitClass}}{id}

\item{\code{rapID}}{id}

\item{\code{grameneId}}{id}

\item{\code{arm}}{id}

\item{\code{locate}}{id}

\item{\code{geneOntology}}{id}

\item{\code{traitOntology}}{id}

\item{\code{plantOntology}}{id}
}}

\examples{
gene <- new("Oryzabase")
}
