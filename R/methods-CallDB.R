library(jsonlite)

command ="python3"

#pour appeler le fichier SCriptv7_Table.py -> BESOIN de l'attribut "RAPID"

#' This function calls Scriptv7 from python
#'
#' Cette fonction permet à l'utilisateur de chercher si le RAPID existe dans 
#' Oryzabase.txt
#' @param RAPID
#' @keywords 
#' @export
#' @examples
#' Appel_Scriptv7("Os06g0654600")
Appel_Scriptv7 <- function (RAPID) {
  
    #chemin pour n'importe quel utilisateur
    debut = getwd()
    path = "/RRicePackage/inst/Python/rricebeta/rricebeta/Scriptv7_Table.py"
    path2script = paste(c(debut,path), collapse = '')

    # Build up args in a vector
    # RAPID_valide = "Os06g0654600" -> exemple valide
    args = c(RAPID)

    # Add path to script as first arg
    allArgs = c(path2script, args)

    Routput = system2(command, args=allArgs, stdout=TRUE)

    #Appelé à chaque fois qu'elle rencontrera un print
    print(Routput)
}

#Ma fonction CallDB1 

#' This function calls run.py
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB1. All these locus will be stocked in liste_genes
#' @param Locus list
#' @keywords 
#' @export
#' @examples
#' callDB1(Liste de locus)
CallDB1 <- function (locusListe) {
  
    debut = getwd()
    path = "/RRicePackage/inst/Python/rricebeta/rricebeta/run.py"
    path2script = paste(c(debut,path), collapse = '')
  
    for (i in 1 : nrow(locusListe)) {
        ch = as.character(locusListe[i,1])
        start = as.character(locusListe[i,2])
        end = as.character(locusListe[i,3])
    
        #print(paste(class(ch)," ",class(start)," ",class(end)))
    
        #appel du script python run.py avec les attributs (chx, start, end, DB) 
        #-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "1")
        allArgs = c(path2script, args)
        Routput = system2(command, args=allArgs, stdout=TRUE)
        #print(Routput)
        test <- fromJSON(Routput)
    }
  
    for (i in 1:length(test)){
        print(paste("numero",i," -> ",test[i][1]))
    }
  
    id_rec = test["ID"]
    position = test["Position"]
    rap_symbole = test["RAP-DB Gene Symbol Synonym(s)"]
    cgsnl_name = test["CGSNL Gene Name"]
    ory_gene_symbole = test["Oryzabase Gene Symbol Synonym(s)"]
    description = test["Description"]
    rap_name = test["RAP-DB Gene Name Synonym(s)"]
    ory_gene_name = test["Oryzabase Gene Name Synonym(s)"]
    cgsnl_gene = test["CGSNL Gene Symbol"]
    
    #prochaine étape -> créer objet et mettre tous les attributs dedans
    #créé un nouvel objet gene
    newgene <- GeneDB1(id = id_rec,
                       locus = position,
                       rapDBGeneNameSynonym = rap_name,
                       rapDBGeneSymbolSynonym = rap_symbole,
                       cgsnlGeneName = cgsnl_name,
                       cgsnlGeneSymbol = cgsnl_gene,
                       oryzabaseGeneNameSynonym = ory_gene_name,
                       oryzabaseGeneSymbolSynonym = ory_gene_symbole,
                       description = description)
    #liste d'objets genes
    liste_genes <- data.frame()
    liste_genes <- append(newgene)
}

