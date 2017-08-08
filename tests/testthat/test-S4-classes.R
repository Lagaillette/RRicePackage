geneTest1 <- new("RAPDB",
                 id="test1", locus=data.frame(),
                 others=list(),
                 rapDBGeneNameSynonym="character",
                 rapDBGeneSymbolSynonym="character",
                 cgsnlGeneName="character",
                 cgsnlGeneSymbol="character",
                 oryzabaseGeneNameSynonym="character",
                 oryzabaseGeneSymbolSynonym="character",
                 position=data.frame(),
                 description="character")

geneTest2 <- new("RAPDB",
                 id="test2", locus=data.frame(),
                 others=list(),
                 rapDBGeneNameSynonym="character",
                 rapDBGeneSymbolSynonym="character",
                 cgsnlGeneName="character",
                 cgsnlGeneSymbol="character",
                 oryzabaseGeneNameSynonym="character",
                 oryzabaseGeneSymbolSynonym="character",
                 position=data.frame(),
                 description="character")

geneDB3Test <- new("Oryzabase",
                   id="test2", locus=data.frame(),
                   others=list(),
                   traitGeneId="character",
                   cgsnlGeneSymbol="character",
                   GeneSymbolSynonim="character",
                   cgsnlSymbolSynonim="character",
                   GeneNameSynonim="character",
                   proteinName="character",
                   allele="character",
                   chromosomeNumber=1,
                   explanation="character",
                   traitClass="character",
                   rapID="character",
                   grameneId="character",
                   arm="character",
                   locate="character",
                   geneOntology="character",
                   traitOntology="character",
                   plantOntology="character")

listTest <- list(geneTest1, geneTest2)
expTest <- new(Class="Experiment",
               name="test",
               date=Sys.Date(),
               genes=listTest,
               others=list())



test_that("Experiment with no error",{
    testthat::expect_s4_class(expTest, "Experiment")
})

test_that("Experiment with some bad consitions",{
    ##Date superior than the current date
    testthat::expect_error(new(Class="Experiment",
                               name="test",
                               date=Sys.Date()+4,
                               genes=listTest,
                               databases = list(1,2),
                               others=list()))
    ##empty list of databases
    testthat::expect_error(new(Class="Experiment",
                               name="test",
                               date=Sys.Date()+4,
                               genes=listTest,
                               databases = list(),
                               others=list()))
})

test_that("GeneDBi with no error",{
    testthat::expect_s4_class(geneTest1, "RAPDB")
    testthat::expect_s4_class(geneDB3Test, "Oryzabase")
})