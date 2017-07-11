geneTest1 <- new("GeneDB1",
                 id = "test1", locus = data.frame(),
                 rapDBGeneNameSynonym = "character",
                 rapDBGeneSymbolSynonym = "character",
                 cgsnlGeneName = "character",
                 cgsnlGeneSymbol = "character",
                 oryzabaseGeneNameSynonym = "character",
                 oryzabaseGeneSymbolSynonym = "character",
                 position = data.frame(),
                 description = "character")

geneTest2 <- new("GeneDB1",
                 id = "test2", locus = data.frame(),
                 rapDBGeneNameSynonym = "character",
                 rapDBGeneSymbolSynonym = "character",
                 cgsnlGeneName = "character",
                 cgsnlGeneSymbol = "character",
                 oryzabaseGeneNameSynonym = "character",
                 oryzabaseGeneSymbolSynonym = "character",
                 position = data.frame(),
                 description = "character")

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