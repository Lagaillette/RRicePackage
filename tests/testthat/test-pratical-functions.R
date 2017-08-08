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

listTest <- list(geneTest1, geneTest2)
frameTest <- data.frame()
frameTest <- append(frameTest, geneTest1)
dbList1 <- list(1, 2, 3)
dblist2 <- list(1, 2, 1)
dbFrameTest <- data.frame()
dbFrameTest <- append(frameTest, 1)

test_that("existsGene with a list or dataframe of genes and character id",{
    testthat::expect_true(existsGene(listTest,"test1"))
    testthat::expect_false(existsGene(listTest,"test3"))
    testthat::expect_false(existsGene(frameTest,"test3"))
})

test_that("existsGene doesn't work with other things than list of Genes",{
    testthat::expect_error(existsGene(1:3, "test1"))
})

test_that("existsGene return FALSE if id is not a character",{
    testthat::expect_false(existsGene(listTest, 2))
    testthat::expect_false(existsGene(listTest, frameTest))
})

test_that("alreadyUsedDB with a list or dataframe of genes and a number",{
    testthat::expect_false(alreadyUsedDB(dbList1, 3))
    testthat::expect_true(alreadyUsedDB(dblist2, 3))
    testthat::expect_false(alreadyUsedDB(dbFrameTest, 1))
})

test_that("alreadyUsedDB error if out of bounds",{
    testthat::expect_error(alreadyUsedDB(dbList1, 4))
})
