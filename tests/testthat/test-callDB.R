

data1 <- data.frame(ch = c("1"),
                   st = c("148907"),
                   end = c("248907"))

data2 <- data.frame(ch = c(""),
                   st = c(""),
                   end = c(""))

data3 <- data.frame()

##result callSnpSeek(data1)
result1 <- list(list(
    list("raprepName" = "Os01g0102700"),
    list("raprepName" = "Os01g0102800"),
    list("raprepName" = "None"),
    list("raprepName" = "None"),
    list("raprepName" = "Os01g0102900"),
    list("raprepName" = "Os01g0103000"),
    list("raprepName" = "Os01g0103100"),
    list("raprepName" = "Os01g0103600"),
    list("raprepName" = "None"),
    list("raprepName" = "Os01g0103800"),
    list("raprepName" = "Os01g0103900"),
    list("raprepName" = "Os01g0104000"),
    list("raprepName" = "Os01g0104100"),
    list("raprepName" = "Os01g0104200"),
    list("raprepName" = "None"),
    list("raprepName" = "Os01g0104400"),
    list("raprepName" = "None"),
    list("raprepName" = "Os01g0104500"),
    list("raprepName" = "Os01g0104600")))

result2 <- list(list())

result3 <- list()


test_that("Test callSnpSeek with multiple values",{
    testthat::expect_equal(callSnpSeek(data1), result1)
    testthat::expect_equal(callSnpSeek(data2), result2)
    testthat::expect_equal(callSnpSeek(data3), result3)
})

test_that("callSnpSeek doesn't work with other things than data.frame",{
    testthat::expect_error(callSnpSeek(1:3))
    testthat::expect_error(callSnpSeek("string"))
    testthat::expect_error(callSnpSeek(list(1)))
})

#19
# "Os01g0102700"
# "Os01g0104600"
# "Os01g0104500"
# "None"

#269
# "Os01g0304300"
# "Os01g0304500"
# "Os01g0305200,Os01g0305300"
# "None"

liste1 <- list(list(list("raprepName" = "Os01g0102700")))
res1 <- list(list(
    new("GeneDB1",
    id = "Os01g0102700",
    locus = data.frame(ch = "?", st = "?", end = "?"),
    others = list(),
    rapDBGeneNameSynonym = "",
    rapDBGeneSymbolSynonym = "",
    cgsnlGeneName = "",
    cgsnlGeneSymbol = "",
    oryzabaseGeneNameSynonym = "",
    oryzabaseGeneSymbolSynonym = "",
    position = data.frame(ch=c("chr01"),st=c("148085"),end=c("150568")),
    description = "Translocon-associated beta family protein. (Os01t0102700-01)"
    )
))

liste2 <- list(list(list()))

test_that("Test callDB1Bis with multiple values",{
    testthat::expect_equal(callDB1Bis(liste1), res1)
    testthat::expect_error(callDB1Bis(liste2))
    testthat::expect_equal(callDB1Bis(1), "IdsList has to be a list")
    testthat::expect_equal(callDB1Bis("string"), "IdsList has to be a list")
})



test_that("Test callDB3 with multiple values",{
    testthat::expect_equal(callDB3(data1), list())
    testthat::expect_equal(callDB3(data2), 
                           list("One of your locus has a problem"))
    testthat::expect_error(callDB3(data3))
})











