# 
# 

# 
# data2 <- data.frame(ch = c(""),
#                    st = c(""),
#                    end = c(""))
# 
# data3 <- data.frame()
# 
# ##result callSnpSeek(data1)
# result1 <- list(list(
#     list("raprepName" = "Os01g0102700"),
#     list("raprepName" = "Os01g0102800"),
#     list("raprepName" = "None"),
#     list("raprepName" = "None"),
#     list("raprepName" = "Os01g0102900"),
#     list("raprepName" = "Os01g0103000"),
#     list("raprepName" = "Os01g0103100"),
#     list("raprepName" = "Os01g0103600"),
#     list("raprepName" = "None"),
#     list("raprepName" = "Os01g0103800"),
#     list("raprepName" = "Os01g0103900"),
#     list("raprepName" = "Os01g0104000"),
#     list("raprepName" = "Os01g0104100"),
#     list("raprepName" = "Os01g0104200"),
#     list("raprepName" = "None"),
#     list("raprepName" = "Os01g0104400"),
#     list("raprepName" = "None"),
#     list("raprepName" = "Os01g0104500"),
#     list("raprepName" = "Os01g0104600")))
# 
# result2 <- list(list())
# 
# result3 <- list()
# 
# 
# test_that("Test callSnpSeek with multiple values",{
#     #testthat::expect_equal(callSnpSeek(data1), result1)
#     testthat::expect_equal(callSnpSeek(data2), result2)
#     testthat::expect_equal(callSnpSeek(data3), result3)
# })
# 

# 
# #19
# # "Os01g0102700"
# # "Os01g0104600"
# # "Os01g0104500"
# # "None"
# 
# #269
# # "Os01g0304300"
# # "Os01g0304500"
# # "Os01g0305200,Os01g0305300"
# # "None"
# 

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#library(testthat)

snp <- list(list(list("Os01g0102700","LOC_Os01g01307","OsNippo01g011900")))

data1 <- data.frame(ch = c("1"),
                    st = c("148907"),
                    end = c("248907"))

data2 <- data.frame(ch = c(""),
                   st = c(""),
                   end = c(""))

data3 <- data.frame()

# result1 <- list(list(
#     list("raprepName" = "Os01g0102700"),
#     list("raprepName" = "Os01g0102800"),
#     list("raprepName" = "Os01g0102900"),
#     list("raprepName" = "Os01g0103000"),
#     list("raprepName" = "Os01g0103100"),
#     list("raprepName" = "Os01g0103600"),
#     list("raprepName" = "Os01g0103800"),
#     list("raprepName" = "Os01g0103900"),
#     list("raprepName" = "Os01g0104000"),
#     list("raprepName" = "Os01g0104100"),
#     list("raprepName" = "Os01g0104200"),
#     list("raprepName" = "Os01g0104400"),
#     list("raprepName" = "Os01g0104500"),
#     list("raprepName" = "Os01g0104600")))

#### test callSnpSeek ####

result1 <- list(list(
    list("Os01g0102700","LOC_Os01g01307","OsNippo01g011900"),
    list("Os01g0102800","LOC_Os01g01312","OsNippo01g011950"),
    list("Os01g0102900","LOC_Os01g01340","OsNippo01g012200"),
    list("Os01g0103000","LOC_Os01g01350","OsNippo01g012250"),
    list("Os01g0103100","LOC_Os01g01360","OsNippo01g012350"),
    list("Os01g0103600","LOC_Os01g01369","OsNippo01g012500"),
    list("Os01g0103800","LOC_Os01g01390","OsNippo01g012700"),
    list("Os01g0103900","LOC_Os01g01400","OsNippo01g012750"),
    list("Os01g0104000","LOC_Os01g01410","OsNippo01g012800"),
    list("Os01g0104100","LOC_Os01g01420","OsNippo01g012850"),
    list("Os01g0104200","LOC_Os01g01430","OsNippo01g012900"),
    list("Os01g0104400","LOC_Os01g01450","OsNippo01g013050"),
    list("Os01g0104500","LOC_Os01g01470","OsNippo01g013150"),
    list("Os01g0104600","LOC_Os01g01484","OsNippo01g013200")))

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

#### test callDB1 ####

gene1 <- list(
    new("RAPDB",
        id = "OsNippo01g011900",
        genesIDs = list(MSU7 = "LOC_Os01g01307",RAPDB = "Os01g0102700"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),
        rapDBGeneNameSynonym = "",
        rapDBGeneSymbolSynonym = "",
        cgsnlGeneName = "",
        cgsnlGeneSymbol = "",
        oryzabaseGeneNameSynonym = "",
        oryzabaseGeneSymbolSynonym = "",
        position = data.frame(ch=c("chr01"),st=c("148085"),end=c("150568")),
        description = paste("Translocon-associated beta", 
                            "family protein. (Os01t0102700-01)")
    )
)

liste2 <- list(list(list()))

##callDB1(list("Os01g0102700"), data1)

test_that("Test callDB1 with multiple values",{
    testthat::expect_equal(callDB1(snp, data1), gene1)
    testthat::expect_error(callDB1(liste2, data3))
    testthat::expect_error(callDB1(1, data3))
    testthat::expect_error(callDB1("string", data3))
})

#### test callDB2 ####
gene2 <- list(
    new("Gramene",
        id = "OsNippo01g011900",
        genesIDs = list(MSU7 = "LOC_Os01g01307",RAPDB = "Os01g0102700"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),
        
        description = "NULL",
        biotype = "protein_coding",
        taxonId = "39947",
        systemName = "oryza_sativa",
        dbType = "core",
        geneIdx = "47",
        location = data.frame(region = "1",
                              start = "148085",
                              end = "150568",
                              strand = "1",
                              map = "GCA_001433935.1")
    )
)

test_that("Test callDB2 with multiple values",{
    testthat::expect_equal(callDB2(snp, data1), gene2)
    testthat::expect_error(callDB2("string", data2))
    testthat::expect_error(callDB2(data1))
})

#### test callDB3 ####

gene3 <- list(
    new("Oryzabase",
        id = "OsNippo01g011900",
        genesIDs = list(MSU7 = "LOC_Os01g01307",RAPDB = "Os01g0102700"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),
        traitGeneId = "NULL",
        cgsnlGeneSymbol = "GELP92",
        GeneSymbolSynonim = "OsGELP92 OsGELP92a OsGELP92b OsGELP92c AChE",
        cgsnlSymbolSynonim = "GDSL ESTERASE/LIPASE PROTEIN 92",
        GeneNameSynonim = paste("GDSL esterase/lipase protein 92",
                                "acetylcholinesterase"),
        proteinName = "NULL",
        allele = "NULL",
        chromosomeNumber = 7,
        explanation = "NULL",
        traitClass = paste(" Biochemical character  Tolerance and resistance -",
                           "Stress tolerance"),
        rapID = "Os07g0586200Oryzabase(IRGSP 1.0/Build5)Rap(IRGSP 1.0/Build5)",
        grameneId = "NULL",
        arm = "NULL",
        locate = "NULL",
        geneOntology = paste0("GO:0004560 - alpha-L-fucosidase activityGO:",
                              "0006629 - lipid metabolic processGO:0009505 - ",
                              "plant-type cell wallGO:0009629 - response to ",
                              "gravityGO:0009630 - gravitropismGO:0016788 - ",
                              "hydrolase activity, acting on ester bonds"),
        traitOntology = "TO:0002693 - gravity response trait",
        plantOntology = ""
    )
)

test_that("Test callDB3 with multiple values",{
    testthat::expect_equal(callDB3(snp, data1), gene3)
    testthat::expect_error(callDB3("string", data2))
    testthat::expect_error(callDB3(data1))
})


#### test callDB4 ####

test_that("Test callDB4 with multiple values",{
    testthat::expect_error(callDB4("string", data2))
    testthat::expect_error(callDB4(data1))
})

#### test callDB5 ####

snp1 <- list(list(list("Os01g0104500","LOC_Os01g01470","OsNippo01g013150")))

gene5 <- list(
    new("PLANTTFDB",
        id = "OsNippo01g013150",
        genesIDs = list(MSU7 = "LOC_Os01g01470",RAPDB = "Os01g0104500"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),

        family = "NAC"
    )
)

# test_that("Test callDB5 with multiple values",{
#     testthat::expect_equal(callDB5(snp1, data1), gene5)
#     testthat::expect_error(callDB5("string", data2))
#     testthat::expect_error(callDB5(data1))
# })

#### test callDB6 ####

gene6 <- list(
    new("PLNTFDB",
        id = "OsNippo01g013150",
        genesIDs = list(MSU7 = "LOC_Os01g01470",RAPDB = "Os01g0104500"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),

        family = "NAC"
    )
)

test_that("Test callDB6 with multiple values",{
    testthat::expect_equal(callDB6(snp1, data1), gene6)
    testthat::expect_error(callDB6("string", data2))
    testthat::expect_error(callDB6(data1))
})

#### test callDB7 ####

gene7 <- list(
    new("Funricigenes",
        id = "OsNippo01g013150",
        genesIDs = list(MSU7 = "LOC_Os01g01470",RAPDB = "Os01g0104500"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),

        symbol = "ONAC020"
    )
)

test_that("Test callDB7 with multiple values",{
    testthat::expect_equal(callDB7(snp1, data1), gene7)
    testthat::expect_error(callDB7("string", data2))
    testthat::expect_error(callDB7(data1))
})

#### test callDB8 ####

snp2 <- list(list(list("Os01g0102800","LOC_Os01g01312","OsNippo01g011950")))

gene8 <- list(
    new("Funricigenes2",
        id = "OsNippo01g011950",
        genesIDs = list(MSU7 = "LOC_Os01g01312",RAPDB = "Os01g0102800"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),

        symbol = "CHR704",
        name = "Snf2_family"
    )
)

test_that("Test callDB8 with multiple values",{
    testthat::expect_equal(callDB8(snp2, data1), gene8)
    testthat::expect_error(callDB8("string", data2))
    testthat::expect_error(callDB8(data1))
})

#### test callDB9 ####

snp3 <- list(list(list("Os01g0104600","LOC_Os01g01484","OsNippo01g013200")))

gene9 <- list(
    new("Funricigenes3",
        id = "OsNippo01g013200",
        genesIDs = list(MSU7 = "LOC_Os01g01484",RAPDB = "Os01g0104600"),
        locus = data.frame(ch = "1", st = "148907", end = "248907"),
        others = list(),

        symbol = "OsDET1",
        keyword = "leaf",
        title = "Mutation of OsDET1 increases chlorophyll content in rice"
    )
)

test_that("Test callDB9 with multiple values",{
    testthat::expect_equal(callDB9(snp3, data1), gene9)
    testthat::expect_error(callDB9("string", data2))
    testthat::expect_error(callDB9(data1))
})

#### test callDB10 - too long ####

test_that("Test callDB10 with multiple values",{
    testthat::expect_error(callDB10("string", data2))
    testthat::expect_error(callDB10(data1))
})









