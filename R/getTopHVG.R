#' getTopHVG
#' Extracts the top variable genes from an input singleCellExperiment object based on
#' a selected column from rowData
#' @param inSCE an input singleCellExperiment object
#' @param varianceColumnName represents which column from rowData of input singleCellExperiment
#' object should be raanked to generate the top most variable genes
#' @param n number of top variable genes to extract
#' @return list of top variable gene names
#' @export
#' @author Irzam Sarfraz
#' @example
#' data(sce_chcl, package = "scds")
#' sce_chcl <- scran_modelGeneVar(sce_chcl, "counts")
#' topGenes <- getTopHVG(sce_chcl, "scran_modelGeneVar_bio", 10) #return top 10 variable genes
#' print(topGenes)

getTopHVG <- function(inSCE, varianceColumnName, n = 2000) {
    variance <- rowData(inSCE)[varianceColumnName]
    featureNames <- rownames(inSCE)
    tempDataFrame <- data.frame(featureNames, variance)
    tempDataFrame <- tempDataFrame[order(-tempDataFrame[varianceColumnName]),]
    return(as.character(tempDataFrame$featureNames[1:n]))
}