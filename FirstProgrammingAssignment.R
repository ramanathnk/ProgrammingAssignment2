pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
    
    extractColumn <- function(colId, argPollutant) {
        data[[colId]][[argPollutant]]
    }
    data <- readData(directory, id)
    extractedColumns <- lapply(seq(length(data)), FUN = extractColumn, argPollutant = pollutant)
    vectorColumns <- unlist(extractedColumns)
    mean (vectorColumns, na.rm = TRUE)
}	

readData <- function(directory, id = 1:332, readOnlyCompleteCases = FALSE) {
    directory <- paste(getwd() , "/", directory, sep = "")
    file <- paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep= "")
    readFileIntoDataFrame <- function(x) {
        dataFrame = read.csv(file = x , header=TRUE, sep=",")
        if (readOnlyCompleteCases == TRUE) {
            dataFrame <- na.omit(dataFrame)
        }
        return (dataFrame)
    }
    
    
    readAllDataFiles <- function() {
        data <- lapply(file, FUN  = readFileIntoDataFrame)
        
    }
    data <- readAllDataFiles()
    
}

complete <- function(directory, id = 1:332) {
    data <- readData(directory, id)
    data.frame(id=sprintf('%3d', id), nobs = sapply(data, FUN = function(x) sum(complete.cases(x))))
    
}

corr <- function(directory, threshold = 0) {
    nitrate <- "nitrate"
    sulfate <- "sulfate"
    data <- readData(directory,readOnlyCompleteCases = TRUE)
    completeCases <- complete(directory)
    casesGreaterThanThreshold <- data[completeCases[2] > threshold]
    dataFrame <- data.frame(id=sprintf('%3d', completeCases[["id"]][completeCases[2]>threshold]), 
               corr = sapply(casesGreaterThanThreshold, FUN = function(x) cor(x[sulfate],x[nitrate])))

    dataFrame[[2]]

}