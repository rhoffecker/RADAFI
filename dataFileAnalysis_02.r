source("dataAnalysisFunctions.r")
start.time <- Sys.time()
runDate <- Sys.Date()
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

### Setting variables defining input/output files
  ### To turn on prompts for file names and delimiter, un-comment this section and comment out hard coded section
  fileName <- inputFile()
  delimiter <- inputDelim()
#   outputName <- outputFile()
#   fileName <- ("ERS_20160603.txt")
#   delimiter <- ("|")
  outputName <- paste("output/test",trim(as.numeric(Sys.time())),".txt", sep="")

### Reading in the data file
myData <- read.table(fileName, sep = delimiter, header = TRUE, stringsAsFactors = FALSE, fill = TRUE)


### Getting a list of attribute names and row/column counts from the header data
attrList <- colnames(myData)
attrCount <- ncol(myData)
rcdCount <- nrow(myData)

### Identifing the advanced assessment criteria to apply per attribute

  cat(sprintf("-----------------------------------------------------------------\n"))
  cat(sprintf("Select if you would like to perform a BASIC or ADVANCED file\n"))
  cat(sprintf("assessment. By slecting advanced, you will be asked to specifiy\n"))
  cat(sprintf("the assessment criteria to apply to each attribute. A Configuration\n"))
  cat(sprintf("file can be used in future advanced assessments if the criteria set\n"))
  cat(sprintf("should be the same.\n\n"))


assessType <- assessType()
assessType <- toupper(assessType)

if (assessType == "A"){
  ### Load an Assessment Congig File
  configLoad <- configLoad()
  configLoad <- toupper(configLoad)
  if (configLoad == "Y"){
    configFile <- configFile()
    assmtConfig <- read.table(configFile, sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
    }
  else { 
    ### Create an Assessment Config File
    configFile <- configCriteria()
  
    assmtConfig <- matrix(NA, nrow = attrCount, ncol=5)
    for (x in 1:10){
      cat(sprintf("Determine the assessment criteria for '%s' attribute. \n",attrList[x]))
      assmtConfig[x, 1] <- attrList[x]
      assmtConfig[x, 2] <- toupper(AC1())
      assmtConfig[x, 3] <- toupper(AC2())
      assmtConfig[x, 4] <- toupper(AC3())
      assmtConfig[x, 5] <- toupper(AC4())
      x <- x + 1
      }
    write.table(x=assmtConfig, file=paste("config/",configFile), sep = ",")
    }
  }

### Creating the report file
sink(outputName)

## Report header with input file level details

cat("=================================================================\n")
cat(sprintf("Data file: %s\n", fileName))
cat(sprintf("Run date: %s\n", runDate))
cat("-----------------------------------------------------------------\n")
cat(sprintf("This program is designed to perform an a data quality assessment \n"))
cat(sprintf("on a specified data file. After an input file is specified, the \n"))
cat(sprintf("program will step through each attribute and allow selection of \n"))
cat(sprintf("assessment criteria relevant to the attribute. Results are output \n"))
cat(sprintf("to a specified report file. For details about running the program \n"))
cat(sprintf("review the readme file. \n"))
cat("-----------------------------------------------------------------\n")
cat(sprintf("Records in file: %s\n", rcdCount))
cat(sprintf("Number of record attributes: %s\n\n", attrCount))

### Loop through attribute list and apply data quality checks
for (i in 1:attrCount){
#   
  ### Record interrogation variables
    attrName <- attrList[i]
    colList <- myData[,i]
    nonMissing <- sum(myData[i] != "")
    missing <- sum(myData[i] == "")
    percentComplete <- nonMissing / rcdCount
    uniqueValues <- length((unique(colList)))
    percentUnique <- uniqueValues / rcdCount
    dataSample <- sample((myData[1:rcdCount, i][!(myData[1:rcdCount, i] == "")]), 5)

  ### List of values logic
    lovListThreshold <- .005
    if (percentUnique < lovListThreshold){
      lov <- unique(colList)
    }
    ### Determine if the attribut is a number logic
#    isNumber <- suppressWarnings(sum(!is.na(as.integer(trim(colList)))))
    isNumber <- sum(as.integer(!is.na(trim(colList))))
    percentNumber <- isNumber / rcdCount
  ### Attribute length logic -  
#       expLength <- 3
#       attrLengthList <- (nchar(as.character(myData[ , i])))
#       lowCount <- (sum(attrLengthList[] < expLength))
#       HighCount <- (sum(attrLengthList[] > expLength))
#     #for range
#       upperLength <- 17
#       lowerLength <- 15
#     #length results
#       countMeets <-
#       percentMeets <-
#       countAbove <-
#       percentAbove <-
#       countBelow <-
#       percentBelow <-
  
  ### Output of basic attribute analysis
    cat("=================================================================\n")
    cat(sprintf("%s\n",attrName))
    cat(sprintf(" Sample values: "))
    cat(sprintf(" %s, ", dataSample))
    cat(sprintf("\n"))
    cat("-----------------------------------------------------------------\n")
    cat(sprintf("BASE ASSESSMENT CRITERIA \n"))
    cat(sprintf(" Non Missing: %s\n", nonMissing))
    cat(sprintf(" Missing: %s\n", missing))
    cat(sprintf(" Percent Complete: %s\n", percent(percentComplete)))
    cat(sprintf(" Count of Unique Values: %s\n", uniqueValues))
    cat(sprintf(" Percent of Unique Values: %s \n", percent(percentUnique)))
   
#   ### Output of advanced assessment criteria
    if (assessType == "A"){ 
      cat(sprintf("\nOPTIONAL ASSESSMENT CRITERIA \n"))
      if (!(is.na(assmtConfig[i , 2])) &  assmtConfig[i, 2] == "Y"){
         ### Shows percent of values that are numbers (needs if statement)
         cat(sprintf(" Count of attributes that only contain numbers: %s\n",isNumber))
         cat(sprintf(" Percent of attributes that is numeric: %s\n", percent(percentNumber,2)))
         cat(sprintf("\n"))
      }
    }
#   ### Show a list of values in the report; retuires a flag at the attribute level (needs if statement)
#     cat(sprintf(" List of Unique Values:\n"))
#     cat(sprintf(" %s",lov))
#     cat(sprintf("\n"))
#   ### Shows percent of values that are numbers (needs if statement)
#     cat(sprintf(" Count of attributes that are numbers: %s\n",isNumber))
#     cat(sprintf(" Percent of attributes that are numbers: %s\n", percent(percentNumber,2)))
#     cat(sprintf("\n"))
#   
    cat(sprintf("\n"))
    i = i+1
}
cat("-----------------------------------------------------------------\n")

sink()

cat(sprintf("\n\nThis analysis is a product of World Domination Corp.\n"));

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
