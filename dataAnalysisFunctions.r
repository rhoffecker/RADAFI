### Input / output file support functions

inputFile <- function(){
  readline("Enter the input file name and press <ENTER>:  \n")
}
inputDelim <- function(){
  readline("Enter the delimiter used in input file and press <ENTER>:  \n")
}
outputFile <- function(){
  readline("Enter the analysis output file name and press <ENTER>:  \n")
}
assessType <- function(){
  a1 <- readline("Assessment type to apply; B = Basic, A = Advanced:  \n")
#   if (a1 != 'a'){
#     a1 <- assessmentType()
#   }
  return(a1)
}
saveCriteria <- function(){
  readline("Would you like to save this criteria set? (Y/N) <ENTER>  \n")
}
configCriteria <- function(){
  readline("Enter the analysis criteria name <ENTER>:  \n")
}

AC1 <- function(){
  as.character(readline("Assess if numeric? (Y/N): \n"))
}
AC2 <- function(){
  as.character(readline("Assess if date field? (Y/N): \n"))
}
AC3 <- function(){
  as.character(readline("Assess if something? (Y/N): \n"))
}
AC4 <- function(){
  as.character(readline("Assess if another? (Y/N): \n"))
}

### Formatting and support functions

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}