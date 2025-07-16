#' @title calcValidSSPResults
#' @description Convert SSPResults to reporting format
#' 
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link[mrcommons]{convertSSPResults}}
#' @examples
#' 
#' \dontrun{ 
#'   calcOutput("ValidSSPResults")
#' }
#' 

calcValidSSPResults<- function(){
  
  out                  <- readSource("SSPResults")
  getNames(out,dim=1)  <- gsub("([^-]*)-([^-]*)-([^-]*)-(.*)","\\1-\\2-\\3\\.\\4",getNames(out,dim=1))
  names(dimnames(out)) <- sub("scenario-", "scenario.", names(dimnames(out)))

  pattern <- regexpr("\\((.*)\\)$", getNames(out, dim=3), perl=TRUE)
  unit <- paste(unique(sub("\\)","",sub("\\(","",regmatches(m=pattern, x=getNames(out, dim=3))))), collapse = ", ")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="SSPResults for different variables, models and scenarios")
  )
}