#' @title downloadGlobalCarbonBudget
#' @description download the most current Global Carbon Budget dataset
#' @author Michael Crawford
#'
#' @return Metadata from the Global Carbon Budget 2023 dataset
#'
#' @examples
#' \dontrun{
#' downloadSource("GlobalCarbonBudget")
#' }
#'
#' @importFrom dplyr %>%

downloadGlobalCarbonBudget <- function() {

  url <- "https://globalcarbonbudgetdata.org/latest-data.html"

  downloadLink <- rvest::read_html(url) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    stringr::str_subset("Global_Carbon_Budget_.*\\.xlsx")

  if (length(downloadLink) == 0) {
    stop("No download link found on the page.")
  }
  downloadLink <- downloadLink[1]

  # Build the full download URL
  baseUrl <- sub("/[^/]*$", "/", url)
  downloadUrl <- ifelse(stringr::str_detect(downloadLink, "^http"), downloadLink, paste0(baseUrl, downloadLink))

  outputFile <- "GCB.xlsx"

  tryCatch(
    {
      download.file(downloadUrl, destfile = outputFile, mode = "wb")
    },
    error = function(e) {
      stop("Failed to download the file: ", e$message)
    }
  )

  # nolint start
  # The reference section for the Global Carbon Budget is hard-coded for 2023
  return(list(
    url = "https://essd.copernicus.org/articles/15/5301/2023/",
    doi = "10.5194/essd-15-5301-2023",
    title = "Global Carbon Budget 2023",
    unit = "Gt C per year",
    author = list(
      person("Pierre", "Friedlingstein"), person("Michael", "O Sullivan"), person("Matthew W.", "Jones"),
      person("Robbie M.", "Andrew"), person("Dorothee C. E.", "Bakker"), person("Judith", "Hauck"),
      person("Peter", "Landschuetzer"), person("Corinne", "Le Quere"), person("Ingrid T.", "Luijkx")
    ),
    release_date = "2023-12-05",
    description = "This dataset provides a comprehensive overview of the global carbon budget, including anthropogenic CO2 emissions and their redistribution among the atmosphere, ocean, and terrestrial biosphere. The data covers the period from 1750 to 2023, with a focus on detailed analysis for the year 2022 and projections for 2023.",
    license = "Creative Commons Attribution 4.0 License",
    reference = "Friedlingstein, P., O Sullivan, M., Jones, M. W., Andrew, R. M., Bakker, D. C. E., Hauck, J., Landschuetzer, P., Le Quere, C., Luijkx, I. T., et al. (2023). Global Carbon Budget 2023. Earth System Science Data, 15, 5301-5369. doi:10.5194/essd-15-5301-2023"
  ))
  # nolint end
}
