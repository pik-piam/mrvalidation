#' @title Download GFED
#' @description Download the GFED (Global Fire Emissions Database) dataset for all years not labeled
#' as beta, in addition to the emission factors.
#'
#' @author Michael S. Crawford
#' @seealso \code{\link{downloadSource}} \code{\link{readGFED}}
#' @examples \dontrun{ a <- downloadSource() }
#'
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom stringr str_subset
#' @importFrom purrr walk
#' @importFrom magrittr %>%
#' @importFrom utils download.file

downloadGFED <- function() {

	# yearly GFED data
	base_URL <- "https://www.geo.vu.nl/~gwerf/GFED/GFED4/"

	year_URLs <- read_html(base_URL) %>%
		html_nodes("a") %>%
		html_attr("href") %>%
		str_subset("GFED4.1s_\\d{4}.hdf5") # implicitly ignoring years in beta

	walk(.x = year_URLs,
		 .f = ~ download.file(paste0(base_URL, .x), destfile = .x))

	# emission factors
	emissionFactors_URL <- "ancill/GFED4_Emission_Factors.txt"
	download.file(paste0(base_URL, emissionFactors_URL), destfile = "GFED_emissionsFactors.txt")

	return(list(url          = base_URL,
				title        = "Global Fire Emissions Database",
				description  = "C and DM emissions from the burning of plant biomass globally, with emission factors.",
				revision     = "4.1",
				comment      = "Readme documentation can be found here: https://www.geo.vu.nl/~gwerf/GFED/GFED4/Readme.pdf",
				unit         = "Spatial data is reported in kg X m^-2 (0.25x0.25 degree grid cells), emission factors are reported in g X per kg.",
				release_date = "2015-07-03",
				license      = "Creative Commons Attribution 3.0 License",
				author       = "Global Fire Emissions Database"))
}
