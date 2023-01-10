#' @title readWBPoverty
#' @description read World Bank poverty percentage of population under poverty line, and gini coef data
#' No download function from WDI API yet, as the older versions (2011PPP as opposed to 2017PPP online) are currently
#' most relevant for validating Soergel poverty model results and must be downloaded manually
#' @param subtype either "Gini", or the three poverty line thresholds:
#' "190PovertyLine", "320PovertyLine", "550PovertyLine"
#' @author David M Chen
#'
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' readSource("readWBPoverty", subtype = "320PovertyLine")
#' }
#'
#' @importFrom tidyr pivot_longer

readWBPoverty <- function(subtype = "320PovertyLine") {

  files <- c(
    "190PovertyLine" = "UNdata_Export_190PovertyLine.csv",
    "320PovertyLine" = "UNdata_Export_320PovertyLine.csv",
    "550PovertyLine" = "UNdata_Export_550PovertyLine.csv",
    "Gini" = "API_SIPOVGINI_DS2_en_csv_v2_4770433.csv")

  file <- toolSubtypeSelect(subtype, files)

if (subtype == "Gini") {

    d <- read.csv(file, header = TRUE, skip = 4)
    d <- pivot_longer(d, cols = c(5:(ncol(d) - 1)), names_to = "year")
    d$year <- as.numeric(gsub("X", "", d$year))

    out <- as.magpie(d[, c("Country.Code", "year", "value")], tidy = TRUE)
    out <- toolCountryFill(out, no_remove_warning = c("AFE", "AFW", "ARB", "CEB", "CHI",
                                                      "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU",
                                                      "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA",
                                                      "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC",
                                                      "LMY", "LTE", "MEA", "MIC", "MNA", "NAC", "OED", "OSS",
                                                      "PRE", "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA",
                                                      "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD", "XKX"))
    } else {

    d <- read.csv(file, header = TRUE, skip = 0)
    d$iso3c <- toolCountry2isocode(d$Country.or.Area, warn = FALSE)
    d <- d[which(!is.na(d$iso3c)), ]
   out <- as.magpie(d[, c("iso3c", "Year", "Value")], tidy = TRUE)
   out <- magpiesort(out)
   out <- toolCountryFill(out, no_remove_warning = "KOS")

  }
  out <- out / 100 # convert to 0-1 scale for Gini and fraction for percentage

  out[which(is.na(out))] <- 9999 # make NAs a high value to weight them 0

    return(out)
}
