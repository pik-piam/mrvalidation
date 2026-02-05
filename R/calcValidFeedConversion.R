#' @title calcValidFeedConversion
#' @description calculates various feed indicators
#'
#' @param livestockSystem if TRUE, ruminant meat and milk are aggregated, and poultry meat and egg are aggregated
#' @param subtractBalanceflow if TRUE, balanceflow is subtracted so that the feed conversion reflects
#' our feedbasket calucaltions. If FALSE, it reflects the FAO values and the pasture demand
#'
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, github Copilot
#' @seealso
#' \code{\link[mrcommons]{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' \dontrun{
#' calcOutput("ValidFeed")
#' }
#'
#' @importFrom magpiesets reporthelper findset
#' @importFrom magclass dimOrder
calcValidFeedConversion <- function(livestockSystem = TRUE, subtractBalanceflow = FALSE) {
  ### calculate product specific feed conversion as quotient between
  ### feed and animal products

  mb <- calcOutput("FAOmassbalance", aggregate = FALSE)

  # subtract balanceflow
  if (subtractBalanceflow) {
    balanceflow <- calcOutput("FeedBalanceflow", aggregate = FALSE, future = FALSE)
    getNames(balanceflow, dim = 1) <- paste0("feed_", getNames(balanceflow, dim = 1))
    balanceflow <- balanceflow * calcOutput("Attributes", aggregate = FALSE)
    balanceflow <- as.magpie(aperm(unwrap(balanceflow), c(1, 2, 4, 3, 5)))
    balanceflowTotal <- dimSums(balanceflow, dim = 3.2)
    mb[, , findset("kap", alias = "feed")] <- mb[, , findset("kap", alias = "feed")] - balanceflow
    mb[, , "feed"] <- mb[, , "feed"] - balanceflowTotal
  }

  mb <- mb[, , c("ge", "nr")]
  x <- NULL
  weight <- NULL

  ap <- mb[, , "production"][, , findset("kli")]
  quotientProductspecfic <- collapseNames(ap)
  # extract feed items from mb
  feed <- mb[, , c("feed", "feed_livst_chick", "feed_livst_egg",
                   "feed_livst_milk", "feed_livst_pig",
                   "feed_livst_rum")]
  feedGrouped <- mbind(
    add_dimension(
                  dimSums(feed[, , c("tece", "trce", "rice_pro", "maiz", "brans")], dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Cereal Intensity"),
    add_dimension(
                  dimSums(feed[, , c("soybean", "groundnut", "puls_pro", "rapeseed", "oils", "oilcakes",
                                     "cassav_sp", "oilpalm", "sunflower")], dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Oilcrop intensity"),
    add_dimension(
                  dimSums(feed[, , c("potato", "cassav_sp", "others",
                                     "sugr_beet", "sugr_cane", "molasses")], dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Roots and other crops"),
    add_dimension(
                  dimSums(feed[, , c("pasture")], dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Pasture intensity"),
    add_dimension(
                  dimSums(feed[, , c("foddr", "res_cereals", "res_fibrous", "res_nonfibrous")],
                          dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Other roughage intensity"),
    add_dimension(
                  dimSums(feed[, , c("livst_chick", "livst_egg", "livst_milk", "livst_pig",
                                     "livst_rum", "fish")], dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Animal product intensity"),
    add_dimension(
                  dimSums(feed[, , c("distillers_grain", "alcohol", "scp")], dim = c("ItemCodeItem")),
                  dim = 3.1, add = "ItemCodeItem", nm = "Other processed items intensity")
  )


  feedProductspecific <- collapseNames(feedGrouped[, , c("feed_livst_chick", "feed_livst_egg",
                                                         "feed_livst_milk", "feed_livst_pig",
                                                         "feed_livst_rum")])
  getNames(feedProductspecific, dim = 2) <- substring(getNames(feedProductspecific, dim = 2), 6)

  if (livestockSystem == TRUE) {
    feedProductspecific <- mbind(
      add_dimension(
                    dimSums(feedProductspecific[, , c("livst_milk", "livst_rum")], dim = c("ElementShort")),
                    dim = 3.2, add = "ElementShort", nm = "Ruminant meat and dairy"),
      add_dimension(
                    dimSums(feedProductspecific[, , c("livst_chick", "livst_egg")], dim = c("ElementShort")),
                    dim = 3.2, add = "ElementShort", nm = "Poultry meat and eggs"),
      add_dimension(
                    dimSums(feedProductspecific[, , c("livst_pig")], dim = c("ElementShort")),
                    dim = 3.2, add = "ElementShort", nm = "Pig meat")
    )
    quotientProductspecfic <- mbind(
      add_dimension(
                    dimSums(quotientProductspecfic[, , c("livst_milk", "livst_rum")], dim = c("ItemCodeItem")),
                    dim = 3.1, add = "ItemCodeItem", nm = "Ruminant meat and dairy"),
      add_dimension(
                    dimSums(quotientProductspecfic[, , c("livst_chick", "livst_egg")], dim = c("ItemCodeItem")),
                    dim = 3.1, add = "ItemCodeItem", nm = "Poultry meat and eggs"),
      add_dimension(
                    dimSums(quotientProductspecfic[, , c("livst_pig")], dim = c("ItemCodeItem")),
                    dim = 3.1, add = "ItemCodeItem", nm = "Pig meat")
    )
  } else {
    getNames(feedProductspecific, dim = 2) <- reportingnames(getNames(feedProductspecific, dim = 2))
    getNames(quotientProductspecfic, dim = 1) <- reportingnames(getNames(quotientProductspecfic, dim = 1))
  }

  # Calculate feed conversion total
  quotientTmp <- collapseNames(dimSums(quotientProductspecfic, dim = c("ItemCodeItem"))[, , "ge"])
  indicatorTmp <- collapseNames(dimSums(feedProductspecific,
                                        dim = c("ItemCodeItem", "ElementShort"))[, , "ge"]) / quotientTmp
  nameIndicator <- "Productivity|Feed conversion"
  indicatorTmp <- setNames(collapseNames(indicatorTmp), paste0(nameIndicator, " (", "GE per GE", ")"))
  quotientTmp <- setNames(collapseNames(quotientTmp), paste0(nameIndicator, " (", "GE per GE", ")"))
  x <- mbind(x, indicatorTmp)
  weight <- mbind(weight, quotientTmp)

  # calculate feed conversion Livestock specific
  quotientTmp <- quotientProductspecfic[, , c("ge", "nr")]
  indicatorTmp <- dimSums(feedProductspecific, dim = c("ItemCodeItem"))[, , c("ge", "nr")] / quotientTmp
  prefix <- "Productivity|Feed conversion|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp[, , "ge"]), nameIndicator))
  weight <- mbind(weight, setNames(collapseNames(quotientTmp[, , "ge"]), nameIndicator))
  prefix <- "Productivity|Feed protein conversion|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "Nr per Nr", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp[, , "nr"]), nameIndicator))
  weight <- mbind(weight, setNames(collapseNames(quotientTmp[, , "nr"]), nameIndicator))

  # calculate feed conversion Livestock and product specific
  quotientTmp <- collapseNames(quotientProductspecfic[, , "ge"])
  indicatorTmp <- collapseNames(feedProductspecific[, , "ge"]) / quotientTmp
  prefix <- "Productivity|Feed conversion|"
  for (item in getNames(feedProductspecific, dim = 2)) {
    nameIndicator <- paste0(prefix, item, "|+|",
                            getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
    x <- mbind(x, setNames(collapseNames(indicatorTmp)[, , item], nameIndicator))
    weightTmp <- collapseNames(indicatorTmp[, , item]) * NA
    weightTmp[, , ] <- collapseNames(quotientTmp[, , item])
    weight <- mbind(weight, setNames(weightTmp, nameIndicator))
  }

  # calculate feed conversion Product specific
  quotientTmp <- collapseNames(dimSums(quotientProductspecfic, dim = c("ItemCodeItem"))[, , "ge"])
  indicatorTmp <- collapseNames(dimSums(feedProductspecific, dim = c("ElementShort"))[, , "ge"]) / quotientTmp
  prefix <- "Productivity|Feed conversion|+|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp), nameIndicator))
  weightTmp <- collapseNames(indicatorTmp) * NA
  weightTmp[, , ] <- collapseNames(quotientTmp)
  weight <- mbind(weight, setNames(weightTmp, nameIndicator))

  # roughage share for ruminants as quotient of roughage and total feed
  quotientTmp <- dimSums(feedProductspecific, dim = "ItemCodeItem")
  indicatorTmp <- dimSums(
                          feedProductspecific[, , c("Other roughage intensity", "Pasture intensity")],
                          dim = "ItemCodeItem") / quotientTmp
  indicatorTmp2 <- dimSums(
                           feedProductspecific[, , c("Pasture intensity")],
                           dim = "ItemCodeItem") / quotientTmp
  if (livestockSystem == TRUE) {
    quotientTmp <- quotientTmp[, , "Ruminant meat and dairy"]
    indicatorTmp <- indicatorTmp[, , "Ruminant meat and dairy"]
    indicatorTmp2 <- indicatorTmp2[, , "Ruminant meat and dairy"]
  }
  prefix <- "Productivity|Roughage share|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp[, , "ge"]), nameIndicator))
  weight <- mbind(weight, setNames(quotientTmp[, , "ge"], nameIndicator))

  prefix <- "Productivity|Pasture share|"
  nameIndicator <- paste0(prefix, getNames(indicatorTmp2, dim = 1), " (", "GE per GE", ")")
  x <- mbind(x, setNames(collapseNames(indicatorTmp2[, , "ge"]), nameIndicator))
  weight <- mbind(weight, setNames(quotientTmp[, , "ge"], nameIndicator))

  # add livestock yields
  livestockYield <- calcOutput("LivestockProductivity", future = FALSE, aggregate = FALSE, supplementary = TRUE)
  prefix <- "Productivity|Livestock system yield|"
  nameIndicator <- paste0(prefix, getNames(livestockYield$x, dim = 1), " (", "DM per live animal", ")")
  x <- mbind(x, setNames(livestockYield$x, nameIndicator))
  weight <- mbind(weight, setNames(livestockYield$weight, nameIndicator))

  x <- add_dimension(x, dim = 3.1, add = "scenario", nm = "historical")
  x <- add_dimension(x, dim = 3.2, add = "model", nm = "Weindl_et_al2017")

  x[weight == 0] <- 0

  getNames(x) <- sub("\\|$", "", getNames(x))
  getNames(weight) <- sub("\\|$", "", getNames(weight))


  return(list(x = x,
              weight = weight + 1e-10,
              unit = "GE feed per GE product",
              description = "Agricultural Demand")
  )
}
