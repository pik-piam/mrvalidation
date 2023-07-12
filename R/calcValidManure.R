#' @title calcValidManure
#' @description Validates the esitmates on  excretion and manure management
#'
#' @param datasource own: own estimation for the past based on feed intake, IPCC: standard excretion factors, FAO: FAO estimates
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcExcretion}},
#' \code{\link{calcExcretionIPCC}}
#' @examples
#'
#' \dontrun{
#' calcOutput("ValidManure")
#' }
#'

calcValidManure<-function(datasource="Bodirsky"){

  if(datasource=="IPCC"){
    out<-calcOutput("ExcretionIPCC",products="magpie",aggregate = FALSE)
    out<-dimSums(out,dim=3.2)
    names_x<-reportingnames(getNames(out))
    names(names_x)<-NULL
    getNames(out) <- paste0("Resources|Nitrogen|Manure|",names_x)
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  } else if(datasource=="Bodirsky"){
    out<-calcOutput("Excretion",aggregate = FALSE)[,,"nr"]

    tot<-dimSums(out,dim=c(3.1,3.2,3.3))
    getNames(tot) <- paste0("Resources|Nitrogen|Manure")

    ex<-dimSums(out,dim=c(3.1,3.3))
    names_x<-reportingnames(getNames(ex))
    names(names_x)<-NULL
    getNames(ex) <- paste0("Resources|Nitrogen|Manure|+|",names_x)

    awms<-dimSums(out,dim=c(3.2,3.3))
    names_x<-reportingnames(getNames(awms))
    names(names_x)<-NULL
    getNames(awms) <- paste0("Resources|Nitrogen|Manure|++|",names_x)

    exawms<-dimSums(out,dim=c(3.3))
    getNames(exawms,dim=1) <-reportingnames(getNames(exawms,dim=1))
    getNames(exawms,dim=2) <-reportingnames(getNames(exawms,dim=2))
    getNames(exawms)=sub(getNames(exawms),pattern = "\\.",replacement = "|+|")
    getNames(exawms) <- paste0("Resources|Nitrogen|Manure|",getNames(exawms))

    conf<-calcOutput("EmisNitrogenPasturePast",aggregate = FALSE)


    out<-mbind(tot,ex,awms,exawms)

    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  } else if(datasource=="FAO"){

    confinement<-readSource("FAO_online",subtype="EmisAgManureManag")
    pasture<-readSource("FAO_online",subtype="EmisAgManurePasture")

    selection<-c(
      "1053|Chickens, broilers",
      "1068|Ducks",
      "1079|Turkeys",
      "1052|Chickens, layers",
      "946|Buffaloes",
      "1760|Camels and Llamas",
      "960|Cattle, dairy",
      "1016|Goats",
      "976|Sheep",
      "1759|Mules and Asses",
      "1051|Swine, breeding",
      "1096|Horses",
      "1049|Swine, market",
      "961|Cattle, non-dairy"
    )

    confinement<-collapseNames(confinement[,,selection][,,"Manure_treated_(N_content)_(kg)"])
    mapping<-toolGetMapping(type = "sectoral",name = "IPCCitems_fao_online.csv", where = "mappingfolder")
    confinement<-toolAggregate(confinement,rel=mapping,from="fao",to="magpie",dim = 3.1)

    pasture<-collapseNames(pasture[,,getNames(pasture,dim=1)[getNames(pasture,dim=1)%in%selection]][,,"Manure_left_on_pasture_(N_content)_(kg)"])

    #pasture<-collapseNames(pasture[,,selection][,,"Manure_(N_content)_(Manure_management)_(Kg)"])
    mapping<-toolGetMapping(type = "sectoral",name = "IPCCitems_fao_online.csv", where = "mappingfolder")
    pasture<-toolAggregate(pasture,rel=mapping,from="fao",to="magpie",dim = 3.1,partrel = T)

    out<-pasture+confinement
    out<-out/10^9
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)

    } else stop("No data exist for the given datasource!")

  names(dimnames(out))[3] <- "scenario.model.variable"
  getNames(out) <- paste0(getNames(out)," (Mt Nr/yr)")
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Manure excretion")
  )
}
