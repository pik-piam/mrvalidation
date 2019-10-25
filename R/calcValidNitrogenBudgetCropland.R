#' @title calcValidNitrogenBudgetCropland
#' @description Validation Script for Nitrogen Budgets on Croplands
#'
#' @param datasource Bodirsky for own calculations, Lassaletta2014 for a country dataset from 
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#' FAO for some N related parameters published in FAOSTAT.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidSNUpE}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNitrogenBudgetCropland")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNitrogenBudgetCropland<-function(datasource="Bodirsky"){
  
  if(datasource=="Bodirsky"){
    budget<-calcOutput("NitrogenBudgetCropland",aggregate = FALSE)
    budget[,,"som"] = -budget[,,"som"]
    all<-getNames(budget)
 
       withdrawaltypes<-c("harvest","ag","bg")
    balancetypes<-c("surplus","som","balanceflow")
    inputtypes<-setdiff(setdiff(all,withdrawaltypes),balancetypes)

    tmp<-budget[,,inputtypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|",reportingnames(getNames(tmp)))
    inputs<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Cropland Budget|Inputs"),
      tmp
    )
    
    tmp<-budget[,,withdrawaltypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|+|",reportingnames(getNames(tmp)))
    withdrawals<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Cropland Budget|Withdrawals"),
      tmp
    )
    
    tmp<-budget[,,balancetypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Cropland Budget|Balance|+|",reportingnames(getNames(tmp)))
    balance<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Cropland Budget|Balance"),
      tmp
    )
    
    out<-mbind(
      inputs,
      withdrawals,
      balance
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  } else if (datasource=="ACCMIP") {
    dep<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",scenario=c("rcp26","rcp45","rcp85"),aggregate = FALSE)
    out<-dep
    out<-dimSums(out,dim=c(3.1,3.3,3.4))
    out <- add_dimension(out, dim=3.2, add="indicator", nm="Resources|Nitrogen|Cropland Budget|Inputs|+|Atmospheric Deposition")
    getSets(out)[3]<-"scenario"
  } else if (datasource=="Lassaletta2014"){
    budget<-readSource("Lassaletta2014",subtype="budget")
    out<-mbind(
      setNames(budget[,,"harvest"],paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|+|",reportingnames(getNames(budget[,,"harvest"])))),
      setNames(budget[,,"harvest",invert=T],paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|",reportingnames(getNames(budget[,,"harvest",invert=T]))))
    )
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  } else if (datasource=="FAO"){    
    residues<-readSource("FAO","EmisAgCropResid")
    residues<-setNames(residues[,,"Residues_(Crop_residues)_(tonnes_of_nutrients)"][,,"1712|All Crops + (Total)"]/10^9,"ag_recycling")
    
    manure<-readSource("FAO","EmisAgManureSoil")
    selection<-c(
      "1053|Chickens, broilers",
      "1068|Ducks",
      "1079|Turkeys",
      "1052|Chickens, layers",
      "946|Buffaloes",
      "1760|Camels and Llamas + (Total)",
      "960|Cattle, dairy",
      "1016|Goats",
      "976|Sheep",
      "1759|Mules and Asses + (Total)",
      "1051|Swine, breeding",
      "1096|Horses",
      "1049|Swine, market",
      "961|Cattle, non-dairy"
    )
    
    tmp2<-collapseNames(manure[,,selection][,,"Manure_(N_content)_(Manure_applied)_(Kg)"])
    mapping<-toolMappingFile(type = "sectoral",name = "IPCCitems.csv",readcsv = T)
    tmp2<-toolAggregate(tmp2,rel=mapping,from="fao",to="magpie",dim = 3.1)
    tmp2=tmp2/10^9
    manure=setNames(dimSums(tmp2,dim=3.1),"manure")
    
    #fertilizer
    fertilizer<-readSource("FAO","EmisAgSynthFerti")[,,c(
      "3102|Nitrogen Fertilizers (N total nutrients).Consumption_in_nutrients_(tonnes_of_nutrients)",
      "1360|Nitrogenous fertilizers.Consumption_in_nutrients_(tonnes)")]
    fertilizer<-dimSums(fertilizer,dim=c(3))/1000000
    fertilizer<-setNames(fertilizer,"fertilizer")
    
    commonyears<-intersect(intersect(getYears(fertilizer),getYears(residues)),getYears(manure))
    
    out<-mbind(residues[,commonyears,],
               manure[,commonyears,],
               fertilizer[,commonyears,])
    
    vcat(2,"instead of removing years with no data, each object should rather be blown up to full dimensionality")
    
    names_x<-reportingnames(getNames(out))
    names(names_x)<-NULL
    getNames(out)<-paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|",names_x)
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  getNames(out) <- paste0(getNames(out)," (Mt Nr/yr)")
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Cropland Nitrogen Budget")
         )
}
