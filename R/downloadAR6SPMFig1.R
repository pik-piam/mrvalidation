#' @title downloadAR6SPMFig1
#' @description download IPCC AR6 Summary for Policymakers Figure 1, global surface temperature change
#' relative to 1850-1900 from observations and CMIP6 models (human and natural forcings simulations)
#' @seealso https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_SPM.pdf
#' @author Michael Crawford
#'
#' @return Metadata on downloaded dataset
#'
#' @examples
#'
#' \dontrun{
#'   downloadSource("AR6SPMFig1")
#' }
#'
#' @importFrom utils download.file

downloadAR6SPMFig1 <- function() {

    # nolint start

    csvURL  <- "https://dap.ceda.ac.uk/badc/ar6_wg1/data/spm/spm_01/v20210809/panel_b/gmst_changes_model_and_obs.csv?download=1"
    download.file(csvURL, destfile = "AR6SPMFig1_GlobalSurfaceTemperatureAnomaly.csv")

    return(list(url          = "https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_SPM.pdf",
                title        = "Summary for Policymakers. In: Climate Change 2021: The Physical Science Basis. 
                Contribution of Working Group I to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change",
                unit         = "C",
                author       = "IPCC [Masson-Delmotte, V., P. Zhai, A. Pirani, S.L. Connors, C. Pean, S. Berger, N. Caud, 
                Y. Chen, L. Goldfarb, M.I. Gomis, M. Huang, K. Leitzell, E. Lonnoy, J.B.R. Matthews, T.K. Maycock, T. Waterfield,
                O. Yelekci, R. Yu, and B. Zhou (eds)]",
                release_date = "2021",
                description  = "Dataset underlying SPM Figure 1 Panel (b) Changes in global surface temperature over the past 
                170 years relative to 1850 to 1900 and annually averaged.",
                license      = "Creative Commons Attribution 4.0 International License",
                reference    = "IPCC, 2021: Summary for Policymakers. In: Climate Change 2021: The Physical Science Basis. 
                Contribution of Working Group I to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change 
                [Masson-Delmotte, V., P. Zhai, A. Pirani, S.L. Connors, C. Pean, S. Berger, N. Caud, Y. Chen, L. Goldfarb, 
                M.I. Gomis, M. Huang, K. Leitzell, E. Lonnoy, J.B.R. Matthews, T.K. Maycock, T. Waterfield, O. Yelekci, R. Yu, 
                and B. Zhou (eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA, 
                pp. 3 to 32, doi:10.1017/9781009157896.001."
                )
            )

    # nolint end
}
