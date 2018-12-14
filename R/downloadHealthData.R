#' Download Raw Dataset
#' @description Description
#' @param dataset The Dataset chosen for download. Options currently include "China","AidDataCore". 
#' @param folder A character string containing the name of a subfolder of the current working directory where the dataset shall be saved in.
#' @param filename A character string for the name of the file (without filetype ending).
#' @param overwrite Logical T/F whether data shall be overwritten if existing already(Defaults to FALSE) 
#' @return A character string of the file path and name of the downloaded raw data.
#' @examples
#' \dontrun{
#' downloadHealthData(dataset = "ghdx",
#'                   folder = "rawdata")
#' }
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
downloadHealthData <- function(dataset = "ghdx",
                               folder = "",
                               overwrite = FALSE){

  linkList <- 
    list(list(
      selector = "ghdx",
        url = list("http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_STATES_A_TO_F_CSV.zip",
                   "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_STATES_G_TO_L_CSV.zip",
                   "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_STATES_M_TO_N_CSV.zip",
                   "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_STATES_O_TO_S_CSV.zip",
                   "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_STATES_T_TO_W_CSV.zip",
                   "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_CODEBOOK_CSV_0.zip"),
        filetype = "zip",
        filenameOut = list("ghdx_af",
                           "ghdx_gl",
                           "ghdx_mn",
                           "ghdx_os",
                           "ghdx_tw",
                           "ghdx_codebook"),
        citeMessage = "Please cite as follows:\nInstitute for Health Metrics and Evaluation (IHME). United States Mortality Rates by County 1980-2014. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2016."),
      list(selector = "lalala")
    )
  
  if(folder != "") dir.create(folder, showWarnings = TRUE)
  
  index <- 
    purrr::map_dbl(linkList,
                   ~ ifelse(.x$selector == dataset,1,0)) 
  li <- 
    linkList[[which(index == 1)]]
  
  
  message(paste0(li["citeMessage"]),"\n\n")
  
  
  xx <- 
   purrr::map2(.x = li$url,
              .y = li$filenameOut,
              .f = function(url,nn){
                
              
                path <- paste0(ifelse(folder == "",folder,paste0(folder,"/")),
                               nn,".",li$filetype)
                
                
                message(paste0("Writing ",nn,".",li$filetype," to '",folder,"'\n"))
                
                
                if(overwrite == TRUE |(! file.exists(path))){
                  download.file(url,
                                destfile = path) 
                }  else {warning(paste0("File ",path," exists already. Not downloaded again"))}
                
                
                
                return(path)
                
              })
  
  
  return(xx)
  

}





'
downloadHealthData(dataset = "ghdx",
                   folder = "rawdata")

'