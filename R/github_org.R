#' GitHub Organization
#' 
#' Get GitHub Organization metadata.
#' @param field Field to search.
#' @inheritParams github_metadata
#' @export
#' @importFrom gh gh gh_token
#' @importFrom data.table data.table
github_org <- function(org,
                       field = NULL, 
                       token = gh::gh_token(),
                       as_datatable = TRUE,
                       verbose = TRUE){ 
    
    endpoint <- paste("https://api.github.com/orgs",org,sep="/")
    org_info <- gh::gh(endpoint = endpoint,
                       .token = token,
                       per_page = 100)  
    if(is.null(field)) {
        messager("Returning general organization metadata.",v=verbose)
        if(as_datatable){
            names(org_info)
            org_info_dt <- data.table::data.table(
                t(data.table::data.table(org_info))
            )
            colnames(org_info_dt) <- names(org_info)
            return(org_info_dt)
        } else {
            return(org_info)
        }
    } else {
        #### Specific field #####
        messager("Returning",field,"metadata.",v=verbose) 
        if(field=="projects"){
            ## No proper API yet?
            endpoint <- paste("https://github.com/orgs",org,"projects",sep="/")
        } else {
            field_nm <- paste(field,"url",sep="_")
            if(!field_nm %in% names(org_info)){
                stop("Could not find field: ",field)
            }
            endpoint <- org_info[[field_nm]]
        }
        field_info <- gh::gh(endpoint = endpoint,
                             .token = token,
                             per_page = 100)   
        if(as_datatable){
            return(gh_to_dt(field_info))
        } else {
            return(field_info)
        } 
    } 
}