#' GitHub metadata
#' 
#' Extract further GitHub metadata from the output out
#'  an initial search with \link[SkillNet]{github_org}.
#' @inheritParams github_network
#' @inheritParams github_org
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom data.table rbindlist
#' @importFrom gh gh gh_token
github_metadata <- function(repos,
                            field,
                            token = gh::gh_token(),
                            verbose = TRUE){ 
    
    if(!field %in% colnames(repos)){
        stop("field must be a column name in repos.")
    }
    res <- mapply(
        stats::setNames(repos[[field]],
                        repos$name), 
        FUN=function(x){
            messager("Processing: ",x,v=verbose)
            gh_response <- gh::gh(endpoint = gsub("{/number}","",x, 
                                                  fixed = TRUE),
                                  .token = token,
                                  per_page = 100)
            gh_to_dt(gh_response = gh_response,
                     verbose = verbose)
        }) |> data.table::rbindlist(use.names = TRUE,
                                    idcol = "name",
                                    fill = TRUE) 
    return(res)
}
