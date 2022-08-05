#' GitHub report
#' 
#' Generate a report from all of a GitHub Organization's repositories.
#' @inheritParams github_network
#' @returns A nested list of \link[data.table]{data.table}s.
#' 
#' @export
#' @examples 
#' report <- github_report(org = "neurogenomics")
github_report <- function(org,
                          add_contributors = TRUE,
                          add_issues = FALSE,
                          add_comments = FALSE,
                          token = gh::gh_token(),
                          verbose = TRUE){
    res_list <- list(org=org)
    #### Organization ####
    res_list[["organization"]] <- github_org(org = org,
                                             token = token, 
                                             verbose = verbose)
    #### Repositories ####
    repos <- github_org(org = org, 
                        field = "repos", 
                        token = token, 
                        verbose = verbose) 
    res_list[["repos"]] <- repos 
    #### Contributors ####
    if(isTRUE(add_contributors)){
        res_list[["contributors"]] <- github_contributors(repos = repos,
                                                          token = token,
                                                          agg_by_repo = TRUE,
                                                          agg_by_user = TRUE,
                                                          verbose = verbose)
    }
    #### Issues ####
    if(isTRUE(add_issues)){
        res_list[["issues"]] <- github_issues(repos = repos, 
                                              add_comments = add_comments,
                                              agg_by_repo = TRUE,
                                              verbose = verbose)
    }
    return(res_list)
}
