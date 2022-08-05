#' GitHub contributors
#' 
#' Get contributors to each GitHub repository.
#' @inheritParams github_network
#' @inheritParams github_org
#' @export
#' @importFrom gh gh_token
#' @importFrom data.table := data.table
#' @importFrom dplyr n_distinct summarise group_by
#' @importFrom stats setNames
#' @examples 
#' repos <- github_org(org = "neurogenomics", field = "repos")
#' contributors <- github_contributors(repos = repos)
github_contributors <- function(repos,
                                agg_by_repo = TRUE,
                                agg_by_user = TRUE,
                                token = gh::gh_token(),
                                sort_rows = TRUE,
                                verbose = TRUE){ 
    
    repos_url <- login <- avatar_url <- name <- contributions <- NULL;
    #### Initialize variables ####
    agg_repos <- NULL; agg_users <- NULL; 
    ##### Add contributors #####
    ## People who supplied code but didn't necessarily make the repo  
    contributors <- github_metadata(repos = repos, 
                                    field = "contributors_url",
                                    token = token, 
                                    verbose = verbose)
    contributors[,repos_url:=unlist(repos_url)]
    contributors[,login:=unlist(login)]
    contributors[,contributions:=unlist(contributions)]
    contributors[,avatar_url:=unlist(avatar_url)]
    #### Agg by repo ####
    if(isTRUE(agg_by_repo)){ 
        agg_repos <- dplyr::group_by(contributors, name) |>  
            dplyr::summarise(contributors_total=dplyr::n_distinct(login),
                             contributors=list(stats::setNames(contributions,
                                                               login))
            ) |> 
            data.table::data.table()
        if(isTRUE(sort_rows)) {
            agg_repos <- agg_repos[
                order(agg_repos$contributors_total,decreasing = TRUE),
            ]
        } 
    }
    #### Agg by user ####
    if(agg_by_user){
        agg_users <- dplyr::group_by(contributors, login, avatar_url) |>  
            dplyr::summarise(
                .groups = "keep",
                n_repos=dplyr::n_distinct(name), 
                contributions_total=sum(contributions,na.rm = TRUE),
                contributions_by_repo=list(stats::setNames(contributions,
                                                           name))
            ) |> 
            data.table::data.table()
        if(isTRUE(sort_rows)) {
            agg_users <- agg_users[
                order(agg_users$contributions_total,decreasing = TRUE),
            ]
        } 
    } 
    return(list(contributors=contributors,
                agg_repos=agg_repos,
                agg_users=agg_users))
}
