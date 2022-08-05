#' GitHub network
#' 
#' Construct a network from a GitHub Organization's 
#' repositories and contributors.
#' @param org GitHub organization name.
#' @param add_issues Extract metadata from Issues within each GitHub repo.
#' @param add_comments Extract metadata from Comments within each GitHub Issue.
#' @param add_contributors Extract metadata about which users 
#' have contributed to which repos. 
#' @param token GitHub authentication token. 
#' See \link[gh]{gh} documentation for more details.
#' @param verbose Print messages. 
#' 
#' @returns igraph object
#' @source 
#' \href{https://docs.github.com/en/rest/repos#get}{GitHub API: Repositories}
#' \href{https://docs.github.com/en/rest/projects/cards}{GitHub API: Projects (classic)}
#' \href{https://docs.github.com/en/enterprise-cloud\\@latest/issues/planning-and-tracking-with-projects/automating-your-project/using-the-api-to-manage-projects}{GitHub API: ProjectsV2}
#' \href{https://github.com/ChadGoymer/githapi}{
#' Non-functional GH package with similar objective}
#' @export
#' @importFrom gh gh_token
#' @importFrom igraph graph_from_data_frame
#' @examples 
#' g <- github_network(org = "neurogenomics")
github_network <- function(org,
                           report = NULL,
                           add_contributors = TRUE,
                           add_issues = FALSE,
                           add_comments = FALSE,
                           token = gh::gh_token(),
                           verbose = TRUE){
    if(is.null(report)){
        report <- github_report(org = org, 
                                token = token,
                                add_contributors = add_contributors,
                                add_issues = add_issues,
                                add_comments = add_comments,
                                verbose = verbose)  
    } 
    #### Prepare vertices #####
    vertices <- github_network_vertices(report = report)
    edges <- github_network_edges(report = report)  
    #### Prepare relations data #####
    g <- igraph::graph_from_data_frame(
        d = edges,
        vertices = vertices,
        directed = TRUE)  
    return(list(report=report,
                graph=g))
}
