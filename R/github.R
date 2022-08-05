# https://docs.github.com/en/rest/repos#get
#### New GH Projects ####
# https://docs.github.com/en/enterprise-cloud@latest/issues/planning-and-tracking-with-projects/automating-your-project/using-the-api-to-manage-projects
#### Classic GH projects ####
# https://docs.github.com/en/rest/projects/cards
### Apears to be broken ####
# https://github.com/ChadGoymer/githapi
source("R/messager.R")

search_org <- function(org = "neurogenomics",
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
            return(to_dt(field_info))
        } else {
            return(field_info)
        } 
    } 
}

to_dt <- function(gh_response,
                  verbose = TRUE){
    if(length(gh_response)==0){
        messager("No metadata retrieved. Returning NULL.",v=verbose)
        return(NULL)
    }
    dt <- data.table::rbindlist(
        lapply(gh_response,function(x){data.frame(t(as.matrix(x)))}), 
        fill = TRUE)
    if("name" %in% names(dt)) dt[,name:=unlist(name)]
    return(dt)
}
get_metadata <- function(repos,
                         field,
                         token = gh::gh_token(),
                         verbose = TRUE){
    closeAllConnections()
    if(!field %in% colnames(repos)){
        stop("field must be a column name in repos.")
    }
    BiocParallel::bpmapply(
        stats::setNames(repos[[field]],
                        repos$name), 
        FUN=function(x){
            messager("Processing: ",x,v=verbose)
            gh_response <- gh::gh(endpoint = gsub("{/number}","",x, 
                                                  fixed = TRUE),
                                  .token = token,
                                  per_page = 100)
            to_dt(gh_response = gh_response,
                  verbose = verbose)
        }) |> data.table::rbindlist(use.names = TRUE,
                                    idcol = "name",
                                    fill = TRUE)
}
get_contributors <- function(repos,
                             agg_by_repo = FALSE,
                             agg_by_user = FALSE,
                             token = gh::gh_token(),
                             sort_rows = TRUE,
                             verbose = TRUE){
    closeAllConnections()
    ##### Add contributors #####
    ## People who supplied code but didn't necessarily make the repo  
    contributors <- get_metadata(repos = repos, 
                                 field = "contributors_url",
                                 token = token, 
                                 verbose = verbose)
    contributors[,repos_url:=unlist(repos_url)]
    contributors[,login:=unlist(login)]
    contributors[,contributions:=unlist(contributions)]
    contributors[,avatar_url:=unlist(avatar_url)]
    #### Agg by repo ####
    if(isTRUE(agg_by_repo)){ 
         repos <- dplyr::group_by(contributors, name) |>  
            dplyr::summarise(contributors_total=dplyr::n_distinct(login),
                             contributors=list(stats::setNames(contributions,login))
            ) |> 
            data.table::data.table()
         if(isTRUE(sort_rows)) {
             repos <- repos[order(repos$contributors_total,decreasing = TRUE),]
         }
         return(repos)
    }
    #### Agg by user ####
    if(agg_by_user){
        users <- dplyr::group_by(contributors, login, avatar_url) |>  
            dplyr::summarise(n_repos=dplyr::n_distinct(name), 
                             contributions_total=sum(contributions,na.rm = TRUE),
                             contributions_by_repo=list(stats::setNames(contributions,name))
            ) |> 
            data.table::data.table()
        if(isTRUE(sort_rows)) {
            users <- users[order(users$contributions_total,decreasing = TRUE),]
        }
        return(users)
    } 
    return(contributors)
}

get_issues <- function(repos, 
                       add_commentors = TRUE,
                       agg_by_repo = FALSE,
                       verbose = TRUE){ 
    #### Get Issues metadata ####
    issues <- get_metadata(repos = repos, 
                           field = "issues_url",
                           verbose = verbose) 
    issues$author <- lapply(issues$user, function(x)x$login)
    issues$assignees_login <- lapply(issues$assignees, function(x){unname(unlist(x))[grepl("login",names(unlist(x))) ] })
    if(isTRUE(add_commentors)){
        messager("Gathering comments metadata.")
        comments <- get_metadata(repos = issues, 
                                 field = "comments_url",
                                 verbose = verbose)  
        comments$author <- lapply(comments$user, function(x)x$login)
        comments[,issue_url:=unlist(issue_url)]
        comments_agg <- dplyr::group_by(comments, issue_url) |> 
            dplyr::summarise(comments_total=dplyr::n_distinct(url),
                             commentors=unique(author)) |>
            data.table::data.table() 
        issues[,url:=unlist(url)]
        issues <- data.table::merge.data.table(x = issues, 
                                               y = comments_agg[,c("issue_url","comments_total","commentors")],
                                               by.x = "url",
                                               by.y = "issue_url", all.x = TRUE) 
    }  
    #### Get everyone who is involved ####
    issues <- issues |>
        dplyr::rowwise() |>
        dplyr::mutate(involved_users = list(na.omit(unique(c(author,assignees_login,commentors))))) |>
        data.table::data.table()
    #### Aggregate to repo-level #####
    if(isTRUE(agg_by_repo)){
        cols <- c("author","assignees_login","commentors","involved_users")
        issues[,repository_url:=unlist(repository_url)]
        # dt <- data.table::copy(issues)
        # dt[ , (cols) := lapply(.SD,list), .SDcols = cols, by=repository_url]   
        repos <- dplyr::group_by(issues, repository_url) |>  
            dplyr::summarise(issues_total=dplyr::n_distinct(url),
                             comments_total=sum(comments_total, na.rm = TRUE),
                             issue_authors=list(author),
                             issue_assignees=list(assignees_login),
                             issue_commentors=list(commentors),
                             involved_users=list(involved_users)
                             ) |> 
            data.table::data.table()
        return(repos)
    } else {
        return(issues) 
    } 
}
org <- search_org(org = "neurogenomics")
repos <- search_org(org = "neurogenomics", 
                    field = "repos") 
contributors <- get_contributors(repos = repos)
contributors_agg <- get_contributors(repos = repos, 
                                     agg_by_user = TRUE)
contributors_repos <- get_contributors(repos = repos,  
                                       agg_by_repo = TRUE) 
repos_annot <- data.table::merge.data.table(repos,
                                            contributors_repos,
                                            by = "name", 
                                            all = TRUE)
contributors_annot <- data.table::merge.data.table(contributors,
                                            repos,
                                            by = "name", 
                                            all = TRUE)

#### Prepare vertices metadata #####
vertices_repos <- repos_annot[,c("name","description","html_url","topics",
               "visibility","fork","forks_count","watchers","stargazers_count",
               "language","size","license",
               "has_issues","open_issues_count","contributors_total")]
vertices_users <- (contributors_agg |> dplyr::rename(name=login))
vertices <- data.table::rbindlist(list(repo = vertices_repos, 
                                       user = vertices_users), 
                                  fill = TRUE,use.names = TRUE, 
                                  idcol = "type")
bat_url <-file.path(
    "https://github.com/RajLabMSSM",
    "Fine_Mapping/blob/master/echolocatoR",
    "images/bat_silhouette.png?raw=true")
vertices[,image:=ifelse(is.na(avatar_url),bat_url,avatar_url)]
vertices[,value:=ifelse(type=="user",10,20)]
# vertices[,value:=ifelse(type=="user",10,20)]
data.table::setcolorder(vertices,"name")

edges <- contributors_annot[,c("login","name","private","contributions")]
edges[,contributions:=as.numeric(contributions)] 
edges[,private:=unlist(private)]
edges <- edges[(!is.na(login) & !is.na(name)),]
vnames <- unique(c(edges$login, edges$name))
vnames[!vnames %in% vertices$name]
 
#### Prepare relations data #####
g <- igraph::graph_from_data_frame(
    d = edges,
    vertices = vertices,
    directed = TRUE)  
# g2 <- igraph::induced_subgraph(graph = g, vids = igraph::V(g)[all_pkgs])
# igraph::V(g)
# igraph::E(g)

igraph::V(g)$value <- ifelse(igraph::V(g)$type=="user",20,40)
igraph::V(g)$title <- paste(
    paste0("<strong>",igraph::V(g)$type,"</strong>: ",
           igraph::V(g)$name
    )
)
# igraph::V(g2)$Version <- meta[names(igraph::V(g2)),]$Version
# igraph::V(g2)$value <- ifelse(names(igraph::V(g2))==pkg_name, 40, 30)
# igraph::V(g2)$group <- ifelse(igraph::V(g2)==pkg_name, 'y', 'n')
# igraph::V(g2)$outputs <- igraph::degree(g2, mode = "out")
# igraph::V(g2)$inputs <- igraph::degree(g2, mode = "in")
#  

