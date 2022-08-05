github_issues <- function(repos, 
                          add_comments = TRUE,
                          agg_by_repo = FALSE,
                          token = gh::gh_token(),
                          verbose = TRUE){ 
    
    repository_url <- url <- issue_url <- author <- NULL;
    assignees_login <- commentors <- comments_total <- involved_users <- NULL;
    #### Intialze variable #####
    comments <- NULL; comments_agg <- NULL; 
    #### Get Issues metadata ####
    issues <- github_metadata(repos = repos, 
                              field = "issues_url",
                              token = token,
                              verbose = verbose) 
    issues[,repository_url:=unlist(repository_url)]
    issues[,url:=unlist(url)]
    issues$author <- lapply(issues$user, function(x)x$login)
    issues$assignees_login <- lapply(
        issues$assignees, 
            function(x){unname(unlist(x))[grepl("login",names(unlist(x))) ] 
        }
    )
    if(isTRUE(add_comments)){
        messager("Gathering comments metadata.",v=verbose)
        comments <- github_metadata(repos = issues, 
                                    field = "comments_url",
                                    token = token,
                                    verbose = verbose)  
        comments$author <- lapply(comments$user, function(x)x$login)
        comments[,issue_url:=unlist(issue_url)]
        comments_agg <- dplyr::group_by(comments, issue_url) |> 
            dplyr::summarise(.groups = "keep",
                             comments_total=dplyr::n_distinct(url),
                             commentors=unique(author)) |>
            data.table::data.table()  
        issues <- data.table::merge.data.table(
            x = issues, 
            y = comments_agg[,c("issue_url","comments_total","commentors")],
            by.x = "url",
            by.y = "issue_url", all.x = TRUE) 
    }  
    #### Get everyone who is involved ####
    issues <- issues |>
        dplyr::rowwise() |>
        dplyr::mutate(involved_users = list(na.omit(unique(c(author,
                                                             assignees_login,
                                                             commentors))))) |>
        data.table::data.table()
    #### Aggregate to repo-level #####
    if(isTRUE(agg_by_repo)){
        # cols <- c("author","assignees_login","commentors","involved_users")
        # dt <- data.table::copy(issues)
        # dt[ , (cols) := lapply(.SD,list), .SDcols = cols, by=repository_url]   
        comments_agg <- dplyr::group_by(issues, repository_url) |>  
            dplyr::summarise(.groups = "keep",
                             issues_total=dplyr::n_distinct(url),
                             comments_total=sum(comments_total, na.rm = TRUE),
                             issue_authors=list(author),
                             issue_assignees=list(assignees_login),
                             issue_commentors=list(commentors),
                             involved_users=list(involved_users)
            ) |> 
            data.table::data.table() 
    }
    return(list(
        issues=issues,
        comments=comments, 
        comments_agg=comments_agg
    ))
}
