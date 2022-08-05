#' GitHub network: vertices
#' 
#' Create GitHub network vertices.
#' @keywords internal
#' @importFrom stringr str_to_sentence
github_network_vertices <- function(report){
    
    repos_annot <- data.table::merge.data.table(report$repos,
                                                report$contributors$agg_repos,
                                                by = "name", 
                                                all = TRUE)
    vertices_repos <- repos_annot[,c("name","description","html_url","topics",
                                     "visibility","fork","forks_count",
                                     "watchers","stargazers_count",
                                     "language","size","license",
                                     "has_issues","open_issues_count",
                                     "contributors_total")]
    vertices_users <- (report$contributors$agg_users |>
                           dplyr::rename(name=login))
    vertices <- data.table::rbindlist(list(repo = vertices_repos, 
                                           user = vertices_users), 
                                      fill = TRUE,use.names = TRUE, 
                                      idcol = "type")
    gh_logo <-file.path(
        "https://github.com/neurogenomics/skillnet/raw/master/inst/images/GitHub-Mark-120px-plus.png")
    vertices[,image:=ifelse(is.na(avatar_url),gh_logo,avatar_url)] 
    vertices[,value:=log(ifelse(type=="user",1,unlist(size)+1),10)] 
    vertices[,link:=ifelse(type=="user",paste('https://github.com',name,sep="/"),unlist(html_url))] 
    vertices[,topics_str:=paste(unlist(topics),collapse=", "), by="name"]
    vertices[,title:=paste(
        paste0("<strong>",stringr::str_to_sentence(type),"</strong>: ",name),
        paste0("<strong>","Link","</strong>: ","<a href='",link,"'>",link,"</a>"),
        ifelse(type=="repo",
               #### Repo-specific ####
               paste(
                   paste0("<strong>","Description","</strong>: ",description),
                   paste0("<strong>","Contributors total","</strong>: ",contributors_total),
                   paste0("<strong>","Topics","</strong>: ",topics_str),
                   paste0("<strong>","Visibility","</strong>: ",unlist(visibility)),
                   paste0("<strong>","Open issues","</strong>: ",unlist(open_issues_count)),
                   paste0("<strong>","Language","</strong>: ",unlist(language)),
                   paste0("<strong>","License","</strong>: ",unlist(license)),
                   paste0("<strong>","Is fork","</strong>: ",unlist(fork)),
                   paste0("<strong>","Forks count","</strong>: ",unlist(forks_count)),
                   paste0("<strong>","Stars","</strong>: ",unlist(stargazers_count)),
                   paste0("<strong>","Watchers","</strong>: ",unlist(watchers)),
                   sep="<br>"
               ),
               #### User-specific ####
               paste(
                   paste0("<strong>","N repos","</strong>: ",n_repos),
                   paste0("<strong>","Contributions total","</strong>: ",contributions_total),
                   sep="<br>"
               )
        ),
        sep="<br>"
    ),by="name"]
    data.table::setcolorder(vertices,"name")
    return(vertices)
}
