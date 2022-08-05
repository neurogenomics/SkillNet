gh_to_dt <- function(gh_response,
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