github_network_edges <- function(report){
    
    contributors_annot <- data.table::merge.data.table(report$contributors$contributors,
                                                       report$repos,
                                                       by = "name", 
                                                       all = TRUE)
    edges <- contributors_annot[,c("login","name","private","contributions")]
    edges[,contributions:=as.numeric(contributions)] 
    edges[,private:=unlist(private)]
    edges <- edges[(!is.na(login) & !is.na(name)),]
    edges[,width:=log(contributions,1.5)]
    edges[,title:=paste(
        paste0("<strong>","Contributions","</strong>: ",contributions),
        sep="<br>"
    )]  
    return(edges)
}
