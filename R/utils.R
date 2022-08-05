prepare_mesh <- function(){
    # https://www.bioconductor.org/packages/release/bioc/vignettes/meshr/inst/doc/MeSH.html
    ah <- AnnotationHub::AnnotationHub()
    # dbfile <- AnnotationHub::query(ah, c("MeSHDb", "MeSH.db"))[[2]]
    dbfile <- AnnotationHub::query(ah, c("MeSH","MeSHDb"))
    dbfile2 <- AnnotationHub::query(ah, c("AH14150"))
    
    file <- dbfile[["AH100375"]]
    dt <- data.table::fread(file)
    
}