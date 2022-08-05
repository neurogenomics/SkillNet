github_network <- function(subgraph,
                           shape = c("image", "hexagon"),
                           image =
                               file.path(
                                   "https://github.com/RajLabMSSM",
                                   "Fine_Mapping/blob/master/echolocatoR",
                                   "images/bat_silhouette.png?raw=true"),
                           layout = echodeps::layout_star,
                           show_plot = list(r=TRUE,
                                            browser=TRUE),
                           save_path = NULL,
                           width = NULL,
                           height = NULL,
                           background = "white",
                           verbose = TRUE){
    
    
    requireNamespace("visNetwork")
    requireNamespace("igraph")
   
    vis <- visNetwork::visIgraph(g) |>
        # layout(pkg_name = pkg_name) |>
        visNetwork::visNodes(
            shape =  tolower(shape[1]),
            borderWidth = 2,
            # image = "image",#image,
            labelHighlightBold = TRUE,
            color = list(
                background =  "#25355c",
                border = "#41c6c8",
                highlight = "#56ffff",
                hover=list(background="blue",
                           border="#41c6c8")
            ),
            font = list(color="white",
                        size=20,
                        face="Tahoma",
                        strokeWidth=10,
                        strokeColor="rgba(103,115,141,.5)"),
            shadow = list(enabled = TRUE,
                          size = 40,
                          color="#537bcb") # "#03b1f0"
        ) |>
        visNetwork::visEdges(
            arrows = 'from',
            shadow = list(enabled=TRUE,color="#686ea6"),
            smooth = TRUE,dashes =FALSE,
            width = 2,
            color = list(color = "#56ffff",
                         opacity=.75,
                         highlight = "#686ea6"),
        ) |>
        # visNetwork::visOptions(nodesIdSelection = list(enabled = FALSE,
        #                                                selected=pkg_name,
        #                                                main="select package"),
        #                        highlightNearest=TRUE,
        #                        width = width,
        #                        height = height) |>
        visNetwork::visInteraction(
            tooltipStyle =paste(
                "position: fixed",
                "visibility: hidden",
                "font-family: Tahoma",
                "background-color: rgba(0,0,0,.5)",
                "box-shadow: 2px 2px 2px 3px rgba(247, 247, 247, 0.5)",
                "color: #fff",
                "padding: 10px",
                sep=";"))
    #### Save ####
    if(!is.null(save_path)) {
        message("Saving dependency graph plot ==> ",save_path)
        visNetwork::visSave(graph = vis,
                            file = save_path,
                            background = background,
                            selfcontained = TRUE)
    }
    #### Show ####
    if(isTRUE(show_plot$r)) {
        messager("Showing plot in R.",v=verbose)
        print(vis)
    }
    if(isTRUE(show_plot$browser) && file.exists(save_path)){
        messager("Showing plot in browser.",v=verbose)
        utils::browseURL(save_path)
    }                                                                                                    shadow = list(enabled = TRUE, color = "#686ea6"), smooth = TRUE, 
                                                                                                                             
}
