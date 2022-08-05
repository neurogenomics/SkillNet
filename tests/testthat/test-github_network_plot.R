test_that("github_network_plot works", {
    
    g <- github_network(org = "neurogenomics",
                        add_comments = FALSE)
    vis <- github_network_plot(graph = g$graph)
    testthat::expect_true(methods::is(g$graph,"igraph"))
    testthat::expect_gte(length(g$graph),109)
    testthat::expect_true(methods::is(vis,"visNetwork"))
})
