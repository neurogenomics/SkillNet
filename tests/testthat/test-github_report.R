test_that("github_report works", {
    
    org <- "neurogenomics"
    report_small <- github_report(org = org, 
                                  add_issues = FALSE, 
                                  add_comments = FALSE, 
                                  add_contributors = FALSE)
    testthat::expect_equal(report_small$org,org)
    testthat::expect_equal(nrow(report_small$organization),1)
    testthat::expect_gte(nrow(report_small$repos),50)  
})
