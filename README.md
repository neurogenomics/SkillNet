<img src='https://github.com/neurogenomics/SkillNet/raw/master/inst/hex/hex.png' height='300'><br><br>
[![](https://img.shields.io/badge/devel%20version-0.99.0-black.svg)](https://github.com/neurogenomics/SkillNet)
[![R build
status](https://github.com/neurogenomics/SkillNet/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/neurogenomics/SkillNet/actions)
[![](https://img.shields.io/github/last-commit/neurogenomics/SkillNet.svg)](https://github.com/neurogenomics/SkillNet/commits/master)
[![](https://app.codecov.io/gh/neurogenomics/SkillNet/branch/master/graph/badge.svg)](https://app.codecov.io/gh/neurogenomics/SkillNet)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
<h4>
Authors: <i>Brian Schilder</i>
</h4>
<h4>
README updated: <i>Aug-05-2022</i>
</h4>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `SkillNet`: Visualize and explore your team’s skills.

### Construct and search networks from skill sets and projects

for all members of a team.

If you use `SkillNet`, please cite:

<!-- Modify this by editing the file: inst/CITATION  -->

> author1, author2, author3 (publicationYear) articleTitle,
> *journalName*; volumeNumber, [linkToPublication](linkToPublication)

## Installation

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("https://github.com/neurogenomics/SkillNet")
library(SkillNet)
```

## Quickstart

``` r
g <- SkillNet::github_network(org = "neurogenomics")
vis <- SkillNet::github_network_plot(graph = g$graph)
```

<a href='https://neurogenomics.github.io/SkillNet/inst/examples/neurogenomics.html' 
target='_blank'>
<img src='https://neurogenomics.github.io/SkillNet/inst/examples/Screenshot.png' height=400px>
</a>

> Click image above for interactive example.

## Documentation

### [Website](https://neurogenomics.github.io/SkillNet)

### [Getting started](https://neurogenomics.github.io/SkillNet/articles/SkillNet)

<hr>

## Contact

### [Neurogenomics Lab](https://www.neurogenomics.co.uk/)

UK Dementia Research Institute  
Department of Brain Sciences  
Faculty of Medicine  
Imperial College London  
[GitHub](https://github.com/neurogenomics)  
[DockerHub](https://hub.docker.com/orgs/neurogenomicslab)

<br>
