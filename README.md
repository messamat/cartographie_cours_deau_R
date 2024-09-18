# R code for 'Inconsistent Regulatory Mapping Quietly Threatens Rivers and Streams'
Traduction de ce README en français: [![fr](https://img.shields.io/badge/lang-fr-blue.svg)](https://github.com/messamat/cartographie_cours_deau_R/blob/master/README.FR.md)  

This repository contains R code associated with _Messager, M. L., Pella, H., & Datry, T. (2024). 
Inconsistent regulatory mapping quietly threatens rivers and streams. Environmental Science & Technology. 
https://doi.org/10.1021/acs.est.4c01859_

A copy of the accepted version (after peer-review) of this article is available under a CC-BY-NC license
at: https://hal.inrae.fr/hal-04700243  
A French translation of the article is also available at: https://hal.inrae.fr/hal-04699448

## Abstract
Even the most stringent environmental law cannot  protect a river if its tributaries remain exposed to pollution and other
threats upstream. Excluding a subset of watercourses from legal protection therefore threatens to alter freshwater ecosystems across 
entire river networks and the services they provide, such as drinking water and flood regulation. 
Considerable attention has been devoted to defining the scope of environmental laws protecting watercourses. 
Yet how these definitions are implemented through regulatory mapping, the cartography of waterbodies that legally
qualify as watercourses and are thus protected, has not been examined outside of the United States.
Here, we demonstrate the consequences of regulatory mapping on the extent of river networks that are protected, 
using France as a case study. By assembling the first map of France’s watercourses protected under the Water Law, 
we estimate that a quarter of previously mapped hydrographic segments were excluded from protection and found stark 
geographical variations in the extent of protected ecosystems. Headwater and nonperennial segments are disproportionately 
excluded by 28% compared to their prevalence (67%) in the overall hydrographic network, with potentially far-reaching 
implications for biodiversity and people. We expect regulatory frameworks in most countries to be equally susceptible to 
local interpretation of legal definitions.

## Introduction

This repository includes the portions of the analysis conducted in R, which relies on spatially pre-formatted files.
This analysis workflow needs to be conducted after running the Python code in the 
following repository: https://github.com/messamat/cartographie_cours_deau. 

These scripts are annotated but could be challenging to follow. If you encounter any trouble, please don't hesitate
to contact Mathis L. Messager for comments and clarifications by email or to log an issue in github.

Files needed to run this analysis are either downloaded directly from this or the Python code or were provided by Directions
Departementales des Territoires. Please reach out to Mathis L. Messager for assistance with obtaining these data.

## Analysis structure and underlying data

This analysis relies as much as possible on [good enough practices in scientific computing](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510), which users are encouraged to read.

**Structure**: the overall project directory is structured with the following sub-directories:  
data/ (raw data, read-only, not to be altered)  
results/ (results of the analysis, mostly reproduceable through code execution. However, also includes manually modified results)
src/ (code written for the project)  
|---- cartographie_cours_deau_R (R project/source code for analysis in R)  

All scripts rely on this structure.

**R Workflow**: this project is setup with a [targets workflow](https://docs.ropensci.org/targets/), ensuring reproducibility.
In the `targets` philosophy, every action is a function, and every R object resulting from a workflow step is a "target" with dependencies.
Intermediate targets/objects are stored in a `_targets` directory. 

**Dependency management**: the R library of this project is managed by [renv](https://rstudio.github.io/renv/articles/renv.html).
This makes sure that the exact same package versions are used when recreating the project.
When calling `renv::restore()`, all required packages will be installed with their specific version. 
Please note that this project was built with R version 4.4 on a Windows 10 operating system.

**Syntax**: this analysis relies on the [data.table](https://rdatatable.gitlab.io/data.table/) syntax, which provides a high-performance version of data.frame. It is concise, faster, and more memory efficient than conventional data.frames and the tidyverse syntax.

## Getting started
### Download the repository for R
In Git Bash, the following commands illustrate the procedure to make a local copy of the Github repository in a newly created directory at 
C://cartographie_cours_deau_R/src :

```{r, engine = 'bash', eval = FALSE}
Mathis@DESKTOP MINGW64 /c/cartographie_cours_deau/src
$ git clone https://github.com/messamat/cartographie_cours_deau_R.git
```

In R Studio for Windows, the following procedure can be used:  

* Click on “File” in the menu ribbon  
* Select “New project…”  
* Choose the “Version control” option in the New Project Wizard window.
* Then, select “Git” in the next window.
* In the next window, fill the fields as follows:  
  * Repository URL: https://github.com/messamat/cartographie_cours_deau_R
  * Project directory name: [will autofill as “cartographie_cours_deau_R”]  
  * Create project as subdirectory of: [choose the parent directory of src]  
* Tick “Open in new session” and then click “Create project”.  


### Github repository structure
- [**R/**](https://github.com/messamat/cartographie_cours_deau_R/tree/main/R) — core of the analysis
  - [*functions.R*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/R/functions.R) - all custom functions used in the data formatting and analysis. 
  - [*packages.R*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/R/packages.R) - all packages used in the workflow.
- [*.Rprofile*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/.Rprofile) — used to activate renv for new R sessions launched in the project.
- [*cartographie_cours_deau_R.Rproj*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/GeneticScaling.Rproj) — R project file.
- [*LICENSE*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/LICENSE) - terms of use, modification and sharing for this software.
- [*README.md*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/README.md) — README for Github (this file)
- [*\_targets.R*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/_targets.R) — configuration script for targets workflow,  this specific file name is required by the targets package. Contains the targets “plan”, the high-level catalog of all the steps in the workflow (see the corresponding chapter in the targets user manual). This plan defines the order of functions to use, their inputs and outputs (usually, targets), and the relationship among targets and steps.
- [*renv.lock*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/renv.lock) — renv lockfile, describing the state of the project’s library (installed packages and their version).
- [*report_20231230.qmd*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/report_20231230.qmd)  — report file in quarto format summarizing data and information to support manuscript writing

## Running the analysis
Provided that your were given the necessary data, the entire analysis can simply be re-run with the following code found in 
```{r rmake, eval = FALSE}
source('_targets.R')
tar_make()
```
`tar_make()` is the central function of the targets approach. It runs all the steps of the workflow in the correct order, skipping any work that is already up to date. Because of how targets tracks global functions and objects as dependencies of targets, the use of `tar_make()`  is needed to run the analysis pipeline in a clean reproducible environment. If all targets are up to date in the caching directory, then nothing will be run.

## Inspecting results
If you were provided intermediate targets (i.e., a `_targets/` directory; or once you have re-run the analysis), you can load individual targets in the environment with the following commands (even if the targets are not up to date due to e.g. a change in source path). 
``` {r loadtarg, eval = FALSE}
tar_load(vulnerable_waters_analysis_tabs) #Load target in memory (R environment) with original target name as variable name 
vulnerable_waters_analysis_tabs <- tar_read(vulnerable_waters_analysis_tabs) #Load target in memory with new variable name
```
