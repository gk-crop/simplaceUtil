<!-- badges: start -->
[![simplaceUtil status badge](https://gk-crop.r-universe.dev/badges/simplaceUtil)](https://gk-crop.r-universe.dev)
[![R-CMD-check](https://github.com/gk-crop/simplaceUtil/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gk-crop/simplaceUtil/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
# simplaceUtil <img src="man/figures/logo.svg" align="right" height="139" />

Some utility functions to work with the Simplace framework.

**Notice:** This is a beta version. 

## Running the shiny app

```
simplaceUtil::runSimplaceGuiApp()
```

## Show solution graphs

Show complete graph

```
library(simplaceUtil)
graph <- solutionToGraph("/path/to/mysolution.sol.xml")
DiagrammeR::render_graph(graph)
```

## Filtering graphs

Show only elements that depend on sim component SlimWater


```
new_graph <-simplaceUtil::getLinkingFromComponent(graph,"SlimWater")
DiagrammeR::render_graph(new_graph)
```

## Show solution information

Extract information from Solution to data.frames

```
elements <- getElementsFromSolutionFile("/path/to/mysolution.sol.xml")
elements$components
elements$variables
elements$links
```

## Modify solution

A solution can be modified on the fly, e.g. for making it suitable for calibration
by replacing parameters from crop files to user defined variables and adding memory 
outputs for target variables.

```
sol <- getSolutionFromFile(solfile)
newsol <- sol |>
  removeNonMemoryOutputs() |>
  addMemoryOutput("Yields",frequence = "YEARLY") |>
  addOutputVariable("Yields","Year","CURRENT.YEAR","INT") |>
  addOutputVariable("Yields", "Yield","LintulBiomass.sWSO","DOUBLE","MAX") |>
  addUserVariable("vLUE",3.0,"DOUBLE") |>
  addUserVariable("vSLA",0.02,"DOUBLE") |>
  replaceVariable("crop.LUE","vLUE") |>
  replaceVariable("crop.SLA","vSLA") 
```

## Plot variables from  CSV output


Plot some scalar variables

```
data <- readr::read_delim("output/water.csv", delim=";")
data <- parseDate(data)
plotScalarOutput(data,"CURRENT.DATE",c("Evaporation","Transpiration"))
```

Plot a layered value (DOUBLEARRAY). Notice: give the column name for the variable 
without underscore and layer number. 

```
plotLayeredOutput(data,"RetainedWater")
```
