# simplaceUtil

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
