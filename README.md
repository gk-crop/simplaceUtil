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

Show only elements that depend on sim component SlimWater

Filtering graph

```{r}
new_graph <-simplaceUtil::getLinkingFromComponent(graph,"SlimWater")
DiagrammeR::render_graph(new_graph)
```

## Show solution information

Extract information from Solution to data.frames

```
elements <- getElementsFromSolutionFile("f:/java/simplace/CompleteP.sol.xml")
elements$components
elements$variables
elements$links
```