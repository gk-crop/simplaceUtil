#' Creates a graph from component and links dataframe
#'
#' @param comp components dataframe
#' @param links links dataframe
#' @param showinterfaces if TRUE, include interfaces in graph
#' @export
#' @importFrom rlang .data
componentsToGraph <- function(comp, links, showinterfaces=FALSE)
{


  if(!showinterfaces)
  {
    comp <- comp[comp$type!="interface",]
  }

  shapes <- c(var="oval",resource="egg", simcomponent="rectangle",output="triangle",interface="triangle")
  ranks <- c(interface=0,var=1,resource=2, simcomponent=3,output=4)
  fills <- c(var="white",resource="#ffff99cc",alias="#ffff99cc",transform="#ffdd88cc", mgm="#ff9999cc",simple="#ff6666cc", normal="#ff3333cc",grouped="#ff1111cc", output="#999999cc")


  nodes <- DiagrammeR::create_node_df(n = nrow(comp), label=comp$id, shape=shapes[comp$type],
                                      type = comp$type,
                                      subtype = comp$subtype,
                                      fillcolor=fills[comp$subtype],fontcolor="black",
                                      width=.6+pmax(0,nchar(comp$id)-10)/20,
                                      height=.2,
                                      fontsize=6,
                                      penwidth=.1,
                                      color="#999999cc",
                                      tooltip=comp$ref)

  edf <- links |>
    dplyr::filter(!is.na(.data$from) & !is.na(.data$to)) |>
    dplyr::group_by(.data$from,.data$to,.data$rel) |>
    dplyr::summarise( count=dplyr::n(), .groups="drop") |>
    dplyr::left_join( nodes |> dplyr::select(to_id=.data$id,.data$label), by=c("to"="label")) |>
    dplyr::left_join( nodes |> dplyr::select(from_id=.data$id,.data$label), by=c("from"="label")) |>
    dplyr::filter(!is.na(.data$from_id) & !is.na(.data$to_id)) |>
    dplyr::mutate(color=dplyr::if_else(.data$to_id-.data$from_id < 0,"red","blue"))



  edgeheads <- c(rule = "curve", value="normal", key="diamond", data="tee", storage="box")

  edges <- DiagrammeR::create_edge_df(from=edf$from_id, to=edf$to_id, rel=edf$rel,
                                      penwidth=.5+log(edf$count)*.3,color=edf$color,
                                      arrowhead = edgeheads[edf$rel],
                                      arrowtail="tee",
                                      minlen=2,
                                      style="solid",
                                      arrowsize=.5,
                                      tooltip = edf$rel)
  graph <- DiagrammeR::create_graph(nodes_df=nodes, edges_df = edges)
  graph
}




#' Creates a graph from a solution
#'
#' @param file solution file
#' @param showinterfaces if true include interfaces
#' @export
solutionToGraph <- function(file,showinterfaces=FALSE)
{
  cp <- getElementsFromSolutionFile(file)
  componentsToGraph(cp$components, cp$links,showinterfaces)

}

#' Selects edges of specific timestep
#'
#' @param graph graph
#' @param linkage all steps, same step or previous step
#' @export
#' @importFrom rlang .data
filterEdges <- function(graph, linkage="allsteps") {

  if(linkage=="prevstep") {
    edf <- graph$edges_df |>
      dplyr::filter(.data$color=="red")
  }
  else if(linkage=="samestep") {
    edf <-graph$edges_df |>
      dplyr::filter(.data$color=="blue")
  }
  else {
    edf <- graph$edges_df
  }
  DiagrammeR::create_graph(graph$nodes_df,edf)
}


#' Get the simcomponent in the neighborhood of given component
#'
#' @param graph the solution graph
#' @param name id of the sim component
#' @param distance maximum number of steps from given component
#' @param linkage all steps, same step or previous step
#' @export
#' @importFrom rlang .data
getNeighborhood <- function(graph, name, distance=1, linkage="allsteps")
{
  graph <- filterEdges(graph, linkage)

  nodeid <- graph |>
    DiagrammeR::select_nodes(conditions = .data$label==name) |>
    DiagrammeR::get_selection()

  graph |>
    DiagrammeR::select_nodes_in_neighborhood(node = nodeid, distance=distance) |>
    DiagrammeR::invert_selection() |>
    DiagrammeR::delete_nodes_ws()
}

#' @importFrom rlang .data
getConnected <- function(graph, name, distance = 50, linkage = "allsteps", dir=1)
{

  fun <- if(dir==1) DiagrammeR::trav_in else DiagrammeR::trav_out

  graph <- filterEdges(graph, linkage)


  nodes <- graph |>
    DiagrammeR::select_nodes(conditions = .data$label==name) |>
    DiagrammeR::get_selection()

  l <- 0
  dist <- 0
  while (length(nodes)>l && dist<distance) {
    l <- length(nodes)
    dist <- dist + 1
    nodes <- graph |>
      DiagrammeR::select_nodes_by_id(nodes) |>
      fun(add_to_selection = TRUE) |>
      DiagrammeR::get_selection()
  }
  graph |>
    DiagrammeR::select_nodes_by_id(nodes) |>
    DiagrammeR::invert_selection() |>
    DiagrammeR::delete_nodes_ws()
}

#' Get components that link to given component
#'
#' @param graph graph
#' @param name name of component
#' @param distance maximum distance from given component
#' @param linkage all steps, same step or previous step
#' @export
getLinkingToComponent <- function(graph, name, distance = 50, linkage = "allsteps")
{
  getConnected(graph, name, distance, linkage, dir=1)
}

#' Get components that are linked from given component
#'
#' @param graph graph
#' @param name name of component
#' @param distance maximum distance from given component
#' @param linkage all steps, same step or previous step
#' @export

getLinkingFromComponent <- function(graph, name, distance = 50, linkage = "allsteps")
{
  getConnected(graph, name, distance, linkage, dir=-1)
}