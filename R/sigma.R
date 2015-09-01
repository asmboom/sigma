#' An htmlwidget wrapper for the sigmajs graph visualization library.
#' 
#' @param gexf GEXF (Graph Exchange XML Format) data file.
#' @param drawEdges logical, whether or not to draw the edges
#' @param drawNodes logical, whether or not to draw the nodes
#' @param width the width of the graph in pixels
#' @param height the height of the graph in pixels
#' @import htmlwidgets
#' 
#' @source sigmajs was developed by  ALEXIS JACOMY with the help of  GUILLAUME PLIQUE. See \url{http://sigmajs.org/} 
#' @examples 
#' data <- system.file("examples/ediaspora.gexf.xml", package = "sigma")
#' sigma(data)
#' @export
sigma <- function(gexf, drawEdges = TRUE, drawNodes = TRUE,
                  width = NULL, height = NULL) {
  
  # read the gexf file
  data <- paste(readLines(gexf), collapse="\n")
  
  # create a list that contains the settings
  settings <- list(
    drawEdges = drawEdges,
    drawNodes = drawNodes
  )
  
  # pass the data and settings using 'x'
  x <- list(
    data = data,
    settings = settings
  )
  
  # create the widget
  htmlwidgets::createWidget("sigma", x, width = width, height = height)
}

#' @export
sigmaOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "sigma", width, height, package = "sigma")
}

#' @export
renderSigma <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
}