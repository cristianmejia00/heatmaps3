##############################################################################
# Helping functions
##############################################################################

#' Vector normalizer.
#'
#' Normalize each keyword vector
#' @param x a numeric vector
#' @export
vector_normalizer <- function(x){
  return (x/sqrt(sum(x^2)))}

#' Transform normalized vectors to dataframes
#'
#' Normalize each keyword vector and transform it to data frame
#' @param x a data frame the first column is for the terms, the second for the value(relevance)
#' @export
get_dtm_dataframes <- function(x){
  lapply(x, function(x) {
    normalized <- vector_normalizer(x[,2])
    framed <- data.frame(t(matrix(normalized)))
    colnames(framed) <- x[,1]
    return(framed)})}

##############################################################################
# Core functions
##############################################################################
#' Get the similarity matrix
#'
#' A similarity matrix based on cosine similarity
#' @param x_axis_lodf a list of data frames. The first column is for the terms, the second for the value(relevance)
#' @param y_axis_lodf a list of data frames. The first column is for the terms, the second for the value(relevance)
#' @param x_label default to "X". a name for the values in the X axis
#' @param y_label default to "Y". a name for the values in the Y axis
#' @param report default to TRUE. whether to save the similarity matrix as .csv file
#' @export
heatmap_matrix <- function(x_axis_lodf, y_axis_lodf, x_label = "X", y_label = "Y", report = TRUE) {
  #Vectorize
  Y_AXIS_dtms <- get_dtm_dataframes(y_axis_lodf)
  X_AXIS_dtms <- get_dtm_dataframes(x_axis_lodf)
  ALL_dtms <- append(Y_AXIS_dtms, X_AXIS_dtms)

  #Replace NA with zeros
  complete_dtm <- rbind.fill(ALL_dtms)
  complete_dtm[is.na(complete_dtm)] <- 0

  #########################################################
  #Calculate the similarity matrix.
  #The output "xross" matrix have the similarity of ALL documents (topic-topic; topic-cluster; cluster-cluster)
  tdm2 <- as.TermDocumentMatrix(t(as.matrix(complete_dtm)), weighting = weightTf)
  xross <- crossprod_simple_triplet_matrix(tdm2)

  #########################################################
  #Because we are interested in the similarity matrix of topics-clusters only, we have to subset xross.
  similarity_txc <- xross[1:length(y_axis_lodf), (1+length(y_axis_lodf)):ncol(xross)]
  similarity_txc_named <- similarity_txc
  rownames(similarity_txc_named) <- sapply(1:nrow(similarity_txc), function(x) paste(y_label, as.character(x)))
  colnames(similarity_txc_named) <- sapply(1:ncol(similarity_txc), function(x) paste(x_label, as.character(x)))

  # write the matrix
  if (report == TRUE) {write.csv(similarity_txc_named, file=similarity_matrix)}

  # Output the matrix
  return(similarity_txc)
}



#' Melted similarity matrix
#'
#' Similarity matrix is transformed to a list, ordered from the highest similarity score. Top keywords are added for comparisson.
#' @param similarity_matrix an unnamed similarity matrix (usually created with \code{\link{heatmap_matrix}})
#' @param x_axis_lodf a list of data frames. The first column is for the terms, the second for the value(relevance)
#' @param y_axis_lodf a list of data frames. The first column is for the terms, the second for the value(relevance)
#' @param report default to TRUE. whether to save the similarity matrix as .csv file
#' @export
heatmap_list <- function(similarity_matrix, x_axis_lodf, y_axis_lodf, report = TRUE) {

  #transform to edges
  edges <- melt(similarity_matrix) #This transform the matrix to edges
  edges <- edges[order(-edges[,3]),]

  #Set names. As is requiered by other functions ahead
  setnames(edges, 1, "Y")
  setnames(edges, 2, "X")

  #Preparation
  #Convert top 10 keywords in a single string
  Y_keywords <- sapply(y_axis_lodf, function(x) {
    keys <- paste(x[1:10,1], collapse = ", ")
    return(keys)
  })

  X_keywords <- sapply(x_axis_lodf, function(x) {
    keys <- paste(x[1:10,1], collapse = ", ")
    return(keys)
  })

  #Attach columns with the proper info.
  #Note: programatically this is a terrible approach as we are replicating lots of data
  #that may consume memory quickly. However, it is enough when the heatmap is 100 x 100 or less.

  #Initialize columns,
  edges$keysX <- edges$keysY <- 1

  #Replace 1 with the proper information
  for (i in (1:length(y_axis_lodf))) {
    edges$keysY[which(edges$Y==i)] <- Y_keywords[i]
  }

  for (i in (1:length(x_axis_lodf))) {
    edges$keysX[which(edges$X==i)] <- X_keywords[i]
  }

  #Write report
  if (report == TRUE) { write.csv(edges, file=edge_list, row.names = F) }

  #Output
  return(edges)
}


#' Visualize the heatmap
#'
#' Plotly implementation for the similarity matrix
#' @param edges a melted similarity matrix created with \code{\link{heatmap_list}}
#' @param x_label default to "X". a name for the values in the X axis
#' @param y_label default to "Y". a name for the values in the Y axis
#' @param appears default to 1 (show all), the opacity of the chart. A vector with the opacity of each dot can also be added
#' @export
heatmap_viz <- function(edges, x_label = "X", y_label = "Y", appears = 1){
  #Plot interactive scatter plot that behaves as heatmap
  p <- plot_ly(edges, x=~X, y=~Y,
               type="scatter",
               mode="markers",
               marker = list(symbol = 1, opacity = appears),

               color = ~value,
               colors = colorRamp(c("white", "green", "red")),
               text = ~paste("Similarity: ", round(value, 3),
                             "<br>X_keywords: ", keysX,
                             "<br>Y_keywords: ", keysY))

  p <- layout(p,
              xaxis = list(categoryarray = colnames(edges),
                           categoryorder = "array",
                           title = x_label),
              yaxis = list(categoryarray = rownames(edges),
                           categoryorder = "array",
                           title = y_label))
  return(p)
}
