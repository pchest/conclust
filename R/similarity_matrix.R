#' Algorithm designed to create a cosine similarity matrix from a fitted word embedding model
#'
#' This function takes a fitted word embedding model and computes the cosine similarity between
#' each word.
#' @param x A word embedding matrix
#' @param words A vector of words that correspond to the word dimension of the fitted word embeddings
#' @keywords conclust
#' @export
#' @examples
#' \dontrun{
#' # Create a set of keywords using a pre-defined set of seeds
#' }
similarity_matrix <- function(x,
                              words = NULL) {
    UseMethod("similarity_matrix")
}

#' @export
similarity_matrix.matrix <- function(x,
                                     words = NULL){
    stop(!is.numeric(x))
    y <- x %*% t(x)
    out <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
    if(!is.null(words)) {dimnames(out) <- list(words, words)}
    return(out)
}

