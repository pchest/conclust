#' Sample similarity matrix generated from the pre-trained English fastText model
#'
#' @description This similarity matrix has 500 rows and 500 columns. Each row
#' has numeric values that are the cosine similarity between that word
#' and the word of each corresponding column
#'
#' @format A 500 row and 500 column cosine similarity matrix
#'
#' @examples
#' \donttest{
#' data(fastText_eng_sample)
#' }
#' @references P. Bojanowski*, E. Grave*, A. Joulin, T. Mikolov,
#' Enriching Word Vectors with Subword Information
#' (\href{https://arxiv.org/abs/1607.04606}{arxiv})
#'
#' @keywords data
"fastText_eng_sample"
