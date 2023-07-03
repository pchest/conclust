#' Sample similarity matrix generated from the pre-trained English fastText model
#'
#' @description This similarity matrix has 2,000 rows and 2,000 columns. Each row
#' has numeric values that are the cosine similarity between that word
#' and the word of each corresponding column
#'
#' @format A 2,000 row and 2,000 column cosine similarity matrix
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

#' Sample from the pre-trained English fastText model
#'
#' @description This is a data frame containing the 2,000 most frequently occurring
#' terms from Facebook's English-language fastText word embeddings model.
#'
#' @format A 2000 row and 301 column data frame. The row represents the word embedding
#' term, while the numeric columns represent the word embedding dimension. The character
#' column gives the terms associated with each word vector.
#'
#' @examples
#' \donttest{
#' data(fastText_vec_eng_sample)
#' }
#' @references P. Bojanowski*, E. Grave*, A. Joulin, T. Mikolov,
#' Enriching Word Vectors with Subword Information
#' (\href{https://arxiv.org/abs/1607.04606}{arxiv})
#'
#' @keywords data
"fastText_vec_eng_sample"
