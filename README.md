# conclust
This package is designed to enable researchers to quickly and efficiently generate customized sets of keywords. It is currently in the early alpha stage of development.

## Installation

devtools::install_github("pchest/conclust")

## Usage

### Creating a cosimilarity matrix from a word embeddings model

library(conclust)

simmat <- similarity_matrix(wordemb_FasttextEng_sample, words = "words")

### Extracting a semantically-related set of keywords from a cosimilarity matrix

seed_months <- c("October", "November")

out_months <- conclust(simmat, seed_words = seed_months, max_n = 10)

out_months |> 
  terms() |>
  head(n = 10)
