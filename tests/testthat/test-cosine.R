require(inline)
require(RcppArmadillo)
require(microbenchmark)
require(data.table)
require(lsa)

## extract cosine similarity between columns
cosine_base <- function(x) {
  #y <- t(x) %*% x
  y <- x %*% t(x)
  res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  return(res)
}

cosine_similarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)  # Compute dot product of vec1 and vec2
  magnitude1 <- sqrt(sum(vec1^2))  # Compute magnitude of vec1
  magnitude2 <- sqrt(sum(vec2^2))  # Compute magnitude of vec2
  similarity <- dot_product / (magnitude1 * magnitude2)  # Compute cosine similarity
  return(similarity)
}

cosine_simp <- function(x) {
    y <- matrix(NA, nrow = nrow(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
        for(j in 1:nrow(x)) {
            y[i,j] <- cosine_similarity(x[i,], x[j,])
            }}
    return(y)
}


cosineRcpp <- cxxfunction(
  signature(Xs = "matrix"),
  plugin = c("RcppArmadillo"),
  body='
    Rcpp::NumericMatrix Xr(Xs);  // creates Rcpp matrix from SEXP
    int n = Xr.nrow(), k = Xr.ncol();
    arma::mat X(Xr.begin(), n, k, false); // reuses memory and avoids extra copy
    arma::mat Y = X * arma::trans(X); // matrix product
    arma::mat res = (Y / (arma::sqrt(arma::diagvec(Y)) * arma::trans(arma::sqrt(arma::diagvec(Y)))));
    return Rcpp::wrap(res);
')

emb <- fread("/media/external/Embeddings/conclust_wv/glove_300_t.csv")
emb_mat <- emb
emb_mat$V1 <- NULL
emb_mat <- as.matrix(emb_mat)
emb_mat_sub <- emb_mat[1:1000,]

start <- Sys.time()
base_slow <- cosine_simp(emb_mat_sub)
Sys.time() - start

start <- Sys.time()
base <- cosine_base(emb_mat_sub)
Sys.time() - start

start <- Sys.time()
c_plus <- cosineRcpp(emb_mat_sub)
Sys.time() - start

start <- Sys.time()
lsa_sim <- as.matrix(lsa::cosine(t(emb_mat_sub)))
Sys.time() - start



all.equal(base_slow, c_plus)
all.equal(base, c_plus)
all.equal(lsa_sim, c_plus, check.attributes = FALSE)

dim(lsa_sim)
dim(c_plus)
str(lsa_sim)
str(c_plus)

microbenchmark(lsa::cosine(emb_mat_sub),
               cosine_base(emb_mat_sub),
               cosineRcpp(emb_mat_sub))

