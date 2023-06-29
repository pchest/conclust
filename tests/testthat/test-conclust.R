context("test conclust")

test_that("conclust works", {
    skip_on_cran()

    seed_words = c("september", "october", "november")

    ## Test 1: conclust produces sensible output

    r1 <- conclust(sim_mat = fastText_eng_sample, seed_words = seed_words, max_n = 8, sim_thresh = .4)

    expect_true(sum(r1$Concept_lex$Term %in% tolower(month.name)) == nrow(r1$Concept_lex))

    ## Test 2: max_n

    expect_true(nrow(r1$Concept_lex) == 8)

    ## Test 3: sim_thresh

    r2 <- conclust(sim_mat = fastText_eng_sample, seed_words = seed_words, max_n = 50, sim_thresh = .7)

    expect_true(min(r2$Concept_lex$Group_similarity) >= 0.4)

})
