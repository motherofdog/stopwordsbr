# R/dispatch.R — class-aware dispatch (internal, not exported)

remove_stopwords_ <- function(x, words, ...) UseMethod("remove_stopwords_")

# --- character: current behaviour, single pre-compiled regex --------------
#' @export
remove_stopwords_.character <- function(x, words, ...) {
  words <- unique(words[nzchar(words)])
  if (!length(words)) return(x)
  pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
  gsub(pattern, "", x, ignore.case = TRUE, ...)
}

# --- quanteda tokens: single words + multi-word phrases -------------------
#' @export
remove_stopwords_.tokens <- function(x, words, ...) {
  words <- unique(words[nzchar(words)])
  if (!length(words)) return(x)

  multi  <- words[grepl("\\s", words)]
  single <- setdiff(words, multi)

  if (length(single)) {
    x <- quanteda::tokens_remove(x, pattern = single,
                                 valuetype = "fixed", ...)
  }
  if (length(multi)) {
    x <- quanteda::tokens_remove(x, pattern = quanteda::phrase(multi), ...)
  }
  x
}

# --- quanteda corpus: round-trip through tokens, keep docvars -------------
#' @export
remove_stopwords_.corpus <- function(x, words, ...) {
  toks <- quanteda::tokens(x)
  toks <- remove_stopwords_(toks, words, ...)
  quanteda::corpus(toks)
}

# --- quanteda dfm: bonus --------------------------------------------------
#' @export
remove_stopwords_.dfm <- function(x, words, ...) {
  quanteda::dfm_remove(x, pattern = unique(words), ...)
}

# --- anything else: loud, explicit failure --------------------------------
#' @export
remove_stopwords_.default <- function(x, words, ...) {
  stop(
    "stopwordsbr does not know how to handle an object of class: ",
    paste(class(x), collapse = "/"),
    ". Supported classes: character, tokens, corpus, dfm.",
    call. = FALSE
  )
}
