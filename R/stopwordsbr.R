# R/stopwordsbr.R — public functions (thin wrappers around the dispatcher)

#' Remove Brazilian Portuguese articles
#'
#' Removes definite and indefinite articles (including contractions with
#' prepositions) from Brazilian Portuguese text.
#'
#' @param text_in_portuguesebr A character vector, or a \pkg{quanteda}
#'   \code{tokens}, \code{corpus}, or \code{dfm} object. Other classes raise
#'   an error.
#' @param ... Forwarded to the removal engine. For character input this is
#'   \code{gsub()} (\code{ignore.case} defaults to \code{TRUE}); for quanteda
#'   input it is \code{tokens_remove()} / \code{dfm_remove()} (e.g.
#'   \code{padding}, \code{case_insensitive}).
#'
#' @return An object of the same class as the input, with articles removed.
#' @export
artigos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_artigos, ...)
}

#' Remove Brazilian Portuguese prepositions
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without prepositions.
#' @export
preposicoes <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_preposicoes, ...)
}

#' Remove Brazilian Portuguese conjunctions
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without conjunctions.
#' @export
conjuncoes <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_conjuncoes, ...)
}

#' Remove Brazilian Portuguese place adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without place adverbs.
#' @export
adv_lugar <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_lugar, ...)
}

#' Remove Brazilian Portuguese temporal adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without temporal adverbs.
#' @export
adv_tempo <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_tempo, ...)
}

#' Remove Brazilian Portuguese mode adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without mode adverbs.
#' @export
adv_modo <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_modo, ...)
}

#' Remove Brazilian Portuguese quantity adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without quantity adverbs.
#' @export
adv_grau <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_grau, ...)
}

#' Remove Brazilian Portuguese affirmation adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without affirmation adverbs.
#' @export
adv_afirmacao <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_afirmacao, ...)
}

#' Remove Brazilian Portuguese negation adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without negation adverbs.
#' @export
adv_negacao <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_negacao, ...)
}

#' Remove Brazilian Portuguese doubt adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without doubt adverbs.
#' @export
adv_duvida <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_adv_duvida, ...)
}

#' Remove all Brazilian Portuguese adverbs
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without adverbs.
#' @export
todos_adverbios <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .all_adverbios, ...)
}

#' Remove Brazilian Portuguese personal pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without personal pronouns.
#' @export
prn_pessoais <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_pessoais, ...)
}

#' Remove Brazilian Portuguese title / treatment pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without title pronouns.
#' @export
prn_tratamento <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_tratamento, ...)
}

#' Remove Brazilian Portuguese direct pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without direct pronouns.
#' @export
prn_diretos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_diretos, ...)
}

#' Remove Brazilian Portuguese indirect pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without indirect pronouns.
#' @export
prn_indiretos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_indiretos, ...)
}

#' Remove Brazilian Portuguese possessive pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without possessive pronouns.
#' @export
prn_possessivos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_possessivos, ...)
}

#' Remove Brazilian Portuguese demonstrative pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without demonstrative pronouns.
#' @export
prn_demonstrativos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_demonstrativos, ...)
}

#' Remove Brazilian Portuguese relative pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without relative pronouns.
#' @export
prn_relativos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_relativos, ...)
}

#' Remove Brazilian Portuguese interrogative pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without interrogative pronouns.
#' @export
prn_interrogativos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_interrogativos, ...)
}

#' Remove Brazilian Portuguese indefinite pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without indefinite pronouns.
#' @export
prn_indefinidos <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .list_prn_indefinidos, ...)
}

#' Remove all Brazilian Portuguese pronouns
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without pronouns.
#' @export
todos_pronomes <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .all_pronomes, ...)
}

#' Remove all Brazilian Portuguese stopwords covered by this package
#'
#' Convenience shortcut that removes every category in one call: articles,
#' prepositions, conjunctions, all adverbs, and all pronouns.
#'
#' @inheritParams artigos
#' @return An object of the same class as the input, without any of the
#'   package's Brazilian Portuguese stopwords.
#' @export
agorapare <- function(text_in_portuguesebr, ...) {
  remove_stopwords_(text_in_portuguesebr, .all_agorapare, ...)
}
