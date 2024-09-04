#'artigos
#'
#'Removes Brazilian Portuguese articles from text.
#'
#'Includes definite and indefinite articles, contractions with prepositions,
#'feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese articles.
#'@export
#'
artigos <- function(text_in_portuguesebr) {
  artigos <- "\\b(o|a|os|as|um|uma|uns|umas|ao|à|aos|às|do|da|dos|das|no|na|nos|nas|pelo|pela|pelos|pelas)\\b"
  clean_textbr <- gsub(artigos, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}


#'preposicoes
#'
#'Removes Brazilian Portuguese prepositions from text.
#'
#'Includes masculine and feminine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese prepositions.
#'
#'@export
#'
preposicoes <- function(text_in_portuguesebr) {
  preposicoes <- "\\b(de|da|do|das|dos|em|na|no|nas|nos|por|pelo|pela|pelos|pelas|a|ao|à|às|para|com|sem|sobre|entre|contra|perante|após|até|desde|sob|trás)\\b"
  clean_textbr <- gsub(preposicoes, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'conjuncoes
#'
#'Removes Brazilian Portuguese conjunctions from text.
#'
#'Includes full expressions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese conjunctions.
#'
#'@export
conjuncoes <- function(text_in_portuguesebr) {
  conjuncoes <- "\\b(e|ou|mas|porém|entretanto|contudo|nem|tampouco|que|porque|se|quando|enquanto|embora|apesar de|como|assim que|desde que|a fim de que|para que|caso)\\b"
  clean_textbr <- gsub(conjuncoes, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'adv_lugar
#'
#' Removes Brazilian Portuguese place adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese place adverbs.
#' @export
adv_lugar <- function(text_in_portuguesebr) {
  adverbs_lugar <- "\\b(aqui|aí|ali|lá|acolá|cá|dentro|fora|em cima|embaixo|perto|longe|adiante|atrás|ao lado|em frente|debaixo)\\b"
  clean_textbr <- gsub(adverbs_lugar, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'adv_tempo
#'
#' Removes Brazilian Portuguese most common temporal adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese most common temporal adverbs.
#' @export
adv_tempo <- function(text_in_portuguesebr) {
  adverbs_tempo <- "\\b(agora|hoje|ontem|amanhã|cedo|tarde|sempre|nunca|já|logo|depois|antes|enquanto|ainda)\\b"
  clean_textbr <- gsub(adverbs_tempo, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}
#' @export

#'adv_modo
#'
#' Removes Brazilian Portuguese most common mode adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese most common mode adverbs.
#' @export
adv_modo <- function(text_in_portuguesebr) {
  adverbs_modo <- "\\b(bem|mal|assim|depressa|devagar|calmamente|rapidamente|cuidadosamente|facilmente|dificilmente)\\b"
  clean_textbr <- gsub(adverbs_modo, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'adv_grau
#'
#' Removes Brazilian Portuguese most common quantity adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese most common quantity adverbs.
#' @export
adv_grau <- function(text_in_portuguesebr) {
  adverbs_grau <- "\\b(muito|pouco|mais|menos|tanto|tão|demais|bastante|quase|completamente)\\b"
  clean_textbr <- gsub(adverbs_grau, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'adv_afirmacao
#'
#' Removes Brazilian Portuguese most common affirmation adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese most common affirmation adverbs.
#' @export
adv_afirmacao <- function(text_in_portuguesebr) {
  adverbs_afirmacao <- "\\b(sim|certamente|realmente|efetivamente|decididamente)\\b"
  clean_textbr <- gsub(adverbs_afirmacao, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'adv_negacao
#'
#' Removes Brazilian Portuguese most common negation adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese most common negation adverbs.
#' @export
adv_negacao <- function(text_in_portuguesebr) {
  adverbs_negacao <- "\\b(não|nunca|jamais|de modo algum)\\b"
  clean_textbr <- gsub(adverbs_negacao, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'adv_duvida
#'
#' Removes Brazilian Portuguese most common doubt adverbs from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese most common doubt adverbs.
#' @export
adv_duvida <- function(text_in_portuguesebr) {
  adverbs_duvida <- "\\b(talvez|possivelmente|provavelmente|quiçá)\\b"
  clean_textbr <- gsub(adverbs_duvida, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}


#'todos_adverbios
#'
#' Removes Brazilian Portuguese all adverbs included in other functions from text.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without most common Brazilian Portuguese adverbs.
#' @export
todos_adverbios <- function(text_in_portuguesebr) {
  adverbios <- c("aqui", "aí", "ali", "lá", "acolá", "cá", "dentro", "fora", "em cima",
                 "embaixo", "perto", "longe", "adiante", "atrás", "ao lado", "em frente",
                 "debaixo", "agora", "hoje", "ontem", "amanhã", "cedo", "tarde", "sempre",
                 "nunca", "já", "logo", "depois", "antes", "enquanto", "ainda", "bem",
                 "mal", "assim", "depressa", "devagar", "calmamente", "rapidamente",
                 "cuidadosamente", "facilmente", "dificilmente", "muito", "pouco", "mais",
                 "menos", "tanto", "tão", "demais", "bastante", "quase", "completamente",
                 "sim", "certamente", "realmente", "efetivamente", "decididamente", "não",
                 "nunca", "jamais", "de modo algum", "talvez", "possivelmente",
                 "provavelmente", "quiçá")

  pattern <- paste0("\\b(", paste(adverbios, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#' Removes Brazilian Portuguese personal pronouns from text.
#'
#'Includes reflexive, feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#' @return Complete text without Brazilian Portuguese personal pronouns.
#' @export
prn_pessoais <- function(text_in_portuguesebr) {
  pronomes_pessoais <- c("a", "as", "comigo", "conosco", "consigo", "contigo",
                         "convosco", "ela", "elas", "ele", "eles", "eu",
                         "lhe", "lhes", "me", "mim", "nos", "nós", "o", "os",
                         "se", "si", "te", "ti", "tu", "vos", "vós")
  pattern <- paste0("\\b(", paste(pronomes_pessoais, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_tratamento
#'
#'Removes Brazilian Portuguese title pronouns from text.
#'
#'Includes feminine and masculine versions and abbreviations.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese title pronouns.
#'@export
prn_tratamento <- function(text_in_portuguesebr) {
  pronomes_tratamento <- c("senhor", "senhora", "senhorita", "você", "vossa alteza",
                           "vossa eminência", "vossa excelência", "vossa magnificência",
                           "vossa majestade", "vossa majestade imperial", "vossa mercê",
                           "vossa onipotência", "vossa paternidade", "vossa reverendíssima",
                           "vossa santidade", "vossa senhoria", "sr.", "sr.ª", "srta.",
                           "v.", "v. a.", "v. em.ª", "v. ex.ª", "v. m.", "v. m. i.",
                           "v. mag.ª", "v. rev.mª", "v. s.", "v. s.ª", "v.m.cê", "v.p")
  pattern <- paste0("\\b(", paste(pronomes_tratamento, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_diretos
#'
#'Removes Brazilian Portuguese direct pronouns from text.
#'
#'Includes reflexive, feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese direct pronouns.
#'@export
prn_diretos <- function(text_in_portuguesebr) {
  pronomes_diretos <- c("me", "te", "o", "a", "nos", "vos", "os", "as")
  pattern <- paste0("\\b(", paste(pronomes_diretos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_indiretos
#'
#'Removes Brazilian Portuguese indirect pronouns from text.
#'
#'Includes reflexive, feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese indirect pronouns.
#'@export
prn_indiretos <- function(text_in_portuguesebr) {
  pronomes_indiretos <- c("me", "te", "lhe", "nos", "vos", "lhes")
  pattern <- paste0("\\b(", paste(pronomes_indiretos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_possessivos
#'
#'Removes Brazilian Portuguese possessive pronouns from text.
#'
#'Includes feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese possessive pronouns.
#'@export
prn_possessivos <- function(text_in_portuguesebr) {
  pronomes_possessivos <- c("meu", "minha", "meus", "minhas", "teu", "tua", "teus", "tuas", "seu", "sua", "seus", "suas", "nosso", "nossa", "nossos", "nossas", "vosso", "vossa", "vossos", "vossas")
  pattern <- paste0("\\b(", paste(pronomes_possessivos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_demonstrativos
#'
#'Removes Brazilian Portuguese demonstrative pronouns from text.
#'
#'Includes feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese demonstrative pronouns.
#'@export
prn_demonstrativos <- function(text_in_portuguesebr) {
  pronomes_demonstrativos <- c("este", "esta", "estes", "estas", "esse", "essa", "esses", "essas", "aquele", "aquela", "aqueles", "aquelas", "isto", "isso", "aquilo",
                               "própria", "próprias", "próprio", "próprios", "semelhante", "semelhantes",
                               "tais", "tal", "mesma", "mesmas", "mesmo", "mesmos")
  pattern <- paste0("\\b(", paste(pronomes_demonstrativos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}


#'prn_relativos
#'
#'Removes Brazilian Portuguese relative pronouns from text.
#'
#'Includes feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese relative pronouns.
#'@export
prn_relativos <- function(text_in_portuguesebr) {
  pronomes_relativos <- c("que", "quem", "onde", "cujo", "cuja", "cujos", "cujas",
                          "quanto", "quanta", "quantos", "quantas", "onde", "aonde")
  pattern <- paste0("\\b(", paste(pronomes_relativos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_interrogativos
#'
#'Removes Brazilian Portuguese interrogative pronouns from text.
#'
#'Includes feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese personal pronouns.
#'@export
prn_interrogativos <- function(text_in_portuguesebr) {
  pronomes_interrogativos <- c("quem", "que", "qual", "quais", "quanto", "quanta", "quantos", "quantas")
  pattern <- paste0("\\b(", paste(pronomes_interrogativos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'prn_indefinidos
#'
#'Removes Brazilian Portuguese undefined pronouns from text.
#'
#'Includes feminine and masculine and singular and plural versions.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese undefined pronouns.
#'@export
prn_indefinidos <- function(text_in_portuguesebr) {
  pronomes_indefinidos <- c("alguém", "ninguém", "tudo", "nada", "algum", "alguma", "alguns",
                            "algumas", "nenhum", "nenhuma", "nenhuns", "nenhumas", "todo", "toda", "todos",
                            "todas", "outro", "outra", "outros", "outras", "muito", "muita", "muitos", "muitas",
                            "pouco", "pouca", "poucos", "poucas", "certo", "certa", "certos", "certas", "vário",
                            "vária", "vários", "várias", "tanto", "tanta", "tantos", "tantas", "quanto", "quanta",
                            "quantos", "quantas", "qualquer", "quaisquer", "cada", "um", "uma", "uns", "umas")
  pattern <- paste0("\\b(", paste(pronomes_indefinidos, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'todos_pronomes
#'
#'Removes all Brazilian Portuguese pronouns from text.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese pronouns.
#'@export
todos_pronomes <- function(text_in_portuguesebr) {
  pronomes <- c("alguém", "ninguém", "tudo", "nada", "algum", "alguma", "alguns",
                "algumas", "nenhum", "nenhuma", "nenhuns", "nenhumas", "todo", "toda", "todos",
                "todas", "outro", "outra", "outros", "outras", "muito", "muita", "muitos", "muitas",
                "pouco", "pouca", "poucos", "poucas", "certo", "certa", "certos", "certas", "vário",
                "vária", "vários", "várias", "tanto", "tanta", "tantos", "tantas", "quanto", "quanta",
                "quantos", "quantas", "qualquer", "quaisquer", "cada", "um", "uma", "uns", "umas",
                "quem", "que", "qual", "quais", "quanto", "quanta", "quantos", "quantas",
                "que", "quem", "onde", "cujo", "cuja", "cujos", "cujas", "quanto", "quanta",
                "quantos", "quantas", "onde", "aonde", "este", "esta", "estes", "estas", "esse",
                "essa", "esses", "essas", "aquele", "aquela", "aqueles", "aquelas", "isto", "isso",
                "aquilo", "própria", "próprias", "próprio", "próprios", "semelhante", "semelhantes",
                "tais", "tal", "mesma", "mesmas", "mesmo", "mesmos", "meu", "minha", "meus", "minhas",
                "teu", "tua", "teus", "tuas", "seu", "sua", "seus", "suas", "nosso", "nossa", "nossos",
                "nossas", "vosso", "vossa", "vossos", "vossas", "me", "te", "o", "a", "nos", "vos",
                "os", "as", "senhor", "senhora", "senhorita", "você", "vossa alteza", "vossa eminência",
                "vossa excelência", "vossa magnificência", "vossa majestade", "vossa majestade imperial",
                "vossa mercê", "vossa onipotência", "vossa paternidade", "vossa reverendíssima",
                "vossa santidade", "vossa senhoria", "sr.", "sr.ª", "srta.", "v.", "v. a.", "v. em.ª",
                "v. ex.ª", "v. m.", "v. m. i.", "v. mag.ª", "v. rev.mª", "v. s.", "v. s.ª", "v.m.cê", "v.p",
                "a", "as", "comigo", "conosco", "consigo", "contigo", "convosco", "ela", "elas", "ele",
                "eles", "eu", "lhe", "lhes", "me", "mim", "nos", "nós", "o", "os", "se", "si", "te", "ti",
                "tu", "vos", "vós")
  pattern <- paste0("\\b(", paste(pronomes, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}

#'agorapare
#'
#'Removes all Brazilian Portuguese stopwords included in other functions from text.
#'
#'@param A text file imported into R.
#'
#'@return Complete text without Brazilian Portuguese stopwords included in other functions.
#'@export
agorapare <- function(text_in_portuguesebr) {
  palavras <- c("aqui", "aí", "ali", "lá", "acolá", "cá", "dentro", "fora", "em cima",
                "embaixo", "perto", "longe", "adiante", "atrás", "ao lado", "em frente",
                "debaixo", "agora", "hoje", "ontem", "amanhã", "cedo", "tarde", "sempre",
                "nunca", "já", "logo", "depois", "antes", "enquanto", "ainda", "bem",
                "mal", "assim", "depressa", "devagar", "calmamente", "rapidamente",
                "cuidadosamente", "facilmente", "dificilmente", "muito", "pouco", "mais",
                "menos", "tanto", "tão", "demais", "bastante", "quase", "completamente",
                "sim", "certamente", "realmente", "efetivamente", "decididamente", "não",
                "nunca", "jamais", "de modo algum", "talvez", "possivelmente",
                "provavelmente", "quiçá", "alguém", "ninguém", "tudo", "nada", "algum", "alguma", "alguns",
                "algumas", "nenhum", "nenhuma", "nenhuns", "nenhumas", "todo", "toda", "todos",
                "todas", "outro", "outra", "outros", "outras", "muito", "muita", "muitos", "muitas",
                "pouco", "pouca", "poucos", "poucas", "certo", "certa", "certos", "certas", "vário",
                "vária", "vários", "várias", "tanto", "tanta", "tantos", "tantas", "quanto", "quanta",
                "quantos", "quantas", "qualquer", "quaisquer", "cada", "um", "uma", "uns", "umas",
                "quem", "que", "qual", "quais", "quanto", "quanta", "quantos", "quantas",
                "que", "quem", "onde", "cujo", "cuja", "cujos", "cujas", "quanto", "quanta",
                "quantos", "quantas", "onde", "aonde", "este", "esta", "estes", "estas", "esse",
                "essa", "esses", "essas", "aquele", "aquela", "aqueles", "aquelas", "isto", "isso",
                "aquilo", "própria", "próprias", "próprio", "próprios", "semelhante", "semelhantes",
                "tais", "tal", "mesma", "mesmas", "mesmo", "mesmos", "meu", "minha", "meus", "minhas",
                "teu", "tua", "teus", "tuas", "seu", "sua", "seus", "suas", "nosso", "nossa", "nossos",
                "nossas", "vosso", "vossa", "vossos", "vossas", "me", "te", "o", "a", "nos", "vos",
                "os", "as", "senhor", "senhora", "senhorita", "você", "vossa alteza", "vossa eminência",
                "vossa excelência", "vossa magnificência", "vossa majestade", "vossa majestade imperial",
                "vossa mercê", "vossa onipotência", "vossa paternidade", "vossa reverendíssima",
                "vossa santidade", "vossa senhoria", "sr.", "sr.ª", "srta.", "v.", "v. a.", "v. em.ª",
                "v. ex.ª", "v. m.", "v. m. i.", "v. mag.ª", "v. rev.mª", "v. s.", "v. s.ª", "v.m.cê", "v.p",
                "a", "as", "comigo", "conosco", "consigo", "contigo", "convosco", "ela", "elas", "ele",
                "eles", "eu", "lhe", "lhes", "me", "mim", "nos", "nós", "o", "os", "se", "si", "te", "ti",
                "tu", "vos", "vós", "o", "a", "os", "as", "um", "uma", "uns", "umas", "ao", "à", "aos", "às",
                "do", "da", "dos", "das", "no", "na", "nos", "nas", "pelo", "pela", "pelos", "pelas", "de",
                "da", "do", "das", "dos", "em", "na", "no", "nas", "nos", "por", "pelo", "pela", "pelos",
                "pelas", "a", "ao", "à", "às", "para", "com", "sem", "sobre", "entre", "contra", "perante",
                "após", "até", "desde", "sob", "trás", "e", "ou", "mas", "porém", "entretanto", "contudo",
                "nem", "tampouco", "que", "porque", "se", "quando", "enquanto", "embora", "apesar de",
                "como", "assim que", "desde que", "a fim de que", "para que", "caso")

  pattern <- paste0("\\b(", paste(palavras, collapse = "|"), ")\\b")
  clean_textbr <- gsub(pattern, "", text_in_portuguesebr, ignore.case = TRUE)
  return(clean_textbr)
}


