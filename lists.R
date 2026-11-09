# R/lists.R — internal word lists (not exported)
# Pure data. Every non-ASCII character is expressed as a \uxxxx escape,
# as required by R CMD check. The rendered strings are unchanged.

.list_artigos <- c(
  "o", "a", "os", "as", "um", "uma", "uns", "umas",
  "ao", "\u00e0", "aos", "\u00e0s", "do", "da", "dos", "das",
  "no", "na", "nos", "nas", "pelo", "pela", "pelos", "pelas"
)

.list_preposicoes <- c(
  "de", "da", "do", "das", "dos", "em", "na", "no", "nas", "nos",
  "por", "pelo", "pela", "pelos", "pelas", "a", "ao", "\u00e0",
  "\u00e0s", "para", "com", "sem", "sobre", "entre", "contra",
  "perante", "ap\u00f3s", "at\u00e9", "desde", "sob", "tr\u00e1s"
)

.list_conjuncoes <- c(
  "e", "ou", "mas", "por\u00e9m", "entretanto", "contudo", "nem",
  "tampouco", "que", "porque", "se", "quando", "enquanto", "embora",
  "apesar de", "como", "assim que", "desde que", "a fim de que",
  "para que", "caso"
)

.list_adv_lugar <- c(
  "aqui", "a\u00ed", "ali", "l\u00e1", "acol\u00e1", "c\u00e1",
  "dentro", "fora", "em cima", "embaixo", "perto", "longe",
  "adiante", "atr\u00e1s", "ao lado", "em frente", "debaixo"
)

.list_adv_tempo <- c(
  "agora", "hoje", "ontem", "amanh\u00e3", "cedo", "tarde", "sempre",
  "nunca", "j\u00e1", "logo", "depois", "antes", "enquanto", "ainda"
)

.list_adv_modo <- c(
  "bem", "mal", "assim", "depressa", "devagar", "calmamente",
  "rapidamente", "cuidadosamente", "facilmente", "dificilmente"
)

.list_adv_grau <- c(
  "muito", "pouco", "mais", "menos", "tanto", "t\u00e3o", "demais",
  "bastante", "quase", "completamente"
)

.list_adv_afirmacao <- c(
  "sim", "certamente", "realmente", "efetivamente", "decididamente"
)

.list_adv_negacao <- c("n\u00e3o", "nunca", "jamais", "de modo algum")

.list_adv_duvida <- c("talvez", "possivelmente", "provavelmente", "qui\u00e7\u00e1")

.list_prn_pessoais <- c(
  "a", "as", "comigo", "conosco", "consigo", "contigo", "convosco",
  "ela", "elas", "ele", "eles", "eu", "lhe", "lhes", "me", "mim",
  "nos", "n\u00f3s", "o", "os", "se", "si", "te", "ti", "tu", "vos",
  "v\u00f3s"
)

.list_prn_tratamento <- c(
  "senhor", "senhora", "senhorita", "voc\u00ea",
  "vossa alteza", "vossa emin\u00eancia", "vossa excel\u00eancia",
  "vossa magnific\u00eancia", "vossa majestade",
  "vossa majestade imperial", "vossa merc\u00ea",
  "vossa onipot\u00eancia", "vossa paternidade",
  "vossa reverend\u00edssima", "vossa santidade",
  "vossa senhoria", "sr.", "sr.\u00aa", "srta.", "v.", "v. a.",
  "v. em.\u00aa", "v. ex.\u00aa", "v. m.", "v. m. i.",
  "v. mag.\u00aa", "v. rev.m\u00aa", "v. s.", "v. s.\u00aa",
  "v.m.c\u00ea", "v.p"
)

.list_prn_diretos <- c("me", "te", "o", "a", "nos", "vos", "os", "as")

.list_prn_indiretos <- c("me", "te", "lhe", "nos", "vos", "lhes")

.list_prn_possessivos <- c(
  "meu", "minha", "meus", "minhas",
  "teu", "tua", "teus", "tuas",
  "seu", "sua", "seus", "suas",
  "nosso", "nossa", "nossos", "nossas",
  "vosso", "vossa", "vossos", "vossas"
)

.list_prn_demonstrativos <- c(
  "este", "esta", "estes", "estas",
  "esse", "essa", "esses", "essas",
  "aquele", "aquela", "aqueles", "aquelas",
  "isto", "isso", "aquilo",
  "pr\u00f3pria", "pr\u00f3prias", "pr\u00f3prio", "pr\u00f3prios",
  "semelhante", "semelhantes",
  "tais", "tal", "mesma", "mesmas", "mesmo", "mesmos"
)

.list_prn_relativos <- c(
  "que", "quem", "onde", "cujo", "cuja", "cujos", "cujas",
  "quanto", "quanta", "quantos", "quantas", "aonde"
)

.list_prn_interrogativos <- c(
  "quem", "que", "qual", "quais", "quanto", "quanta", "quantos", "quantas"
)

.list_prn_indefinidos <- c(
  "algu\u00e9m", "ningu\u00e9m", "tudo", "nada",
  "algum", "alguma", "alguns", "algumas",
  "nenhum", "nenhuma", "nenhuns", "nenhumas",
  "todo", "toda", "todos", "todas",
  "outro", "outra", "outros", "outras",
  "muito", "muita", "muitos", "muitas",
  "pouco", "pouca", "poucos", "poucas",
  "certo", "certa", "certos", "certas",
  "v\u00e1rio", "v\u00e1ria", "v\u00e1rios", "v\u00e1rias",
  "tanto", "tanta", "tantos", "tantas",
  "quanto", "quanta", "quantos", "quantas",
  "qualquer", "quaisquer", "cada",
  "um", "uma", "uns", "umas"
)

# --- pre-computed unions used by the aggregate functions -------------------

.all_adverbios <- unique(c(
  .list_adv_lugar, .list_adv_tempo, .list_adv_modo,
  .list_adv_grau,  .list_adv_afirmacao, .list_adv_negacao,
  .list_adv_duvida
))

.all_pronomes <- unique(c(
  .list_prn_pessoais, .list_prn_tratamento, .list_prn_diretos,
  .list_prn_indiretos, .list_prn_possessivos, .list_prn_demonstrativos,
  .list_prn_relativos, .list_prn_interrogativos, .list_prn_indefinidos
))

.all_agorapare <- unique(c(
  .all_adverbios, .all_pronomes,
  .list_artigos, .list_preposicoes, .list_conjuncoes
))
