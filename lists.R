# R/lists.R — internal word lists (not exported)

.list_artigos <- c(
  "o", "a", "os", "as", "um", "uma", "uns", "umas",
  "ao", "à", "aos", "às", "do", "da", "dos", "das",
  "no", "na", "nos", "nas", "pelo", "pela", "pelos", "pelas"
)

.list_preposicoes <- c(
  "de", "da", "do", "das", "dos", "em", "na", "no", "nas", "nos",
  "por", "pelo", "pela", "pelos", "pelas", "a", "ao", "à", "às",
  "para", "com", "sem", "sobre", "entre", "contra", "perante",
  "após", "até", "desde", "sob", "trás"
)

.list_conjuncoes <- c(
  "e", "ou", "mas", "porém", "entretanto", "contudo", "nem",
  "tampouco", "que", "porque", "se", "quando", "enquanto", "embora",
  "apesar de", "como", "assim que", "desde que", "a fim de que",
  "para que", "caso"
)

.list_adv_lugar <- c(
  "aqui", "aí", "ali", "lá", "acolá", "cá", "dentro", "fora",
  "em cima", "embaixo", "perto", "longe", "adiante", "atrás",
  "ao lado", "em frente", "debaixo"
)

.list_adv_tempo <- c(
  "agora", "hoje", "ontem", "amanhã", "cedo", "tarde", "sempre",
  "nunca", "já", "logo", "depois", "antes", "enquanto", "ainda"
)

.list_adv_modo <- c(
  "bem", "mal", "assim", "depressa", "devagar", "calmamente",
  "rapidamente", "cuidadosamente", "facilmente", "dificilmente"
)

.list_adv_grau <- c(
  "muito", "pouco", "mais", "menos", "tanto", "tão", "demais",
  "bastante", "quase", "completamente"
)

.list_adv_afirmacao <- c(
  "sim", "certamente", "realmente", "efetivamente", "decididamente"
)

.list_adv_negacao <- c("não", "nunca", "jamais", "de modo algum")

.list_adv_duvida <- c("talvez", "possivelmente", "provavelmente", "quiçá")

.list_prn_pessoais <- c(
  "a", "as", "comigo", "conosco", "consigo", "contigo", "convosco",
  "ela", "elas", "ele", "eles", "eu", "lhe", "lhes", "me", "mim",
  "nos", "nós", "o", "os", "se", "si", "te", "ti", "tu", "vos", "vós"
)

.list_prn_tratamento <- c(
  "senhor", "senhora", "senhorita", "você",
  "vossa alteza", "vossa eminência", "vossa excelência",
  "vossa magnificência", "vossa majestade", "vossa majestade imperial",
  "vossa mercê", "vossa onipotência", "vossa paternidade",
  "vossa reverendíssima", "vossa santidade", "vossa senhoria",
  "sr.", "sr.ª", "srta.", "v.", "v. a.", "v. em.ª", "v. ex.ª",
  "v. m.", "v. m. i.", "v. mag.ª", "v. rev.mª", "v. s.", "v. s.ª",
  "v.m.cê", "v.p"
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
  "própria", "próprias", "próprio", "próprios",
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
  "alguém", "ninguém", "tudo", "nada",
  "algum", "alguma", "alguns", "algumas",
  "nenhum", "nenhuma", "nenhuns", "nenhumas",
  "todo", "toda", "todos", "todas",
  "outro", "outra", "outros", "outras",
  "muito", "muita", "muitos", "muitas",
  "pouco", "pouca", "poucos", "poucas",
  "certo", "certa", "certos", "certas",
  "vário", "vária", "vários", "várias",
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
