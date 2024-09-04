
# stopwordsbr

<!-- badges: start -->
<!-- badges: end -->

This package makes the removal of Brazilian Portuguese words easier from corpuses. Users can remove stopwords by grammatical class according to need or all stopwords at once.

## Installation

You can install the development version of stopwordsbr like so:

``` r
# #install.packages("remotes")
remotes::install_github("motherofdog/stopwordsbr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stopwordsbr)
## basic example code
#'text_in_portuguesebr <- "Ao  verme que primeiro roeu as frias carnes do meu cadáver dedico como  saudosa lembrança estas Memórias Póstumas. Dito isto, expirei às duas horas da tarde de uma sexta-feira do mês de agosto de 1869, na minha bela chácara de Catumbi. Tinha uns sessenta e quatro anos, rijos e prósperos, era solteiro, possuía cerca de trezentos contos e fui acompanhado ao cemitério por onze amigos. Onze amigos! Verdade é que não houve cartas nem anúncios. Acresce que chovia — peneirava uma chuvinha miúda, triste e constante, tão constante e tão triste, que levou um daqueles fiéis da última hora a intercalar esta engenhosa idéia no discurso que proferiu à beira de minha cova: — “Vós, que o conhecestes, meus senhores, vós podeis dizer comigo que a natureza parece estar chorando a perda irreparável de um dos mais belos caracteres que têm honrado a humanidade. Este ar sombrio, estas gotas do céu, aquelas nuvens escuras que cobrem o azul como um crepe funéreo, tudo isso é a dor crua e má que lhe rói à Natureza as mais íntimas entranhas; tudo isso é um sublime louvor ao nosso ilustre finado.”"

#'cleanedtext_br <- artigos(text_in_portuguesebr)


#cleanedtext_br2 <- agorapare(text_in_portuguesebr)
#print(cleanedtext_br2)
```

