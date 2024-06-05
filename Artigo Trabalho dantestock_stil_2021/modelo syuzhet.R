# 1) limpeza do texto
# 2) lematização
# 3) classificação de acordo com o syuzhet
# 4) análise dos resultados

# 1)
library(readr)
library(tm)
library(textmineR)


lemma_dic <- read.delim(file = "https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE)
names(lemma_dic) <- c("stem", "term")


for (j in 1:length(palavras)){
  comparacao <- palavras[j] == lemma_dic$term
  if (sum(comparacao) == 1){
    palavras[j] <- as.character(lemma_dic$stem[comparacao])
  } else {
    palavras[j] <- palavras[j]
  }
}