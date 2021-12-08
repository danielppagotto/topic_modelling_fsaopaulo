#--------------------------------Pacotes necessarios

library(anytime)
library(tidyverse)
library(rvest)
library(tidytext)
library(dplyr)
library(topicmodels)
library(tm)
library(reshape2)
library(ggplot2)
library(stringr)

#---------------------------------------------------

setwd("~/GitHub/topic_modelling_fsaopaulo")

Base_limpa_teste <- readxl::read_excel("Base_limpa_teste.xlsx", 
                               col_types = c("text", "date", "text", 
                                             "text", "text"))

# Preprocessamento 
# criando funcao para retirar acentos 

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}


# Retirando NA e reportagens repetidas

Base_Folha_SP <-Base_limpa_teste %>%
  filter(titulos_sp != "NA") %>%
  distinct(titulos_sp, .keep_all = TRUE)


# Limpando o corpus 

Base_Folha_SP$corpos_sp_limpo <- tolower(Base_Folha_SP$corpos_sp)
Base_Folha_SP$corpos_sp_limpo <- removeNumbers(Base_Folha_SP$corpos_sp_limpo)
Base_Folha_SP$corpos_sp_limpo <- stripWhitespace(Base_Folha_SP$corpos_sp_limpo)
Base_Folha_SP$corpos_sp_limpo <- removePunctuation(Base_Folha_SP$corpos_sp_limpo)

sw_pt_tm <- tm::stopwords("pt") %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
minhas_sw <- c("facebook", "icon", "linkedin", "voltar", "social", "leia mais", 
               "ic_share", "icone fechar", "ic_save", "mais", "voltar", "ver novamente",
               "comparilhe", "icone facebook", "facebook", "icone whatsapp", "whatsapp",
               "icone twitter", "twitter", "icone de messenger", "messenger", "icone de envelope",
               "envelope", "e-mail", "linkedin", "icone de linkedin", "icone", 
               "linkcadeado", "representando", "link", "copiar", "icsave", "icshare", 
               "email", "r", "pode", "vai", "compartilhe", ' "" ' , "colunaobituagrupofolhacombr",
               "ser", "coisa","fazer", "acho", "ter", "pra","mim", "vou", "gente","ícone",
             "diz", "ainda", "nesta", "neste", "sido", "coluna", "afirma", "disse", 
             "faria", "clique", "chegou", "tudo", "sobre")

sw_pt <- c(minhas_sw, sw_pt_tm)

Base_Folha_SP$corpos_sp_limpo <- rm_accent(Base_Folha_SP$corpos_sp_limpo)
Base_Folha_SP$corpos_sp_limpo <- removeWords(Base_Folha_SP$corpos_sp_limpo, sw_pt)

#Realizando exploracao geral por contagem

folha_modelagem <- Base_Folha_SP %>%
  select(titulos_sp, corpos_sp_limpo)%>%
  unnest_tokens("word", corpos_sp_limpo, token = "ngrams", n = 1) 
  
df_contagem <- folha_modelagem %>%
  count(word) %>% 
  arrange(desc(n))



# Separando o df em 4 -----------------------------------------------------

base1 <- Base_Folha_SP %>% 
            filter(datas_folha < "2020-03-01")

base2 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2020-03-01" & datas_folha < "2020-10-01")

base3 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2020-10-01" & datas_folha < "2021-02-01")

base4 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2020-10-01" & datas_folha < "2021-02-01")

base5 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2021-02-01" & datas_folha < "2021-06-21")

base6 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2021-06-21")



# Aplicando procedimentos para base 1 -------------------------------------

# Criando matriz dos termos

proc1 <- stm::textProcessor(base1$corpos_sp_limpo, metadata = base1, language = "portuguese",
                           customstopwords = sw_pt, stem = TRUE)


out1 <- stm::prepDocuments(proc1$documents, proc1$vocab, proc1$meta,
                          lower.thresh = 10)


storage1 <- stm::searchK(out1$documents, out1$vocab, K = c(3:10),
                        data = out1$meta)


fit1 <- stm::stm(documents = out1$documents, vocab = out1$vocab, data = out1$meta,  K = 5,
  max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit1, "summary")


stm::labelTopics(fit1)

# Aplicando procedimentos para base 2 -------------------------------------

# Criando matriz dos termos

proc2 <- stm::textProcessor(base2$corpos_sp_limpo, metadata = base2, language = "portuguese",
                            customstopwords = sw_pt, stem = TRUE)


out2 <- stm::prepDocuments(proc2$documents, proc2$vocab, proc2$meta,
                           lower.thresh = 20)


storage2 <- stm::searchK(out2$documents, out2$vocab, K = c(3:10),
                         data = out2$meta)


fit2 <- stm::stm(documents = out2$documents, vocab = out2$vocab, data = out2$meta,  K = 5,
                 max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit2, "summary")


stm::labelTopics(fit2)

# Aplicando procedimentos para base 3 -------------------------------------

# Criando matriz dos termos

proc3 <- stm::textProcessor(base3$corpos_sp_limpo, metadata = base3, language = "portuguese",
                            customstopwords = sw_pt, stem = TRUE)


out3 <- stm::prepDocuments(proc3$documents, proc3$vocab, proc3$meta,
                           lower.thresh = 20)


storage3 <- stm::searchK(out3$documents, out3$vocab, K = c(3:10),
                         data = out3$meta)


fit3 <- stm::stm(documents = out3$documents, vocab = out3$vocab, data = out3$meta,  K = 5,
                 max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit3, "summary")


stm::labelTopics(fit3)


# base 4 ------------------------------------------------------------------

# Criando matriz dos termos

proc4 <- stm::textProcessor(base4$corpos_sp_limpo, metadata = base4, language = "portuguese",
                            customstopwords = sw_pt, stem = TRUE)


out4 <- stm::prepDocuments(proc4$documents, proc4$vocab, proc4$meta,
                           lower.thresh = 20)


storage4 <- stm::searchK(out4$documents, out4$vocab, K = c(3:10),
                         data = out4$meta)


fit4 <- stm::stm(documents = out4$documents, vocab = out4$vocab, data = out4$meta,  K = 5,
                 max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit4, "summary")


stm::labelTopics(fit4)


# Base 5 ------------------------------------------------------------------

# Aplicando procedimentos para base 5 -------------------------------------

# Criando matriz dos termos

proc5 <- stm::textProcessor(base5$corpos_sp_limpo, metadata = base5, language = "portuguese",
                            customstopwords = sw_pt, stem = TRUE)


out5 <- stm::prepDocuments(proc5$documents, proc5$vocab, proc5$meta,
                           lower.thresh = 20)


storage5 <- stm::searchK(out5$documents, out5$vocab, K = c(3:10),
                         data = out5$meta)


fit5 <- stm::stm(documents = out5$documents, vocab = out5$vocab, data = out5$meta,  K = 5,
                 max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit5, "summary")


stm::labelTopics(fit5)
