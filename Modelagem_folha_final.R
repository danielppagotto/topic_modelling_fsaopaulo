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

#setwd("C:/Users/adale/Desktop/Faculdade/2020.2/LAPEI/Produ√ß√£o Artigo/Programa R/base_limpeza_folha/Materiais finais")
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
  
  if(any(pattern=="«"))
    pattern[pattern=="«"] <- "Á"
  
  symbols <- c(
    acute = "·ÈÌÛ˙¡…Õ”⁄˝›",
    grave = "‡ËÏÚ˘¿»Ã“Ÿ",
    circunflex = "‚ÍÓÙ˚¬ Œ‘€",
    tilde = "„ı√’Ò—",
    umlaut = "‰ÎÔˆ¸ƒÀœ÷‹ˇ",
    cedil = "Á«"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("¥","`","^","~","®","Á")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

# Retirando NA e reportagens repetidas

Base_Folha_SP <-Base_limpa_teste %>%
  filter(titulos_sp != "NA") %>%
  distinct(titulos_sp, .keep_all = TRUE) %>% 
  filter(!str_detect(links_sp,"painelsa"))


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
               "email", "r", "pode", "vai", "compartilhe" , "colunaobituagrupofolhacombr",
               "ser", "coisa","fazer", "acho", "ter", "pra","mim", "vou", "gente",
             "diz", "ainda", "nesta", "neste", "sido", "coluna", "afirma", "disse", 
             "faria", "clique", "chegou", "tudo", "sobre", "fechar", "ano", "pessoa", "dia","vez","Ìcone",
             "instagram")

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



# Separando o df em 7 -----------------------------------------------------


base1 <- Base_Folha_SP %>% 
  filter(datas_folha > "2018-12-31" & datas_folha < "2019-07-01")

base2 <- Base_Folha_SP %>% 
  filter(datas_folha > "2019-06-30" & datas_folha < "2020-01-01")

base3 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2020-01-01" & datas_folha < "2020-03-01")

base4 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2020-03-01" & datas_folha < "2020-10-01")

base5 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2020-10-01" & datas_folha < "2021-02-01")

base6 <- Base_Folha_SP %>% 
  filter(datas_folha >= "2021-02-01" & datas_folha < "2021-06-21")

base7 <- Base_Folha_SP %>% 
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

# Base 6 ------------------------------------------------------------------

# Aplicando procedimentos para base 6 -------------------------------------

# Criando matriz dos termos

proc6 <- stm::textProcessor(base6$corpos_sp_limpo, metadata = base6, language = "portuguese",
                            customstopwords = sw_pt, stem = TRUE)


out6 <- stm::prepDocuments(proc6$documents, proc6$vocab, proc6$meta,
                           lower.thresh = 20)


storage6 <- stm::searchK(out6$documents, out6$vocab, K = c(3:10),
                         data = out6$meta)


fit6 <- stm::stm(documents = out6$documents, vocab = out6$vocab, data = out6$meta,  K = 5,
                 max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit6, "summary")


stm::labelTopics(fit6)

# Base 7 ------------------------------------------------------------------

# Aplicando procedimentos para base 7 -------------------------------------

# Criando matriz dos termos

proc7 <- stm::textProcessor(base7$corpos_sp_limpo, metadata = base7, language = "portuguese",
                            customstopwords = sw_pt, stem = TRUE)


out7 <- stm::prepDocuments(proc7$documents, proc7$vocab, proc7$meta,
                           lower.thresh = 20)


storage7 <- stm::searchK(out7$documents, out7$vocab, K = c(3:10),
                         data = out7$meta)


fit7 <- stm::stm(documents = out7$documents, vocab = out7$vocab, data = out7$meta,  K = 5,
                 max.em.its = 75, init.type = "Spectral", verbose = FALSE)


plot(fit7, "summary")


stm::labelTopics(fit7)

#-------------------------------------

# extrair a reportagem predominante e contar a frequencia

#------------------------------Base 3 - Mudar os subsequentes, uma vez que adicionamos duas bases novas (1 e 2)

head(fit1$theta)

base1 %>% 
  filter(row_number() == 1) %>% 
  select(datas_folha, titulos_sp)


nomes_topicos_1 <- c("populacao negra, feminina e periferica", "empreendedorismo de impacto, startups, sustentabilidade",
                     "operacao de PMEs","empresas e financas", "governo e poder")


# extrair a maior probabilidade pra cada reportagem

maior_prob <- apply(fit1$theta, 1, max)

# extrair o nome do topico com a maior probabilidade

topico_reportagem <- nomes_topicos_1[apply(fit1$theta, 1, which.max)]

# acrescentar esses dados no dataframe principal


#ver problema 
df_topico_1 <- base1%>% 
  mutate(maior_prob = maior_prob,
         topico = topico_reportagem)

roxo <- "mediumpurple4"

# grafico da quantidade de videos por topico


df_topico_1 %>% 
  count(topico) %>% 
  # classificar em ordem decrescente
  mutate(topico = forcats::fct_reorder(topico, n)) %>% 
  ggplot(aes(x = topico, y = n)) + 
  geom_col(fill = roxo) +
  theme_minimal() + 
  labs(x = NULL, y = "Reportagem",
       title = "Quantidade de Reportagem por topico 1",
       subtitle = "Jan/20 a Fev/20") +
  coord_flip() 



#tabela

materias_topicos1 <- df_topico_1 %>% 
  group_by(topico) %>% 
  select(datas_folha, titulos_sp, topico, maior_prob)



#------------------------------Base 2

head(fit2$theta)

base2 %>% 
  filter(row_number() == 1) %>% 
  select(datas_folha, titulos_sp)


nomes_topicos_2 <- c("vendas e connsumo: delivery, apps", "nao identificado",
                     "doacoes","premiacoes,impacto", "governo e poder")


# extrair a maior probabilidade pra cada reportagem

maior_prob <- apply(fit2$theta, 1, max)

# extrair o nome do topico com a maior probabilidade

topico_reportagem <- nomes_topicos_2[apply(fit2$theta, 1, which.max)]

# acrescentar esses dados no dataframe principal


#ver problema 
df_topico_2 <- base2%>% 
  mutate(maior_prob = maior_prob,
         topico = topico_reportagem)

roxo <- "mediumpurple4"

# grafico da quantidade de videos por topico


df_topico_2 %>% 
  count(topico) %>% 
  # classificar em ordem decrescente
  mutate(topico = forcats::fct_reorder(topico, n)) %>% 
  ggplot(aes(x = topico, y = n)) + 
  geom_col(fill = roxo) +
  theme_minimal() + 
  labs(x = NULL, y = "Reportagem",
       title = "Quantidade de Reportagem por topico 2",
       subtitle = "Mar/20 a Set/20") +
  coord_flip()

#tabela

materias_topicos2 <- df_topico_2 %>% 
  group_by(topico) %>% 
  select(datas_folha, titulos_sp, topico, maior_prob)

#------------------------------Base 3

head(fit3$theta)

base3 %>% 
  filter(row_number() == 1) %>% 
  select(datas_folha, titulos_sp)


nomes_topicos_3 <- c("populacao negra", "impacto da covid e empreendedorismo",
                     "nao identificado","startup, operacao de negocios", "governo e poder")


# extrair a maior probabilidade pra cada reportagem

maior_prob <- apply(fit3$theta, 1, max)

# extrair o nome do topico com a maior probabilidade

topico_reportagem <- nomes_topicos_3[apply(fit3$theta, 1, which.max)]

# acrescentar esses dados no dataframe principal


#ver problema 
df_topico_3<- base3%>% 
  mutate(maior_prob = maior_prob,
         topico = topico_reportagem)

roxo <- "mediumpurple4"

# grafico da quantidade de videos por topico


df_topico_3 %>% 
  count(topico) %>% 
  # classificar em ordem decrescente
  mutate(topico = forcats::fct_reorder(topico, n)) %>% 
  ggplot(aes(x = topico, y = n)) + 
  geom_col(fill = roxo) +
  theme_minimal() + 
  labs(x = NULL, y = "Reportagem",
       title = "Quantidade de Reportagem por topico 3",
       subtitle = "Out/21 a Jan/21") +
  coord_flip()

#tabela

materias_topicos3 <- df_topico_3 %>% 
  group_by(topico) %>% 
  select(datas_folha, titulos_sp, topico, maior_prob)

#------------------------------Base 4

head(fit4$theta)

base4 %>% 
  filter(row_number() == 1) %>% 
  select(datas_folha, titulos_sp)


nomes_topicos_4 <- c("startup, inovacao", "governo e poder",
                     "doacoes","financas e empreendedorismo", "nao identificado")


# extrair a maior probabilidade pra cada reportagem

maior_prob <- apply(fit4$theta, 1, max)

# extrair o nome do topico com a maior probabilidade

topico_reportagem <- nomes_topicos_4[apply(fit4$theta, 1, which.max)]

# acrescentar esses dados no dataframe principal


#ver problema 
df_topico_4 <- base4%>% 
  mutate(maior_prob = maior_prob,
         topico = topico_reportagem)

roxo <- "mediumpurple4"

# grafico da quantidade de videos por topico


df_topico_4 %>% 
  count(topico) %>% 
  # classificar em ordem decrescente
  mutate(topico = forcats::fct_reorder(topico, n)) %>% 
  ggplot(aes(x = topico, y = n)) + 
  geom_col(fill = roxo) +
  theme_minimal() + 
  labs(x = NULL, y = "Reportagem",
       title = "Quantidade de Reportagem por topico - 4",
       subtitle = "Fev/21 a Junho/21") +
  coord_flip()


#tabela

materias_topicos4 <- df_topico_4 %>% 
  group_by(topico) %>% 
  select(datas_folha, titulos_sp, topico, maior_prob)

#------------------------------Base 6

head(fit5$theta)

base5 %>% 
  filter(row_number() == 1) %>% 
  select(datas_folha, titulos_sp)


nomes_topicos_5 <- c("dificuldades para empreendedor", "educacao",
                     "governo e poder","populacao vulneravel", "startups de impacto")


# extrair a maior probabilidade pra cada reportagem

maior_prob <- apply(fit5$theta, 1, max)

# extrair o nome do topico com a maior probabilidade

topico_reportagem <- nomes_topicos_5[apply(fit5$theta, 1, which.max)]

# acrescentar esses dados no dataframe principal


#ver problema 
df_topico_5 <- base5%>% 
  mutate(maior_prob = maior_prob,
         topico = topico_reportagem)

roxo <- "mediumpurple4"

# grafico da quantidade de videos por topico


df_topico_5 %>% 
  count(topico) %>% 
  # classificar em ordem decrescente
  mutate(topico = forcats::fct_reorder(topico, n)) %>% 
  ggplot(aes(x = topico, y = n)) + 
  geom_col(fill = roxo) +
  theme_minimal() + 
  labs(x = NULL, y = "Reportagem",
       title = "Quantidade de Reportagem por topico - 5",
       subtitle = "Junho/21 a Set/21") +
  coord_flip()

#tabela

materias_topicos5 <- df_topico_5 %>% 
  group_by(topico) %>% 
  select(datas_folha, titulos_sp, topico, maior_prob)


