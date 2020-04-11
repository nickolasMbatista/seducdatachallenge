### Bibliotecas ####

library(tidyverse)

### Download de dados ####

## SARESP 2013 ----

DADOS_SARESP2013 <- "https://dados.educacao.sp.gov.br/sites/default/files/Resultado_Geral_2013.csv"

SARESP2013 <- read_csv(DADOS_SARESP2013, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, CODESC, CODMUN, SERIE_ANO, Sexo, profic_lp, profic_mat)%>%
  mutate(CODESC = as.numeric(CODESC))



## Questionario SARESP 2013 ----

##Alunos 3EM

DADOS_SARESP2013_QALUNO_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_3EM.csv"


SARESP2013_QALUNO_3EM <- read_csv2(DADOS_SARESP2013_QALUNO_3EM, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO,Q56)


##Alunos AF

DADOS_SARESP2013_QALUNO_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_7_9EF.csv"

SARESP2013_QALUNO_AF <- read_csv2(DADOS_SARESP2013_QALUNO_AF, locale = readr::locale(encoding = "ASCII") )%>%
  select(CD_ALUNO,Q59)



##Alunos AI

DADOS_SARESP2013_QALUNO_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_2_3_5EF.csv"

SARESP2013_QALUNO_AI <- read_csv2(DADOS_SARESP2013_QALUNO_AI, locale = readr::locale(encoding = "ASCII") )%>%
  select(CD_ALUNO,Q41)



##PAIS 3EM

DADOS_SARESP2013_QPAIS_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_3EM.csv"

SARESP2013_QPAIS_3EM <- read_csv2(DADOS_SARESP2013_QPAIS_3EM, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, Q2_1,Q3_1,Q3_2,Q8_F,Q23_1,Q23_2,Q23_3,Q23_4,Q23_5)


##PAIS AF

DADOS_SARESP2013_QPAIS_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_7_9EF.csv"

SARESP2013_QPAIS_AF <- read_csv2(DADOS_SARESP2013_QPAIS_AF, locale = readr::locale(encoding = "ASCII") )%>%
  select(CD_ALUNO, Q2_1,Q3_1,Q3_2,Q8_F,Q23_1,Q23_2,Q23_3,Q23_4,Q23_5)



##PAIS AI

DADOS_SARESP2013_QPAIS_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_2_3_5EF.csv"

SARESP2013_QPAIS_AI <- read_csv2(DADOS_SARESP2013_QPAIS_AI, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, Q2_1,Q3_1,Q3_2,Q8_F,Q23_1,Q23_2,Q23_3,Q23_4,Q23_5)



## BASE FINAL EM ----



SARESP2013_EM <-SARESP2013%>%
  filter(SERIE_ANO == "EM-3* sirie")%>%
  select(CD_ALUNO,CODESC,Sexo, profic_lp,profic_mat)%>%
  drop_na()


SARESP2013_EM<-SARESP2013_EM%>%  
  left_join(SARESP2013_QALUNO_3EM, by = "CD_ALUNO")%>%
  left_join(SARESP2013_QPAIS_3EM, by = "CD_ALUNO")



## BASE FINAL AF ----

SARESP2013_AF <-SARESP2013%>%
  filter(SERIE_ANO %in%c("9: Ano EF","7: Ano EF"))%>%
  select(CD_ALUNO,CODESC,Sexo, profic_lp,profic_mat)%>%
  drop_na()


SARESP2013_AF <- SARESP2013_AF%>%
  left_join(SARESP2013_QALUNO_AF, by = "CD_ALUNO")%>%
  left_join(SARESP2013_QPAIS_AF, by = "CD_ALUNO")

## BASE FINAL AI ----

SARESP2013_AI <-SARESP2013%>%
  filter(SERIE_ANO %in%c("2: Ano EF","3: Ano EF","5: Ano EF"))%>%
  select(CD_ALUNO,CODESC,Sexo, profic_lp,profic_mat)%>%
  drop_na()

SARESP2013_AI <- SARESP2013_AI %>%  
  left_join(SARESP2013_QALUNO_AI, by = "CD_ALUNO")%>%
  left_join(SARESP2013_QPAIS_AI, by = "CD_ALUNO")

