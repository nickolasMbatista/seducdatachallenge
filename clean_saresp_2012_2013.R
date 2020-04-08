### Bibliotecas ####

library(tidyverse)

### Download de dados ####

## SARESP 2013 ----

DADOS_SARESP2013 <- "https://dados.educacao.sp.gov.br/sites/default/files/Resultado_Geral_2013.csv"

SARESP2013 <- read_csv(DADOS_SARESP2013, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, CODESC, CODMUN, SERIE_ANO, SEXO, profic_lp, profic_mat)%>%
  mutate(CODESC = as.numeric(CODESC))

## Questionario SARESP 2013 ----

    ##Alunos 3EM

DADOS_SARESP2013_QALUNO_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_3EM.csv"

SARESP2013_QALUNO_3EM <- read_csv2(DADOS_SARESP2013_QALUNO_3EM, locale = readr::locale(encoding = "ASCII") )


    ##Alunos AF

DADOS_SARESP2013_QALUNO_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_7_9EF.csv"

SARESP2013_QALUNO_AF <- read_csv2(DADOS_SARESP2013_QALUNO_AF, locale = readr::locale(encoding = "ASCII") )

    ##Alunos AI

DADOS_SARESP2013_QALUNO_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_2_3_5EF.csv"

SARESP2013_QALUNO_AI <- read_csv2(DADOS_SARESP2013_QALUNO_AI, locale = readr::locale(encoding = "ASCII") )

    ##PAIS 3EM

DADOS_SARESP2013_QPAIS_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_3EM.csv"

SARESP2013_QPAIS_3EM <- read_csv2(DADOS_SARESP2013_QALUNO_3EM, locale = readr::locale(encoding = "ASCII") )

    ##PAIS AF

DADOS_SARESP2013_QPAIS_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_7_9EF.csv"

SARESP2013_QPAIS_AF <- read_csv2(DADOS_SARESP2013_QPAIS_AF, locale = readr::locale(encoding = "ASCII") )

    ##PAIS AI

DADOS_SARESP2013_QPAIS_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_2_3_5EF.csv"

SARESP2013_QPAIS_AI <- read_csv2(DADOS_SARESP2013_QPAIS_AI, locale = readr::locale(encoding = "ASCII"))


## SARESP 2012 ----

DADOS_SARESP2012 <- "https://dados.educacao.sp.gov.br/sites/default/files/Resultado_Geral_2012_consolidado.csv"

SARESP2012 <- read_csv(DADOS_SARESP2012, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, CODESC, CODMUN, SERIE_ANO, SEXO, profic_lp, profic_mat)%>%
  mutate(CODESC = as.numeric(CODESC))

## Questionario SARESP 2012 ----

##Alunos 3EM

DADOS_SARESP2012_QALUNO_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2012_Alunos_3EM.csv"

SARESP2012_QALUNO_3EM <- read_csv2(DADOS_SARESP2012_QALUNO_3EM, locale = readr::locale(encoding = "ASCII") )


##Alunos AF

DADOS_SARESP2012_QALUNO_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2012_Alunos_7_9EF.csv"

SARESP2012_QALUNO_AF <- read_csv2(DADOS_SARESP2012_QALUNO_AF, locale = readr::locale(encoding = "ASCII") )

##Alunos AI

DADOS_SARESP2012_QALUNO_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2012_Alunos_2_3_5EF.csv"

SARESP2012_QALUNO_AI <- read_csv2(DADOS_SARESP2012_QALUNO_AI, locale = readr::locale(encoding = "ASCII") )

##PAIS 3EM

DADOS_SARESP2012_QPAIS_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2012_Pais_3EM.csv"

SARESP2012_QPAIS_3EM <- read_csv2(DADOS_SARESP2012_QALUNO_3EM, locale = readr::locale(encoding = "ASCII") )

##PAIS AF

DADOS_SARESP2012_QPAIS_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2012_Pais_7_9EF.csv"

SARESP2012_QPAIS_AF <- read_csv2(DADOS_SARESP2012_QPAIS_AF, locale = readr::locale(encoding = "ASCII") )

##PAIS AI

DADOS_SARESP2012_QPAIS_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2012_Alunos_2_3_5EF.csv"

SARESP2012_QPAIS_AI <- read_csv2(DADOS_SARESP2012_QPAIS_AI, locale = readr::locale(encoding = "ASCII"))

