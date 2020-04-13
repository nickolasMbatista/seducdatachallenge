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
  select(CD_ALUNO,Q56)%>%
  mutate(etnia = if_else(Q56 == "A", "Branco(a)",
                         if_else(Q56 == "B", "Negro(a)",
                                 if_else(Q56 == "C", "Pardo(a)",
                                         if_else(Q56 == "D", "Oriental",
                                                 if_else(Q56 == "E","Indígena", NULL))))))%>%
  select(CD_ALUNO,etnia)


##Alunos AF

DADOS_SARESP2013_QALUNO_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_7_9EF.csv"

SARESP2013_QALUNO_AF <- read_csv2(DADOS_SARESP2013_QALUNO_AF, locale = readr::locale(encoding = "ASCII") )%>%
  select(CD_ALUNO,Q59)%>%
  select(CD_ALUNO,Q59)%>%
  mutate(etnia = if_else(Q59 == "A", "Branco(a)",
                         if_else(Q59 == "B", "Negro(a)",
                                 if_else(Q59 == "C", "Pardo(a)",
                                         if_else(Q59 == "D", "Oriental",
                                                 if_else(Q59 == "E","Indígena", NULL))))))%>%
  select(CD_ALUNO,etnia)



##Alunos AI

DADOS_SARESP2013_QALUNO_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Alunos_2_3_5EF.csv"

SARESP2013_QALUNO_AI <- read_csv2(DADOS_SARESP2013_QALUNO_AI, locale = readr::locale(encoding = "ASCII") )%>%
  select(CD_ALUNO,Q41)%>%
  mutate(etnia = if_else(Q41 == "A", "Branco(a)",
                         if_else(Q41 == "B", "Negro(a)",
                                 if_else(Q41 == "C", "Pardo(a)",
                                         if_else(Q41 == "D", "Oriental",
                                                 if_else(Q41 == "E","Indígena", NULL))))))%>%
  select(CD_ALUNO,etnia)




##PAIS 3EM

DADOS_SARESP2013_QPAIS_3EM <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_3EM.csv"

SARESP2013_QPAIS_3EM <- read_csv2(DADOS_SARESP2013_QPAIS_3EM, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, Q2_1,Q3_1,Q3_2,Q8_F,Q23_1,Q23_2,Q23_3,Q23_4,Q23_5)%>%
  
  mutate(info = if_else(Q2_1 %in% c("A","B"),1,if_else(Q2_1 %in% c("C","D"),0,NULL)),
         r_pais = if_else(Q8_F == "A",1,0),
         nota_prof = if_else(Q3_1 == "A",0,
                             if_else(Q3_1 == "B",1,
                                     if_else(Q3_1 == "C",2,
                                             if_else(Q3_1 == "D",3,
                                                     if_else(Q3_1 == "E",4,
                                                             if_else(Q3_1 == "F",5,
                                                                     if_else(Q3_1 == "G",6,
                                                                             if_else(Q3_1 == "H",7,
                                                                                     if_else(Q3_1 == "I",8,
                                                                                             if_else(Q3_1 == "J",9,
                                                                                                     if_else(Q3_1 == "K",10,NULL))))))))))),
         nota_dir = if_else(Q3_2 == "A",0,
                            if_else(Q3_2 == "B",1,
                                    if_else(Q3_2 == "C",2,
                                            if_else(Q3_2 == "D",3,
                                                    if_else(Q3_2 == "E",4,
                                                            if_else(Q3_2 == "F",5,
                                                                    if_else(Q3_2 == "G",6,
                                                                            if_else(Q3_2 == "H",7,
                                                                                    if_else(Q3_2 == "I",8,
                                                                                            if_else(Q3_2 == "J",9,
                                                                                                    if_else(Q3_2 == "K",10,NULL))))))))))),
         agua  = if_else(Q23_1 == "A",1, if_else(Q23_1 == "B",0,NULL)),
         esgoto = if_else(Q23_2 == "A",1, if_else(Q23_2 == "B",0,NULL)),
         luz = if_else(Q23_3 == "A",1, if_else(Q23_3 == "B",0,NULL)),
         gas = if_else(Q23_4 == "A",1, if_else(Q23_4 == "B",0,NULL)),
         lixo = if_else(Q23_5 == "A",1, if_else(Q23_5 == "B",0,NULL))
  )%>%
  select(CD_ALUNO,info, r_pais,nota_prof,nota_dir,agua,esgoto,luz,gas,lixo)



##PAIS AF



DADOS_SARESP2013_QPAIS_AF <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_7_9EF.csv"

SARESP2013_QPAIS_AF <- read_csv2(DADOS_SARESP2013_QPAIS_AF, locale = readr::locale(encoding = "utf-8") )%>%
  select(CD_ALUNO, Q2_1,Q3_1,Q3_2,Q8_F,Q23_1,Q23_2,Q23_3,Q23_4,Q23_5)%>%
  mutate(info = if_else(Q2_1 %in% c("A","B"),1,if_else(Q2_1 %in% c("C","D"),0,NULL)),
         r_pais = if_else(Q8_F == "A",1,0),
         nota_prof = if_else(Q3_1 == "A",0,
                             if_else(Q3_1 == "B",1,
                                     if_else(Q3_1 == "C",2,
                                             if_else(Q3_1 == "D",3,
                                                     if_else(Q3_1 == "E",4,
                                                             if_else(Q3_1 == "F",5,
                                                                      if_else(Q3_1 == "G",6,
                                                                              if_else(Q3_1 == "H",7,
                                                                                      if_else(Q3_1 == "I",8,
                                                                                              if_else(Q3_1 == "J",9,
                                                                                                      if_else(Q3_1 == "K",10,NULL))))))))))),
         nota_dir = if_else(Q3_2 == "A",0,
                             if_else(Q3_2 == "B",1,
                                     if_else(Q3_2 == "C",2,
                                             if_else(Q3_2 == "D",3,
                                                     if_else(Q3_2 == "E",4,
                                                             if_else(Q3_2 == "F",5,
                                                                     if_else(Q3_2 == "G",6,
                                                                             if_else(Q3_2 == "H",7,
                                                                                     if_else(Q3_2 == "I",8,
                                                                                             if_else(Q3_2 == "J",9,
                                                                                                     if_else(Q3_2 == "K",10,NULL))))))))))),
         agua  = if_else(Q23_1 == "A",1, if_else(Q23_1 == "B",0,NULL)),
         esgoto = if_else(Q23_2 == "A",1, if_else(Q23_2 == "B",0,NULL)),
         luz = if_else(Q23_3 == "A",1, if_else(Q23_3 == "B",0,NULL)),
         gas = if_else(Q23_4 == "A",1, if_else(Q23_4 == "B",0,NULL)),
         lixo = if_else(Q23_5 == "A",1, if_else(Q23_5 == "B",0,NULL))
         )%>%
  select(CD_ALUNO,info, r_pais,nota_prof,nota_dir,agua,esgoto,luz,gas,lixo)


##PAIS AI

DADOS_SARESP2013_QPAIS_AI <- "https://dados.educacao.sp.gov.br/sites/default/files/Saresp2013_Pais_2_3_5EF.csv"

SARESP2013_QPAIS_AI <- read_csv2(DADOS_SARESP2013_QPAIS_AI, locale = readr::locale(encoding = "ASCII"))%>%
  select(CD_ALUNO, Q2_1,Q3_1,Q3_2,Q8_F,Q23_1,Q23_2,Q23_3,Q23_4,Q23_5)%>%
    mutate(info = if_else(Q2_1 %in% c("A","B"),1,if_else(Q2_1 %in% c("C","D"),0,NULL)),
         r_pais = if_else(Q8_F == "A",1,0),
         nota_prof = if_else(Q3_1 == "A",0,
                             if_else(Q3_1 == "B",1,
                                     if_else(Q3_1 == "C",2,
                                             if_else(Q3_1 == "D",3,
                                                     if_else(Q3_1 == "E",4,
                                                             if_else(Q3_1 == "F",5,
                                                                     if_else(Q3_1 == "G",6,
                                                                             if_else(Q3_1 == "H",7,
                                                                                     if_else(Q3_1 == "I",8,
                                                                                             if_else(Q3_1 == "J",9,
                                                                                                     if_else(Q3_1 == "K",10,NULL))))))))))),
         nota_dir = if_else(Q3_2 == "A",0,
                            if_else(Q3_2 == "B",1,
                                    if_else(Q3_2 == "C",2,
                                            if_else(Q3_2 == "D",3,
                                                    if_else(Q3_2 == "E",4,
                                                            if_else(Q3_2 == "F",5,
                                                                    if_else(Q3_2 == "G",6,
                                                                            if_else(Q3_2 == "H",7,
                                                                                    if_else(Q3_2 == "I",8,
                                                                                            if_else(Q3_2 == "J",9,
                                                                                                    if_else(Q3_2 == "K",10,NULL))))))))))),
         agua  = if_else(Q23_1 == "A",1, if_else(Q23_1 == "B",0,NULL)),
         esgoto = if_else(Q23_2 == "A",1, if_else(Q23_2 == "B",0,NULL)),
         luz = if_else(Q23_3 == "A",1, if_else(Q23_3 == "B",0,NULL)),
         gas = if_else(Q23_4 == "A",1, if_else(Q23_4 == "B",0,NULL)),
         lixo = if_else(Q23_5 == "A",1, if_else(Q23_5 == "B",0,NULL))
  )%>%
  select(CD_ALUNO,info, r_pais,nota_prof,nota_dir,agua,esgoto,luz,gas,lixo)




## BASE FINAL EM ----



SARESP2013_EM <-SARESP2013%>%
  filter(SERIE_ANO == "EM-3* sirie")%>%
  select(CD_ALUNO,CODESC,Sexo, profic_lp,profic_mat)%>%
  drop_na()


SARESP2013_EM<-SARESP2013_EM%>%  
  left_join(SARESP2013_QALUNO_3EM, by = "CD_ALUNO")%>%
  left_join(SARESP2013_QPAIS_3EM, by = "CD_ALUNO")%>%
  drop_na()%>%
  fastDummies::dummy_cols(select_columns = "etnia")%>%
  write_csv2("SARESP2013_EM.csv")



## BASE FINAL AF ----

SARESP2013_AF <-SARESP2013%>%
  filter(SERIE_ANO %in%c("9: Ano EF","7: Ano EF"))%>%
  select(CD_ALUNO,CODESC,Sexo, profic_lp,profic_mat)%>%
  drop_na()


SARESP2013_AF <- SARESP2013_AF%>%
  left_join(SARESP2013_QALUNO_AF, by = "CD_ALUNO")%>%
  left_join(SARESP2013_QPAIS_AF, by = "CD_ALUNO")%>%
  drop_na()%>%
  fastDummies::dummy_cols(select_columns = "etnia")%>%
  write_csv2("SARESP2013_AF.csv")


## BASE FINAL AI ----

SARESP2013_AI <-SARESP2013%>%
  filter(SERIE_ANO %in%c("2: Ano EF","3: Ano EF","5: Ano EF"))%>%
  select(CD_ALUNO,CODESC,Sexo, profic_lp,profic_mat)%>%
  drop_na()

SARESP2013_AI <- SARESP2013_AI %>%  
  left_join(SARESP2013_QALUNO_AI, by = "CD_ALUNO")%>%
  left_join(SARESP2013_QPAIS_AI, by = "CD_ALUNO")%>%
  drop_na()%>%
  fastDummies::dummy_cols(select_columns = "etnia")%>%
  write_csv2("SARESP2013_AI.csv")



