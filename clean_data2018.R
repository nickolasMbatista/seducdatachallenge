### Bibliotecas ####

library(tidyverse)

### Download de dados ####

## Formacao por Servidor ----

DADOS_FORMACAO1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_FORMACAO_1118.csv'

FORMACAO1118 <- read_csv2(DADOS_FORMACAO1118, locale = readr::locale(encoding = "ASCII"))

AUXILIAR <- FORMACAO1118%>%
  rename(CD_ESCOLA = "CIE_ESCOLA")%>%
  select(id_interno,CD_ESCOLA,UA_EXERC)

### FORMACAO1118_PERC - Descreve o percentual de membro do QM por nível de formação

FORMACAO1118_PERC <- FORMACAO1118%>%
  group_by(CIE_ESCOLA)%>%
  filter(CIE_ESCOLA != 0, QUADRO_E == 'QM')%>%
  select(CIE_ESCOLA, NOME_UA_EXERC,QUADRO_E, NMCARGO_E,FORMACAO, id_interno)%>%
  mutate(TOTAL_QM = n())

FORMACAO1118_PERC <- FORMACAO1118_PERC%>%
  group_by(CIE_ESCOLA,FORMACAO)%>%
  mutate(perc_form = n()/TOTAL_QM)%>%
  select(CIE_ESCOLA, FORMACAO,perc_form)%>%
  distinct()%>%
  reshape2::dcast(CIE_ESCOLA  ~ FORMACAO, value.var="perc_form")%>%
  mutate_all(list(~replace_na(.,0)))%>%
  rename(EM = "ENSINO MLdDIO",
         BA_TE ="BACHARELADO/TECNLRLOGO",
         BA_TE_DO ="BACHARELADO/TECNLRLOGO + DOUTORADO",
         BA_TE_ES ="BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO",
         BA_TE_MA ="BACHARELADO/TECNLRLOGO + MESTRADO",
         BA_TE_MA_DO = "BACHARELADO/TECNLRLOGO + MESTRADO + DOUTORADO",
         ES = "ESPECIALIZAL`LDO",
         ES_MA = "ESPECIALIZAL`LDO+MESTRADO",
         LI = "LICENCIATURA",
         LI_BA_TE = "LICENCIATURA + BACHARELADO/TECNLRLOGO",
         LI_BA_TE_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + DOUTORADO",
         LI_BA_TE_ES = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO",
         LI_BA_TE_ES_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO + MESTRADO",
         LI_BA_TE_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO + DOUTORADO",
         LI_BA_TE_ES_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO+ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_BA_TE_ES_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO+ESPECIALIZAL`LDO+DOUTORADO",
         LI_DO = "LICENCIATURA + DOUTORADO",
         LI_ES = "LICENCIATURA + ESPECIALIZAL`LDO",
         LI_ES_MA = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO",
         LI_ES_MA_DO = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_ES_DO = "LICENCIATURA + ESPECIALIZAL`LDO+DOUTORADO",
         LI_MA = "LICENCIATURA + MESTRADO",
         LI_MA_DO = "LICENCIATURA + MESTRADO+DOUTORADO",
         MA = "MESTRADO",
         LI_BA_TE_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO",
         CD_ESCOLA = "CIE_ESCOLA")%>%
  mutate(PERC_DO = LI_MA_DO + LI_ES_DO + LI_ES_MA_DO + LI_DO + LI_BA_TE_ES_DO + LI_BA_TE_ES_MA_DO + LI_BA_TE_MA_DO + LI_BA_TE_DO + BA_TE_MA_DO + BA_TE_DO,
         PERC_MA = LI_BA_TE_MA +LI_MA + LI_ES_MA + LI_BA_TE_ES_MA + ES_MA + BA_TE_MA + MA,
         PERC_ES = LI_ES + LI_BA_TE_ES + ES,
         PERC_LI_BA = LI_BA_TE + LI +BA_TE)%>%
  select(CD_ESCOLA,PERC_DO,PERC_MA,PERC_ES,PERC_LI_BA)


### FORMACAO1118_DIR - Nível de formação do diretor da unidade de ensino

source("function_str.R")

FORMACAO1118_DIR <- FORMACAO1118%>%
  filter(CIE_ESCOLA != 0, CARGO_E == 6200)%>%
  mutate(DIR_POS = if_else(right(FORMACAO,8)=="MESTRADO"|right(FORMACAO,9)=="DOUTORADO",1,0))%>%
  rename(CD_ESCOLA = "CIE_ESCOLA")%>%
  select(CD_ESCOLA, DIR_POS)


## Ausencias por servidor ----

## Ausencia 1118

dados_ausencia_1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_AUSENCIAS_1118.csv'

ausencia_1118 <-  read.csv2(dados_ausencia_1118)

### ausencia_1118_escola

ausencia_1118_escola <- ausencia_1118%>%
  filter(left(QUADRO_E,2)=="QM")%>%
  select(UA_E,QUADRO_E,CARGO_E,TT_DIAS_FALTA_INJUST,id_interno)%>%
  left_join(AUXILIAR, by ="id_interno")%>%
  group_by(CD_ESCOLA)%>%
  mutate(TOTAL_FALTAS_INJUST = sum(TT_DIAS_FALTA_INJUST, na.rm = T))%>%
  select(CD_ESCOLA,TOTAL_FALTAS_INJUST)%>%
  distinct()


## Servidores ativos por Unidade ----

DADOS_SA_1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_SERVIDORES_ATIVOS_1118.csv'

SA_1118 <- read_csv2(DADOS_SA_1118)

###SA_1118

SA_1118_ESCOLA<- SA_1118%>%
  filter(QUADRO_E == "QM")%>%
  mutate(ANO_EXERC = (difftime("2018-12-31",as.Date(DATA_INICIO_EXERCICIO_E,format='%d/%m/%Y'),units = "weeks"))/52.25)%>%
  left_join(AUXILIAR, by ="id_interno")%>%
  group_by(CD_ESCOLA)%>%
  mutate(MED_ANO_EXERC = round(mean(ANO_EXERC, na.rm = T), digits = 2),
         MED_IDADE = round(mean(IDADE, na.rm = T),digits = 2))%>%
  select(CD_ESCOLA,MED_ANO_EXERC,MED_IDADE)%>%
  unique()





## Instalacoes fisicas por Unidade Escolar ----

DADOS_IF_ESCOLA <-'https://dados.educacao.sp.gov.br/sites/default/files/06_Escolas_Dependencias.csv'

IF_ESCOLA <- read_csv2(DADOS_IF_ESCOLA)%>%
  rename(CD_ESCOLA = "CODESC")



## Enderecos de escolas ----

DADOS_END18 <- 'https://dados.educacao.sp.gov.br/sites/default/files/escolas_enderecos_0.csv'

END18 <- read_csv(DADOS_END18)

### TABELA END

END18 <- END18%>%
  select(CD_ESCOLA,LATITUDE,LONGITUDE)

## Histirico de matriculas por turma ----

DADOS_ALUNO_ESCOLA <- 'https://dados.educacao.sp.gov.br/sites/default/files/10_Escolas_Classes_Qtde_Alunos.csv'

ALUNO_ESCOLA <- read_csv2(DADOS_ALUNO_ESCOLA,locale = readr::locale(encoding = "latin1"))

### ALUNO_ESCOLA - Número de alunos dos grupos de ensino (AI,AF,EM)

ALUNO_ESCOLA <- ALUNO_ESCOLA%>%
  filter(ANO == 2018)

ALUNO_ESCOLA_GERAL <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  mutate(MED_ALUNO_TURMA = mean(QTDE_ALUNOS,na.rm = T))%>%
  rename(CD_ESCOLA = "COD_ESC")%>%
  select(CD_ESCOLA,GRAU,MED_ALUNO_TURMA,QTDE_ALUNOS)%>%
  filter(GRAU %in% c(14,30,2))%>%
  mutate(MED_ALUNO_TURMA_REGULAR = mean(QTDE_ALUNOS,na.rm = T))%>%
  select(CD_ESCOLA,MED_ALUNO_TURMA,MED_ALUNO_TURMA_REGULAR)%>%
  distinct()

ALUNO_ESCOLA_AI <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  filter(GRAU %in% c(14,30), SERIE %in% c(1,2,3,4,5) )%>%
  mutate(MED_ALUNO_TURMA_AI = mean(QTDE_ALUNOS,na.rm = T))%>%
  rename(CD_ESCOLA = "COD_ESC")%>%
  select(CD_ESCOLA,MED_ALUNO_TURMA_AI)%>%
  distinct()

ALUNO_ESCOLA_AF <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  filter(GRAU == 14, SERIE %in% c(6,7,8,9)|GRAU == 1)%>%
  mutate(MED_ALUNO_TURMA_AF = mean(QTDE_ALUNOS,na.rm = T))%>%
  rename(CD_ESCOLA = "COD_ESC")%>%
  select(CD_ESCOLA,MED_ALUNO_TURMA_AF)%>%
  distinct()

ALUNO_ESCOLA_EM <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  filter(GRAU == 2)%>%
  mutate(MED_ALUNO_TURMA_em = mean(QTDE_ALUNOS,na.rm = T))%>%
  rename(CD_ESCOLA = "COD_ESC")%>%
  select(CD_ESCOLA,MED_ALUNO_TURMA_em)%>%
  distinct()

ALUNO_ESCOLA_FINAL <- ALUNO_ESCOLA_GERAL%>%
  left_join(ALUNO_ESCOLA_AI, by = "CD_ESCOLA")%>%
  left_join(ALUNO_ESCOLA_AF, by = "CD_ESCOLA")%>%
  left_join(ALUNO_ESCOLA_EM, by = "CD_ESCOLA")%>%
  unique()



rm(list = c("ALUNO_ESCOLA_AI","ALUNO_ESCOLA_AF","ALUNO_ESCOLA_EM","ALUNO_ESCOLA_GERAL"))


## Indice de Desenvolvimento da Educacao do Estado de Sao Paulo (IDESP) por Escola -----

## IDESP Escola - 2018

DADOS_IDESP_ESCOLA_2018 <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP%20por%20Escola%20-%202018.csv'

IDESP_ESCOLA_2018 <- read.csv2(DADOS_IDESP_ESCOLA_2018)

### TABELA IDESP 2018

IDESP_ESCOLA_2018 <- IDESP_ESCOLA_2018%>%
  rename(CD_ESCOLA = "CODIGO_CIE")%>%
  select(CD_ESCOLA,ANOS_INICIAIS,ANOS_FINAIS,ENSINO_MEDIO)

## IDESP AI - 2007--2018

DADOS_IDESP_AI <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_AI.CSV'

IDESP_AI <- read_csv(DADOS_IDESP_AI)

### BASE IDESP AI    

IDESP_AI <- IDESP_AI%>%
  rename(CD_ESCOLA = "CODIGO CIE")%>%
  select(CD_ESCOLA,`2018`)%>%
  mutate_all(list(~replace_na(.,0)))


## IDESP AF - 2007--2018

DADOS_IDESP_AF <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_AF.CSV'

IDESP_AF <- read_csv(DADOS_IDESP_AF)

### BASE IDESP AF

IDESP_AF <- IDESP_AF%>%
  rename(CD_ESCOLA = "CODIGO CIE")%>%
  select(CD_ESCOLA,`2018`)%>%
  mutate_all(list(~replace_na(.,0)))



## IDESP EM - 2007--2018----

DADOS_IDESP_EM <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_EM.CSV'

IDESP_EM <- read_csv(DADOS_IDESP_EM)

### BASE IDESP EM
IDESP_EM <- IDESP_EM%>%
  rename(CD_ESCOLA = "CODIGO CIE")%>%
  select(CD_ESCOLA,`2018`)%>%
  mutate_all(list(~replace_na(.,0)))


## Histirico de Diretores, Vice Diretores e Professores Coordenadores por Unidade Escolar ------

## Coordenadores

DADOS_COORD <- 'https://dados.educacao.sp.gov.br/sites/default/files/PROFESSOR_COORDENADOR.csv'

COORD <- read_csv2(DADOS_COORD)

###BASE - Formacao coordenadoras

COORDa <- COORD%>%
  filter(`2018` == "SIM", CD_SITUACAO ==1)%>%
  group_by(CD_ESCOLA)%>%
  mutate(ESCOLA_COORD = 1)%>%
  select(CD_ESCOLA,ESCOLA_COORD)%>%
  unique()




COORDb <- COORD%>%
  filter(`2018` == "SIM", CD_SITUACAO == 1)%>%
  left_join(FORMACAO1118, by ="id_interno")%>%
  select(CD_ESCOLA, id_interno, FORMACAO)%>%
  distinct()%>%
  drop_na()%>%
  group_by(CD_ESCOLA)%>%
  mutate(total_coord = n())%>%
  group_by(CD_ESCOLA, FORMACAO)%>%
  mutate(COORD_PERC = n()/total_coord)%>%
  select(CD_ESCOLA, FORMACAO,COORD_PERC)%>%
  distinct()%>%
  reshape2::dcast(CD_ESCOLA  ~ FORMACAO, value.var="COORD_PERC")%>%
  rename(EM = "ENSINO MLdDIO",
         BA_TE ="BACHARELADO/TECNLRLOGO",
         LI = "LICENCIATURA",
         LI_BA_TE = "LICENCIATURA + BACHARELADO/TECNLRLOGO",
         LI_BA_TE_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + DOUTORADO",
         LI_BA_TE_ES = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO",
         LI_BA_TE_ES_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO + MESTRADO",
         LI_BA_TE_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO + DOUTORADO",
         LI_BA_TE_ES_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO+ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_DO = "LICENCIATURA + DOUTORADO",
         LI_ES = "LICENCIATURA + ESPECIALIZAL`LDO",
         LI_ES_MA = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO",
         LI_ES_MA_DO = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_MA = "LICENCIATURA + MESTRADO",
         LI_MA_DO = "LICENCIATURA + MESTRADO+DOUTORADO",
         LI_BA_TE_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO")%>%
  mutate_all(list(~replace_na(.,0)))%>%
  mutate(COORD_PERC_DO = LI_MA_DO + LI_ES_MA_DO + LI_DO + LI_BA_TE_ES_MA_DO + LI_BA_TE_MA_DO + LI_BA_TE_DO ,
         COORD_PERC_MA = LI_BA_TE_MA +LI_MA + LI_ES_MA + LI_BA_TE_ES_MA,
         COORD_PERC_ES = LI_ES + LI_BA_TE_ES,
         COORD_PERC_LI_BA = LI_BA_TE + LI +BA_TE)%>%
  select(CD_ESCOLA,COORD_PERC_DO,COORD_PERC_MA,COORD_PERC_ES,COORD_PERC_LI_BA)


## Diretores

DADOS_DIRET <- 'https://dados.educacao.sp.gov.br/sites/default/files/DIRETORES%20DE%20ESCOLA.csv'

DIRET <- read_csv2(DADOS_DIRET)


## Vice Diretores
DADOS_VICE_DIRET <- 'https://dados.educacao.sp.gov.br/sites/default/files/VICE_DIRETOR.csv'

VICE_DIRET <-  read_csv2(DADOS_VICE_DIRET)


VICE_DIRETa <- VICE_DIRET%>%
  filter(`2018` == "SIM", CD_SITUACAO ==1)%>%
  group_by(CD_ESCOLA)%>%
  mutate(ESCOLA_VDIR = 1)%>%
  select(CD_ESCOLA,ESCOLA_VDIR)%>%
  unique()


VICE_DIRETb <- VICE_DIRET%>%
  filter(`2018` == "SIM", CD_SITUACAO == 1)%>%
  left_join(FORMACAO1118, by ="id_interno")%>%
  select(CD_ESCOLA, id_interno, FORMACAO)%>%
  distinct()%>%
  drop_na()%>%
  group_by(CD_ESCOLA)%>%
  mutate(total_coord = n())%>%
  group_by(CD_ESCOLA, FORMACAO)%>%
  mutate(COORD_PERC = n()/total_coord)%>%
  select(CD_ESCOLA, FORMACAO,COORD_PERC)%>%
  distinct()%>%
  reshape2::dcast(CD_ESCOLA  ~ FORMACAO, value.var="COORD_PERC")%>%
  rename(EM = "ENSINO MLdDIO",
         BA_TE ="BACHARELADO/TECNLRLOGO",
         LI = "LICENCIATURA",
         LI_BA_TE = "LICENCIATURA + BACHARELADO/TECNLRLOGO",
         LI_BA_TE_ES = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO",
         LI_BA_TE_ES_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO + MESTRADO",
         LI_BA_TE_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO + DOUTORADO",
         LI_BA_TE_ES_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO+ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_ES = "LICENCIATURA + ESPECIALIZAL`LDO",
         LI_ES_MA = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO",
         LI_ES_MA_DO = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_MA = "LICENCIATURA + MESTRADO",
         LI_MA_DO = "LICENCIATURA + MESTRADO+DOUTORADO",
         LI_BA_TE_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO")%>%
  mutate_all(list(~replace_na(.,0)))%>%
  mutate(VDIR_PERC_DO = LI_MA_DO + LI_ES_MA_DO + LI_BA_TE_ES_MA_DO + LI_BA_TE_MA_DO ,
         VDIR_PERC_MA = LI_BA_TE_MA +LI_MA + LI_ES_MA + LI_BA_TE_ES_MA,
         VDIR_PERC_ES = LI_ES + LI_BA_TE_ES,
         VDIR_PERC_LI_BA = LI_BA_TE + LI +BA_TE)%>%
  select(CD_ESCOLA,VDIR_PERC_DO,VDIR_PERC_MA,VDIR_PERC_ES,VDIR_PERC_LI_BA)

## Proficiencia do Sistema de Avaliacao de Rendimento Escolar do Estado de Sao Paulo (SARESP) por escola ------

DADOS_SARESP2018 <- 'https://dados.educacao.sp.gov.br/sites/default/files/SARESP_escolas_2018.csv'

SARESP2018 <- read_csv2(DADOS_SARESP2018, locale = readr::locale(encoding = "latin1"))

### Base SARESP

SARESP2018a <- SARESP2018%>%
  mutate(CODESC = as.numeric(CODESC))%>%
  group_by(CODESC,ds_comp)%>%
  mutate(MED_NOTA = mean(medprof, na.rm = T))%>%
  select(CODESC,SERIE_ANO,ds_comp,medprof,MED_NOTA)%>%
  rename(CD_ESCOLA = "CODESC")%>%
  reshape2::dcast(CD_ESCOLA ~ SERIE_ANO + ds_comp, value.var="medprof")



SARESP2018b <- SARESP2018%>%
  mutate(CODESC = as.numeric(CODESC))%>%
  group_by(CODESC,ds_comp)%>%
  mutate(MED_NOTA = mean(medprof, na.rm = T))%>%
  select(CODESC, ds_comp, MED_NOTA)%>%
  distinct()%>%
  rename(CD_ESCOLA = "CODESC")%>%
  reshape2::dcast(CD_ESCOLA ~ ds_comp, value.var="MED_NOTA")


## BASE CONSOLIDADA SARESP - 2018 ----


BASE_FINAL_SARESP <- SARESP2018b%>%
  left_join(SARESP2018a, by = "CD_ESCOLA")%>%
  left_join(ALUNO_ESCOLA_FINAL, by = "CD_ESCOLA")%>%
  left_join(ausencia_1118_escola, by = "CD_ESCOLA")%>%
  left_join(END18, by = "CD_ESCOLA")%>%
  left_join(FORMACAO1118_DIR, by = "CD_ESCOLA")%>%
  left_join(FORMACAO1118_PERC, by = "CD_ESCOLA")%>%
  left_join(IDESP_ESCOLA_2018, by = "CD_ESCOLA")%>%
  left_join(IF_ESCOLA, by = "CD_ESCOLA")%>%
  left_join(SA_1118_ESCOLA, by = "CD_ESCOLA")%>%
  left_join(COORDa , by = "CD_ESCOLA")%>%
  left_join(COORDb , by = "CD_ESCOLA")%>%
  left_join(VICE_DIRETa, by = "CD_ESCOLA")%>%
  left_join(VICE_DIRETb, by = "CD_ESCOLA")%>%
  mutate_at(vars(ESCOLA_VDIR,DIR_POS,ESCOLA_COORD), ~replace_na(., 0))%>%
  unique()%>%
  mutate(dupicate = duplicated(CD_ESCOLA))%>%
  filter(dupicate == F)

write_csv(BASE_FINAL_SARESP, "BASE_FINAL_SARESP2018.csv", append = FALSE)
