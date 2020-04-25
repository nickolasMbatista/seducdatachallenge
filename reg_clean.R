#### Regressões 2018
library("plm")
library("stargazer")
library("tidyverse")
library("lfe")
library("kableExtra")
library("devtools")

source_url("https://raw.githubusercontent.com/MatthieuStigler/Misconometrics/master/Gelbach_decompo/dec_covar.R")

  ##Vice Diretor 2018 ----

## Matemática
linear_vdir1mt <- lm(`MATEMÁTICA` ~ ESCOLA_VDIR, data = SARESP2018)
covar_vdir1mt         <- vcovHC(linear_vdir1mt, type = "HC1")
robust_vdir1mt    <- sqrt(diag(covar_vdir1mt))


linear_vdir2mt <- lm(`MATEMÁTICA` ~ ESCOLA_VDIR + TOTAL_FALTAS_INJUST , data = SARESP2018)
covar_vdir2mt       <- vcovHC(linear_vdir2mt, type = "HC1")
robust_vdir2mt    <- sqrt(diag(covar_vdir2mt))


linear_vdir3mt <- lm(`MATEMÁTICA` ~ ESCOLA_VDIR + TOTAL_FALTAS_INJUST + MED_ANO_EXERC + MED_IDADE, data = SARESP2018)
covar_vdir3mt       <- vcovHC(linear_vdir3mt, type = "HC1")
robust_vdir3mt    <- sqrt(diag(covar_vdir3mt))


linear_vdir4mt <- lm(`MATEMÁTICA` ~ ESCOLA_VDIR + TOTAL_FALTAS_INJUST + MED_ANO_EXERC + MED_IDADE + LAB_INFO, data = SARESP2018)
covar_vdir4mt       <- vcovHC(linear_vdir4mt, type = "HC1")
robust_vdir4mt    <- sqrt(diag(covar_vdir4mt))

stargazer::stargazer(linear_vdir1mt, linear_vdir2mt, linear_vdir3mt, linear_vdir4mt, se=list(robust_vdir1mt, robust_vdir2mt,robust_vdir3mt,robust_vdir4mt ), title = "Results", type = "latex", out = "over.txt")


## Língua Portuguesa
linear_vdir1lp <- lm(`LÍNGUA PORTUGUESA` ~ ESCOLA_VDIR, data = SARESP2018)
covar_vdir1lp         <- vcovHC(linear_vdir1lp, type = "HC1")
robust_vdir1lp    <- sqrt(diag(covar_vdir1lp))


linear_vdir2lp <- lm(`LÍNGUA PORTUGUESA` ~ ESCOLA_VDIR + TOTAL_FALTAS_INJUST , data = SARESP2018)
covar_vdir2lp       <- vcovHC(linear_vdir2lp, type = "HC1")
robust_vdir2lp    <- sqrt(diag(covar_vdir2lp))


linear_vdir3lp <- lm(`LÍNGUA PORTUGUESA` ~ ESCOLA_VDIR + TOTAL_FALTAS_INJUST + MED_ANO_EXERC + MED_IDADE, data = SARESP2018)
covar_vdir3lp       <- vcovHC(linear_vdir3lp, type = "HC1")
robust_vdir3lp    <- sqrt(diag(covar_vdir3lp))


linear_vdir4lp <- lm(`LÍNGUA PORTUGUESA` ~ ESCOLA_VDIR + TOTAL_FALTAS_INJUST + MED_ANO_EXERC + MED_IDADE + LAB_INFO, data = SARESP2018)
covar_vdir4lp       <- vcovHC(linear_vdir4lp, type = "HC1")
robust_vdir4lp    <- sqrt(diag(covar_vdir4lp))

stargazer(linear_vdir1lp, linear_vdir2lp, linear_vdir3lp, linear_vdir4lp, se=list(robust_vdir1lp, robust_vdir2lp,robust_vdir3lp,robust_vdir4lp ), title = "Results", type = "text")



    ##Coordenadores 2018 ----

##Matemática
  
linear_coord1mt <- lm(`MATEMÁTICA` ~ COORD_PERC_LI_BA, data = SARESP2018)
covar_coord1mt         <- vcovHC(linear_coord1mt, type = "HC1")
robust_coord1mt    <- sqrt(diag(covar_coord1mt))

linear_coord2mt <- lm(`MATEMÁTICA` ~ COORD_PERC_LI_BA + TOTAL_FALTAS_INJUST , data = SARESP2018)
covar_coord2mt       <- vcovHC(linear_coord2mt, type = "HC1")
robust_coord2mt    <- sqrt(diag(covar_coord2mt))

linear_coord3mt <- lm(`MATEMÁTICA` ~ COORD_PERC_LI_BA + TOTAL_FALTAS_INJUST + MED_ANO_EXERC + MED_IDADE , data = SARESP2018)
covar_coord3mt       <- vcovHC(linear_coord3mt, type = "HC1")
robust_coord3mt    <- sqrt(diag(covar_coord3mt))

linear_coord4mt <- lm(`MATEMÁTICA` ~ COORD_PERC_LI_BA + TOTAL_FALTAS_INJUST + + MED_ANO_EXERC + MED_IDADE + LAB_INFO , data = SARESP2018)
covar_coord4mt       <- vcovHC(linear_coord4mt, type = "HC1")
robust_coord4mt    <- sqrt(diag(covar_coord4mt))

stargazer::stargazer(linear_coord1mt, linear_coord2mt,linear_coord3mt, linear_coord4mt, se=list(robust_coord1mt, robust_coord2mt, robust_coord3mt, robust_coord3mt), title = "Results", type = "text")

##Língua Portuguesa

linear_coord1lp <- lm(`LÍNGUA PORTUGUESA` ~ COORD_PERC_LI_BA, data = SARESP2018)
covar_coord1lp         <- vcovHC(linear_coord1lp, type = "HC1")
robust_coord1lp    <- sqrt(diag(covar_coord1lp))


linear_coord2lp <- lm(`LÍNGUA PORTUGUESA` ~ COORD_PERC_LI_BA + TOTAL_FALTAS_INJUST , data = SARESP2018)
covar_coord2lp       <- vcovHC(linear_coord2lp, type = "HC1")
robust_coord2lp    <- sqrt(diag(covar_coord2lp))

linear_coord3lp <- lm(`LÍNGUA PORTUGUESA` ~ COORD_PERC_LI_BA + TOTAL_FALTAS_INJUST + MED_ANO_EXERC + MED_IDADE , data = SARESP2018)
covar_coord3lp       <- vcovHC(linear_coord3lp, type = "HC1")
robust_coord3lp    <- sqrt(diag(covar_coord3lp))

linear_coord4lp <- lm(`LÍNGUA PORTUGUESA` ~ COORD_PERC_LI_BA + TOTAL_FALTAS_INJUST + + MED_ANO_EXERC + MED_IDADE + LAB_INFO , data = SARESP2018)
covar_coord4lp       <- vcovHC(linear_coord4lp, type = "HC1")
robust_coord4lp    <- sqrt(diag(covar_coord4lp))


stargazer::stargazer(linear_coord1lp, linear_coord2lp,linear_coord3lp, linear_coord4lp, se=list(robust_coord1lp, robust_coord2lp, robust_coord3lp, robust_coord4lp), title = "Results", type = "text")


  ##SARESP 2013 - EM ----

##Matemática - EM

SARESP2013_EM <- data.frame (read_csv2("SARESP2013_EM.csv"))


linear_em1 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena`, data =SARESP2013_EM )
covar_em1       <- vcovHC(linear_em1, type = "HC1")
robust_em1    <- sqrt(diag(covar_em1))

linear_em2 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo, data =SARESP2013_EM )
covar_em2       <- vcovHC(linear_em2, type = "HC1")
robust_em2    <- sqrt(diag(covar_em2))


linear_em3 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info, data =SARESP2013_EM )
covar_em3       <- vcovHC(linear_em3, type = "HC1")
robust_em3    <- sqrt(diag(covar_em3))


linear_em4 <- lfe::felm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info | CODESC, 
                        data =SARESP2013_EM)
covar_em4       <- vcovHC(linear_em4, type = "HC1")
robust_em4    <- sqrt(diag(covar_em4))


stargazer::stargazer(linear_em1,linear_em2,linear_em3,linear_em4, se=list(robust_em1,robust_em2, robust_em3,robust_em4), title = "Results", type = "text", 
                     keep = c("etnia_Negro","etnia_Pardo","agua","esgoto","luz","gas","lixo","info"))


##Língua Portuguesa - LP

linear_em1lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena`, data =SARESP2013_EM )
covar_em1lp       <- vcovHC(linear_em1lp, type = "HC1")
robust_em1lp    <- sqrt(diag(covar_em1lp))

linear_em2lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo, data =SARESP2013_EM )
covar_em2lp       <- vcovHC(linear_em2lp, type = "HC1")
robust_em2lp    <- sqrt(diag(covar_em2lp))


linear_em3lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info, data =SARESP2013_EM )
covar_em3lp       <- vcovHC(linear_em3lp, type = "HC1")
robust_em3lp    <- sqrt(diag(covar_em3lp))


linear_em4lp <- felm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info | CODESC, 
                          data =SARESP2013_EM)
covar_em4lp       <- vcovHC(linear_em4lp, type = "HC1")
robust_em4lp    <- sqrt(diag(covar_em4lp))


stargazer::stargazer(linear_em1lp,linear_em2lp,linear_em3lp,linear_em4lp, se=list(robust_em1lp,robust_em2lp, robust_em3lp,robust_em4lp), title = "Results", type = "text", 
                     keep = c("etnia_Negro","etnia_Pardo","agua","esgoto","luz","gas","lixo","info"))


  ## Decomposição de fatores EM ----

## Matemática - EM


decomp_em_mat <- dec_covar(linear_em3, var_main = c("etnia_Negro.a.","etnia_Pardo.a.","etnia_Oriental","etnia_Indígena"), format = "long", add_coefs = FALSE, conf.int = TRUE)%>%
  filter(variable == "etnia_Negro.a.")%>%
  select(covariate, variable, delta)

var_em_mat <- summary(linear_em4)$coefficients[1] - summary(linear_em1)$coefficients[2] + sum(decomp_em_mat$delta)


decomp_em_mat <- add_row(decomp_em_mat,covariate = "E.F. Escola", variable = "etnia_Negro.a.", delta = -var_em_mat)%>%
  mutate(delta_perc = delta/sum(delta))


kable(decomp_em_mat, "latex",caption = "Decomposição - Ensino Médio - Matemática ",booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



## LP - EM
decomp_em_lp <- dec_covar(linear_em3lp, var_main = c("etnia_Negro.a.","etnia_Pardo.a.","etnia_Oriental","etnia_Indígena"), format = "long", add_coefs = FALSE, conf.int = TRUE)%>%
  filter(variable == "etnia_Negro.a.")%>%
  select(covariate, variable, delta)
  
var_em_lp <- summary(linear_em4lp)$coefficients[1] - summary(linear_em1lp)$coefficients[2] + sum(decomp_em_lp$delta)

    
decomp_em_lp <- add_row(decomp_em_mat,covariate = "E.F. Escola", variable = "etnia_Negro.a.", delta = -var_em_lp)%>%
  mutate(delta_perc = delta/sum(delta))


kable(decomp_em_lp, "latex",caption = "Decomposição - Ensino Médio - Língua Portuguesa",booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))


##SARESP 2013 - AF ----

##Matemática - AF 

SARESP2013_AF <- data.frame (read_csv2("SARESP2013_AF.csv"))


linear_af1 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena`, data =SARESP2013_AF )
covar_af1       <- vcovHC(linear_af1, type = "HC1")
robust_af1    <- sqrt(diag(covar_af1))

linear_af2 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo, data =SARESP2013_AF )
covar_af2       <- vcovHC(linear_af2, type = "HC1")
robust_af2    <- sqrt(diag(covar_af2))


linear_af3 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info, data =SARESP2013_AF )
covar_af3       <- vcovHC(linear_af3, type = "HC1")
robust_af3    <- sqrt(diag(covar_af3))


linear_af4 <- lfe::felm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info | CODESC, 
                        data =SARESP2013_AF)
covar_af4       <- vcovHC(linear_af4, type = "HC1")
robust_af4    <- sqrt(diag(covar_af4))


stargazer::stargazer(linear_af1,linear_af2,linear_af3,linear_af4, se=list(robust_af1,robust_af2, robust_af3,robust_af4), title = "Results", type = "text", 
                     keep = c("etnia_Negro","etnia_Pardo","agua","esgoto","luz","gas","lixo","info"))


##Língua Portuguesa - LP

linear_af1lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena`, data =SARESP2013_AF )
covar_af1lp       <- vcovHC(linear_af1lp, type = "HC1")
robust_af1lp    <- sqrt(diag(covar_af1lp))

linear_af2lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo, data =SARESP2013_AF )
covar_af2lp       <- vcovHC(linear_af2lp, type = "HC1")
robust_af2lp    <- sqrt(diag(covar_af2lp))

linear_af3lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info, data =SARESP2013_AF )
covar_af3lp       <- vcovHC(linear_af3lp, type = "HC1")
robust_af3lp    <- sqrt(diag(covar_af3lp))


linear_af4lp <- felm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info | CODESC, 
                     data =SARESP2013_AF)
covar_af4lp       <- vcovHC(linear_af4lp, type = "HC1")
robust_af4lp    <- sqrt(diag(covar_af4lp))


stargazer::stargazer(linear_af1lp,linear_af2lp,linear_af3lp,linear_af4lp, se=list(robust_af1lp,robust_af2lp, robust_af3lp,robust_af4lp), title = "Results", type = "text", 
                     keep = c("etnia_Negro","etnia_Pardo","agua","esgoto","luz","gas","lixo","info"))


## Decomposição de fatores AF ----

## Matemática - AF


decomp_af_mat <- dec_covar(linear_af3, var_main = c("etnia_Negro.a.","etnia_Pardo.a.","etnia_Oriental","etnia_Indígena"), format = "long", add_coefs = FALSE, conf.int = TRUE)%>%
  filter(variable == "etnia_Negro.a.")%>%
  select(covariate, variable, delta)

var_af_mat <- summary(linear_af4)$coefficients[1] - summary(linear_af1)$coefficients[2] + sum(decomp_af_mat$delta)


decomp_af_mat <- add_row(decomp_af_mat,covariate = "E.F. Escola", variable = "etnia_Negro.a.", delta = -var_af_mat)%>%
  mutate(delta_perc = delta/sum(delta))


kable(decomp_af_mat, "latex",caption = "Decomposição - Ensino Médio - Matemática ",booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



## LP - EM
decomp_af_lp <- dec_covar(linear_af3lp, var_main = c("etnia_Negro.a.","etnia_Pardo.a.","etnia_Oriental","etnia_Indígena"), format = "long", add_coefs = FALSE, conf.int = TRUE)%>%
  filter(variable == "etnia_Negro.a.")%>%
  select(covariate, variable, delta)

var_af_lp <- summary(linear_af4lp)$coefficients[1] - summary(linear_af1lp)$coefficients[2] + sum(decomp_af_lp$delta)


decomp_af_lp <- add_row(decomp_em_mat,covariate = "E.F. Escola", variable = "etnia_Negro.a.", delta = -var_af_lp)%>%
  mutate(delta_perc = delta/sum(delta))


kable(decomp_af_lp, "latex",caption = "Decomposição - Ensino Médio - Língua Portuguesa",booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))

##SARESP 2013 - AI ----

##Matemática - AI 

SARESP2013_AI <- data.frame (read_csv2("SARESP2013_AI.csv"))


linear_ai1 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena`, data =SARESP2013_AF )
covar_ai1       <- vcovHC(linear_ai1, type = "HC1")
robust_ai1    <- sqrt(diag(covar_ai1))

linear_ai2 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo, data =SARESP2013_AF )
covar_ai2       <- vcovHC(linear_ai2, type = "HC1")
robust_ai2    <- sqrt(diag(covar_ai2))

linear_ai3 <- lm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info, data =SARESP2013_AF )
covar_ai3       <- vcovHC(linear_ai3, type = "HC1")
robust_ai3    <- sqrt(diag(covar_ai3))


linear_ai4 <- lfe::felm(profic_mat ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info | CODESC, 
                        data =SARESP2013_AF)
covar_ai4       <- vcovHC(linear_ai4, type = "HC1")
robust_ai4    <- sqrt(diag(covar_ai4))


stargazer::stargazer(linear_ai1,linear_ai2,linear_ai3,linear_ai4, se=list(robust_ai1,robust_ai2, robust_ai3,robust_ai4), title = "Results", type = "text", 
                     keep = c("etnia_Negro","etnia_Pardo","agua","esgoto","luz","gas","lixo","info"))


##Língua Portuguesa - LP

linear_ai1lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena`, data =SARESP2013_AF )
covar_ai1lp       <- vcovHC(linear_ai1lp, type = "HC1")
robust_ai1lp    <- sqrt(diag(covar_ai1lp))

linear_ai2lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo, data =SARESP2013_AF )
covar_ai2lp       <- vcovHC(linear_ai2lp, type = "HC1")
robust_ai2lp    <- sqrt(diag(covar_ai2lp))

linear_ai3lp <- lm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info, data =SARESP2013_AF )
covar_ai3lp       <- vcovHC(linear_ai3lp, type = "HC1")
robust_ai3lp    <- sqrt(diag(covar_ai3lp))


linear_ai4lp <- felm(profic_lp ~ `etnia_Negro.a.`+`etnia_Pardo.a.`+etnia_Oriental+`etnia_Indígena` + agua + esgoto + luz + gas + lixo + info | CODESC, 
                     data =SARESP2013_AF)
covar_ai4lp       <- vcovHC(linear_ai4lp, type = "HC1")
robust_ai4lp    <- sqrt(diag(covar_ai4lp))


stargazer::stargazer(linear_ai1lp,linear_ai2lp,linear_ai3lp,linear_ai4lp, se=list(robust_ai1lp,robust_ai2lp, robust_ai3lp,robust_ai4lp), title = "Results", type = "text", 
                     keep = c("etnia_Negro","etnia_Pardo","agua","esgoto","luz","gas","lixo","info"))


## Decomposição de fatores AI ----

## Matemática - AI


decomp_ai_mat <- dec_covar(linear_ai3, var_main = c("etnia_Negro.a.","etnia_Pardo.a.","etnia_Oriental","etnia_Indígena"), format = "long", add_coefs = FALSE, conf.int = TRUE)%>%
  filter(variable == "etnia_Negro.a.")%>%
  select(covariate, variable, delta)

var_ai_mat <- summary(linear_ai4)$coefficients[1] - summary(linear_ai1)$coefficients[2] + sum(decomp_ai_mat$delta)


decomp_ai_mat <- add_row(decomp_af_mat,covariate = "E.F. Escola", variable = "etnia_Negro.a.", delta = -var_ai_mat)%>%
  mutate(delta_perc = delta/sum(delta))


kable(decomp_ai_mat, "latex",caption = "Decomposição - Ensino Médio - Matemática ",booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



## LP - AI
decomp_ai_lp <- dec_covar(linear_ai3lp, var_main = c("etnia_Negro.a.","etnia_Pardo.a.","etnia_Oriental","etnia_Indígena"), format = "long", add_coefs = FALSE, conf.int = TRUE)%>%
  filter(variable == "etnia_Negro.a.")%>%
  select(covariate, variable, delta)

var_ai_lp <- summary(linear_ai4lp)$coefficients[1] - summary(linear_ai1lp)$coefficients[2] + sum(decomp_ai_lp$delta)


decomp_ai_lp <- add_row(decomp_em_mat,covariate = "E.F. Escola", variable = "etnia_Negro.a.", delta = -var_ai_lp)%>%
  mutate(delta_perc = delta/sum(delta))


kable(decomp_em_lp, "latex",caption = "Decomposição - Ensino Médio - Língua Portuguesa",booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



