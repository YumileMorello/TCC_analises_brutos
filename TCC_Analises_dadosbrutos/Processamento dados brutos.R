library(dplyr)
library(readr)
library(here)
library(data.table)
library(janitor)
library(stringr)
library(readxl)
library(tidyverse)
here()

#Carregando dados#
TCC_Estudo2 <- read_excel("Dados/TCC_Dados_Brutos.xlsx", 
                                 sheet = "Link 1")
TCC_Estudo1 <- read_excel("Dados/TCC_Dados_Brutos.xlsx", 
                          sheet = "Link 2")
TCC_Estudo3 <- read_excel("Dados/TCC_Dados_Brutos.xlsx", 
                          sheet = "Link 3")
#Mudando nomes colunas"
muda_nomes_variaveis2 <- function(df) {
  if(!require(janitor)) {install.packages("janitor")}
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::slice(-1) %>% # remove primeira linha
    rename(genero_especifico = x15,
           partido_especifico = x20,
           psicotropicos = voce_faz_uso_de_algum_medicamento_que_atue_no_seu_sistema_nervoso_central_isso_inclui_antidepressivos_ansioliticos_anticonvulsivantes_etc,
           doencas_psiquias = voce_tem_historico_de_doencas_neurologicas_psiquiatricas_e_psicologicas_severas,
           uso_drogas = voce_tem_historico_de_dependencia_de_alcool_ou_outras_drogas,
           filiacao_partidaria = voce_e_filiado_a_algum_partido_politico,
           iniciais = digite_abaixo_as_iniciais_de_seus_nomes_e_sobrenomes_e_sua_idade_para_que_possamos_identificar_seus_dados_mantendo_seu_anonimato_ex_lynm23,
           ladder = em_que_degrau_voce_se_posiciona,
           liberal_conservador = de_maneira_geral_voce_se_considera_liberal_ou_conservador_em_uma_perspectiva_social_igualdade_de_casamento_aborto,
           brasileiro_especial = brasileiros_merecem_tratamento_especial,
           brasileiro_importancia = poucas_pessoas_parecem_compreender_completamente_a_importancia_dos_brasileiros,
           brasileiro_reconhecimento = eu_nunca_ficarei_satisfeito_ate_que_os_brasileiros_tenham_o_reconhecimento_que_merecem,
           nacionalismo_identidade_brasileiro = eu_me_identifico_como_brasileiro,
           nacionalismo_brasileiro_quem_sou = ser_um_brasileiro_e_um_importante_aspecto_de_quem_eu_sou,
           ajuda_imigrantes = politicas_de_ajuda_a_imigrantes,
           fronteira_imigrantes = fechamento_das_fronteiras_para_imigrantes,
           atitude_lula_bolsonaro = o_quao_favoravel_voce_e_as_ideias_defendidas_pelos_partidos_associados_as_figuras_politicas_abaixo,
           img_alinhamento_bolsonarismo = escolha_a_alternativa_que_melhor_descreve_sua_relacao_com_o_grupo_de_partidos_alinhados_com_as_ideias_propostas_por_jair_bolsonaro,
           img_alinhamento_lulismo = escolha_a_alternativa_que_melhor_descreve_sua_relacao_com_o_grupo_de_partidos_alinhados_com_as_ideias_propostas_por_luis_inacio_lula_da_silva_lula,
           decisao_compartilhamento_bolsonaro = qual_a_sua_decisao_51,
           decisao_compartilhamento_lula = qual_a_sua_decisao_53,
           participacao_estudo = essa_e_a_primeira_vez_que_voce_participa_desse_estudo,
           nivel_identificacao =  o_quao_favoravel_voce_e_as_ideias_defendidas_pelos_partidos_associados_as_figuras_politicas_abaixo) 
  return(df)
}

TCC_Estudo2 <- muda_nomes_variaveis2(TCC_Estudo2)
TCC_Estudo1 <- muda_nomes_variaveis2(TCC_Estudo1)
TCC_Estudo3 <- muda_nomes_variaveis2(TCC_Estudo3)

#Legenda das variáveis - transformando strings em fatores#


unique(TCC_Estudo1$estado_civil) #mostra valores existentes no vetor


adiciona_legenda_respostas <- function(df) {
  df <- df %>%
    mutate(idade = as.numeric(idade),
           estado_civil = factor(estado_civil, levels = c("Solteiro(a)","Casado(a)","Separado(a)","Viúvo(a)"), labels = c("solteiro", "casado", "separado", "viuvo")),
           genero = factor(genero, levels = c("Homem", "Mulher", "Outro (especifique)"), labels = c("homem", "mulher", "outros")),
           grau_de_escolaridade = factor(grau_de_escolaridade, levels=c("Ensino Superior Completo", "Ensino Superior Incompleto", "Ensino Médio Completo", "Ensino Médio Incompleto", "Ensino Fundamental Completo")),
           filiacao_partidaria = factor(filiacao_partidaria, levels = unique(filiacao_partidaria)),
           nivel_identificacao = factor(nivel_identificacao, levels = unique(nivel_identificacao)))
  return(df)
} #em R, o último comando rodado é o que vai ser retornado na função. return(df) funciona para devolver o resultado final do que for rodado na função, e não os outputs intermediários. Se escrevermos unique sem () temos o código da função#


TCC_Estudo1 <- adiciona_legenda_respostas(TCC_Estudo1)
TCC_Estudo2 <- adiciona_legenda_respostas(TCC_Estudo2)
TCC_Estudo3 <- adiciona_legenda_respostas(TCC_Estudo3)

#Remover participantes que não concluíram a pesquisa#
TCC_Estudo1 <- TCC_Estudo1 %>% drop_na(nivel_identificacao)
TCC_Estudo2 <- TCC_Estudo2 %>% drop_na(nivel_identificacao)
TCC_Estudo3 <- TCC_Estudo3 %>% drop_na(nivel_identificacao)

#Remover participantes que podem ter participado duas vezes do estudo#
testes <- TCC_Estudo1%>%filter(participacao_estudo != "Não, já fiz um estudo muito parecido antes")
unique(testes$participacao_estudo) #AJUDA# removeu os na?

#Fazer mutate: usar case when pra criar a coluna de político. 1. se o valor em imagem bolsonaro for True, cria códico 1 em político. Criar caso em que os dois forem NA (is.NA). Usar função AND.
TCC_Estudo1 <- TCC_Estudo1%>%
  mutate(politico_escolhido = case_when(
    !is.na(img_alinhamento_bolsonarismo) ~ '1',
    !is.na(img_alinhamento_lulismo) ~ '2',
    TRUE ~ '0'))
TCC_Estudo2 <- TCC_Estudo2%>%
  mutate(politico_escolhido = case_when(
    !is.na(img_alinhamento_bolsonarismo) ~ '1',
    !is.na(img_alinhamento_lulismo) ~ '2',
    TRUE ~ '0'))
TCC_Estudo3 <- TCC_Estudo3%>%
  mutate(politico_escolhido = case_when(
    !is.na(img_alinhamento_bolsonarismo) ~ '1',
    !is.na(img_alinhamento_lulismo) ~ '2',
    TRUE ~ '0'))

#Transformando a variável de grupo em fator#
TCC_Estudo1 <- TCC_Estudo1 %>% 
 mutate(politico_escolhido = factor(politico_escolhido,
                  levels = c("0","1","2"),
                  labels = c("Nenhum", "Bolsonaro", "Lula")))
TCC_Estudo2 <- TCC_Estudo2 %>% 
  mutate(politico_escolhido = factor(politico_escolhido,
                                     levels = c("0","1","2"),
                                     labels = c("Nenhum", "Bolsonaro", "Lula")))
TCC_Estudo3 <- TCC_Estudo3 %>% 
  mutate(politico_escolhido = factor(politico_escolhido,
                                     levels = c("0","1","2"),
                                     labels = c("Nenhum", "Bolsonaro", "Lula")))


#Empilha os dados ramificados#
empilha_Dados_ramificados <- function(df) {
  
  df_Bolsonaro <- df %>%
    dplyr::filter(politico_escolhido == "Bolsonaro") %>%
    dplyr::select(-img_alinhamento_lulismo) %>%
    dplyr::select(-decisao_compartilhamento_lula) %>%
    rename(img_alinhamento = img_alinhamento_bolsonarismo,
           decisao_compartilhamento = decisao_compartilhamento_bolsonaro)
  
  
  df_lula <- df %>%
    dplyr::filter(politico_escolhido == "Lula") %>%
    dplyr::select(-img_alinhamento_bolsonarismo) %>%
    dplyr::select(-decisao_compartilhamento_bolsonaro) %>%
    rename(img_alinhamento = img_alinhamento_lulismo,
           decisao_compartilhamento = decisao_compartilhamento_lula)
  
  df_neutro <- df %>%
    dplyr::select(!starts_with("img_alinhamento")) %>%
    dplyr::select(-c("decisao_compartilhamento_lula", "decisao_compartilhamento_bolsonaro")) %>%
    dplyr::filter(politico_escolhido == "Nenhum") %>%
    mutate(img_alinhamento = NA)
  
  
  df_junto <- bind_rows(df_Bolsonaro, df_lula, df_neutro) %>%
    mutate(nivel_identificacao_recode = as.numeric(str_extract(nivel_identificacao, "[0-9]+")))
  
  return(df_junto)
}

TCC_Estudo1 <- empilha_Dados_ramificados(TCC_Estudo1)
TCC_Estudo2 <- empilha_Dados_ramificados(TCC_Estudo2)
TCC_Estudo3 <- empilha_Dados_ramificados(TCC_Estudo3)

#Criando coluna de identificação do estudo
TCC_Estudo1 <- TCC_Estudo1 %>%
  mutate(Estudo = 1)
TCC_Estudo2 <- TCC_Estudo2 %>%
  mutate(Estudo = 2)
TCC_Estudo3 <- TCC_Estudo3 %>%
  mutate(Estudo = 3)

#Criando banco final com os três estudos#
compartilhamento_3_estudos <- bind_rows(TCC_Estudo1, TCC_Estudo2, TCC_Estudo3)
glimpse(compartilhamento_3_estudos)

#Preenchendo com 'outro' o campo gênero para pessoas não-binárias#
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(genero = as.character(genero),
         genero = if_else(grepl("binári", genero_especifico), "outros", genero),
         genero = as.factor(genero))

#Mudando o nome das opções da variável 'decisão_compartilhamento'
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(decisao_compartilhamento = if_else(grepl("Não", decisao_compartilhamento), decisao_compartilhamento, 
                                            str_sub(decisao_compartilhamento, 45, end = 60)),
         decisao_compartilhamento = gsub("stas ", "", decisao_compartilhamento))

#Convertendo números que estavam como string para numérico e strings que seriam fatores para fatores#
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(ladder = str_extract(ladder, "[0-9]+")) %>% # extra número (de 1 abaixo de todos e 10 acima de todos)
  mutate(across(ladder:prostituicao, as.numeric),
         across(psicotropicos:filiacao_partidaria, as.factor),
         across(c(img_alinhamento, decisao_compartilhamento, participacao_estudo, Estudo), as.factor))


# salva o banco em formato RDS
saveRDS(compartilhamento_3_estudos, file= "Transformados/compartilhamento_3_estudos.rds")

# para ler, basta rodar (apontando para o diretório onde os dados estão. Dados processados eu salvo na pasta Transformados)
compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")


#A PARTIR DAQUI SÃO FEITAS AS ANÁLISES DESCRITIVAS DOS DADOS

# amostra por estudo
compartilhamento_3_estudos %>%
  group_by(Estudo) %>%
  summarise(amostra_total = n())

# amostra por estudo e político
compartilhamento_3_estudos %>%
  group_by(Estudo, politico_escolhido) %>%
  summarise(amostra_total = n(), .groups="drop_last") %>%
  mutate(subtotal_estudo = sum(amostra_total),
         perc_politico = round(amostra_total/subtotal_estudo, 2))

# demográficos por estudo e político

# gênero (percentual por estudo)
compartilhamento_3_estudos %>%
  group_by(genero, Estudo) %>%
  summarise(amostra_total = n(), .groups="drop") %>%
  mutate(subtotal_estudo = sum(amostra_total),
         perc_genero = round(amostra_total/subtotal_estudo, 3)) %>%
  arrange(Estudo)

# idade média
compartilhamento_3_estudos %>%
  group_by(Estudo, politico_escolhido) %>%
  summarise(idade_mediana = median(idade), .groups="drop")

# 
# escolaridade (percentual por estudo)
escol <- compartilhamento_3_estudos %>%
  group_by(grau_de_escolaridade, Estudo) %>%
  summarise(amostra_total = n(), .groups="drop") %>%
  mutate(subtotal_estudo = sum(amostra_total),
         perc_escolaridade = round(amostra_total/subtotal_estudo, 3)) %>%
  arrange(Estudo)

# gráfico de barra de escolaridade

escol %>%
  ggplot(aes(y=perc_escolaridade, x=grau_de_escolaridade)) + geom_col() +
  coord_flip() + scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Estudo)

# cruzamento entre liberal-conservador e bolsonarismo ou lulismo

# primeiro, coloco Lula como número negativo, ou seja, 80 lula é -80 etc.
ideologia <- compartilhamento_3_estudos %>%
  mutate(liberal_conservador = as.numeric(liberal_conservador),
         nivel_identificacao_recode = if_else(politico_escolhido == "Bolsonaro", nivel_identificacao_recode, -nivel_identificacao_recode))

ideologia %>%
  filter(!is.na(nivel_identificacao_recode)) %>%
  summarise(amostra = n(),
            cor(liberal_conservador, nivel_identificacao_recode))
# 67,4% de correlação.


# decisao versus politico de identificação
compartilhamento_3_estudos %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  group_by(Estudo, politico_escolhido, decisao_compartilhamento) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = decisao_compartilhamento, 
              values_from = total,
              values_fill = 0) %>%
  clean_names() %>%
  ungroup() %>%
  group_by(estudo) %>%
  mutate(total = nao_publicar_nenhuma_noticia + opcao_1 + opcao_2,
         prop_op1 = opcao_1 /total,
         prop_op2 = opcao_2/total,
         nenhuma = nao_publicar_nenhuma_noticia/total)

# Gênero influencia decisão de compartilhar?
# estudo 1
compartilhamento_3_estudos %>%
  filter(Estudo == 1,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(decisao_compartilhamento)) + geom_bar() +
  facet_grid(genero ~ politico_escolhido)

# estudo 2
compartilhamento_3_estudos %>%
  filter(Estudo == 2,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(decisao_compartilhamento)) + geom_bar() +
  facet_grid(genero ~ politico_escolhido)


# estudo 3
compartilhamento_3_estudos %>%
  filter(Estudo == 3,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(decisao_compartilhamento)) + geom_bar() +
  facet_grid(genero ~ politico_escolhido)

# idade e compartilhamento

# estudo 1
compartilhamento_3_estudos %>%
  filter(Estudo == 1,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(col=decisao_compartilhamento, x=idade)) + geom_density()

# opção 2 é bem maior entre mais jovens

# estudo 2
compartilhamento_3_estudos %>%
  filter(Estudo == 2,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(col=decisao_compartilhamento, x=idade)) + geom_density()
# opção 2 é bem maior entre mais jovens, mas me parece que a opção de não publicar ganha uma faixa de idade um pouco mais a frente

# estudo 3
compartilhamento_3_estudos %>%
  filter(Estudo == 3,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(col=decisao_compartilhamento, x=idade)) + geom_density()
# único caso em que opção 2 não é maior entre mais jovens, mas a opção não publicar se destaca entre 40 e 50 anos


# Calcular scores de narcisismo e identidade nacional
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(score_narcissism = select(., 24:26) %>% apply(1, sum, na.rm=TRUE))
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(score_id_nac = select(., 27:28) %>% apply(1, sum, na.rm=TRUE))

#Narcisismo e Id.Nacional em função do político escolhido
# estudo 1
compartilhamento_3_estudos %>%
  filter(Estudo == 1,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico_escolhido)) + geom_point() +
  ggtitle("Estudo 1") +
  facet_wrap(vars(politico_escolhido), nrow = 2)

# estudo 2
compartilhamento_3_estudos %>%
  filter(Estudo == 2,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico_escolhido)) + geom_point() +
  ggtitle("Estudo 2") +
  facet_wrap(vars(politico_escolhido), nrow = 2)

# estudo 3
compartilhamento_3_estudos %>%
  filter(Estudo == 3,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico_escolhido)) + geom_point() +
  ggtitle("Estudo 3") +
  facet_wrap(vars(politico_escolhido), nrow = 2)




## scores e decisão

# estudo 1
compartilhamento_3_estudos %>%
  filter(Estudo == 1,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico_escolhido)) + geom_point() +
  ggtitle("Estudo 1") +
  facet_wrap(vars(decisao_compartilhamento, politico_escolhido), nrow = 3)

# estudo 2
compartilhamento_3_estudos %>%
  filter(Estudo == 2,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico_escolhido)) + geom_point() +
  ggtitle("Estudo 2") +
  facet_wrap(vars(decisao_compartilhamento, politico_escolhido), nrow = 3)

# estudo 3
compartilhamento_3_estudos %>%
  filter(Estudo == 3,
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico_escolhido)) + geom_point() +
  ggtitle("Estudo 3") +
  facet_wrap(vars(decisao_compartilhamento, politico_escolhido), nrow = 3)