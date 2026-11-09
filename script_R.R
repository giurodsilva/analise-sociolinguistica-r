# Carregar pacotes essenciais:

library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)
library(rms)
library(sjPlot)
library(gridExtra)

# etc

# Carregar a planilha de dados em um objeto

#Inicialmente, a planilha foi carregada sem especificações sobre as variáveis, 
#a fim de observar os dados com as funções View(), str() e summary().

dados <- read_csv("a_atono.csv")

View(dados)
str(dados)
summary(dados)

#Depois, as seguinte especificações foram adicionadas: o padrão foi definido como "factor".
#GERACAO foi importada como factor, por conter valores categóricos.
#IDADE foi importada como "integer", por ter valores contínuos.
#INDICE.SOCIO como "double", por ter valores decimais. 
#As variáveis FAIXA.ETARIA, RENDA.I, RENDA.F, GERACAO, N.SILABAS, ESCOLARIDADE,
#VOGAL.SEGUINTE e a VD tiveram seus níveis reorganizados com a função 'fct_relevel'
#Nos cinco primeiros, a intenção foi deixar em ordem crescente. Já no caso de
#VOGAL.SEGUINTE, a organização seguiu o triângulo vocálico, começando pelas
#vogais mais altas e anteriores.
#Em ESCOLARIDADE, o nível médio foi colocada primeiro.
#Em VD, o nível "n" foi colocado como o primeiro, por ser o foco da análise.
#As mesmas funções utilizadas anteriormente para observar os dados foram repetidas.

dados <- read_csv("a_atono.csv",
                  col_types = cols(.default = col_factor(),
                                   FAIXA.ETARIA = col_factor(levels = c("1a", "2a", "3a")),
                                   GERACAO = col_factor(levels = c("0", "1", "2", "3", "4")),
                                   RENDA.I = col_factor(levels = c("1a2", "2a4", "4a10", "10a20", "20+")),
                                   RENDA.F = col_factor(levels = c("1a2", "2a4", "4a10", "10a20", "20+")),
                                   INDICE.SOCIO = col_double(),
                                   ESCOLARIDADE = col_factor(levels = c("medio", "superior")),
                                   IDADE = col_integer(),
                                   VOGAL.SEGUINTE = col_factor(levels= c("i", "e", "3", "a", "an", "c", "o", "u")),
                                   N.SILABAS = col_factor(levels= c("duas", "tres", "quatro", "cinco", "seis")),
                                   VD = col_factor(levels= c("n", "o"))
                  ))


View(dados)
str(dados)
summary(dados)

#Notou-se que os dados de RENDA.I e RENDA.F do participante MauricioB 
#não foi importado corretamente. No arquivo, algumas linhas estavam com "#REF". 
#Como em outras linhas a informação estava presente, apenas subtituí
#os dados que estavam com o erro:

dados$RENDA.I[c(1746:1759)] <- "4a10"
dados$RENDA.F[c(1746:1759)] <- "4a10"

#Um dos valores da variável AFIXO era igual ao da VD (ambas estão com "n" - 
#no caso de VD, significa nasal e, no caso de Afixo, não); então,
#optou-se por substituí-los: "s" por "sim" e "n" por "não". 
#Para fazer isso, utilizou-se a função 'fct_recode()'. 
#(retirado de https://www.quora.com/How-do-I-replace-values-in-a-column-using-a-dplyr-package-in-R).

#Como precaução, tabulei os dados antes de realizar a substituição
#para confirmar que nenhum dado seria perdido:

table(dados$AFIXO)

dados <- dados %>%
  mutate(AFIXO = fct_recode(AFIXO,
                               "sim" = "s",
                               "não" = "n"))


levels(dados$AFIXO)

table(dados$AFIXO)

######################ANÁLISE EXPLORATÓRIA######################################

#Para ter uma visão geral da distribução dos dados,optou-se por gerar gráficos 
#simples de cada varíavel que será quantitativamente analisada. 

#Para as varíaveis independente nominais, dataframes com os valores de 
#proporções foram tabulados por meio das funções 'count', group_by' e 'mutate',
#do pacote tidyverse. 

#Depois, gráficos simples em barra (geom_bar) foram gerados com o usa da função
#'ggplot()'. Os valores das proporções foram colocadas no eixo y, e a varíavel
#independente no eixo x.

#Para a varíavel IDADE (númerica contínua), foi plotado um gráfico do tipo boxplot.

#Para a varíavel INDICE.SOCIO (decimais), foi plotado um gráfico de barra e 
#um gráfico de linhas.

#Atribui-se um "nome" para cada gráfico, facilitando a exportação para o 
#diretório de trabalho atual, feita com a função 'ggsave()'. 
#Além das funções primordiais ('aes', 'geom_bar', 'geom_boxplot', geom_line(),
#'geom_point()' etc) foram adicionadas funções para modificar a estética 
#do gráfico: 
#ggtitle(), para adicionar o título;
#labs(), para nomear os eixos x e y;
#theme_light(), para modificar o background;
#e theme(plot.title = element_text(hjust = 0.5)), para deixar o título
#centralizado (retirado de https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2).

#Tanto as tabelas quanto os gráficos foram colocados entre parênteses para 
#obter automaticamente o output.

####Gráficos das variáveis sociais:

#Faixa Etária

(df.FAIXA <- dados %>% 
  count(FAIXA.ETARIA, VD) %>% 
  group_by(FAIXA.ETARIA) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_faixaetaria <- ggplot(df.FAIXA, aes(x = FAIXA.ETARIA, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \npor faixa etária") +
  labs(x = "Faixa Etária", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))
          
#Idade

(grafico_idade <- ggplot(dados, aes(x = VD, y = IDADE, fill = VD)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Realização de /a/ átono", y = "Idade")+
  ggtitle("Distribuição de idade por variante de /a/ átono") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Geração

(df.GERACAO <- dados %>% 
    count(GERACAO, VD) %>% 
    group_by(GERACAO) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_geracao <- ggplot(df.GERACAO, aes(x = GERACAO, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono por geração") +
  labs(x = "Geração", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Região

(df.REGIAO <- dados %>% 
  count(REGIAO, VD) %>% 
  group_by(REGIAO) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_regiao <- ggplot(df.REGIAO, aes(x = REGIAO, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono por região") +
  labs(x = "Região", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Zona

(df.ZONA <- dados %>% 
    count(ZONA, VD) %>% 
    group_by(ZONA) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_zona <- ggplot(df.ZONA, aes(x = ZONA, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono por zona") +
  labs(x = "Zona", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Sexo

(df.SEXO <- dados %>% 
  count(SEXO.GENERO, VD) %>% 
  group_by(SEXO.GENERO) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_sexogenero <- ggplot(df.SEXO, aes(x = SEXO.GENERO, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono por sexo") +
  labs(x = "Sexo", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Escolaridade

(df.ESCOLARIDADE <- dados %>% 
  count(ESCOLARIDADE, VD) %>% 
  group_by(ESCOLARIDADE) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_escolaridade <- ggplot(df.ESCOLARIDADE, aes(x = ESCOLARIDADE, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono por escolaridade") +
  labs(x = "Escolaridade", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Renda Individual

(df.RENDA_I <- dados %>% 
    count(RENDA.I, VD) %>% 
    group_by(RENDA.I) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_rendaind <- ggplot(df.RENDA_I, aes(x = RENDA.I, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \npor renda individual") +
  labs(x = "Renda Individual", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Renda Familiar

(df.RENDA_F <- dados %>% 
    count(RENDA.F, VD) %>% 
    group_by(RENDA.F) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_rendafam <- ggplot(df.RENDA_F, aes(x = RENDA.F, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \npor renda familiar") +
  labs(x = "Renda Familiar", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Índice Socioeconômico

(df.INDICESOCIO <- dados %>% 
    count(INDICE.SOCIO, VD) %>% 
    group_by(INDICE.SOCIO) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_is_barras <- ggplot(df.INDICESOCIO, aes(x = INDICE.SOCIO, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \npor índice socioeconômico") +
  labs(x = "Índice Socioeconômico", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

(grafico_is_linhas <- ggplot(df.INDICESOCIO, aes(x = INDICE.SOCIO, y = prop, color = VD, group = VD)) +
  geom_line() +
  geom_point() +
  ggtitle("Proporções das variantes de /a/ átono \npor índice socioeconômico") +
  labs(x = "Índice Socioeconômico", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

####Depois, as linguísticas.

#Nasal

(df.NASAL_SEG <- dados %>% 
  count(NASAL.SEGUINTE, VD) %>% 
  group_by(NASAL.SEGUINTE) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_nasalseg <- ggplot(df.NASAL_SEG, aes(x = NASAL.SEGUINTE, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função da nasal seguinte") +
  labs(x = "Nasal", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Posição Tonal

(df.POS_TONAL <- dados %>% 
  count(POSICAO.TON, VD) %>% 
  group_by(POSICAO.TON) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_postonal <- ggplot(df.POS_TONAL, aes(x = POSICAO.TON, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") +
  ggtitle("Proporções das variantes de /a/ átono \nem função da posição tonal") +
  labs(x = "Posição", y = "Proporção") +  theme_classic() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Posição na Palavra

(df.POS_PALAVRA <- dados %>% 
  count(POSICA.PAL, VD) %>% 
  group_by(POSICA.PAL) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_pospalavra <- ggplot(df.POS_PALAVRA, aes(x = POSICA.PAL, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função da posição na palavra") +
  labs(x = "Posição", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Vogal Seguinte

(df.VOGAL.SEGUINTE <- dados %>% 
  count(VOGAL.SEGUINTE, VD) %>% 
  group_by(VOGAL.SEGUINTE) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_vogalseg <- ggplot(df.VOGAL.SEGUINTE, aes(x = VOGAL.SEGUINTE, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função da vogal seguinte") +
  labs(x = "Vogal", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Número de Sílabas

(df.N.SILABAS <- dados %>% 
  count(N.SILABAS, VD) %>% 
  group_by(N.SILABAS) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_nsilabas <- ggplot(df.N.SILABAS, aes(x = N.SILABAS, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função do número de sílabas da palavra") +
  labs(x = "Número de Sílabas", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Classe Morfológica

(df.CLASSE.MORFO <- dados %>% 
  count(CLASSE.MORFO, VD) %>% 
  group_by(CLASSE.MORFO) %>% 
  mutate(prop = prop.table(n)*100))

(grafico_cm <- ggplot(df.CLASSE.MORFO, aes(x = CLASSE.MORFO, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função da classe morfológica") +
  labs(x = "Classe Morfológica", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Acento seguinte

(df.AC_SEG <- dados %>% 
    count(ACENTO.SEGUINTE, VD) %>% 
    group_by(ACENTO.SEGUINTE) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_acentoSEG <- ggplot(df.AC_SEG, aes(x = ACENTO.SEGUINTE, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono em função \ndo acento da sílaba seguinte") +
  labs(x = "Acento Seguinte", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

#Acento do /a/ em foco

(df.AC_A <- dados %>% 
    count(ACENTO.A, VD) %>% 
    group_by(ACENTO.A) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_acentoA <- ggplot(df.AC_A, aes(x = ACENTO.A, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função do acento") +
  labs(x = "Acento", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))


#Afixo

(df.AFIXO <- dados %>% 
    count(AFIXO, VD) %>% 
    group_by(AFIXO) %>% 
    mutate(prop = prop.table(n)*100))

(grafico_afixo <- ggplot(df.AFIXO, aes(x = AFIXO, y = prop, fill = VD)) + 
  geom_bar(stat = "identity", color = "black") + 
  ggtitle("Proporções das variantes de /a/ átono \nem função de ser ou não afixo") +
  labs(x = "Afixo Derivacional", y = "Proporção") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)))

###Função para salvar os gráficos no diretório de trabalho atual
###(retirado de https://ggplot2.tidyverse.org/reference/ggsave.html)

ggsave("grafico_faixaetaria.png", grafico_faixaetaria, width = 8, height = 6, dpi = 300)
ggsave("grafico_idade.png", grafico_idade, width = 8, height = 6, dpi = 300)
ggsave("grafico_geracao.png", grafico_geracao, width = 8, height = 6, dpi = 300)
ggsave("grafico_regiao.png", grafico_regiao, width = 8, height = 6, dpi = 300)
ggsave("grafico_zona.png", grafico_zona, width = 8, height = 6, dpi = 300)
ggsave("grafico_sexogenero.png", grafico_sexogenero, width = 8, height = 6, dpi = 300)
ggsave("grafico_escolaridade.png", grafico_escolaridade, width = 8, height = 6, dpi = 300)
ggsave("grafico_rendaind.png", grafico_rendaind, width = 8, height = 6, dpi = 300)
ggsave("grafico_rendafam.png", grafico_rendafam, width = 8, height = 6, dpi = 300)
ggsave("grafico_is_barras.png", grafico_is_barras, width = 8, height = 6, dpi = 300)
ggsave("grafico_is_linhas.png", grafico_is_linhas, width = 8, height = 6, dpi = 300)
ggsave("grafico_nasalseg.png", grafico_nasalseg, width = 8, height = 6, dpi = 300)
ggsave("grafico_postonal.png", grafico_postonal, width = 8, height = 6, dpi = 300)
ggsave("grafico_pospalavra.png", grafico_pospalavra, width = 8, height = 6, dpi = 300)
ggsave("grafico_vogalseg.png", grafico_vogalseg, width = 8, height = 6, dpi = 300)
ggsave("grafico_nsilabas.png", grafico_nsilabas, width = 8, height = 6, dpi = 300)
ggsave("grafico_cm.png", grafico_cm, width = 8, height = 6, dpi = 300)
ggsave("grafico_acentoSEG.png", grafico_acentoSEG, width = 8, height = 6, dpi = 300)
ggsave("grafico_acentoA.png", grafico_acentoA, width = 8, height = 6, dpi = 300)
ggsave("grafico_afixo.png", grafico_afixo, width = 8, height = 6, dpi = 300)

######################ANÁLISES UNIVARIADAS######################################

#Observando os gráficos e as tabelas, notou-se que 
#as variaveis ESCOLARIDADE, ACENTO.A, IDADE, REGIÃO e SEXO
#parecem não ter diferenças significativas quanto ao uso das variáveis da VD. 
#Contudo, para que seja possível saber de fato, testes univariados foram realizados
#com cada variável.

#Para as varíaveis independentes nominais, criaram-se tabelas contando
#quantas ocorrências existem para cada combinação possível das duas variáveis
#por meio da função base do R 'table'. A partir das tabelas,
#foi aplicado o teste de qui-quadrado, com a função 'chisq.test'.

#Para as variáveis numéricas, aplicou-se inicialmente o teste de Shapiro-Wilk 
#para verificar a normalidade da distribuição em cada grupo 
#da variável dependente.
#Como em todos os casos a distribuição foi anormal,o teste Wilcox foi utilizado
#com a função 'wilcox.test'.

#Os valores do valor p, que mostra se há uma associação estatisticamente 
#significativa entre as variáveis, foi colocado abaixo de cada código. 
#Novamente, todas as strings foram colocadas entre parênteses para o output
#ser automático.

#SEXO.GENERO
(tab.sexgen <- with(dados, table(SEXO.GENERO, VD)))
(x2.sexgen <- chisq.test(tab.sexgen))

##Resultado: p < 0.05

#IDADE
shapiro.test(dados$IDADE[dados$VD == "o"])
shapiro.test(dados$IDADE[dados$VD == "n"])

wilcox.test(IDADE ~ VD, data = dados)

##Resultado: Shapiro - distribuição anormal para "o" e "n". Wilcox - p < 0.05.

#FAIXA ETÁRIA
(tab.fe <- with(dados, table(FAIXA.ETARIA, VD)))
(x2.fe <- chisq.test(tab.fe))

##Resultado: p < 0.05.

#ESCOLARIDADE
(tab.esc <- with(dados, table(ESCOLARIDADE, VD)))
(x2.esc <- chisq.test(tab.esc))

##Resultado: p < 0.05.

#REGIAO
(tab.regiao <- with(dados, table(REGIAO, VD)))
(x2.regiao <- chisq.test(tab.regiao))

##Resultado: p > 0.05.

#ZONA
(tab.zona <- with(dados, table(ZONA, VD)))
(x2.zona <- chisq.test(tab.zona))

##Resultado: p < 0.05.

#GERACAO
(tab.geracao <- with(dados, table(GERACAO, VD)))
(x2.geracao <- chisq.test(tab.geracao))

##Resultado: p > 0.05.

#RENDA.I
(tab.rendai <- with(dados, table(RENDA.I, VD)))
(x2.rendai <- chisq.test(tab.rendai))

##Resultado: p < 0.05.

#RENDA.F
(tab.rendaf <- with(dados, table(RENDA.F, VD)))
(x2.rendaf <- chisq.test(tab.rendaf))

##Resultado: p < 0.05.

#INDICE.SOCIO

shapiro.test(dados$INDICE.SOCIO[dados$VD == "o"])
shapiro.test(dados$INDICE.SOCIO[dados$VD == "n"])

wilcox.test(INDICE.SOCIO ~ VD, data = dados)

##Resultado: Shapiro - distribuição anormal para "o" e "n". Wilcox: p < 0.05.

#NASAL.SEGUINTE
(tab.nasalseg <- with(dados, table(NASAL.SEGUINTE, VD)))
(x2.nasalseg <- chisq.test(tab.nasalseg))

##Resultado: p < 0.05.

#POSICAO.TON
(tab.poston <- with(dados, table(POSICAO.TON, VD)))
(x2.ponton <- chisq.test(tab.poston))

##Resultado: p < 0.05.

#POSICA.PAL
(tab.pospal <- with(dados, table(POSICA.PAL, VD)))
(x2.pospal <- chisq.test(tab.pospal))

##Resultado: p < 0.05.

#N.SILABAS
(tab.nsil <- with(dados, table(N.SILABAS, VD)))
(x2.nsil <- chisq.test(tab.nsil))

##Resultado: p < 0.05.

#CLASSE.MORFO
(tab.cm <- with(dados, table(CLASSE.MORFO, VD)))
(x2.cm <- chisq.test(tab.cm))

##Resultado: p < 0.05.

#ACENTO.SEGUINTE
(tab.acentoseg <- with(dados, table(ACENTO.SEGUINTE, VD)))
(x2.acentoseg <- chisq.test(tab.acentoseg))

##Resultado: p < 0.05.

#ACENTO.A
(tab.acentoA <- with(dados, table(ACENTO.A, VD)))
(x2.acentoA <- chisq.test(tab.acentoA))

##Resultado: p < 0.05.

#AFIXO
(tab.afixo <- with(dados, table(AFIXO, VD)))
(x2.afixo <- chisq.test(tab.afixo))

##Resultado: p < 0.05.

#VOGAL.SEGUINTE
(tab.vogalseg <- with(dados, table(VOGAL.SEGUINTE, VD)))
(x2.vogalseg <- chisq.test(tab.vogalseg))

##Resultado: p < 0.05.

#Somente escolaridade e região não apresentaram diferenças significativas.

######################ANÁLISES MULTIVARIADAS######################################

#Após observar as tabelas e os gráficos, optou-se por amalgamar os níveis de
#VOGAL.SEGUINTE, GERACAO, ACENTO.SEGUINTE, CLASSE.MORFO e ZONA, a fim de reduzir
#o excesso de níveis (o que poderia "travar" os modelos de regressão) e/ou 
#equilibrar a distribuição de dados entre as categorias. 
#O processo foi feito por meio da função 'fct_collapse'.

#Os dados de VOGAL.SEGUINTE foram reclassificados de acordo com a posição da 
#língua. Tal formato foi escolhido, pois era o que mais condizia com a 
#ditribuição de dados e as proporções vistas nas tabelas geradas na análise
#univariada.

dados$VOGAL.SEGUINTE <- fct_collapse(dados$VOGAL.SEGUINTE,
                                     posterior = c("o", "c", "u"),
                                     anterior = c("e", "i", "3"),
                                     central = c("a", "an")
)

#Os dados de GERACAO foram amalgamados considerando: se os pais não são de 
#São Paulo; se um ou ambos os pais são de São Paulo; e se um ou ambos os avós
#são de São Paulo.

dados$GERACAO <- fct_collapse(dados$GERACAO,
                              nao.SP = c("0"),
                              pais.SP = c("1", "2"),
                              avos.SP = c("3", "4")
)

#Os dados de ACENTO.SEGUINTE foram reorganizados considerado a quantidade de 
#dados (primario tem 3722 dados, enquanto secundario tem 355 e atono tem 707)
#e as proporções: as categorias atono e primario tem proporções mais próximas
#quanto à distribuição da variante nasal (68 e 68.7, respectivamente) em 
#comparação à primario (21.0).

dados$ACENTO.SEGUINTE <- fct_collapse(dados$ACENTO.SEGUINTE,
                                      primario = c("primario"),
                                      nao.primario = c("atono", "secundario")
)

dados$ACENTO.SEGUINTE <- fct_relevel(dados$ACENTO.SEGUINTE, "nao.primario", "primario")

#A reorganização de CLASSE.MORFO considerou a ditribuição das proporções.
#subatantivos, adjetivos e nomes tem proporções próximas (22% de nasais
#aproximadamente), enquanto advérbio possui apenas 8% e verbo 49%.

dados$CLASSE.MORFO <- fct_collapse(dados$CLASSE.MORFO,
                                   verbo = c("verbo"),
                                   adverbio = c("adverbio"),
                                   nominais = c("substantivo", "nome", "adjetivo")
)


dados$CLASSE.MORFO <- fct_relevel(dados$CLASSE.MORFO, "verbo", "nominais", "adverbio")

#O mesmo critério de CLASSE.MORFO foi utilizado ao reorganizar os níveis da
#variável ZONA:

dados$ZONA <- fct_collapse(dados$ZONA,
                           norte = c("norte"),
                           centro_sul = c("centro", "sul"),
                           leste_oeste = c("leste", "oeste")
)

dados$ZONA <- fct_relevel(dados$ZONA, "norte", "centro_sul", "leste_oeste")

#Por fim, também optou-se por amalgamar a váriavel SILABAS para diminuir a 
#quantidade de níveis. Anteriormente, considerou não fazê-lo, pois a categoria
#duas apresenta poucos dados e um comportamente diferente em relação ao restante,
#segundo os gráficos. Mas como os modelos estavam tem problemas em convergir,
#os níveis foram reduzidos para três:

dados$N.SILABAS <- fct_collapse(dados$N.SILABAS,
                           s2 = c("duas"),
                           s3a4 = c("tres", "quatro"),
                           sa6 = c("cinco", "seis")
)

#Então, verificou-se se os níveis foram reorganizado corretamente.

levels(dados$VOGAL.SEGUINTE)
levels(dados$GERACAO)
levels(dados$ACENTO.SEGUINTE)
levels(dados$CLASSE.MORFO)
levels(dados$ZONA)

#Antes de treinar os modelos, almejou-se testar possíveis colinearidades entre as
#variáveis por meio da função vif(). Primeio, criou-se um modelo com a função
#'glm()' e, depois, o GVIF foi gerado.

#Modelo com todas as variáveis:

modelo.vif_geral <- glm(VD ~ SEXO.GENERO + IDADE
                        + FAIXA.ETARIA + ESCOLARIDADE + REGIAO + ZONA
                        + GERACAO + RENDA.I + RENDA.F + INDICE.SOCIO
                        + NASAL.SEGUINTE + POSICAO.TON + POSICA.PAL
                        +N.SILABAS + CLASSE.MORFO + ACENTO.SEGUINTE
                        +ACENTO.A + AFIXO + VOGAL.SEGUINTE,
                        data = dados, 
                        family = binomial)

car::vif(modelo.vif_geral)

#Resultado: RENDA.I, RENDA.F e FAIXA.ETARIA com GVIF^(1/(2*Df)) > 2; IDADE > 3 e
#INDICE.SOCIO > 19.

#Modelo isolando renda + escolaridade
modelo.vif_econ <- glm(VD ~ RENDA.I + RENDA.F + INDICE.SOCIO + ESCOLARIDADE,
                        data = dados, 
                        family = binomial)

car::vif(modelo.vif_econ)

#Resultado: RENDA.I, RENDA.F e FAIXA.ETARIA com GVIF^(1/(2*Df)) > 2; IDADE > 3 e
#INDICE.SOCIO > 19.

#Modelo isolando regiao + zona + geracao
modelo.vif_local <- glm(VD ~ REGIAO + ZONA + GERACAO, 
                       data = dados, 
                       family = binomial)

car::vif(modelo.vif_local)

#Resultado: Todas as variáveis com GVIF^(1/(2*Df)) < 2.

#Modelo isolando idade + faixa.etaria.geracao
modelo.vif_idade <- glm(VD ~ IDADE + FAIXA.ETARIA + GERACAO, 
                       data = dados, 
                       family = binomial)

car::vif(modelo.vif_idade)

#Resultado: GVIF^(1/(2*Df)) IDADE > 2. Restando < 2.


#Modelo geral sem IDADE, RENDA.I e RENDA.F

modelo.vif_geral2 <- glm(VD ~ SEXO.GENERO + FAIXA.ETARIA + ESCOLARIDADE + REGIAO + ZONA
                        + GERACAO + INDICE.SOCIO
                        + NASAL.SEGUINTE + POSICAO.TON + POSICA.PAL
                        +N.SILABAS + CLASSE.MORFO + ACENTO.SEGUINTE
                        +ACENTO.A + AFIXO + VOGAL.SEGUINTE,
                        data = dados, 
                        family = binomial)

car::vif(modelo.vif_geral2)

#Resultado: Todas as variáveis com GVIF^(1/(2*Df)) < 2.

#Optou-se por excluir IDADE e manter FAIXA.ETARIA para comparação com outros
#estudos que utilizam essa categorização. Sobre INDICE.SOCIO, RENDA.I e RENDA.F,
#somente o primeiro foi mantido, considerando que representa a média das duas 
#últimas.

##################TESTES ESTATÍSTICOS COM REGRESSÃO LOGÍSTICA##########################

#Primeiro, foram testados quatro modelos: um 'glmer' e um 'lrm' somente com as
#variáveis linguísticas (mais as variáveis aleatórias no caso do 'glmer'); 
#e um 'glmer' e um 'lrm' somente com as variáveis sociais (mais as variáveis
#aleatórias no caso do 'glmer').  

#O Segundo passo foi testar interações entre as variáveis sociais. Para isso, 
#20 modelos de efeitos mistos ('glmer') foram treinados.

#Para reportar os resultados, utilizou-se a função 'tab_model', do pacote
#'sjPlot', para exportar as tabelas para um documento de texto editavel. 

#Modelo misto ('glmer') com variáveis linguísticas

mod.linguistico <- glmer(VD ~ NASAL.SEGUINTE +
                           POSICAO.TON +
                           POSICA.PAL +
                           N.SILABAS + 
                           CLASSE.MORFO +
                           ACENTO.SEGUINTE +
                           ACENTO.A + 
                           AFIXO +
                           VOGAL.SEGUINTE +
                         (1|FALANTE) + (1|ITEM.LEXICAL), 
                         data = dados, family = binomial)

summary(mod.linguistico)

(tab_model(mod.linguistico, show.re.var = TRUE, transform = "plogis", file = "resultado_glmerling.doc"))

#Resultado: Todas as variáveis se mostraram significativas, contudo o modelo 
#não convergiu.

lrm(formula = VD ~ NASAL.SEGUINTE +
      POSICAO.TON +
      POSICA.PAL +
      N.SILABAS + 
      CLASSE.MORFO +
      ACENTO.SEGUINTE +
      ACENTO.A + 
      AFIXO +
      VOGAL.SEGUINTE, 
    data = dados)

#Resultado: C = 0.838 

#Modelo misto ('glmer') com variáveis sociais

mod.social <- glmer(VD ~ SEXO.GENERO + 
                      FAIXA.ETARIA + 
                      ESCOLARIDADE + 
                      REGIAO + 
                      ZONA +
                      GERACAO +
                      INDICE.SOCIO +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)
summary(mod.social)

(tab_model(mod.social, show.re.var = TRUE, transform = "plogis", file = "resultado_glmersoc.doc"))

#Resposta: O modelo convergiu, mas falhou no teste de gradiente. Nenhuma
#variável se mostrou significativa estatisticamente.

lrm(formula = VD ~ SEXO.GENERO + 
      FAIXA.ETARIA + ESCOLARIDADE + 
      REGIAO + 
      ZONA +
      GERACAO +
      INDICE.SOCIO, 
    data = dados)

#Resultado: C = 0.590.

#Verificando a existência de interações

mod.social1 <- glmer(VD ~ SEXO.GENERO* 
                      FAIXA.ETARIA +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social2 <- glmer(VD ~ SEXO.GENERO*INDICE.SOCIO + 
                      + (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social3 <- glmer(VD ~ FAIXA.ETARIA*ESCOLARIDADE + 
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social4 <- glmer(VD ~ FAIXA.ETARIA*REGIAO + 
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social5 <- glmer(VD ~ FAIXA.ETARIA*ZONA +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social6 <- glmer(VD ~ FAIXA.ETARIA*GERACAO +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social7 <- glmer(VD ~ ESCOLARIDADE*REGIAO + 
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social8 <- glmer(VD ~ ESCOLARIDADE*INDICE.SOCIO +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social9 <- glmer(VD ~ REGIAO*GERACAO +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

mod.social10 <- glmer(VD ~ REGIAO*INDICE.SOCIO +
                      (1|FALANTE) + (1|ITEM.LEXICAL), 
                    data = dados, family = binomial)

#A função 'summary' foi aplicadas posteriormente para facilitar a cópia dos 
#resultados para um software de edição de texto, que possibilita uma melhor
#visualização dos resultados.

summary(mod.social2)
summary(mod.social3)
summary(mod.social4)
summary(mod.social5)
summary(mod.social6)
summary(mod.social7)
summary(mod.social8)
summary(mod.social9)
summary(mod.social10)

#Os resultados dos modelos cujas interações mostraram-se estatisticamente
#significativas foram exportados:

(tab_model(mod.social5, show.re.var = TRUE, transform = "plogis", file = "resultado_faixa_zona.doc"))
(tab_model(mod.social6, show.re.var = TRUE, transform = "plogis", file = "resultado_faixa_geracao.doc"))
(tab_model(mod.social8, show.re.var = TRUE, transform = "plogis", file = "resultado_escolaridade_indice.doc"))
(tab_model(mod.social10, show.re.var = TRUE, transform = "plogis", file = "resultado_regiao_indice.doc"))

#Gráficos dos modelos que tiveram a interação significativa estatisticamente,
#também tiveram gráficos gerados com a função "allEffects" e salvos com a função
#'png'

png("grafico_faixa_zona.png", width = 800, height = 600, res = 120)
plot(allEffects(mod.social5), type = "response")
dev.off()

png("grafico_faixa_geracao.png", width = 800, height = 600, res = 120)
plot(allEffects(mod.social6), type = "response")
dev.off()

png("grafico_escolaridade_indice.png", width = 800, height = 600, res = 120)
plot(allEffects(mod.social8), type = "response")
dev.off()

png("grafico_regiao_indice.png", width = 800, height = 600, res = 120)
plot(allEffects(mod.social10), type = "response")
dev.off()

#Testando todas as variáveis sociais com as interações positivas

mod_teste_com_interacoes <- glmer(VD ~ SEXO.GENERO + FAIXA.ETARIA*GERACAO+
                  FAIXA.ETARIA*ZONA + INDICE.SOCIO*ESCOLARIDADE+
                  INDICE.SOCIO*REGIAO + (1|FALANTE) +
                    (1|ITEM.LEXICAL), data = dados, family = binomial)

summary(mod_teste_com_interacoes)

#O modelo não convergiu. INDICE.SOCIO*REGIAO e FAIXA.ETARIA*GERACAO não foram
#considerados significativos.
#Isoladamente, SEXO.GENERO, INDICE.SOCIO E REGIAO também não mostraram efeitos
#significativos.

#O próximo passo foi testar as funções métodos de seleção de variáveis com as
#funções 'step' (nos modos foward, back e both) e 'drop1'.

mod_geral1 <- glm(VD ~ FAIXA.ETARIA*ZONA +
                    INDICE.SOCIO*ESCOLARIDADE + 
        GERACAO +
        NASAL.SEGUINTE +
        POSICAO.TON + 
        POSICA.PAL + 
        N.SILABAS + 
        CLASSE.MORFO + 
        ACENTO.SEGUINTE +
        ACENTO.A + 
        AFIXO + 
        VOGAL.SEGUINTE,
        data = dados, family = binomial)

summary(mod_geral1)

#step foward

m0 <- glm(VD ~ 1, data = dados, family = binomial)


m.fw <- step(m0, direction = "forward", scope = ~ 
               FAIXA.ETARIA*ZONA +
               INDICE.SOCIO*ESCOLARIDADE + 
               GERACAO +
               NASAL.SEGUINTE +
               POSICAO.TON + 
               POSICA.PAL + 
               N.SILABAS + 
               CLASSE.MORFO + 
               ACENTO.SEGUINTE +
               ACENTO.A + 
               AFIXO + 
               VOGAL.SEGUINTE)

m.fw

#step backward

m.bw <- step(mod_geral1, direction = "backward")

m.bw

#step both

m.both <- step(m0, direction = "forward", scope = ~ FAIXA.ETARIA*ZONA +
                 INDICE.SOCIO*ESCOLARIDADE + 
                 GERACAO +
                 NASAL.SEGUINTE +
                 POSICAO.TON + 
                 POSICA.PAL + 
                 N.SILABAS + 
                 CLASSE.MORFO + 
                 ACENTO.SEGUINTE +
                 ACENTO.A + 
                 AFIXO + 
                 VOGAL.SEGUINTE)

m.both

#drop1

drop1(mod_geral1, test = "Chisq")

#Resultado: O método backward resultou no melhor ajuste (AIC: 3522),
#preservando as interações de Faixa Etária/Zona e Índice Sócio/Escolaridade.

#Os métodos Forward e Both convergiram para um modelo mais simples (AIC: 3543),
#excluindo a interação INDICE.SOCIO*ESCOLARIDADE.

#Já o drop1 descartou apenas ACENTO.A. 

#####Testes com variáveis aleatórias

#Para os teste de modelos finais, ACENTO.A.  Além disso, em testes preliminares,
#os modelos de efeitos mistos falhavam em convergir; então, mas variáveis foram
#excluidas com base nos resultados de sgnificância dos teste preliminares 
#(GERACAO e ACENTO.SEG tiveram p < 0.05) e a observação realizada dos gráficos
#e proporções.

#Outra modificação realizada para contornar o problema de conversão foi 
#nos parâmetros do modelo por meio da função 'glmerControl()'. 
#(retirado de: https://www.geeksforgeeks.org/machine-learning/how-to-resolve-r-glmer-warnings-model-fails-to-converge-and-model-is-nearly-unidentifiable/
#e https://mahayana.me/mlm/)

#As modificações foram:
#mudar o otimizador padrão do R (Nelder-Mead) para o BOBYQA;
#aumentar o número de iterações para 200.000.

#Modelo 1: somente FALANTE 

modelo_variaveisaletorias <- glmer(VD ~ FAIXA.ETARIA*ZONA +
                                     INDICE.SOCIO*ESCOLARIDADE + 
                                     NASAL.SEGUINTE +
                                     POSICAO.TON + 
                                     POSICA.PAL + 
                                     N.SILABAS + 
                                     CLASSE.MORFO + 
                                     AFIXO + 
                                     VOGAL.SEGUINTE +(1|FALANTE),
                   data = dados, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

#Modelo 2: somente ITEM.LEXICAL

modelo_variaveisaletorias2 <- glmer(VD ~ FAIXA.ETARIA*ZONA +
                                      INDICE.SOCIO*ESCOLARIDADE + 
                                      NASAL.SEGUINTE +
                                      POSICAO.TON + 
                                      POSICA.PAL + 
                                      N.SILABAS + 
                                      CLASSE.MORFO + 
                                      AFIXO + 
                                      VOGAL.SEGUINTE +(1|ITEM.LEXICAL),
                                    data = dados, family = binomial,
                                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
#Modelo 3: somente FALANTE e ITEM.LEXICAL

modelo_variaveisaletorias3 <- glmer(VD ~ FAIXA.ETARIA*ZONA +
                                      INDICE.SOCIO*ESCOLARIDADE + 
                                      NASAL.SEGUINTE +
                                      POSICAO.TON + 
                                      POSICA.PAL + 
                                      N.SILABAS + 
                                      CLASSE.MORFO + 
                                      AFIXO + 
                                      VOGAL.SEGUINTE + (1|FALANTE) + (1|ITEM.LEXICAL),
                                    data = dados, family = binomial,
                                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

summary(modelo_variaveisaletorias)
summary(modelo_variaveisaletorias2)
summary(modelo_variaveisaletorias3)


#Para comparar os modelos, utilizou-se a função 'anova' (retirado de Gries, 2019)
anova(modelo_variaveisaletorias, modelo_variaveisaletorias2, modelo_variaveisaletorias3)

#Resultado: O modelo considerado mais significativo foi o 
#"modelo_variaveisaleatorias3" com Pr(>Chisq) < 2.2e-16

lrm(VD ~ FAIXA.ETARIA*ZONA +
      INDICE.SOCIO*ESCOLARIDADE + 
      NASAL.SEGUINTE +
      POSICAO.TON + 
      POSICA.PAL + 
      N.SILABAS + 
      CLASSE.MORFO + 
      AFIXO + 
      VOGAL.SEGUINTE, data = dados)

#Resultado: C = 0.858

#Exportação dos resultados em logoods e em porcentagem para um documento editável:
tab_model(modelo_variaveisaletorias3, show.re.var = TRUE, transform = "exp", file = "resultados_modelofinal.doc")
tab_model(modelo_variaveisaletorias3, show.re.var = TRUE, transform = "plogis", file = "resultados_pesos.doc")

#plotando gráficos com as variáveis de maior significância.
plot(effect('AFIXO', modelo_variaveisaletorias3))
plot(effect('NASAL.SEGUINTE', modelo_variaveisaletorias3))
plot(effect('POSICA.PAL', modelo_variaveisaletorias3))
plot(effect('FAIXA.ETARIA:ZONA', modelo_variaveisaletorias3))
plot(effect('INDICE.SOCIO:ESCOLARIDADE', modelo_variaveisaletorias3))

#plotando os efeitos aleatórios
#(retirado de https://strengejacke.github.io/sjPlot/articles/plot_interactions.html)
plot_model(modelo_variaveisaletorias3, type = "re")
