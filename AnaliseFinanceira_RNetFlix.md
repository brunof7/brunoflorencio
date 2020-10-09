# brunoflorencio
# Saúde Financeira - NETFLIX 

# Direcionando para o local do projeto
setwd("C:/DSA/AnaliseR/Projeto")
getwd()


#Carregando as bilbiotecas necessárias

 
library(readxl)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(magrittr)
library(stringr)
library(psych) # para usar a função describBY
library(RVAideMemoire) # função utilizada para fazer o teste de shapiro 
library(car) # para fazer o teste de levane
library(DescTools) # para os teste post-hoc Tukey HSD

#Carregando o arquivo Netflix_Data para o dataset

Netflix_Dados<- as.data.frame (read_xlsx ("Netflix_Data.xlsx", sheet="Dados", col_names = TRUE))
View(Netflix_Dados)

# Trocando o nome das colunas (variáveis)
# A opção deve-se primeiramente para melhorar as buscas pelo nome das variáveis 
# e fiz a tradução, isso me ajudou entender melhor o dataset

Netflix_Dados <- rename (Netflix_Dados, "Data"                = "Time",  
                                        "Assinatura_Total"    = "Total subscriptions at end of period",
                                        "Assinatura_Paga"     = "Paid subscriptions at end of period",
                                        "Teste_gratuito"      = "Free Trails",
                                        "Receita"             = "Revenue",
                                        "Custo_Receita"       = "Cost of revenues",
                                        "Custo_Marketing"     = "Marketing",
                                        "Margem_Lucro"        = "Contribution profit",
                                        "Margem_Contribuição" = "Contribution Margin",
                                        "Custo_Cliente"       = "Cost per Customer (excluding marketing)",
                                        "Receita_Cliente"     = "Revenue per Customer",
                                        "Ganho_cliente"       = "Earnings per Customer",
                                        "Segmento"            = "Segment")

#Transformação para data, penso que aqui, irei trabalhar com algumas delas
Netflix_Dados$Data<- mdy(Netflix_Dados$Data)
Netflix_Dados$Mes<- month(Netflix_Dados$Data)
Netflix_Dados$Ano<- year(Netflix_Dados$Data)

# Essa função vai trazer o nome dos meses de acordo com a configuração local
# do meu equipamento
Netflix_Dados$Mes_Ano<- paste(month(Netflix_Dados$Mes, label = TRUE), Netflix_Dados$Ano, sep="/") 

# Criação de uma variavel categorica para relacionar o semestre
# Talvez foi o maior desafio nessa análise, pois para criarmos os testes de
# hipóstes e ANOVA, não tinhamos váriaveis categórica com mais de 2 níveis
# portanto, optei por primeiramente criar uma coluna para semestre e outra para
# trimestre (será útilizada na ANOVA)

Netflix_Dados$Semestre <- as.factor (mapvalues(Netflix_Dados$Mes, c(3,6,9,12), 
                       c("1ºSemestre", "1ºSemestre","2ºSemestre","2ºSemestre")))
Netflix_Dados$Trimestre <- as.factor (mapvalues(Netflix_Dados$Mes, c(3,6,9,12), 
                       c("1ºTrimestre", "2ºTrimestre","3ºTrimestre","4ºTrimestre")))

View(Netflix_Dados)

# Iniciando a análise exploratória dos dados e vendo alguns sumários estatísticos 
summary(Netflix_Dados$Receita)

#Plotando os dados para averiguar as diferenças no semestre e verificar existencia 'outliers'
ggplot (Netflix_Dados, aes(x = Semestre, y = Receita)) + geom_boxplot()

#Criando um novo objeto para não alterar o banco de dados original
Dados<- Netflix_Dados

attach(Dados) 

View(Dados)
describeBy(Dados$Receita, group = Dados$Semestre)
describeBy(Dados$Custo_Marketing, group = Dados$Semestre)


#Aqui utilizamos a função ddply para fazermos alguns resumos estatísticos.
ddply(Dados, ~ Semestre, summarize,
      Media = mean(Receita),
      DP  = sd(Receita),
      Erro_Margem = DP  / sqrt(length(Receita))
)
summary(Dados$Receita)

amostra1<- Dados %>% filter (Semestre == "1ºSemestre" ) %>% sample_n(10)
amostra2<- Dados %>% filter (Semestre == "2ºSemestre" ) %>% sample_n(10)

summary(amostra1$Receita)
summary(amostra2$Receita)

#Teste de Hipoteses
#H0 : Não há diferença significativa entre a média das receitas do 1º e 2º semestre  
#H1 : Há média de receita no 1º semestre é < que a do 2º Semestre
# Unicaudal a esquerda  
teste_t_dados <- t.test (amostra1$Receita, amostra2$Receita, alternative = "less" )
teste_t_dados

# Nosso estudo constata que a media da Receita, não difere significativamente 
# portanto, falhamos em rejeitar a hipótese nula, considerando que o valor p
# ficou acima do nível significância.  

#Teste de Anova
#H0 : Não há diferença significativa entre a média das receitas do 1º, 2º, 3º e 4º trimestre  
#H1 : Há diferença significativa entre os trimestres 

# similar ao str
glimpse(Dados) 

# Uma função do pacote psych para fazer análise descritiva
describeBy(Dados$Receita, group = Dados$Trimestre) 

# verifica a distribuição normal no grupo,se p maior que 0.05, é normalmente distribuida
byf.shapiro(Receita ~ Trimestre, Dados) 

# verifica a homogeneiedade da variância
leveneTest(Receita ~ Trimestre, center = mean) 

# verificando se tem outliers no pelo boxplot
ggplot (Dados, aes(x = Trimestre, y = Receita)) + geom_boxplot() 

tab_anova <- aov(Receita~Trimestre, Dados) # função nativa do R para fazermos a tabela anova
summary(tab_anova)

# O resultado da Anova demonstrou que realmente não há diferença significativa entre os trimestres 
# Portanto, falhamos em rejeitar a hipótese nula com [F(3,24) = 0.112; p= 0.95]. Inclusive cabe
# destacar que o F muito pequena indica que o valor p não detém tamanha significância para este teste.
# Apesar disso, pode-se dizer que, apesar de não deter uma diferença relevante entre as média 
# dos grupos, pode haver, algumas médias significativas entre eles, por isso, faremos o teste 
# post-hoc Tukey HSD, por se mostrar o mais equilibrado.

PostHocTest(tab_anova, method = "hsd")

# Novamente o teste de Tukey HSD demonstrou que há diferenças entre as medias entre
# os trimestres, porém sem um nível de significância


# Relatório 
# Foi, para mim, desafiador, pois trabalhei com conceitos e fórmulas que até 
# então nunca havia utilizado, porém acrescentou um novo conjunto de ferramentas
# em minhas análise agora por diante. Particularmente a definição dos testes de
# hipóteses foi a parte mais difícil, entender o problema de negócio, conforme
# evidenciado ao longo do treinamento, é o mais complexo.
# Entendido isso, pude procurar na internet técnicas de extração de dados a 
# análise dos dados. Não obstante, o fato de não ter tido uma variável do 
# tipo fator no DATASET de forma explicíta, complicou inicialmente minha análise,
# até que optei transformara a coluna de datas como fator para semestre e trimestre.

# A partir daí, procurei testar diversas váriaveis do DATASET, achei algumas análises
# com outliers, cujo tratei (e trago em anexo o código fonte ao final deste relatório),
# e vi como se comportavam, porém fique com a Receita e se ela mantinha ao longo do tempo
# o seu comportamento.

# Os teste de hipóteses feitos levaram a falha da rejeição da hipótese nula, ou seja,
# as médias dos grupos de tempo, não se alterava, o que pode inferir-se que a receita
# da Netflix a príncpio, vem mantendo uma consistência ao longo do período delimitado
# no dados.

