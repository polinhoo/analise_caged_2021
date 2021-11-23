####Análise da base do caged 2021 para pcd's
#setando o lugar onde está a base 
setwd("~/Documentos/pcds")

#trazendo a base de dados para dentro do R
base <- read.csv("caged2021.csv", sep=";", colClasses = "character")

#fazendo um resumo da base de dados
summary(base) 
  # todas estão em character

#vendo o nome das variáveis
names(base)
  
#uma tabela dos anos contidos na base
table(base$ano)

#uma tabela dos meses contidos na base
table(base$mes)

#mudando a variável mês para tipo factor (categórico)
base$mes <- as.factor(base$mes)

#verificando os rótulos (levels) da categoria mês
levels(base$mes)
#atribuindo o nome correto dos meses aos rótulos
levels(base$mes) <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto")
#pedindo a tabela mais uma vez
table(base$mes)


#instalando o pacite epiDisplay
install.packages("epiDisplay")
 #só precisa instalar uma única vez
#ativando o pacote
library(epiDisplay)
 #sempre que for usar o pacote pela primeira vez, lembrar de ativá-lo

#pedindo a mesma tabela porém com o comando do epiDisplay
tab1(base$mes)

#pedindo uma tabela para a variável uf
tab1(base$sigla_uf)
#pedindo a mesma tabela porém ordenando pela quantidade
tab1(base$sigla_uf, sort.group = "decreasing")

#trazendo outra base
muni <- read.csv("municipios_ibge.csv", sep = ",", colClasses = "character")
  
#olhando a base de municípios
View(muni)

#deixando apenas da quarta letra até o final na descrição
substr(muni$Descrição, 4, nchar(muni$Descrição))
#alocando isso em uma variável nova
muni$desc2 <-  substr(muni$Descrição, 4, nchar(muni$Descrição))

#juntando as bases pelo código
base2 <- merge(base, muni, by.x = "id_municipio_6", by.y = "Código", all.x = T,
               all.y = F, incomparables = NA)

#retirando as duas bases mais antigas:
rm(base)
rm(muni)

#olhando a variável grau de instrução
tab1(base2$grau_instrucao, graph = F)

#trocando os labels
base2$grau_instrucao[which(base2$grau_instrucao=="1")] <- "Analfabeto"
base2$grau_instrucao[which(base2$grau_instrucao=="2")] <- "Até 5ª Incompleto"
base2$grau_instrucao[which(base2$grau_instrucao=="3")] <- "5ª Completo"
base2$grau_instrucao[which(base2$grau_instrucao=="4")] <- "6ª a 9ª Fundamental"
base2$grau_instrucao[which(base2$grau_instrucao=="5")] <- "Fundamental Completo"
base2$grau_instrucao[which(base2$grau_instrucao=="6")] <- "Médio Incompleto"
base2$grau_instrucao[which(base2$grau_instrucao=="7")] <- "Médio Completo"
base2$grau_instrucao[which(base2$grau_instrucao=="8")] <- "Superior Incompleto"
base2$grau_instrucao[which(base2$grau_instrucao=="9")] <- "Superior Completo"
base2$grau_instrucao[which(base2$grau_instrucao=="10")] <- "Mestrado"
base2$grau_instrucao[which(base2$grau_instrucao=="11")] <- "Doutorado"
base2$grau_instrucao[which(base2$grau_instrucao=="80")] <- "Pós Grad. Completa"
base2$grau_instrucao[which(base2$grau_instrucao=="99")] <- "Não Identificado"

  #olhando a variável grau de instrução novamente
tab1(base2$grau_instrucao, graph = F, sort.group = "decreasing")

#mudando os rótulos da variável tipo de deficiência
base2$tipo_deficiencia[which(base2$tipo_deficiencia==0)] <- "Não deficiente"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==1)] <- "Física"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==2)] <- "Auditiva"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==3)] <- "Visual"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==4)] <- "Intelectual"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==5)] <- "Múltipla"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==6)] <- "Reabilitado"
base2$tipo_deficiencia[which(base2$tipo_deficiencia==9)] <- "Não identificado"

#mudando os rótulos para o tipo de fonte
base2$fonte[which(base2$fonte==1)] <- "Não desligado"
base2$fonte[which(base2$fonte==2)] <- "Desligado"
base2$fonte[which(base2$fonte==3)] <- "Desligado"  

#mudando os rótulos para a variável sexo
base2$sexo[which(base2$sexo==1)] <- "Masculino"
base2$sexo[which(base2$sexo==3)] <- "Feminino"
base2$sexo[which(base2$sexo==9)] <- "Ignorado"

#mudando os rótulos para a variável raça cor
base2$raca_cor[which(base2$raca_cor==1)] <- "Branca"
base2$raca_cor[which(base2$raca_cor==2)] <- "Preta"
base2$raca_cor[which(base2$raca_cor==3)] <- "Parda"
base2$raca_cor[which(base2$raca_cor==4)] <- "Amarela"
base2$raca_cor[which(base2$raca_cor==5)] <- "Indígena"
base2$raca_cor[which(base2$raca_cor==6)] <- "Ignorado"
base2$raca_cor[which(base2$raca_cor==9)] <- "Ignorado"

#trazendo a base de seção para fazer o merge
secao <- read.csv("secao.csv", colClasses = "character")
#fazendo o merge com a base
base3 <- merge(base2, secao, by.x = "cnae_2_secao", by.y = "Código", all.x = T,
               all.y = F, incomparables = NA)

#retirando as bases antigas
rm(base2)
rm(secao)

#vendo as variáveis e suas posições
names(base3)

#ficando com as variáveis que nos importam
base3 <- base3[,c(3,4,5,12,13,15,16,20,23,26,27,28,29)]

#exportando a base
write.csv2(base3,"caged_tratado.csv", row.names = F)

#contando trabalhadores com deficiência
library(dplyr)
base3 %>% group_by(mes) %>% count(tipo_deficiencia)

#visualizando a tabela na íntegra
  tab1 <- base3 %>% group_by(mes) %>% count(tipo_deficiencia)
  View(tab1)  

#Transformando os diversos tipos de deficiências para com ou sem
  base3$defic_der <- ""
  base3$defic_der[which(base3$tipo_deficiencia=="Não deficiente")] <- "Sem deficiência"
  base3$defic_der[which(base3$tipo_deficiencia=="Não identificado")] <- "Sem deficiência"
#Vendo como ficou
table(base3$defic_der)

#Todo o restante ganhará "com deficiência"
base3$defic_der[which(base3$defic_der=="")] <- "Com deficiência"

#Vendo como ficou novamente
table(base3$defic_der)

#Repetindo a tabela por mês
  tab1 <- base3 %>% group_by(mes) %>% count(defic_der)
  View(tab1)  

#Fazendo um gráfico com quantidades
library(ggplot2)
ggplot(base3, aes(mes, fill=defic_der))+
  geom_bar()

#adicionando o nome dos eixos
ggplot(base3, aes(mes, fill=defic_der))+
  geom_bar()+
  labs(x="Meses do ano", y="Quantidade (n)", fill="")
  
#comparando a renda entre com e sem deficiência
 #primeiro mudando par tipo numerico
  base3$salario_mensal <- as.numeric(base3$salario_mensal)
#agora comparando pela media
    base3 %>% group_by(defic_der) %>% summarise(mean(salario_mensal))
    base3 %>% group_by(defic_der) %>% summarise(mean(salario_mensal, na.rm = T))

    3599/11588
#comparando com a mediana também
    base3 %>% group_by(defic_der) %>% summarise(mean(salario_mensal, na.rm = T), median(salario_mensal, na.rm = T))
    
#fazendo um boxplot do valor mensal
    ggplot(base3, aes(defic_der, salario_mensal))+
      geom_boxplot()
  
#resumo da variável salário
    summary(base3$salario_mensal)
    
#fazendo um boxplot do valor mensal com limite de até 15000
    ggplot(base3, aes(defic_der, salario_mensal))+
      geom_boxplot()+
      ylim(0,15000)

#primeiro eu crio uma variável tipo categoria
base3$salario_cat <- ""

#depois eu corto a variável salário mensal pelo valor do salário mínimo (1100) e ponho nessa nova variável
base3$salario_cat <- cut(base3$salario_mensal, breaks = c (0, 1099, 2199, 3299, 4399, 5499, Inf))

#vendo como ficou
table(base3$salario_cat)
    
#agora podemos nomear
#checando se é uma categoria
is.factor(base3$salario_cat)
#como estão os nomes das categorias?
levels(base3$salario_cat)
#eu posso renomea-las
levels(base3$salario_cat) <- c("< 1SM", "1SM < 2SM", "2SM < 3SM", "3SM < 4SM", "4SM < 5SM", "5SM OU +")

#tabela comparando o salário entre com ou sem deficiência
tabpct(base3$defic_der, base3$salario_cat, graph = F, percent = "row")

#separando apenas os trabalhadores com deficiência
comdefic <- subset(base3, defic_der=="Com deficiência")

#distribuição de trabalhadores com deficiência por estados
tab1(comdefic$sigla_uf, sort.group="decreasing")

#trazendo pacote para mapas brasileiros
library(geobr)
library(sf)

#fazendo download para os mapas em forma de objeto
estados <- read_state(year=2019)

#criando uma tabela com a quantidade por estados
qtde <- table(comdefic$sigla_uf)
qtde <- cbind(Estados=row.names(qtde), qtde)

#olhando
qtde

#transformando essa tabela em uma base de dados (dataframe)
qtde <- as.data.frame.matrix(qtde)

#transformando as respostas para tipo numérico
qtde$qtde <- as.character(qtde$qtde)
qtde$qtde <- as.numeric(qtde$qtde)

#juntando as bases
estados2 <- dplyr::left_join(estados, qtde, by = c("abbrev_state" = "Estados"))

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())
#plotando o mapa
library(RColorBrewer)
mapa <- ggplot() +
  geom_sf(data=estados2, aes(fill=qtde), size=.15) +
  labs(subtitle="Distribuiçao de trabalhadores com deficiência por estados do BR", size=8) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Quantidade", limits = c(min(estados2$qtde), max(estados2$qtde))) +
  theme_bw()+
  no_axis

mapa


#distribuição de trabalhadores com deficiência por gênero
tab1(comdefic$sexo, sort.group="decreasing", graph = F)

#distribuição de trabalhadores com deficiência por raça
tab1(comdefic$raca_cor, sort.group="decreasing", graph = F)
#criando outra raça para compreender a negra
comdefic$raca2 <- comdefic$raca_cor
#chamando os labels
levels(comdefic$raca2)
#Mudando apenas os labels Parda e Preta
levels(comdefic$raca2) <- c("Amarela", "Branca", "Ignorado", "Indígena","Negra","Negra")

#pedindo novamente a distribuição pela raca2
tab1(comdefic$raca2, sort.group="decreasing", graph = F)

#grau de instrução
tab1(comdefic$grau_instrucao, sort.group="decreasing", graph = F)

#tipo de deficiencia
tab1(comdefic$tipo_deficiencia, sort.group="decreasing", graph = F)

#por seção
tab1(comdefic$Descrição.y,graph=F ,sort.group="decreasing")

#resumindo gênero e raça por salário categorizado
ggplot(comdefic, aes(raca2, fill=salario_cat))+
  geom_bar(position = 'fill')+
  facet_wrap(~sexo)+
  labs(x="Raça/cor", y="Porcentagem", fill="Faixas salarias")

#repetindo um gráfico interativo
library(plotly)
ggplotly(ggplot(comdefic, aes(raca2, fill=salario_cat))+
           geom_bar(position = 'fill')+
           facet_wrap(~sexo)+
           labs(x="Raça/cor", y="Porcentagem", fill="Faixas salarias"))
