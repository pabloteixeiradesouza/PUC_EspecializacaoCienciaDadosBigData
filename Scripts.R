install.packages("rlang")
library("rlang")
install.packages("RSQLite")
library("RSQLite")
install.packages("sqldf")
library("sqldf")
library(ggplot2)
library(tidyverse)
library(readxl)



# Lê Ocorrencias
oco <-read_excel("D:/Pablo/PUC - Especialização Ciência de Dados e Big Data/13. TCC - Ciência de Dados e Big Data/Ocorrencias.xlsx")
#View(oco)
attach(oco)

# Lê Aeronaves Envolvidas
aer <-read_excel("D:/Pablo/PUC - Especialização Ciência de Dados e Big Data/13. TCC - Ciência de Dados e Big Data/AeronavesEnvolvidas.xlsx")
aer$aeronave_ano_fabricacao_idade <- (2020 - aer$aeronave_ano_fabricacao)
#View(aer)
attach(aer)

# Lê Fatores Contribuintes
fat <-read_excel("D:/Pablo/PUC - Especialização Ciência de Dados e Big Data/13. TCC - Ciência de Dados e Big Data/FatoresContribuintes.xlsx")
#View(fat)
attach(fat)

# Lê Recomendacoes
rec <-read_excel("D:/Pablo/PUC - Especialização Ciência de Dados e Big Data/13. TCC - Ciência de Dados e Big Data/Recomendacoes.xlsx")
#View(rec)
attach(rec)


#Criar DATAFRAMES de Totalizadores de Fatores COntribuintes por Aspecto
tot_fatotores <- sqldf("select fator_aspecto, count(1) as qtd_aspecto
                       from fat
                       where fator_aspecto <> '***'
                       group by fator_aspecto order by 2 desc")


#GRAFICO SOBRE OS MAIORES FATORES DE OCORRÊNCIAS
COLORS= heat.colors(10)
barplot(tot_fatotores$qtd_aspecto, 
        horiz = TRUE,
        beside = T, 
        xlim=c(0,max(tot_fatotores$qtd_aspecto) + 1000),
        main = "Ocorrências por Fatores",
        xlab = "Qtde Ocorrências",
        col = c(COLORS)
)

grid(nx=NULL, ny=NA)

barplot(tot_fatotores$qtd_aspecto, 
        horiz = TRUE,
        beside = T, 
        xlim=c(0,max(tot_fatotores$qtd_aspecto) + 1000),
        main = "Ocorrências por Fatores",
        xlab = "Qtde Ocorrências",
        col = c(COLORS),
        add= TRUE
)

legend("right", tolower(as.character(tot_fatotores$fator_aspecto)), yjust=1,col = COLORS, lty=c(1,1)) 


tot_global <- sqldf("select 
                      (select count(1) from oco) as total_ocorrencias,
                      (select count(1) from fat where fator_aspecto <> '***') as total_ocorrencias_classificadas, 
                      (select count(1) from fat where fator_aspecto = 'DESEMPENHO DO SER HUMANO') as Total_humano,
                      ((select Cast(Count(1) as varchar(10)) + '.01' from fat where fator_aspecto = 'DESEMPENHO DO SER HUMANO') / (select Cast(Count(1) as varchar(10)) + '.01' from fat where fator_aspecto <> '***')) * 100 as Per_humano,
                      (select count(1) from fat where fator_aspecto = 'ASPECTO PSICOLÓGICO') as Total_psicologico,
                      ((select Cast(Count(1) as varchar(10)) + '.01' from fat where fator_aspecto = 'ASPECTO PSICOLÓGICO') / (select Cast(Count(1) as varchar(10)) + '.01' from fat where fator_aspecto <> '***')) * 100 as Per_psicologico
                    ")

View(tot_global)


# Dispersão Anual "DESEMPRENHO DO SER HUMANO" e "ASPECTO PSICOLÓGICO"
oco_fat_merge = merge(oco, fat, by="codigo_ocorrencia")
View(oco_fat_merge)#Correlação entre ocorrências, Fatores contribuintes e  recomendações
oco_rec_fat_merge = merge(oco, rec, by="codigo_ocorrencia")
oco_rec_fat_merge = merge(oco_rec_fat_merge, fat, by="codigo_ocorrencia")
View(oco_rec_fat_merge)


dis_DSH <- sqldf(" select ocorrencia_ano as ano, count(1) as qtde
                       from oco_fat_merge
                       where fator_aspecto = 'DESEMPENHO DO SER HUMANO'
                       group by ocorrencia_ano order by 1")
dis_PSI <- sqldf(" select ocorrencia_ano as ano, count(1) as qtde
                       from oco_fat_merge
                       where fator_aspecto = 'ASPECTO PSICOLÓGICO'
                       group by ocorrencia_ano order by 1")
View(dis_DSH)
View(dis_PSI)

#Dispersão
head(dis_DSH)
ggplot(data = dis_DSH, aes(x=ano, y=qtde)) +
  geom_point(aes(col=qtde)) +
  geom_smooth(method="loess", se = F) +
  labs(subtitle = "Fator Contribuinte: DESEMPENHO DO SER HUMANO",
       y = "Qtde Ocorrências", x = "Ano") 

head(dis_PSI)  
ggplot(data = dis_PSI, aes(x=ano, y=qtde)) +
  geom_point(aes(col=qtde)) +
  geom_smooth(method="loess", se = F) +
  labs(subtitle = "Fator Contribuinte: ASPECTO PSICOLÓGICO",
       y = "Qtde Ocorrências", x = "Ano") 



#Correlação entre ocorrências, Fatores contribuintes e  recomendações
oco_rec_fat_merge = merge(oco, fat, by="codigo_ocorrencia")
View(oco_rec_fat_merge)
                       

dis_rec_fat_DSH <- sqldf(" select ocorrencia_ano as ano, count(1) as qtde
                       from oco_rec_fat_merge
                       where fator_aspecto = 'DESEMPENHO DO SER HUMANO'
                       group by ocorrencia_ano order by 1")

dis_rec_fat_PSI <- sqldf(" select ocorrencia_ano as ano, count(1) as qtde
                       from oco_rec_fat_merge
                       where fator_aspecto = 'ASPECTO PSICOLÓGICO'
                       group by ocorrencia_ano order by 1")

view(dis_rec_fat_DSH)
view(dis_rec_fat_PSI)


view(rec_fat_DSH)


#Dispersão
head(dis_rec_fat_DSH)
ggplot(data = dis_rec_fat_DSH, aes(x=ano, y=qtde)) +
  geom_point(aes(col=qtde)) +
  geom_smooth(method="loess", se = F) +
  labs(subtitle = "Recomendações: DESEMPENHO DO SER HUMANO",
       y = "Qtde Recomendações", x = "Ano") 

head(dis_rec_fat_PSI)  
ggplot(data = dis_rec_fat_PSI, aes(x=ano, y=qtde)) +
  geom_point(aes(col=qtde)) +
  geom_smooth(method="loess", se = F) +
  labs(subtitle = "Recomendaçoes: ASPECTO PSICOLÓGICO",
       y = "Qtde Recomendações", x = "Ano") 

#correlação FATOR HUMANO

oco_fat_merge = merge(oco, fat, by="codigo_ocorrencia")
View(oco_fat_merge)#Correlação entre ocorrências, Fatores contribuintes


rec_fat_DSH <- sqldf ("select ocorrencia_ano as ano, count(total_recomendacoes) as TotRecomendacoes
                      from oco_fat_merge
                      where fator_aspecto = 'DESEMPENHO DO SER HUMANO'
                      group by ocorrencia_ano order by 1")

View(rec_fat_DSH)

rec_fat_PSI <- sqldf ("select ocorrencia_ano as ano, count(total_recomendacoes) as TotRecomendacoes
                      from oco_fat_merge
                      where fator_aspecto = 'ASPECTO PSICOLÓGICO'
                      group by ocorrencia_ano order by 1")

View(rec_fat_PSI)


r1 <- cor.test(dis_rec_fat_DSH$qtde,rec_fat_DSH$TotRecomendacoes)
r1

#correlação DESEMPENHO DO SER HUMANO
plot(rec_fat_DSH$TotRecomendacoes, dis_rec_fat_DSH$qtde)
ajuste <- lm(dis_rec_fat_DSH$qtde ~ rec_fat_DSH$TotRecomendacoes, data = dis_rec_fat_DSH)
summary(ajuste)
lines(rec_fat_DSH$TotRecomendacoes,ajuste$fitted.values,col=2 )

#correlação ASPECTO PSICOLOGICO
r2 <- cor.test(rec_fat_PSI$TotRecomendacoes, dis_rec_fat_PSI$qtde)
r2

plot(dis_rec_fat_PSI$qtde, rec_fat_PSI$TotRecomendacoes)
ajuste <- lm(rec_fat_PSI$TotRecomendacoes ~ dis_rec_fat_PSI$qtde, data = dis_rec_fat_PSI)
summary(ajuste)
lines(dis_rec_fat_PSI$qtde,ajuste$fitted.values,col=2 )

