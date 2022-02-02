#entrando com os dados por ordem dos tratamentos (linhas); aaaa,bbbb, cccc...
#digita a função scan e escreve na tela de respostas, depois da enter 
#digita cada os elemento por linha 
x <- scan()

# Agora vamos montar um data.frame com os dados e os indicadores de tratamentos, linhas e colunas
trat<-c("B","A","D","E","C","C","B","E","A","D","A","D","B","C","E","D","E","C","B","A","E","C","A","D","B")
trat<-as.factor(trat)


lat <- data.frame(linha=factor(rep(1:5, each=5)), col=factor(rep(1:5,5)), trat=trat, resp=x)
#Note que usamos a função factor para indicar que as variáveis linhas e colunas são níveis de fatores e não valores numéricos.

#O mesmo vale para tratamentos.
#Pode-se utilizar o comando read.table para ler o arquivo de dados
lat <- read.table("lat.txt", header=T)
lat
#Caso o arquivo esteja em outro diretório deve-se colocar o caminho completo deste diretório no argumento de read.table acima.


#Explorando os dados:

names(lat)
summary(lat) #Mostra os dados/quartis do experimento

attach(lat)


plot(resp ~ col + linha + trat)

lat.mt <- tapply(resp, trat, mean)
lat.mt
lat.ml <- tapply(resp, linha, mean)
lat.ml
lat.mc <- tapply(resp, col, mean)
lat.mc

x11()
plot.default(trat, resp)
points(lat.mt, pch="x", col=2, cex=1.5)
#Nos gráficos e resultados acima procuramos captar os principais aspectos dos dados bem como verificar se não há interação entre linhas, colunas e tratamentos, o que não deve acontecer neste tipo de experimento.
#A seguir vamos ajustar o modelo e obter outros resultados, incluindo a análise de resíduos e testes para verificar a validades dos pressupostos do modelo.

#ANOVA
lat.av <- aov(resp ~ col + linha + trat)
anova(lat.av)
names(lat.av)
#REPARAR SE OS GRAUS DE LIBERDADE ESTÃO OK
#REJEITAMOS HO QUANDO P-VALOR FOR MENOR QUE 0.5

#ANALISE DE RESÍDUOS:

#graficamentes 
par(mfrow=c(2,2))
x11()
plot(lat.av)

#Homocedasticidade, Normalidade e Independência
lat.av <- aov(resp ~ col + linha + trat)
residuos <- (lat.av$residuals)

par(mfrow=c(2,2))

plot(lat$trat,residuos)
title("Resíduos vs Estágios \n Homocedasticidade")

preditos <- (lat.av$fitted.values)

plot(residuos,preditos)
title("Resíduos vs Preditos \n Independência")

qqnorm(residuos,ylab="Residuos", main=NULL)
qqline(residuos)
title("Grafico Normal de \n Probabilidade dos Resíduos")

par(mfrow=c(2,1))

respad <- (residuos/sqrt(anova(lat.av)$"Mean Sq"[4]))
boxplot(respad)
title("Resíduos Padronizados - outliers")

outlier<-c(max(respad),min(respad))
outlier

#Teste para Normalidade dos Resíduos
residuos <- (lat.av$residuals)
shapiro.test(residuos)
#Como foi detectado efeito de tratamentos faz-se um teste de comparações múltiplas e encerra-se as análises desanexando o objeto do caminho de procura

#Teste para Comparações Múltiplas
lat.tk <- TukeyHSD(lat.av, "trat", ord=T)
lat.tk

x11()
plot(lat.tk)

detach(lat)
