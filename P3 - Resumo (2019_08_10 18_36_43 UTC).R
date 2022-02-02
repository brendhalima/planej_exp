# PLANEJAMENTO DE EXPERIMENTO
# Konstanz Tonegawa Winter
# Estudo para a 3 prova

# Para arrumar se as letras estiverem erradas clique em:
# File > Reopen with Encoding > ISO - 8859 - 1

# para rodar basta ir clicando em Ctrl + enter
# qualquer altera??o me enviem tamb?m por favor:
# konstanz.tw@gmail.com

### PROVA 3 ===========================================================================

## 1 ----------------------------------------------------------------------------------
# 1) Os dados da Tabela 1 foram obtidos de um experimento realizado em quadrado 
# latino 5 x 5 (para ler a tabela seguir as linhas)
linha <- as.factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5))
coluna<- as.factor(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5))
trat<- as.factor(c("B","A","D","E","C","C","B","E","A","D","A","D","B","C","E","D","E","C","B","A","E","C","A","D","B"))
resp<-c(17.5,9.8,12.5,13.4,10.8,14.9,5.3,22.8,8.9,10.1,10.8,8.6,8.4,14.4,14.9,11.8,13.2,15.8,7.4,9.6,12,22.8,6,9.8,9)
prova3<-data.frame(linha,coluna,trat,resp)
prova3

head(prova3)
str(prova3)

attach(prova3)
plot(resp ~ trat)

# Analise os dados por meio da ANOVA e forne?a o valor do teste F para
# tratamentos (n?o ? necess?rio verificar os pressupostos)

# ANOVA .............................................................................
prova3.av <- aov(resp ~ trat + linha + coluna, data = prova3)
anova(prova3.av)
# Valor do teste F 0.07717 deu significativo em 10% 
# Tenho evid?ncia de que h? pelo menos um contraste
# de m?dias de tratamentos significativos (ns 10%)



## 2 ----------------------------------------------------------------------------------------------------------
# 2) Em cada uma das duas situa??es abaixo verifique se h? ou n?o necessidade
#    de utiliza??o de um delineamento em quadrado
#    latino explicando o motivo. Leia com aten??o...
# a) Situa??o 2: Um farmac?utico necessita fazer um experimento para comparar
#    dois rem?dios.
#    Ele vai utilizar camundongos individuais como unidades experimentais ...
#    medir o efeito destes rem?dios. Ele possui 200 camundongos. Destes, 100 
#    s?o de uma ra?a e 100 de outra. Dentro de cada ra?a, metade dos animais
#    s?o f?meas e a outra metade machos. Ra?a e sexo s?o dois fatores que 
#    influenciam na vari?vel re... interesse do pesquisador
###RESPOSTA: 
### USA QUADRADO LATINO, PORQUE TEMOS DUAS FONTES DE VARIAÇÕA (UMA É O SEXO E A OUTRA A RAÇA)


#    b) Situa??o 1: Um engenheiro deseja realizar um experimento para testar ... 
#    novas m?quinas. Ele possui m?quinas de 5 fabricantes, todas com o mesmo ... 
#    uso e mesma pot?ncia. Neste experimento o efeito dos fabricantes n?o ... 
#    vari?vel resposta mas tempo de uso e pot?ncia influenciam.
####RESPOSTA:
###NÃO PRECISA USAR QUADRADO LATINO PORQUE DAS 3 FONTES DE VARIAÇÃO 2 SÃO IGAUIS E 
### E 1 NÃO É IMPORTANTE PARA O PESQUISADOR, RESUMINDO... SÓ TEMOS UMA FONTE DE VARIAÇÃO

#obs: Não escrever EXATAMENTE igual ao que esta, para não ficar 
     #identico em todas as provas


## 3 ----------------------------------------------------------------------------------------------------------
# 3) Repeti??es
trat <- 9   #Número de tratamentos
gl_res_ant <- 45  #Número de graus de liberdadde do resíduo do experimento anterior 
CVperc <- (sqrt(1938)/400)*100 # % 
s2_ant <- CVperc^2 #desvio padrão da variável resposta
Dperc <- 25 # %
d <- Dperc

# r1 = 10
ri <- 10 #Valor qualquer que você vai chutar para começar a rodar a função 
gl_res1 <- (trat-1)*(ri-1) #blocos gl do res?duo
q1 <- qtukey(0.95, trat, gl_res1)
f1 <- qf(0.95, gl_res1, gl_res_ant)
r1 <- ((q1^2)*(s2_ant)*(f1))/(d^2)
abs(r1 - ri) #Para convergir esse valor deve ser menor que 1

#O próximo chute (ri) deve ser feito entre ri(anterior) e r1
#Nesse caso entre ri= 10 e r1= 3.7, vamos usar o valor 8

# r2 = 8
ri <- 8
gl_res2 <- (trat-1)*(ri-1) #blocos grau de liberdade do res?duo
q2 <- qtukey(0.95, trat, gl_res2)
f2 <- qf(0.95, gl_res2, gl_res_ant)
r2 <- ((q2^2)*(s2_ant)*(f2))/(d^2)
abs(r2 - ri)

# r3 = 7
ri <- 7
gl_res3 <- (trat-1)*(ri-1) #blocos grau de liberdade do res?duo
q3 <- qtukey(0.95, trat, gl_res3)
f3 <- qf(0.95, gl_res3, gl_res_ant)
r3 <- ((q3^2)*(s2_ant)*(f3))/(d^2)
abs (r3 - ri) # <1 convergiu

#Obs: Irá convergir quando a diferença entre rn - ri < 1 
####RESPOSTA
###Precisamos realizar 7 (útimo ri usado) repetições para ter uma 
  #diferença significativa de 25 "unidades de medida" 

## 4 ----------------------------------------------------------------------------------------------------------
# 4)
trat <- 5
gl_res_ant <- 60
s2_ant <- 5^2
d <- 17


# r1 = 5
ri <- 5
gl_res1 <- (trat-1)*(ri-1) #blocos grau de liberdade do res?duo
q1 <- qtukey(0.95, trat, gl_res1)
f1 <- qf(0.95, gl_res1, gl_res_ant)
r1 <- ((q1^2)*(s2_ant)*(f1))/(d^2)
r1 - ri

# r2 = 4
ri <- 4
gl_res2 <- (trat-1)*(ri-1) #blocos grau de liberdade do res?duo
q2 <- qtukey(0.95, trat, gl_res2)
f2 <- qf(0.95, gl_res2, gl_res_ant)
r2 <- ((q2^2)*(s2_ant)*(f2))/(d^2)
r2 - ri # 0,63 < 1 ent?o convergiu



# NUMERO DE REPETICOES ==============================================================================================

# Estimativa de erro conhecida (? priori)
# exigem uma medida de erro = exp PILOTO

# METODO TUKEY ------------------------------------------------------------------------------------------------------
# sempre converge - independente do chute inicial e o resultado ? o mesmo

# r = n de rep
# q = dist tukey (tabelado)
# s = estimativa de erro (exp anterior)
# tabela F = dist F => f alfa (n1=gl novo, n2=gl antigo) = gl residuo #q2 <- qtukey(0.95, 5, 10)
# d = dif absoluta => pesquisador diz quanto ? importante / se CV%, subst s por CV% e transf d%
# metodo interativo = ir at? convergir
# ri = chute inicial
# crit?rio = para qnd r-ri<=1 => CONVERGENCIA
# BCC - gl res

# tab 4 - pg 92

# EXEMPLO PG 85 -------------------------------------------------------------------------------------------------------

# 5 trat
# s = 7,4 Kg/parcela
# n2 = 60 gl res
# d = 15 kg/parcela - quer q detecte esta dif (em m?dia) como sig... o pesquisador diz q ? importante
# r1 = 5
# gl

trat <- 5 #t
gl_res_ant <- 60 #n2
s2_ant <- 7.4^2 #s
d <- 15 #d

ri <- 5 # n rep que eu precisaria para ex as condi??es acima
gl_res1 <- (trat-1)*(ri-1)
q1 <- qtukey(0.95, trat, gl_res1)
f1 <- qf(0.95, gl_res1, gl_res_ant)
r1 <- ((q1^2)*(s2_ant)*(f1))/(d^2)
r1 - ri #? menor que 1?- se n?o continue churanto ri

ri <- 7 # n rep que eu precisaria para ex as condi??es acima
gl_res2 <- (trat-1)*(ri-1)
q2 <- qtukey(0.95, trat, gl_res2)
f2 <- qf(0.95, gl_res2, gl_res_ant)
r2 <- ((q2^2)*(s2_ant)*(f2))/(d^2)
r2 - ri #? menor que 1?- se sim convergiu - pare

# Para conseguir um delineamento em blocos nas condi??es ?timas para detectar
# uma diferen?a (d) de 15Kg, precisa-se de 7 repeti??es (ri)

#-------------------------- AULA 06 ------------------------------------------------------------------------------------------------------
# com f?rmula

bcc.rep<-function(n2,s,t,d,ri,alpha=0.95)
{
  ## C?lculo do n?mero de repeti??es para um experimento em BCC
  
  ## n2 = graus de liberdade do res?duo de um experimento anterior - TEM QUE SER FORNECIDO
  ## s  = desvio padr?o da vari?vel resposta - FORNECIDO
  ## t  = n?mero de tratamentos - (ver gl res?duo) - FORNECIDO
  ## d  = diferen?a significativa - FORNECIDO
  ## ri = n?mero inicial de repeti??es - CHUTE 
  
  df.res<- (trat-1)*(ri-1) #mudar para outro tipo se n for BCC
  q<-qtukey(alpha,t,df.res)
  f<-qf(alpha,df.res,n2)
  r<-(q^2*s^2*f)/(d^2)
  return(c(r=r,df.res=df.res,q=q,f=f,Converge=r-ri)) #SE CONVERGE <1 PARE
}


bcc.rep(60,7.4,5,15,5)
bcc.rep(60,7.4,5,15,7)

# QUADRADO LATINO ===================================================================================================

# BCC - Controla uma fonte de erro ou 2 parcelas
# QL - Controla duas fontes de erro (se existir grupos homog?neos faz-se duplo bloqueamento)
# As fontes de varia??o no quadrado latino n?o s?o paralelas
# Como linha = coluna >2, quanto mais, mais unidades experimentais, normalmente s?o feitos com poucos tratamentos
# linha e coluna = restri??o na casualiza??o

# quadrado com 4 tratamentos:
trat<- as.factor(c("A","B","C","D","D","A","B","C","C","D","A","B","B","C","D","A"))
QL<-data.frame(linha=factor(rep(1:4,each=4)),coluna=factor(rep(1:4,4)),trat=trat)
QL
TQL<-t(matrix(trat,4,4))
TQL
# sorteio linhas (2,4,1,3)
# sorteio coluna (3,1,2,4)
trat <- as.factor(c("B","D","A","C","D","B","C","A","C","A","B","D","A","C","D","B"))
TQL <- t(matrix(trat,4,4))
TQL

# mi = m?dia geral / Li = efeito das linhas / Cj = efeito das colunas / Tk = efeito dos tratamentos (tenho)
# Eij = erro (estimar)
# yij = observei

# Eij = yij - (Li+Cj+Tk)
# -------------------------------------------------------------------------------------------------------------------

# EX01 DA AULA ------------------------------------------------------------------------------------------------------
col<-as.factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5))
linha<-as.factor(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5))
trat<-as.factor(c("B","C","A","D","E","A","B","D","E","C","D","E","B","C","A","E","A","C","B","D","C","D","E","A","B"))
resp<-c(7.6,10.4,6.0,8.8,15.0,8.2,5.4,7.2,13.0,16.0,10.4,16.0,7.0,14.2,7.0,11.2,7.4,11.0,7.2,8.2,9.0,8.4,12.4,8.0,7.5)
lat01<-data.frame(linha,coluna,trat,resp)
lat01

str(lat01) # Garantir que as colunas, linhas e tratamentos est?o em fator.

names(lat01)[1] <- "coluna"
summary(lat01)

attach(lat01)

# Bloxplot para ver comportamento por coluna, linha e tratamento
# verificar se n?o h? intera??o entre linhas, colunas e tratamentos, 
# o que n?o deve acontecer neste tipo de experimento
dev.off()
par(mfrow=c(2,2))
plot(resp ~ coluna + linha + trat)
par(mfrow=c(1,1))

# M?dias
lat01.mt <- tapply(resp, trat, mean)
lat01.mt
lat01.ml <- tapply(resp, linha, mean)
lat01.ml
lat01.mc <- tapply(resp, coluna, mean)
lat01.mc

plot.default(trat, resp)
points(lat01.mt, pch="x", col=2, cex=1.5)

### ANOVA -----------------------------------------------------------------------------------------------------------
lat01.av <- aov(resp ~ coluna + linha + trat, data = lat01)
anova(lat01.av) # ordem de entrada dos efeitos n?o altera o resultado
# conferir graus de liberdade
# Teste F para o tratamento deu significativo em 0,1%
# Ou seja, rejeito H0, tenho pelo menos um contraste significativo entre as m?dias dos tratamentos
# Verificar os pressupostos

### AN?LISE DE RES?DUOS ---------------------------------------------------------------------------------------------
# Homocedasticidade, Normalidade e Independ?ncia:

###   GRAFICAMENTE   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

par(mfrow=c(2,2))
plot(lat01.av)
layout(1)

###   HOMOCEDASTICIDADE   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

bartlett.test(lat01.av$res, trat)                   
#pelo teste se p< rej ho
#H0: sig?=sig?=...sig? // H1: sig?<>sig? para pelo menos um par
# Se a estat?stica do teste for maior que X? ent?o rejeita-se H0
residuos <- (lat01.av$res)

plot.default(lat01$trat, residuos)                  
# espera que os desvios variem de forma homog?nea
title("Res?duos vs Est?gios \n Homocedasticidade")

plot(lat01$trat,residuos)                           
# espera que eles sejam semelhantes, var ? a mesma entre caixas 
title("Res?duos vs Est?gios \n Homocedasticidade")
# se cocorrer heterocedasticidade, transforma??o de dados 
# (seno, cosseno ou ra?z) - quando existe rela??o conhecida entre 
# m?dia e vari?ncia (heterocedasticidade regular - se n?o n?o adianta transformar)

###   NORMALIDADE   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

hist(lat01.av$res, main=NULL)                      
#formato de normal
title("Histograma dos Res?duos")

qqnorm(residuos, ylab = "Res?duos", main = NULL)   
#devem estar sobre a linha e mais concentrado no meio
qqline(residuos)                                   
#os pts afastados podem ser outliers
title("Gr?fico Normal de \n Probabilidade dos Res?duos")

shapiro.test(res?duos)    # =
shapiro.test(lat01.av$res)
#teste para normalidade dos res?duos se p(<) rej ho = falta de norm

# N?o rejeito H0 - Normalidade ok.
# foi detectado efeito dos tratamentos, faz compara??o m?ltipla

###   INDEPENDENCIA   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
preditos <- (lat01.av$fitted.values)

# se houver independ?ncia haver? ausencia de padr?o
plot(residuos, preditos)               #sem ordenar
title("Res?duos vs Preditos \n Independ?ncia")

plot(lat01.av$fit, lat01.av$res, xlab="valores ajustados", ylab="res?duos") 
#ordenando
title("res?duos vs Preditos")
plot(lat01.av$fit, order(lat01.av$res), xlab="valores ajustados", ylab="res?duos")
title("res?duos vs Preditos")

### OUTLIER . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

par(mfrow=c(2,2))
plot(lat01.av)
par(mfrow=c(1,1))

respad <- (residuos/sqrt(anova(lat01.av)$"Mean Sq"[2]))
# Pego o quadrado m?dio do res?duo (estimativa da vari?ncia residual);
# erro padronizado = erro/sqrt(estimativa_var_residual)
boxplot(respad)
title("Res?duos Padronizados - outliers")

summary(respad)
# Observar o m?ximo e o m?nimo e observar o intervalo (-3 e +3 desvios?)!!
# 1 = 68%, 2 = 95%, 3 = 99%
outlier <- c(max(respad), min(respad))
outlier      

plot.default(lat01$trat,respad, xlab="Linhagens")
title("Res?duos Padronizados" )

### TESTE PARA COMPARA??ES M?LTIPLAS . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

lat01.tk <- TukeyHSD(lat01.av, "trat", ord = T)
# ord = TRUE: 
lat01.tk
plot(lat01.tk)


detach(lat01)

# ESTUDO PARA 3 PROVA - TRABALHO EM SALA =============================================================================

lanc<-as.factor(c(4,4,4,4,3,3,3,3,2,2,2,2,1,1,1,1))
cron<-as.factor(c(4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3))
trat<-as.factor(c("A","D","C","B","D","C","B","A","C","B","A","D","B","A","D","C"))
tempo<-c(1.51,1.05,1.24,1.71,1.20,0.84,1.13,1.60,1.60,1.31,1.36,1.55,1.36,1.13,1.73,1.48)
dados<-data.frame(lanc,cron,trat,tempo)
dados

#lanc = col
#cron = lin
#tempo = resp

summary(dados)

str(dados)
attach(dados)

# Bloxplot para ver comportamento por coluna, linha e tratamento
dev.off()
par(mfrow=c(2,2))
plot(tempo ~ lanc + cron + tempo)
par(mfrow=c(1,1))

# M?dias
dados.mt <- tapply(tempo, trat, mean)
dados.mt
dados.ml <- tapply(tempo, cron, mean)
dados.ml
dados.mc <- tapply(tempo, lanc, mean)
dados.mc

plot.default(trat, tempo)
points(dados.mt, pch="x", col=2, cex=1.5)

#ANOVA
dados.av <- aov(tempo ~ lanc + cron + trat, data = dados)
anova(dados.av)

# neste caso existe evic?ncia de variabilidade entre os cronometristas ao 
# n?vel de signific?ncia de 10% - rej h0
# Os tratamentos n?o diferem entre s?
# Os contrastes n?o s?o significativos 
# entre as m?dias dos tratamentos

### AN?LISE DE RES?DUOS ---------------------------------------------------------------------------------------------
# Homocedasticidade, Normalidade e Independ?ncia:

###   GRAFICAMENTE   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

par(mfrow=c(2,2))
plot(dados.av)
layout(1)

###   HOMOCEDASTICIDADE   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

bartlett.test(dados.av$res, trat)                   #pelo teste se p< rej ho

residuos <- (dados.av$res)

plot.default(dados$trat, residuos)                  #pelo gr?fico, res X trat
title("Res?duos vs Est?gios \n Homocedasticidade")

par(mfrow = c(2, 2))
plot(dados$trat,residuos)  
title("Res?duos vs Est?gios \n Homocedasticidade")

###   NORMALIDADE   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

hist(dados.av$res, main=NULL)       # formato de normal
title("Histograma dos Res?duos")

qqnorm(residuos, ylab = "Residuos", main = NULL)   
#devem estar sobre a linha e mais concentrado no meio
qqline(residuos)
title("Grafico Normal de \n Probabilidade dos Res?duos")

shapiro.test(residuos)              # =
shapiro.test(dados.av$res)          
#teste para normalidade dos res?duos se p< rej ho

# N?o rejeito H0 - Normalidade ok.
# foi detectado efeito dos tratamentos, faz compara??o m?ltipla

###   INDEPENDENCIA   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
preditos <- (dados.av$fitted.values)

plot(residuos, preditos)            #sem ordenar
title("Res?duos vs Preditos \n Independ?ncia")

plot(dados.av$fit, dados.av$res, xlab="valores ajustados", ylab="res?duos") 
#ordenando
title("res?duos vs Preditos")
plot(dados.av$fit, order(dados.av$res), xlab="valores ajustados", ylab="res?duos")
title("res?duos vs Preditos")

### OUTLIER . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

par(mfrow=c(2,2))
plot(dados.av)
par(mfrow=c(1,1))

respad <- (residuos/sqrt(anova(dados.av)$"Mean Sq"[2]))
# Pego o quadrado m?dio do res?duo (estimativa da vari?ncia residual);
# erro padronizado = erro/sqrt(estimativa_var_residual)
boxplot(respad)
title("Res?duos Padronizados - outliers")

summary(respad)
# Observar o m?ximo e o m?nimo e observar o intervalo (-3 e +3 desvios?)!!

outlier <- c(max(respad), min(respad))
outlier      

plot.default(dados$trat,respad, xlab="Linhagens")
title("Res?duos Padronizados" )

### TESTE PARA COMPARA??ES MULTIPLAS . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

dados.tk <- TukeyHSD(dados.av, "trat", ord = T)
# ord = TRUE: 
dados.tk
plot(dados.tk)


detach(dados)

### CONTRASTES . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

#  na coluna do p valor, os pares de contraste com p valor baixo s?o
#  significativos e diferem as m?dias dos tratamentos

detach(dados)

# EXERC?CIO DAS NOTAS DE AULA =============================================================================
linha <- as.factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5))
coluna<- as.factor(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5))
trat<- as.factor(c(4,1,2,3,5,3,4,1,2,4,5,2,3,4,1,2,4,5,1,3,1,3,4,5,2))
resp<-c(432,518,458,583,331,724,478,524,550,400,489,384,556,297,420,494,500,313,486,501,515,660,438,394,318)
lat02<-data.frame(linha,coluna,trat,resp)
lat02

str(lat02)
attach(lat02)

### An?lise Descritiva:

par(mfrow=c(2,2))
plot(resp ~ coluna)
plot(resp ~ linha)
plot(resp ~ trat)
layout(1)

# M?dia por tratamento:
trat.m <- tapply(resp, trat, mean)
trat.m

# M?dia por coluna:
col.m <- tapply(resp, coluna, mean)
col.m

# M?dia por linha:
lin.m <- tapply(resp, linha, mean)
lin.m

# Gr?fico de dispers?o:
plot.default(resp ~ trat)
points(trat.m, pch = "x", col = "red", cex = 1.2)


### ANOVA:
lat02.av <- aov(resp ~ trat + coluna + linha, data = lat02)
anova(lat02.av)

# Conferindo graus de liberdade (fator e tal)
# Tenho evid?ncia que h? pelo menos um contraste de m?dias de tratamentos
# significativo (n.s. = 5%).
# Para um pr?ximo experimento, ? recomendado a utiliza??o do bloqueamento
# das colunas, mas n?o o das linhas, dado que este se mostrou como
# um efeito n?o significativo.
# No entanto, ainda preciso realizar a an?lise de res?duos para confiar
# nesses resultados.

# ANCOVA =====================================================================================================
maq <- as.factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3))
cov <- c(20,25,24,24,32,22,28,22,30,28,21,23,26,21,15)
resp <- c(36,41,39,42,49,40,48,39,45,44,35,37,42,34,32)

# Inicialmente faz-se a leitura e organiza??o dos dados.
# resp (comp dos fios) e cov (diametro) s?o num?ricas

ex12 <- data.frame(maq,cov,resp)  
ex12  

dim(ex12)  
names(ex12)  
summary(ex12)  

# Na an?lise de covari?ncia os testes de signific?ncia tem que ser obtidos 
# em ajustes separados. ? necess?rio verificar se o efeito da covari?vel 
# influencia na vari?vel resposta.

## An?lise explorat?ria
# Gr?ficos igual anova
# Interesse: Var Resp X Cov. 
# Metodologia: regress?o

attach(ex12)  
ex12.lm<-lm(cov~resp)  
plot(cov~resp)  
abline(ex12.lm$coef)
# H? associa??o entre var resp e cov (linear)

cor(cov,resp)  #para dados n?o completos cor(...,use="complete.obs")
# Portanto, a rela??o entre essas vari?veis pode estar afetando os resultados 
# experimentais.? significativo? ANCOVA

# ANCOVA -------------------------------------------------------------------------
# Primeiro testa-se o intercepto (coeficiente B0) da reta de regress?o. 
# Na an?lise de vari?ncia abaixo deve-se considerar apenas o teste referente 
# ? vari?vel cov que neste caso est? corrigida para o efeito de maq. Note que 
# para isto a vari?vel cov tem que ser a ?ltima na especifica??o do modelo.
ex12.av <- aov(resp ~ maq + cov, data=ex12)  
summary(ex12.av)  

# testa-se o efeito do fator maq corrigindo para o efeito da covari?vel. 
# Para isto basta inverter a ordem dos termos na especifica??o do modelo.

ex12.av <- aov(resp ~ cov + maq, data=ex12)  
summary(ex12.av)  
# Portanto, para esse experimento, a covari?vel teve um efeito significativo 
# na interpreta??o dos resultados.
# Caso o efeito da covari?vel fosse n?o significativo a an?lise poderia ser 
# feita da forma habitual, ou seja, somente com o fator maq no modelo.
# Observe que, nesse caso, as conclus?es ainda seriam as mesmas. Mas, compare 
# os p-valores para maq nas duas situa??es (com e sem a covari?vel) e observe 
# que os valores s?o bem distintos.

summary(aov(resp~maq))
# Ajustando as m?dias

# Como o efeito da Covari?vel foi significativo, isso implica na corre??o das 
# m?dias dos tratamentos.
# Com o comando lm pode-se obter o valor de B0 da covari?vel di?metro para 
# corre??o das m?dias.

beta<-lm(resp~cov+maq)  
summary(beta)  

beta$coef[2]  
cov  
# 0.9539877 (algum valor est? errado, neste caso deu 0.945122)
# Depois, pode-se obter o valor da m?dia corrigida, por exemplo da m?quina 1:
  
media.resp<-tapply(resp,maq,mean)  
media.cov<-tapply(cov,maq,mean)  
media.cor1<-media.resp[1]-beta$coef[2]*(media.cov[1]-mean(cov))
media.cor1
