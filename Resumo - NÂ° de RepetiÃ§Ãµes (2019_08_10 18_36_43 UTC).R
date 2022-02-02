#Para número de repetições 

bcc.rep<-function(n2,s,t,d,ri,alpha=0.95)
{
  ## Cálculo do número de repetições para um experimento em BCC
  
  ## n2 = graus de liberdade do resíduo de um experimento anterior
  ## s  = desvio padrão da variável resposta
  ## t  = número de tratamentos
  ## d  = diferença significativa
  ## ri = número inicial de repetições 
  
  df.res<-(ri*t-1)-(ri-1)-(t-1)
  q<-qtukey(alpha,t,df.res)
  f<-qf(alpha,df.res,n2)
  r<-(q^2*s^2*f)/(d^2)
  return(c(r=r,df.res=df.res,q=q,f=f))
}

####### EXEMPLO
# n2 = 60 GL
# s = 7,4 kg
# t = 5
# d = 15 kg
# ri = 5


bcc.rep(n2=60,s=7.4,t=5,d=15,ri=5)

# O valor de r é o primeiro a ser calculado #Nesse caso deu 8.29 ou 8.3
# Na prática se der arredonda o número de repetições para mais!
# r = valor calculado.

###Resultados: r = 8.3 -> nÚMERO DE REPETIÇÕES

#Fazendo |ri - r|:
#abs() = calcula em módulo
abs(5-8.3)

#O resultado dessa diferença é 3.3 > 1 = Sem convergência

#Logo temos que fazer um novo chute entre ri e r ou  5 e 8.3
#Usamos os mesmos dados porem com ri = 7 (que esta entre 5 e 8.3)

bcc.rep(n2=60,s=7.4,t=5,d=15,ri=7)

#Valor de r=7.2 -> nÚMERO DE REPETIÇÕES

abs(7-7.2)

#O resultado dessa diferença é 0.2 < 1 = Convergiu 

##### RESPOSTA

#Para conseguir um delineamento em blocos nas condições acima para detectar 
#uma diferença de 15kg precisamos realizar 7 repetições 
