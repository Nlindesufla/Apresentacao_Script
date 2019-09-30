# Obtendo valores iniciais

##################################
# exemplo artigo de Celulose 2017
x=c(20,40,60,80,100,120)
y=c(37.2, 39.8, 42.1, 43.5, 44.3, 43.7)

plot(x,y)
# modelo escolhido: Y = A*B^(ln(X))

reg<-nls(y ~ A*B^(log(x)), start=c(A=25 ,B=1 ))
summary(reg)
lines(x,fitted(reg), col="red")

##################################
# Sugestão Taciana  #1510081
# dados crescimento de equinos
idade=c(7,10,13,16,19,22,26,32,38,45,50,56,65)
altura=c(123.4,128.9,132.3,133.5,139.3,141.1,146.5,144.3,147.5,147.6,145.8,148.6,145.6)

# Obtendo estimativa dos chutes iniciais
max(altura)
yg=log(1 - altura/149)  # yg=log(-1*log(altura/149))  # outra alternativa
modg=lm(yg~idade)
summary(modg)

bg=coef(modg)[1];-bg # Estimativa inicial do parâmetro b
gmg=coef(modg)[2]; -gmg # Estimativa inicial do parâmetro k
ag=max(altura); ag
# A estimativa inicial do parâmetro alfa é sempre um pouco maior
# que o maior dos valores da variável dependente.

ajuste <- nls(altura~a/(1+exp(k*(b-idade))), start=c(a= ,k= ,b= ))
summary(ajuste)

plot(idade,altura)
lines(idade,fitted(ajuste), col=3)

# Em curvas de crescimento o principal problema está no chute inicial do parâmetro k
# Veja que mesmo com o modelo escrito de outra forma ocorre a convergêcia
reg1<-  nls(altura~a/(1+b*exp(-k*idade)), start=c(a=149 ,k=0.05 ,b=1.67 ))
summary(reg1)

########################################################################################
#### Chutes iniciais via observação do modelo
# dados de crescimento do fruto de pequi
daa<-c(12,27,42,57,72,87,102,117)
massa<-c(0.09,3.7,32.1,68.3,96.2,104.3,109,107.4)

plot(daa,massa)
reg2=nls(massa~a*exp(-exp(k*(b-daa))),start=c(a=110,b=40,k=0.04))
summary(reg2)
coef(reg2)

# Obtendo estimativa do chute inicial do oarâmetro k
max(massa)
yg=log(1 - massa/110)  # yg=log(-1*log(altura/149))  # outra alternativa
modg=lm(yg~daa)

gmg=coef(modg)[2]; -gmg # Estimativa inicial do parâmetro k

#graficamente
plot(daa,massa)
lines(daa,fitted(reg2), col=4)

# vejamos como obter estas estimativas no Shiny