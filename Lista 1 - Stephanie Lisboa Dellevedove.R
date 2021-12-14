####Exercício 1

a <- 3+4*8
a

b <- (3+4)*8
b

c <- 8*5+2*3
c

d <- 8*(5+2)*3
d

e <- 2^3+2*((3)^(1/2))
e

f <- (2/3) + (5/6)
f

g <- 1+2+3+4+5+6+7+8
g

h <- 1*2*3*4*5*6*7*8
h

i <- (1+(1/8))^3
i

j <- cos (pi)
j

k <- (sin (pi))^2 + (cos(pi))^2
k

l <- log(9)
l


####Exercício 2
plot(seq(0,10,0.1),sin(seq(0,10,0.1)),type="l", xlab="x", ylab="sen(x)",col="red", main = "Seno de X")

####Exercício 3
#Importando o banco de dados do computador
library(readxl)
dados<-read_excel("C:/Users/Stephanie/Downloads/Dados exercicio 1.xlsx")

#Construindo um Boxplot para a altura dos atletas
boxplot(dados$altura, las = 2, main ="Altura dos Atletas")

#Construindo um Histograma para a altura dos atletas
hist(dados$altura,las = 1, main="Altura dos atletas", xlab="Altura(cm)",ylab="Frequência", breaks =5)

#Encontrando as estatísticas descritivas para a idade dos atletas
#Média
mean(dados$idade)
#Desvio Padrão
sd(dados$idade)
#Mediana
median(dados$idade)

#Encontrando as estatísticas descritivas para a altura dos atletas de acordo com suas posições
#Média
tapply(dados$altura,dados$posicao,mean)
#Desvio Padrão
tapply(dados$altura,dados$posicao,sd)
#Mediana
tapply(dados$altura,dados$posicao,median)

#Construindo um gráfico da dispersão entre a idade e a altura dos atletas
plot (dados$idade,dados$altura,las = 1, xlab = "Idade", ylab = "Altura",main = "Altura em função da idade")

#Calculando o coeficiente de correlação de Pearson entre a idade e a altura dos atletas
cor.test(dados$idade,dados$altura,method = c("pearson"))

