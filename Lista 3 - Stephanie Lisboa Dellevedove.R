####Exercício 1
#Criando uma função que calcula o desvio padrão de amostras
dp <- function(x)
{
  #Tamanho da váriável
  L <- length(x)
  #Média dos valores da variável
  M <- sum(x)/L
  S <- 0
  #Somatório
  for(i in 1:L)
  {
    S <- S + (x[i] - M)^2
  }  
  SD <- sqrt(S/(L-1))
  SD
}

####Exercício 2
#Criando uma função que calcula o IMC
imc <- function(x,y)
{
  ym <- y/100
  IMC <- x / (ym^2)
  IMC
}

####Exercício 3
#Análise de regressão linear
#Importando o banco de dados
dados<-read_excel("C:/Users/Stephanie/Downloads/altman.xlsx")

#Calculando os coeficientes da reta de ajuste da CTP em função da idade
FIT1 <- lm(idade ~ CTP, data = dados)
FIT1
#Fazendo o gráfico de dispersão dos valores de CTP em função da idade juntamente com sua reta de ajuste
plot(idade ~ CTP, data = dados,xlab = "Idade (Anos)", ylab = "CTP (L)", main = "CTP em função da Idade")
abline(FIT1)

#Calculando os coeficientes da reta de ajuste da CTP em função da altura
FIT2 <- lm(altura ~ CTP, data = dados)
FIT2
#Fazendo o gráfico de dispersão dos valores de CTP em função da altura juntamente com sua reta de ajuste
plot(altura ~ CTP, data = dados,xlab = "altura (cm)", ylab = "CTP (L)", main = "CTP em função da altura")
abline(FIT2)

#Fazendo o boxplot dos dados de CTP separados entre Homens e Mulheres
boxplot(CTP~sexo, data = dados, ylab = "CTP (L)", main = "CTP dos Homens e Mulheres")

