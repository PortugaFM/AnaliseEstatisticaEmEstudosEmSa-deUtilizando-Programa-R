####Exerc�cio 1
#Criando uma fun��o que calcula o desvio padr�o de amostras
dp <- function(x)
{
  #Tamanho da v�ri�vel
  L <- length(x)
  #M�dia dos valores da vari�vel
  M <- sum(x)/L
  S <- 0
  #Somat�rio
  for(i in 1:L)
  {
    S <- S + (x[i] - M)^2
  }  
  SD <- sqrt(S/(L-1))
  SD
}

####Exerc�cio 2
#Criando uma fun��o que calcula o IMC
imc <- function(x,y)
{
  ym <- y/100
  IMC <- x / (ym^2)
  IMC
}

####Exerc�cio 3
#An�lise de regress�o linear
#Importando o banco de dados
dados<-read_excel("C:/Users/Stephanie/Downloads/altman.xlsx")

#Calculando os coeficientes da reta de ajuste da CTP em fun��o da idade
FIT1 <- lm(idade ~ CTP, data = dados)
FIT1
#Fazendo o gr�fico de dispers�o dos valores de CTP em fun��o da idade juntamente com sua reta de ajuste
plot(idade ~ CTP, data = dados,xlab = "Idade (Anos)", ylab = "CTP (L)", main = "CTP em fun��o da Idade")
abline(FIT1)

#Calculando os coeficientes da reta de ajuste da CTP em fun��o da altura
FIT2 <- lm(altura ~ CTP, data = dados)
FIT2
#Fazendo o gr�fico de dispers�o dos valores de CTP em fun��o da altura juntamente com sua reta de ajuste
plot(altura ~ CTP, data = dados,xlab = "altura (cm)", ylab = "CTP (L)", main = "CTP em fun��o da altura")
abline(FIT2)

#Fazendo o boxplot dos dados de CTP separados entre Homens e Mulheres
boxplot(CTP~sexo, data = dados, ylab = "CTP (L)", main = "CTP dos Homens e Mulheres")

