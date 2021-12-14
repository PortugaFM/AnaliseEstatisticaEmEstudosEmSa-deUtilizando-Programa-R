#Importando o banco de dados do computador
dados<-read.table("C:/Users/Stephanie/Downloads/R/R/antropo.txt",header=T)

####Exercício 1
#Adicionando uma coluna de dados do peso em Kg(o nome da variavel será weightkg)
dados$weightkg <- dados$Weight*0.45359237

####Exercício 2
#Adicionando uma coluna de dados da altura em cm(o nome da variavel será heightcm)
dados$heightcm <- dados$Height*2.54

####Exercício 3
#Plotando um boxplot para cada váriavel contínua do banco de dados
boxplot(dados$Density,main="Densidade",ylab="g/cm^3",las=2)
boxplot(dados$Fat,main="Percentual de gordura",ylab="%",las=2)
boxplot(dados$Age,main="Idade",ylab="anos",las=2)
boxplot(dados$Weight,main="Peso",ylab="Libras",las=2)
boxplot(dados$Height,main="Altura",ylab="Polegadas",las=2)
boxplot(dados$Neck,main="Circunferência do Pescoço",ylab="cm",las=2)
boxplot(dados$Chest,main="Circunferência do Peito",ylab="cm",las=2)
boxplot(dados$Abdomen,main="Circunferência do Abdomen",ylab="cm",las=2)
boxplot(dados$Hip,main="Circunferência do Quadril",ylab="cm",las=2)
boxplot(dados$Thigh,main="Circunferência da Coxa",ylab="cm",las=2)
boxplot(dados$Knee,main="Circunferência do Joelho",ylab="cm",las=2)
boxplot(dados$Ankle,main="Circunferência do Tornozelo",ylab="cm",las=2)
boxplot(dados$Biceps,main="Circunferência do Bíceps",ylab="cm",las=2)
boxplot(dados$Forearm,main="Circunferência do Antebraço",ylab="cm",las=2)
boxplot(dados$Wrist,main="Circunferência do Pulso",ylab="cm",las=2)
boxplot(dados$weightkg,main="Peso (kg)",ylab="Kg",las=2)
boxplot(dados$heightcm,main="Altura (cm)",ylab="cm",las=2)

####Exercício 4
#Construindo o gráfico de dispersão entre o peso e a altura das pessoas
plot(dados$weightkg,dados$heightcm,ylab="Altura (cm)",xlab="Peso (Kg)",main = "Altura em função do peso")

####Exercício 5
#Construindo histogramas para a idade, peso e a altura das pessoas
hist(dados$Age, main="Histograma das Idades", xlab="Idade (anos)",ylab="Frequência",  ylim = c(0,60),xlim = c(0,100))
hist(dados$weightkg, main="Histograma dos Pesos", xlab="Peso (kg)",ylab="Frequência",  ylim = c(0,100),xlim = c(30,200))
hist(dados$heightcm,main="Histograma das alturas",xlab="altura(cm)",ylab="Frequência",  ylim = c(0,150),xlim = c(50,220))


####Exercício 6
#Calculando os gráficos de dispersão do percentual de gordura gordura m função das outras variáveis antropométricas
plot(dados$Fat,dados$Density,xlab="Percentual de gordura (%)",ylab="Densidade (g/cm³)",main = "Densidade")
plot(dados$Fat,dados$Age,xlab="Percentual de gordura (%)",ylab="Idade (Anos)",main = "Idade")
plot(dados$Fat,dados$Weight,xlab="Percentual de gordura (%)",ylab="Peso (lb)",main = "Peso")
plot(dados$Fat,dados$Height,xlab="Percentual de gordura (%)",ylab="Altura (in))",main = "Altura")
plot(dados$Fat,dados$Neck,xlab="Percentual de gordura (%)",ylab="Circunferência do Pescoco (cm)",main = "Circunferencia do Pescoço")
plot(dados$Fat,dados$Chest,xlab="Percentual de gordura (%)",ylab="Circunferência do Peito (cm)",main = "Circunferencia do Peito")
plot(dados$Fat,dados$Abdomen,xlab="Percentual de gordura (%)",ylab="Circunferência do Abdomen (cm)",main = "Circunferencia do Abdômen")
plot(dados$Fat,dados$Hip,xlab="Percentual de gordura (%)",ylab="Circunferência do Quadril (cm)",main = "Circunferencia do Quadril")
plot(dados$Fat,dados$Thigh,xlab="Percentual de gordura (%)",ylab="Circunferência da Coxa (cm)",main = "Circunferencia da Coxa ")
plot(dados$Fat,dados$Knee,xlab="Percentual de gordura (%)",ylab="Circunferência do Joelho (cm)",main = "Circunferencia do Joelho")
plot(dados$Fat,dados$Ankle,xlab="Percentual de gordura (%)",ylab="Circunferência do Tornozelo (cm)",main = "Circunferência do Tornozelo")
plot(dados$Fat,dados$Biceps,xlab="Percentual de gordura (%)",ylab="Circunferência do Bíceps (cm)",main = "Circunferência do Bíceps")
plot(dados$Fat,dados$Forearm,xlab="Percentual de gordura (%)",ylab="Circunferência do Antebraço (cm)",main = "Circunferência do Antebraço")
plot(dados$Fat,dados$Wrist,xlab="Percentual de gordura (%)",ylab="Circunferência do Pulso (cm)",main = "Circunferência do Pulso (cm)")
plot(dados$Fat,dados$weightkg,xlab="Percentual de gordura (%)",ylab="Peso (kg)",main = "Peso (kg)")
plot(dados$Fat,dados$heightcm,xlab="Percentual de gordura (%)",ylab="Altura (cm)",main = "Altura (cm)")

####Exercício 7
#Cálculo dos coeficientes de correlação de Pearson do percentual de gordura em função das outras variáveis antropométricas
cor.test(dados$Fat,dados$Density,method = c("pearson"))
cor.test(dados$Fat,dados$Age,method = c("pearson"))
cor.test(dados$Fat,dados$Weight,method = c("pearson"))
cor.test(dados$Fat,dados$Height,method = c("pearson"))
cor.test(dados$Fat,dados$Neck,method = c("pearson"))
cor.test(dados$Fat,dados$Chest,method = c("pearson"))
cor.test(dados$Fat,dados$Abdomen,method = c("pearson"))
cor.test(dados$Fat,dados$Hip,method = c("pearson"))
cor.test(dados$Fat,dados$Thigh,method = c("pearson"))
cor.test(dados$Fat,dados$Knee,method = c("pearson"))
cor.test(dados$Fat,dados$Ankle,method = c("pearson"))
cor.test(dados$Fat,dados$Biceps,method = c("pearson"))
cor.test(dados$Fat,dados$Forearm,method = c("pearson"))
cor.test(dados$Fat,dados$Wrist,method = c("pearson"))
cor.test(dados$Fat,dados$weightkg,method = c("pearson"))
cor.test(dados$Fat,dados$heightcm,method = c("pearson"))


####Exercício 8
plot(dados$Age,dados$Abdomen,xlab="Idade (Anos)",ylab="Circunferência do Abdomem (cm)",main = "Circunferência do Abdomem")
cor.test(dados$Abdomen,dados$Age,method = c("pearson"))


