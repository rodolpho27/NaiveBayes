# Machine learning 

#Carrengando a biblioteca
library('e1071')

#lendo o diretorio onde se encontra o arquivo 
credito = read.csv(choose.files(), sep = ",", header = T)
View(credito)
#Conferindo se o arquivo foi carregado corretamente 

head(credito)
str(credito)
#Quantidade de registros e colunas 
dim(credito)

#Treinar o modelo com 70% dos dados e o restante criar o classificador 
amostra = sample(2, 1000, replace = T,prob = c(0.7, 0.3))

#Selecionando os 70% das amostras da vari�vel credito
creditotreino = credito[amostra==1,]

#Selecionando os 30% das amostras da vari�vel credito 
creditoteste = credito[amostra==2,]

#Conferindo os registros e colunas que resultaram depois da escolha 
#das amostras para treino e teste 
dim(creditotreino)
dim(creditoteste)

#Gerando o modelo 
modelo = naiveBayes(class ~ . , creditotreino, )

#Fazendo a predi��o sobre os dados testes
predicao = predict(modelo , creditoteste)
predicao 

#Conferindo a acertividade do modelo atrav�s de uma matriz de confus�o
confusao = table(creditoteste$class, predicao)
confusao

#Porcentagem de acerto do meu modelo, acertos
taxaacerto = (confusao[1] + confusao[4]) / sum(confusao)
taxaacerto

#Porcentagem de erro do meu modelo, erro
taxaerro = (confusao[2] + confusao[3])/ sum(confusao)
taxaerro

#Fazendo a previs�o de um cliente que est� solicitando um credito
#Porque at� ent�o tenho dados hist�ricos 
novocredito = read.csv(choose.files(), sep = ",", header = T)

#Tenho um cliente com apenas 20 colunas pois preciso saber a resposta 
dim(novocredito)

#Prevendo para o novo cliente 

predict(modelo, novocredito)
