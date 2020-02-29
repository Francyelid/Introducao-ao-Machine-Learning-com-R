# Carregando os dados de um banco da internet
df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# Nome das vari??veis
names(df)

# sum??rio dos dados
summary(df)

# Carregando o banco de um pacote
library(DAAG)

data(houseprices)

names(houseprices)

summary(houseprices)



# -----------------------------------------------------------------------------
#                                MODELO LINEAR
# -----------------------------------------------------------------------------
# o objetivo ?? verificar o qu?? afeta o pre??o das casas

mr <- lm(sale.price~area, data = houseprices)
summary(mr)

plot(sale.price~area, data = houseprices, xlab = "??rea da casa em m??", ylab = "Pre??o de venda em US$")

areaNova <- data.frame(area = c(1100, 1200)) # um novo objeto sendo criado com novos valores
predict(mr, areaNova, type = "response") # predizendo  novos valores

plot(sale.price~area, data = houseprices, xlab = "??rea da casa em m??", ylab = "Pre??o de venda em US$")
p <- predict(mr, areaNova, type = "response")
points(c(1100, 1200), p, col = "red")

curve(70.7504+0.1878*x, add=T, col="blue") # y = a + b * x


# -----------------------------------------------------------------------------
#                                MODELO BINOMIAL
# -----------------------------------------------------------------------------
# O objetivo ?? verificar as probabilidades 
# de um aluno ser aceito ou n??o, com base nos dados de outros alunos

names(df)
# ADMIT ?? um booleano que  diz se ele foi adimitido ou n??o
# GRE (Graduate Record Exam) ?? o valor mais alto da avalia????o
# GPA (Grade Point Avarege) ?? o valor m??dio

mb <- glm(admit~gre, data = df)
summary(mb)

#________________Exemplo de alunos que tem gre entre 250 e 280_________________
greNovo <- data.frame(gre = c(250, 280))

predict(mb, greNovo, type = "response")

# plotando os dois elementos em um gr??fico
plot(admit~gre, data = df)
p2 <-predict(mb, greNovo, type = "response")
points(c(250,280), p2, col = "red")



# -----------------------------------------------------------------------------
#                           MODELO LINEAR M??LTIPLO
# -----------------------------------------------------------------------------
# Cont??m mais vari??veis explicativas

mr2 <- lm(sale.price~area + bedrooms, data = houseprices)
names(houseprices)
summary(mr2)

plot(sale.price~area, data = houseprices, xlab = "??rea da casa em m??", ylab = "Pre??o de venda em US$", ylim = c(70, 400))
p <- predict(mr, areaNova, type = "response")
points(c(1100, 1200), p, col = "red")

areaNova2 <- data.frame(area = c(1100, 1200), bedrooms = c(2, 1))
predict(mr2,areaNova2, type = "response")
points(c(1100, 1200), p2, col = "green")



# -----------------------------------------------------------------------------
#                           MODELO BINOMIAL M??LTIPLO
# -----------------------------------------------------------------------------
# Cont??m mais vari??veis explicativas
names(df)
mb2 <- glm(admit~gre+gpa, data = df)
summary(mb2)

# Sabendo que as duas vari??veis (GRE e GPA) afetam a admiss??o do aluno...
greNovo2 <- data.frame(gre = c(250, 580), gpa = c(3.13, 3.80))

# Com base no padr??o, ?? poss??vel dar um predict dos novos valores
p3 <- predict(mb2, greNovo2, type = "response")

plot(admit~gre, data = df)
points(c(250, 580), p2, col = "red")
points(c(250, 580), p3, col = "purple")



# -----------------------------------------------------------------------------
#                              MACHINE LEARNING
# -----------------------------------------------------------------------------
# toda vez que se roda os bancos, os pacotes randomizam os dados
set.seed(123) # esse comando controla os dados, padronizando desde a primeira vez

# carregando o pacote do ML
library(caret)


# dividindo o BD para o modelo de regress??o
dataIndex <-  createDataPartition(houseprices$sale.price, p = .7, list = FALSE) # 70% dos dados ser??o usados para treinar o algoritmo

houseTreino <- houseprices[dataIndex,]

houseTeste <-  houseprices[-dataIndex,]

#######################
# Modelo de regress??o #
#######################

modeloML1 <- train(sale.price~bedrooms + area, data = houseTreino, method = "glm") # algoritmo GLM
modeloML2 <- train(sale.price~bedrooms + area, data = houseTreino, method = "ranger", importance = "impurity") # algoritmo Random Forest

#Comparando os modelos
listModelos <- list(glm = modeloML1, ranger = modeloML2)

comparacao <- resamples(listModelos)

dotplot(comparacao)

# Nesse caso, o algoritmo GLM apresenta um menor erro
library(caTools)

plot(varImp(modeloML1)) # importancia das vari??veis

predict(modeloML1, newdata = houseTeste)



###########################
# Modelo de classifica????o #
###########################
df$admit <- as.factor(df$admit) # transformando em um fator

dataIndex2 <- createDataPartition(df$admit, p = .7, list = FALSE)

dfTreino <- df[dataIndex2,]
dfTeste <- df[-dataIndex2,]

modeloML3 <- train(admit~gre+gpa+rank, data = dfTreino, method = "glm")
modeloML4 <- train(admit~gre+gpa+rank, data = dfTreino, method = "ranger", importance = "impurity")

pGLM <- predict(modeloML3, dfTeste)
pRANGER <- predict(modeloML4, dfTeste)

#Comparando os modelos
confusionMatrix(factor(pGLM), factor(dfTeste$admit))

confusionMatrix(factor(pRANGER), factor(dfTeste$admit))

plot(varImp(modeloML3)) # importancia das vari??veis

predict(modeloML3, newdata = dfTeste)
# -----------------------------------------------------------------------------