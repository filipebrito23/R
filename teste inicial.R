#Definir vari�veis
x <- 4 #<- atribui valor � vari�vel
y <- 3
z <- "7"

print(x)
print(paste(x,y,z))

#Opera��es
primeiros_pares <- c(2,4,6,8,10,12,14) #cria vetor a ser utilizado
mean(primeiros_pares) #m�dia
max(primeiros_pares) #Pega o maior valor do vetor ou matriz
sd(primeiros_pares) #Desvio Padr�o
var(primeiros_pares) #Vari�ncia
median(primeiros_pares) #Mediana
fivenum(primeiros_pares) #quartis e mediana
x%%y # %% indica resto de divis�o

#sequ�ncias
primeiro_pares*y
primeiros_impares <- seq(1,13, by=2) #sequ�ncia definindo extremos
print(primeiros_impares)

plot(primeiros_pares,primeiros_impares) #pares no x e �mpares no y

popul <- floor(runif(20,min = 10, max = 100)) #floor arredonda para baixo, runif gera n�meros aleat�rios)
popul
hist(popul, col="blue")

# Vari�veis s�o livres, podem receber flutuantes, n�meros, textos, etc.

#WorkSpace

ls() # ls() mostra quantas vari�veis est�o definidas.
rm(x) #limpa a vari�vel x
rm(list = ls()) #apaga todas as vari�veis

?hist #usa o help pra pesquisa a fun��o

#Dados
#Verifica com Class(valor)
#inteiro - Integer ; Flutuante - Numeric; Character; L�gico;
#OBS: ao tratar de n�mero inteiro, se usa L ap�s o n�mero, para que possa entender que � inteiro; Ex: 4L � o mesmo que 4
#OBS2: Character � sempre entre "", L�gico � TRUE ou FALSE

#Condi��es para avalia��o de dados:
is.integer(y) #is.algo retorna o valor l�gico

#Convers�o de dados (Coer��o)
as.integer(z) #as.[tipo de dado]() trata o dado como o tipo indicado

#Vetores - fun��o c
vet1 <- c(1, 4, 3, 5, 2)
vet1[2] #retorna a segunda coluna do vetor (come�a no 1, diferente de C)

v1 <- seq(2,185,5) #sequencia inicia em 2, termina em 185 e vai de 5 em 5
v2 <- seq(5,185,5)

#nomeando
sem1 <- c("Seg", "Ter", "Qua", "Qui", "Sex")
names(vet1) <- sem1 #Nomeia o vetor com a devida correspondencia
vet1["Ter"] #Permite selecionar o vetor pelo nome

#Adicionar elemento ao vetor
vet1[length(vet1)+1] <- "Sab"

vet1 <- c(vet1, "Dom")

vet1 <- vet1[-3]
vet1 <- vet1[-c(1:3)]

#matrizes

matrix(1:5,5,2)

es

