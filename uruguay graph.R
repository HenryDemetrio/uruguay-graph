##############################

# Autores
# Bernardo Stocco Kruschewsky Whehaibe
# Henry Dem�trio

# Data de cria��o: 15/03/2022

##############################

# Instalando o pacote e a biblioteca do 'igraph'.

install.packages('igraph')

library(igraph)


# Executando a matriz em formato CSV.

matriz1 <- read.table(row.names=1,file=file.choose(),header = TRUE,sep = ';')

matriz1

matriz1 <- as.matrix(matriz1)


# Executando o grafo

grafo1 <- graph.adjacency(matriz1, mode='undirected', weighted=TRUE)

plot(grafo1)

plot(grafo1, edge.label = round(E(grafo1)$weight,3))


# Plotando o grafo utilizando o comando 'tkplot'.

tkplot(grafo1)


# Calculando os graus dos v�rtices do grafo.

degree(grafo1)

graus <- degree(grafo1)

graus

# Calculando o histograma dos graus

hist(graus)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando o maior grau

maiorGrau <- max (graus)

maiorGrau


## A cidade de Durazno possui o MaiorGrau por ter mais fronteiras com os outros 
## estados, justamente por ser um estado que se encontra no centro do pa�s.

menorGrau <- min (graus)

menorGrau

## De acordo com os resultados obtemos que Artigas possui o menor grau por
## consequ�ncia de ser o pa�s mais isolado, fazendo fronteira apenas com Salto.

# Indetificando qual v�rtice pertence a qual grau

which (graus == graus)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando a proximidade dos v�rtices do grafo.

proximidade <- closeness(grafo1)

proximidade

1/proximidade

# Executando o histograma da proximidade dos v�rtices.

hist(proximidade)

# Calculando a maior proximidade

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

maiorProximidade <- max (proximidade)

maiorProximidade

## Novamente, Durazno, por fazer mais fronteiras com os outros estados possui 
## a maior proximidade.

# Calculando a menor proximidade

menorProximidade <- min (proximidade)

menorProximidade

## E por conseguinte, Artigas, por possuir menos fronteiras possui a menor
## proximidade.

# Indetificando qual v�rtice pertence a qual proximidade

which (maiorProximidade == maiorProximidade)

which (menorProximidade == menorProximidade)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando as intermedia��es dos v�rtices do grafo.

intermediacaoVertices <- betweenness(grafo1)

intermediacaoVertices

# Executando o histograma das intermedia��es dos v�rtices.

hist(intermediacaoVertices)

# Calculando a maior intermedia��o dos v�rtices do grafo.

maiorIntermediacaoVertices <- max (intermediacaoVertices)

maiorIntermediacaoVertices

# Calculando a menor intermedia��o dos v�rtices do grafo.

menorIntermediacaoVertices <- min (intermediacaoVertices)

menorIntermediacaoVertices


# Calculando as intermedia��es (betweenes) das arestas do grafo.

intermediacaoArestas = edge.betweenness(grafo1)

intermediacaoArestas

# Executando o histograma das intermedia��es dos v�rtices.

hist(intermediacaoArestas)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a maior proximidade

maiorIntermediacaoArestas <- max (intermediacaoArestas)

maiorIntermediacaoArestas

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a menor proximidade

menorIntermediacaoArestas <- min (intermediacaoArestas)

menorIntermediacaoArestas

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Indetificando qual v�rtice pertence a qual proximidade

which (intermediacaoArestas == intermediacaoArestas)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando as dist�ncias dos v�rtices do grafo.

distancias <- distances(grafo1)

distancias

# Executando o histograma das dist�ncias dos v�rtices.

hist(distancias)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a maior proximidade

maiorDistancia <-max (distancias)

maiorDistancia

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a menor proximidade

menorDistancia <-min (distancias)

menorDistancia

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Indetificando qual v�rtice pertence a qual proximidade

which (distancias == distancias)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando o di�metro dos v�rtices do grafo 

get_diameter(grafo1)

diametro <- get_diameter(grafo1)

diametro

# Executando o histograma do di�metro dos v�rtices.

hist(diametro)

## O que voc� pode dizer sobre essa m�trica e os aspectos
## socioeconomicos desses estados/provincias/paises

caminhoDoDiametro <- get_diameter(grafo1)
caminhoDoDiametro

vcol <- rep("white", vcount(grafo1))
vcol[caminhoDoDiametro] <- "purple"
ecol <- rep("gray80", ecount(grafo1))
ecol[E(grafo1, path=caminhoDoDiametro)] <- "orange"
plot(grafo1, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)


tkplot(grafo1, vertex.color = "cyan")



