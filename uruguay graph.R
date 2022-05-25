##############################

# Autores
# Bernardo Stocco Kruschewsky Whehaibe
# Henry Demétrio

# Data de criação: 15/03/2022

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


# Calculando os graus dos vértices do grafo.

degree(grafo1)

graus <- degree(grafo1)

graus

# Calculando o histograma dos graus

hist(graus)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando o maior grau

maiorGrau <- max (graus)

maiorGrau


## A cidade de Durazno possui o MaiorGrau por ter mais fronteiras com os outros 
## estados, justamente por ser um estado que se encontra no centro do país.

menorGrau <- min (graus)

menorGrau

## De acordo com os resultados obtemos que Artigas possui o menor grau por
## consequência de ser o país mais isolado, fazendo fronteira apenas com Salto.

# Indetificando qual vértice pertence a qual grau

which (graus == graus)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando a proximidade dos vértices do grafo.

proximidade <- closeness(grafo1)

proximidade

1/proximidade

# Executando o histograma da proximidade dos vértices.

hist(proximidade)

# Calculando a maior proximidade

## O que você pode dizer sobre essa métrica e os aspectos
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

# Indetificando qual vértice pertence a qual proximidade

which (maiorProximidade == maiorProximidade)

which (menorProximidade == menorProximidade)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando as intermediações dos vértices do grafo.

intermediacaoVertices <- betweenness(grafo1)

intermediacaoVertices

# Executando o histograma das intermediações dos vértices.

hist(intermediacaoVertices)

# Calculando a maior intermediação dos vértices do grafo.

maiorIntermediacaoVertices <- max (intermediacaoVertices)

maiorIntermediacaoVertices

# Calculando a menor intermediação dos vértices do grafo.

menorIntermediacaoVertices <- min (intermediacaoVertices)

menorIntermediacaoVertices


# Calculando as intermediações (betweenes) das arestas do grafo.

intermediacaoArestas = edge.betweenness(grafo1)

intermediacaoArestas

# Executando o histograma das intermediações dos vértices.

hist(intermediacaoArestas)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a maior proximidade

maiorIntermediacaoArestas <- max (intermediacaoArestas)

maiorIntermediacaoArestas

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a menor proximidade

menorIntermediacaoArestas <- min (intermediacaoArestas)

menorIntermediacaoArestas

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Indetificando qual vértice pertence a qual proximidade

which (intermediacaoArestas == intermediacaoArestas)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando as distâncias dos vértices do grafo.

distancias <- distances(grafo1)

distancias

# Executando o histograma das distâncias dos vértices.

hist(distancias)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a maior proximidade

maiorDistancia <-max (distancias)

maiorDistancia

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Calculando a menor proximidade

menorDistancia <-min (distancias)

menorDistancia

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

# Indetificando qual vértice pertence a qual proximidade

which (distancias == distancias)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises


#


# Calculando o diâmetro dos vértices do grafo 

get_diameter(grafo1)

diametro <- get_diameter(grafo1)

diametro

# Executando o histograma do diâmetro dos vértices.

hist(diametro)

## O que você pode dizer sobre essa métrica e os aspectos
## socioeconomicos desses estados/provincias/paises

caminhoDoDiametro <- get_diameter(grafo1)
caminhoDoDiametro

vcol <- rep("white", vcount(grafo1))
vcol[caminhoDoDiametro] <- "purple"
ecol <- rep("gray80", ecount(grafo1))
ecol[E(grafo1, path=caminhoDoDiametro)] <- "orange"
plot(grafo1, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)


tkplot(grafo1, vertex.color = "cyan")



