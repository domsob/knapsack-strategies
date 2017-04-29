# Tutorial for GA library: https://cran.r-project.org/web/packages/GA/GA.pdf

library(GA)

set.seed(42);

limit <- 10
objects <- 100
values <- sample(1:1000, objects, TRUE)
weights <- sample(1:15, objects, TRUE)

cat('\n\n+-----------------------------------+\n')
cat('|              Settings             |\n')
cat('+-----------------------------------+\n\n')
cat('limit   = ')
cat(limit)
cat('\n')
cat('objects = ')
cat(objects)
cat('\n\n\n')

cat('+-----------------------------------+\n')
cat('|               Values              |\n')
cat('+-----------------------------------+\n\n')
print(values)
cat('\n\n')
cat('+-----------------------------------+\n')
cat('|              Weights              |\n')
cat('+-----------------------------------+\n\n')
print(weights)
cat('\n\n')

greedyStrategy <- function() {
    fitnessValue <- 0
    weightSum <- 0
    i <- 1
    for(entry in weights) {
        if((weightSum + entry) < (limit + 1)) {
            fitnessValue <- fitnessValue + values[i]
            weightSum <- weightSum + entry
        }
        i <- i + 1
    }
    return(fitnessValue)
}

cat('+-----------------------------------+\n')
cat('|          Greedy Strategy          |\n')
cat('+-----------------------------------+\n\n')
cat('Fitness function value = ')
cat(greedyStrategy())
cat('\n\n\n')

densityStrategy <- function(){
    x <- values / weights
    w <- weights
    v <- values
    n<-length(x)
    for(j in 1:(n-1)){
        for(i in 1:(n-j)){
            if(x[i]<x[i+1]){
                temp<-x[i]
                temp2<-w[i]
                temp3<-v[i]
                x[i]<-x[i+1]
                w[i]<-w[i+1]
                v[i]<-v[i+1]
                x[i+1]<-temp
                w[i+1]<-temp2
                v[i+1]<-temp3
            }
        }
    }
    k <- 1
    valueSum <- 0
    weightSum <- 0
    for(entry in w) {
        if((weightSum + entry) < (limit + 1)) {
            weightSum <- weightSum + entry
            valueSum <- valueSum + v[k]
        }
        k <- k + 1
    }
    return(valueSum)
}

cat('+-----------------------------------+\n')
cat('|         Density Strategy          |\n')
cat('+-----------------------------------+\n\n')
cat('Fitness function value = ')
cat(densityStrategy())
cat('\n\n\n')


fitnessFunction <- function(chromosome) {
    valueSum <- 0
    weightSum <- 0
    i <- 1
    for(gene in chromosome) {
        if(gene == 1) {
            valueSum <- valueSum + values[i]
            weightSum <- weightSum + weights[i] 
        }
        i <- i + 1
    }
    fitnessValue <- valueSum
    if(weightSum > limit) {
        fitnessValue <- 0
    }
    return(fitnessValue)
}

populationFunction <- function(object) {
    m0 <- matrix(0, object@popSize, objects)
    m01 <- apply(m0, c(1,2), function(x) sample(c(0,1),1,TRUE,c(0.96,0.04))) # probability for 0 and 1
    return(m01)
}

result <- ga(type="binary", fitness=fitnessFunction, nBits=objects, popSize=100, maxiter=1000, pcrossover=0.8, pmutation=0.1, elitism=20, run=1000, population=populationFunction)
summary(result)
#summary(result)$solution
plot(result)
