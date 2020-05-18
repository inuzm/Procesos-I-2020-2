# Simulación de cadenas de Markov homogéneas
# 18/05/20

# Para simular cadenas de Markov necesitamos saber:
#   El espacio de estados: E.
#   Las probabilidades de transición: P

# Primero vamos a simular cdM con las matrices de transición
# La siguiente función recibe
#   n: número de pasos a simular
#   E : el conjunto de estados
#   P: matriz de transición
# Vamos a regresar un vector X con la simulación de la cadena
simular_con_matriz <- function(n, E, P){
    x0 <- sample(x = E, size = 1)
    X <- c(x0)
    for(i in 1:n){
        edo.actual <- which(E == X[i])
        X[i+1] <- sample(x = E, size = 1, prob = P[edo.actual, ])
    }
    return(X)
}

# La cadena cadena de Markov más sencilla es la de 2 estados E = {0,1} con
# P_01 = p y P_10 = q
P1 <- matrix(
    data = c(
        1/3, 2/3,
        1/3, 2/3
    ),
    nrow = 2, byrow = TRUE
)
E1 <- c(0,1)
X1 <- simular_con_matriz(n = 1e5, E = E1, P = P1)
# Vemos los primeros 100 pasos, incluyendo el paso 0
plot(X1[1:100], pch = 20, type = "o")
# En este caso la proporción de pasos que la cadena estuvo en el estado 0
# se aproxima bastante bien a lo que diría la distribución estacionaria
mean(X1 == 0)
mean(X1 == 1)

# Ahora vamos a simular la cadena de rachas
# Recordemos que en esta cadena podemos avanzar de i a i+1 o podemos ir de i a 0
# P(i, i+1) = p = 1 - P(i, 0)

cadena_de_rachas <- function(n, p){
    x0 <- 0
    X <- numeric(length = n+1)
    X[1] <- x0
    for(i in 1:n){
        X[i+1] <- ifelse(runif(n = 1) < p, X[i] + 1, 0)
    }
    return(X)
}

X2 <- cadena_de_rachas(n = 1e5, p = 1/4)
plot(X2[1:100], pch = 20, type = "o")
edos.visitados <- sort(unique(X2))
# Con la siguiente línea se puede calcular el número promedio de pasos que está
# la cadena en cada uno de los estados y, nuevamente, para esta cadena, nos da una buena
# estimación de lo que será su distribución estacionaria
sapply(edos.visitados, function(k) mean(X2 == k) )


# Simularemos la cadena de la fila de espera.
# X_{n+1} = Y_{n+1} si X_n = 0
# X_{n+1} = Y_{n+1} + X_n - 1 si X_n > 0
# Y_n nos dice cuántas personas llegan a la fila en un período de tiempo, mientras
# que X_n nos dice cuántas personas hay en la fila. Y se atiende a un solo cliente por
# período. 
# Consideraremos que Y_n ~ Poisson(lambda)

fila_de_espera <- function(n, lambda, xini){
    X <- numeric(length = n+1)
    X[1] <- xini
    for(i in 1:n){
        if(X[i] == 0){
            X[i+1] <- rpois(n = 1, lambda = lambda)
        } else {
            X[i+1] <- rpois(n = 1, lambda = lambda) + X[i] - 1
        }
    }
    return(X)
}

X3 <- fila_de_espera(n = 1e5, lambda = 1, xini = 3)
plot(X3[1:100], pch = 20, type = "o")
edos.visitados <- sort(unique(X3))

# Cadena de nacimiento y muerte en los naturales con
# P(i,i) = 0
# P(i, i+1) = 1/(i+1)
# P(i, i-1) = 1-1/(i+1)
cadena_nym1 <- function(n){
    X <- numeric(length = n+1)
    X[1] <- 0
    for(i in 1:n){
        # Aquí estoy usando que P(0,-1) = 0. Si P(0,-1) > 0 con la fórmula que demos
        # hay que separar en casos mediante un if-else.
        X[i+1] <- sample(
            x = c(X[i]-1, X[i],X[i]+1), size = 1, 
            prob = c(1-1/(X[i]+1), 0, 1/(X[i]+1))
        )
    }
    return(X)
}

X4 <- cadena_nym1(n = 1e5)
plot(X4[1:100], pch = 20, type = "o")
edos.visitados <- sort(unique(X4))
# Con la siguiente línea se puede calcular el número promedio de pasos que está
# la cadena en cada uno de los estados y, nuevamente, para esta cadena, nos da una buena
# estimación de lo que será su distribución estacionaria
sapply(edos.visitados, function(k) mean(X4 == k) )
