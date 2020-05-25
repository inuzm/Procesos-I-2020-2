# 25/05
# INM

# Cargamos librerías
require(ggplot2)
require(dplyr)
require(reshape2)
require(kableExtra)

# Vamos a trabajar con el modelo de difusión de Bernoulli—Laplace. Tenemos 2N pelotas, N blancas y N rojas,
# de las cuales depositamos de forma aleatoria N en la urna 1 y N en la urna 2. Tomamos una pelota de la urna 1 y
# una de la urna 2 y las intercambiamos. Si X_n es la cantidad de pelotas blancas en la urna 1 después del 
# n-ésimo intercambio, {X_n} es una cadena de Markov con espacio de estados E = {0, 1, ..., N} y probabilidades 
# de transición:
# P(i, i-1) = i^2 / N^2
# P(i, i) = 2 * i * (N-i) / N^2
# P(i, i+1) = (N-i)^2 / N^2
difusionBL <- function(n, N, xini = NULL){
    X <- numeric(length = n+1)
    if(is.null(xini)){
        X[1] <- floor((N+1) * runif(n = 1, min = 0, max = 1))
    } else {
        X[1] <- xini
    }
    for(i in 1:n){
        X[i+1] <- sample(x = c(X[i]-1, X[i], X[i]+1), size = 1, prob = c(X[i]^2, 2* X[i] * (N - X[i]), (N - X[i])^2))
    }
    return(X)
}

X1 <- difusionBL(n = 100, N = 10)
plot(X1, pch = 20)

corre_varios_dBL <- function(nsim, npasos, N){
    datos <- data.frame(
        edos = numeric(0),
        prop = numeric(0),
        simulacion = numeric(0)
    )
    edosBL <- 0:N
    for(i in 1:nsim){
        X <- difusionBL(n = npasos, N = N)
        # edosBL <- sort(unique(X))
        prop.visitados <- numeric(length = N+1)
        for(j in 0:N){
            prop.visitados[j+1] <- mean(X == edosBL[j+1])
        }
        datos_aux <- data.frame(
            edos = edosBL,
            prop = prop.visitados,
            simulacion = rep(i, N+1)
        )
        datos <- rbind(datos, datos_aux)
    }
    return(datos)
}

X2 <- corre_varios_dBL(nsim = 10, npasos = 1e5, N = 7)

crear_tabla <- function(datos, formato = "r"){
    if(formato == "r"){
        datos %>%
            acast(simulacion ~ edos, value.var = "prop") %>%
            kable(format = "rst", row.names = TRUE) %>%
            print()
    } else if (formato == "latex"){
        datos %>%
            acast(simulacion ~ edos, value.var = "prop") %>%
            kable(format = "latex", row.names = TRUE, booktabs = TRUE, align = 'c') %>%
            kable_styling(latex_options = "striped", full_width = FALSE) %>%
            print()
    }
}

crear_tabla(datos = X2, formato = "latex")

# ¿Para qué sirve %>%?
# %>% se llama pipe y permite mandar el resultado de un comando a otro
1:10 %>%
    sum()
