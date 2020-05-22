# 22/05
# INM

require(ggplot2)
require(reshape2)
require(kableExtra)
require(knitr)

# Vamos a simular a los amigos del movimiento browniano
# Movimiento browniano con deriva
# X_t es un movimiento browniano con deriva si
# X_t = mu t + sigma W_t
# donde W_t es un movimiento browniano estándar
browniano_con_deriva <- function(npasos, dt, mu, sigma){
    difW <- rnorm(n = npasos, mean = 0, sd = sqrt(dt))
    W <- c(0, cumsum(difW))
    return(
        data.frame(
            t = (0:npasos) * dt,
            W = W,
            X = (0:npasos) * dt * mu + sigma * W
        )
    )
}

X1 <- browniano_con_deriva(npasos = 1e5, dt = 1e-4, mu = 3, sigma = 1)

ggplot(data = X1, aes(x = t, y = X)) + 
    geom_abline(slope = 3, intercept = 0, col = "red") +
    geom_path() +
    theme_minimal()

# Movimiento browniano geométrico
# S_t es un movimiento browniano geométrico con parámetros mu y sigma si
# S_t = exp{ (mu-sigma^2/2) * t + sigma * W_t }
# donde W_t es un movimiento browniano estándar
browniano_geometrico <- function(npasos, dt, mu, sigma){
    difW <- rnorm(n = npasos, mean = 0, sd = sqrt(dt))
    W <- c(0, cumsum(difW))
    t1 <- (0:npasos) * dt
    S <- exp( (mu - sigma^2/2) * t1 + sigma * W )
    return(
        data.frame(
            t = t1,
            S = S
        )
    )
}

S1 <- browniano_geometrico(npasos = 1e5, dt = 1e-4, mu = 0, sigma = 1)

ggplot(data = S1, aes(x = t, y = S)) +
    geom_path() +
    theme_minimal()

S2 <- browniano_geometrico(npasos = 1e5, dt = 1e-4, mu = 0, sigma = 1.1)

ggplot(data = S2, aes(x = t, y = S)) +
    geom_path() +
    theme_minimal()

S3 <- browniano_geometrico(npasos = 1e5, dt = 1e-4, mu = -1, sigma = 1)

ggplot(data = S3, aes(x = t, y = S)) +
    geom_path() +
    theme_minimal()

# Para complacer a Fernando que viene a hacer sus desastres, vamos a considerar un movimiento browniano
# con deriva cos(mu * t)
browniano_con_deriva_Fernando <- function(npasos, dt, mu, sigma){
    difW <- rnorm(n = npasos, mean = 0, sd = sqrt(dt))
    W <- c(0, cumsum(difW))
    return(
        data.frame(
            t = (0:npasos) * dt,
            W = W,
            X = cos((0:npasos) * dt * mu ) + sigma * W
        )
    )
}

X2 <- browniano_con_deriva_Fernando(npasos = 1e5, dt = 1e-4, mu = pi, sigma = 1)

ggplot(data = X2, aes(x = t, y = X)) +
    geom_path() +
    theme_minimal()

############## Aquí termina lo del curso

# Vamos a seguir con cadenas de Markov
# Cadena de Ehrenfest
# Es un modelo de urnas en el que tenemos 2 urnas y d pelotas etiquetadas del 1 al d distribuidas 
# en las 2 urnas. Realizamos un experimento que consiste en seleccionar de forma equiprobable una
# de las pelotas, sacarla de la urna en la que se encuentra y depositarla en la urna contraria.
# Si X_n es el número de pelotas en la urna 1 después del n-ésimo experimento, {X_n} es una cadena de 
# Markov con espacio de estados E = {0, 1, …, d} y probabilidades de transición
# P(i,i-1) = i/d
# P(i, i+1) = (d-i)/d
ehrenfest <- function(n, d, xini = NULL){
    X <- numeric(length = n+1)
    if(is.null(xini)){
        X[1] <- floor( (d+1) * runif(n = 1, min = 0, max = 1) )
    } else {
        X[1] <- xini
    }
    for(i in 1:n){
        aux <- -2 * (runif(n = 1, min = 0, max = 1) < X[i] / d) + 1
        X[i+1] <- X[i] + aux
    }
    return(X)
}

# Simulación 1 de Ehrenfest
X1 <- ehrenfest(n = 1e5, d = 10)
edos.visitados <- sort(unique(X1))
prop.pasos <- numeric(length = length(edos.visitados))
for(i in 1:length(prop.pasos)){
    prop.pasos[i] <- mean(X1 == edos.visitados[i])
}
# Creamos una tabla
Y <- data.frame(
    edos = edos.visitados,
    prop = prop.pasos,
    simulacion = rep(1, length(edos.visitados))
)

# Simulación 2 de Ehrenfest
X2 <- ehrenfest(n = 1e5, d = 10)
edos.visitados <- sort(unique(X2))
prop.pasos <- numeric(length = length(edos.visitados))
for(i in 1:length(prop.pasos)){
    prop.pasos[i] <- mean(X2 == edos.visitados[i])
}
Y2 <- data.frame(
    edos = edos.visitados,
    prop = prop.pasos,
    simulacion = rep(2, length(edos.visitados))
)
# Juntamos las tablas
Y <- rbind(Y, Y2)

# Creamos una tabla en la que se muestran las proporciones de pasos
# que estuvo cada una de las cadenas en cada estado para las
# simulaciones 1 y 2
# Para verla en la terminal
acast(Y, simulacion ~ edos, value.var = "prop") %>%
    kable(format = "rst", row.names = TRUE)
# Para copiar y pegar en LaTeX con el formato de booktabs
acast(Y, simulacion ~ edos, value.var = "prop") %>%
    kable(format = "latex", row.names = TRUE, booktabs = TRUE, align = "c") %>%
    kable_styling(latex_options = "striped", full_width = F)
