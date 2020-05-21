# 21/05/20
# INM

require(ggplot2)

# Simularemos un proceso Poisson compuesto.
# S_t = X_1 + X_2 + ... + X_{N__t}
# donde N_t es un proceso Poisson homogéneo con intensidad lambda y
# {X_n} es una sucesión de variables aleatorias iid, independiente de N_t

# X_1 ~ N(mu, sigma^2) con número fijo de saltos
# Recordemos que los tiempos entre saltos del proceso Poisson son vaiid exponenciales
# con parámetro lambda
poisson_compuesto_normal <- function(nsaltos, lambdap, mu, sigma2){
    Tsaltos <- rexp(n = nsaltos, rate = lambdap)
    X <- rnorm(n = nsaltos, mean = mu, sd = sqrt(sigma2))
    return(
        data.frame(
            t = c(0, cumsum(Tsaltos)),
            N = 0:nsaltos,
            S = c(0, cumsum(X))
        )
    )
}

X <- poisson_compuesto_normal(nsaltos = 20, lambdap = 1, mu = pi, sigma2 = 10)

# El proceso Poisson subyacente
ggplot(data = X, aes(x = t, y = N)) +
    geom_step() +
    theme_minimal()

# El proceso Poisson compuesto
ggplot(data = X, aes(x = t, y = S)) +
    geom_step() +
    theme_minimal()

# Para X_1 general, con un número fijo de saltos
poisson_compuesto <- function(nsaltos, lambdap, distx, ...){
    Tsaltos <- rexp(n = nsaltos, rate = lambdap)
    X <- distx(n = nsaltos, ...)
    return(
        data.frame(
            t = c(0, cumsum(Tsaltos)),
            N = 0:nsaltos,
            S = c(0, cumsum(X))
        )
    )
}

X2 <- poisson_compuesto(nsaltos = 15, lambdap = 1, distx = rgeom, prob = 1/3)

# El proceso Poisson subyacente
ggplot(data = X2, aes(x = t, y = N)) +
    geom_step() +
    theme_minimal()

# El proceso Poisson compuesto
ggplot(data = X2, aes(x = t, y = S)) +
    geom_step() +
    theme_minimal()


# Browniano
# W_t es un movimiento browniano estándar si
#   W_0 = 0
#   Tiene incrementos independientes y estacionarios
#   W_t ~ N(0, t)
# Para dt > 0 vamos a simular variables aleatorias N(0,dt)
# y las vamos a sumar
# W_{dt} ~ N(0, dt), W{2dt} = (W_{2dt} - W_{dt}) + W_{dt} ~ N(0, 2dt)
browniano <- function(npasos, dt){
    difW <- rnorm(n = npasos, mean = 0, sd = sqrt(dt))
    W <- cumsum(difW)
    return(
        data.frame(
            t = (0:npasos) * dt,
            W = c(0, W)
        )
    )
}

W1 <- browniano(npasos = 1e3, dt = 1e-3)

ggplot(data = W1, aes(x = t, y = W)) +
    geom_line() +
    theme_minimal()

W2 <- browniano(npasos = 1e5, dt = 1e-4)
ggplot(data = W2, aes(x = t, y = W)) +
    geom_line() +
    theme_minimal()
