#Simulacion del juego consiste en lanzar una moneda
#Enunciado: Suponga que un juagor accede al juego de lanzar
#una moneda y por cada sello que botenga en el lanzamiento
#cobra un aunidad monetaria, asuma que el juegor inicia el juego
#con 0 unidades monetarias.
try <- cumsum(sample(c(0,1), size = 6, replace = TRUE))
plot(1:length(try), try, type = "b", col = 2)

# Función que genera una trayectoria tras realizar n lanzamientos
try <- function(n){
  return(cumsum(sample(c(0,1), size = n, replace = TRUE)))# 0:= cara y 1:=sello
  # suma acumulada cumsum()
}
try(6)

# Función que genera m replias de trayectorias.
rpl <- function(m,n){
  res <- matrix(0, ncol = n, nrow = m)
  for(j in 1:m){
    res[j,] <- try(n)
  }
  pr <- table(res[,n])/m
  return(list(Matriz = res, Vector = pr, CapitalEsperado = sum(0:n*pr)))
}

system.time(T1 <- rpl(1000000,10))
T1 <- rpl(1000000,6)
T1$Vector
T1$CapitalEsperado
