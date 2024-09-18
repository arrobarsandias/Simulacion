# Tarea 1

set.seed(1)


martingala <- function(bolsa=100,apuesta=10,limite=500,prob=18/37){
  
  apuesta1 <- apuesta
  
  repeat{
    
    resultado <- rbinom(1,1,prob)
    
    if (resultado==1){
      
      bolsa <- bolsa+apuesta
      
      apuesta <- apuesta1}
    
    else{
      
      bolsa <- bolsa-apuesta
      
      apuesta <- apuesta*2}
    
    if (bolsa>limite | bolsa<apuesta) break
  }
  
  return (bolsa)
}


martingala()


# Tarea 2

# Tarea 3

# Tarea 4

# Tarea 5