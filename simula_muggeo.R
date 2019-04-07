setwd('/Users/scp93/Dropbox/SER/')
source('aplicacao/pwlm2/data/gerabd2.R')
require(segmented)

S = c(5, 25, 50)

for( s in S){
  set.seed(1)
  out = data.frame(K = rep(9, 300), b01 = rep(NA, 300), b1 = rep(NA, 300), 
                   b2 = rep(NA, 300), Psi1 = rep(NA, 300), Psi2 = rep(NA, 300), Sigma = rep(NA, 300))
  aux = 0
  for(k in 0:2){
    cont = 0
    while (sum(out$K == k) < 100) {
      simulado =  gerabd(n = 100, k = k, b01 = -20, b1 = 20, b2 = 5, psi1 = 20, psi2 = 70,  sigma = s )
      x = simulado$x
      y = simulado$y
      
      # estima modelo
      fit = lm(y~1)
      
      fit = tryCatch({
        segmented(fit, seg.Z= ~x, psi=NA, control = seg.control(K = 2, stop.if.error = T))
      },  error = function(e){NULL})
      
      if(is.null(fit)){
        # break()
      } else {
        out[aux, ] = c(k, fit$coefficients[1], fit$coefficients[2], fit$coefficients[3],
                       fit1$psi[1:2,2], sd(fit$residuals))
        cont = cont +1
        aux = aux+1
        print(aux)
      }
    }
  }
  
  K = out$K
  
  theta = c(-20, 20, -15, 20, 70, s)
  
  erro_relativo = t(apply(out[,-1], 1, function(x){
    (x-theta)/theta
  }))
  
  erro_medio = as.data.frame(t(apply(as.matrix(0:2), 1, function(k){
    idx = which(K==k)
    out = apply(abs(erro_relativo[idx,]), 2, mean)
    
    return(out)
  })))
  
  # rownames(erro_medio) = 0:2
  out = data.frame(sigma = s ,k = 0:2, erro_medio)
  print(out)
  file = paste('sim_Sig_', s, '.csv', sep="")
  
  write.csv(out , file, row.names = F)
}