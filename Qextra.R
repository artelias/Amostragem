# Atividade Extra amostragem
# Aluno:Arthur Hneiruq Elias de Lima
# Definindo variaveis 
mecânico=c(1,2,3,4,5,6,7,8)
mecânico=as.character(mecânico)
tempo=c(62,75,60,65,81,71,77,89)
N=50;n=8;pik=n/N;yk=tempo

# Oriando data.frame pra visualização 
tabela1=data.frame(mecânico,tempo)
tabela1

# Estimador de HH para variancia 
varY_HH=function(pik,yk,N,n){
    varY_HH=((1/n)*sum(pik*((yk/pik)-(sum(yk)/pik))^2))/N^2
    return(varY_HH)
}

# Estimador de HH para media
Y_HH=function(pik,yk,N){
  Y_HH=(sum(yk)/pik)/N
  return(Y_HH)
}

#LETRA B
# Comparativo das medias
Y_HH(8/50,tempo,N=50)
mean(tempo)
# Observamos que o estimador é centrado, pois a media Y_HH,
# é igual a media da amostra


#LETRA C
# Analisando a variancia de HH
HH=varY_HH(pik,tempo,N,n);HH

# Estimador de Horvitz-Thompson
varY_HT=function(yk,pik,n){
  var_HT =(1 - pik)*var(yk)/n
  return(var_HT)
}
HT=varY_HT(tempo,pik,n);HT
# Temos que o estimador de HH apresenta variância maior que o
# estimador HT. Portanto, no caso estudado, o estimador HT é
# mais eficiênte.
ic_media_Y_HT <- c(mean(tempo) + qt(0.025, n - 1)*HT,
                   mean(tempo) + qt(0.975, n - 1)*HT); ic_media_Y_HT
#a um intervalo de confiança de 95%,
#HT está entre [47.81332 ; 97.18668]
