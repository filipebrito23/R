require(rmutil)
a = NULL #vari�vel auxiliar
B=1000 
n = 5 
sigma2=1 #vari�ncia da normal
m=2 #m�dia da normal
for(i in 1:B){
  r = rnorm(n, m, sigma2); #distribui��o normal
  a[i]= sum(r)
}

plot(density(a,adjust=2,cut = 0),lwd=2, col = "blue", lty = 1, ylim = c(0,0.25), xlab = "X", 
     ylab= "Y", main="Gr�fico Ponto de Sela") #gr�fico

fsn = function(s,n,k,theta){
  lchap = (s/n - m)/sigma2 #par�metro
  K1 = (lchap*m)+((lchap^2)*sigma2/2) #Fun��o geratriz Cumulante
  K2 = sigma2 # Cumulante 2
  return(((exp((-(s-n*m)^2)/2*n*sigma2))/sqrt(2*n*pi*sigma2))) #fdp
}

curve(fsn(x,n,2.5,0.8), lwd=2, col = "red", type="l", add=T) #Ajuste
legend("topright",legend=c("Expans�o Ponto de Sela", "Densidade Emp�rica"),
       col=c("red","blue"),lwd=2, bty="n") 