_______________________________#Tugas Kelompok_____________________________
# Kelompok 6
# Teknik Simulasi
# Amalia JIhan Syafiqoh B2A020078
# Anita Silvia Fatmawati B2A20083
# Septiana Putri Milasari B2A020058

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,4)
  colnames(xj)<-c("aZ","Xj","Uj","Distribusi Gamma")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    A<-xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
    lambda<-3
    alpha<-4
    U<-log(runif(A*alpha))
    Um<-matrix(U,n)
    Y<-apply(Um,1,sum)
    xj[,4]<--Y/lambda
  }
  View(xj)
}
multiplicative_RNG(45,21139,417,150)
