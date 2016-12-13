
#****************************************************************
calcres=function(C){
  n=ncol(C)
  #Calcul des réserves PAR ANNEE DE SURVENANCE
  reserves=matrix(data=0,1,6) #INITIALISATION
  reserves[1]=0
  for (i in 2:n)
  {
    reserves[i]=C[i,7]-C[i,7+1-i]
  }
  #CALCUL DE LA RESERVE TOTAL A EFFECTUER
  reservetotale=sum(reserves)
  
  return(reservetotale)
}



#IMPORT DU TABLEAU TRIANGULAIRE SUPERIEUR INITIAL NOMME Y
#Y=read.table("gre.csv",h=1,sep=";",row.names = 1)

#Fonction chain ladder
tpclmat=function(Y){
#INITIALISATION DU NOMBRE DE LIGNE ET DU NOMBRE DE COLONNES DE LA MATRICssE INITIALE
k=0 #VARIABLE DE PARCOURS DANS LA MATRICE
n=ncol(Y);
C=matrix(data=0,n,n) #INITIALISATION
#CACLCUL DES PAIEMENTS CUMULES A PARTIR DE LA MATRICE DES PAIEMENTS DEJA EFFECTUEES Y.
#LA MATRICE DES PAIEMENTS CUMULE EST NOMMEE C
for (i in 1:n)
{
  for (j in 1:(n-k))
  {
    C[i,j]=sum(Y[i,1:j])
  }
  k=k+1
}

############(1) APPLICATION DE LA METHODE CHAIN LADDER

################(a) CALCUL DES FACTEURS DE DEVELOPPEMENT 

lamdachapo=matrix(data=0,1,6) #INITIALISATION DU VECTEUR DES FACTEURS DE DEVELOPPEMENT
for (j in 1:(n-1))
{
  lamdachapo[j]=sum(C[1:(n-j),(j+1)])/sum(C[1:(n-j),j])
}

################(b) CALCUL DES RESERVES POUR TOUTES LES ANNEES DE SURVENANCE
#ESTIMATIONS DES PAIEMENTS CUMULES DE LA PARTIE INFERIEUR DU TABLEAU
a=0 #VALEUR DE PARCOURS
for (i in 2:n)
{
    for (j in (n-a):n)
    {
      C[i,j]=lamdachapo[j-1]*C[i,j-1] 
    }
  a=a+1
}
 return(C)
}