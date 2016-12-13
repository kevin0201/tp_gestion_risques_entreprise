###**********************************************
##********************FONCTIONS CHAINLADDER***********************
##**************************************************************

#*************************Calculs des reserves totales à effectuer par Chain ladder***************************************
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

#Fonction chain ladder calcul de la matrice des paiements cumulés
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


############################
######Nouveau triangle######
############################
INC=matrix(NA,7,7)
INC[1,]=c(30517139,23450472,7974165,5305321,3654797,2133777,300305)
INC[2,]=c(39463137,29117717,7444614,5637025,4791972,3774356,NA)
INC[3,]=c(44272955,35271431,7427330,6657567,5906195,NA,NA)
INC[4,]=c(59715567,40217155,7552557,6491741,NA,NA,NA)
INC[5,]=c(67705747,53723770,9920666,NA,NA,NA,NA)
INC[6,]=c(74362979,60433759,NA,NA,NA,NA,NA)
INC[7,]=c(90472065,NA,NA,NA,NA,NA,NA)
INC 

nc = ncol(INC)
nl = nrow(INC)
C=INC
D = INC
for (i in 1:nc){D[i,] =cumsum(INC[i,])} 


#facteurs de développement

LAMBDA = rep(NA,nc-1)
for(k in 1:(nc-1)){
  LAMBDA[k]=(sum(D[1:(nl-k),k+1])/sum(D[1:(nl-k),k]))}
#calcul des réserves

RESERVES=matrix(0,nc,nc)
TRIANGLE = D
for(i in 1:(nc-1)){
  TRIANGLE[(nl-i+1):(nl),i+1]=LAMBDA[i]*TRIANGLE[(nl-i+1):(nl),i]}
CU = TRIANGLE[,nc]
PM = diag(TRIANGLE[,nc:1])
for  (s in 1:nc){RESERVES[s,] = CU-PM}
RS=rep(0,nc)
for  (s in 1:nc){RS[s]=RESERVES[1,s]}

#réserve totale
Rtot=sum(RS)

#######################
#### CHAPITREIII:GLM ##
#######################

#nc = ncol(INC)
#nl = nrow(INC)
Y = as.vector(INC)

ligne = rep(1:nl, each=nc) 
colonne = rep(1:nc, nl)

col = as.factor(ligne)
lig = as.factor(colonne)
#Regression lineaire entre les paiements effectuer et les année de survenance + de developpement
CL=glm(formula = Y ~ lig + col -1, family = poisson(link="log")) #APPLICATION D4UN GLM POUR L4OBTENTION DES ALPHA ET DES BETA
summary(CL,cor=F)

##Le vecteur ainsi que la matrice des prédictions st donnés par:
#Creation d'une matrice dite de Prediction à partir de la regression lineaire généralisée
Ypred = predict(CL,newdata=data.frame(lig,col),type="response")
MatricePredictionRegression = matrix (data=Ypred,nrow=nc)
#Matrice de prediction obtenue à partir de la régression en paiements et années lignes et colonnes
MatricePredictionRegression

V= (residus=residuals(CL,type="pearson")) #résidus de pearson du glm
R=sqrt((nc*(nc+1)/2)/((nc*(nc+1)/2)-(2*nc-1)))*V #Residus de pearson réajustés

reservetotale=matrix(NA,1,5)
for (w in 1:5) 
{
              #Reechantillonnage non parametrique des residus
              Rechantillon=sample(R,sum(1:7),replace=TRUE) #REECHANTILLONNAGE DES RESIDU DE PEARSON REAJUSTE
              ####Pseudo triangle
              B=rep(NA,nl*nc)
              B[is.na(Y)==FALSE]=Rechantillon 
              ResidusRechantillon=matrix(B,nl,nc)#MATRICE TRIANGULAIRE DES RESIDUS 
              Pseudotriangle=round(ResidusRechantillon*sqrt(MatricePredictionRegression)+MatricePredictionRegression)
              Pseudotriangle=replace(Pseudotriangle,is.na(Pseudotriangle),0)#on remplace les na du triangle par 0
              
              Pseudotriangle #PSEUDO TRIANGLE OBTENU PAR INVERSION DES RESIDUS 
              #******************************************************************
              #******************************************************************
              #On applique la méthode de chain ladder au triangle obtenue
              PseudoMatriceCumule =tpclmat(Pseudotriangle) #CALCUL DE LA RESERVE PAR CHAIN LADDER DU PSEUDO TRIANGLE
              PseudoMatriceCumule
              ReservePseudoTriangle1=calcres(PseudoMatriceCumule)
              ReservePseudoTriangle1
              #Calcul des pseudo Paiement de r+1
              sumPseudotriangle=rowSums(Pseudotriangle)
              #sumPseudotriangle=sumPseudotriangle[7:1]
              sumPseudotriangle
              z=2
              Paiementrplus1=matrix(NA,1,6)
              for (zz in 7:2)
              {
                Paiementrplus1[z-1]=round(PseudoMatriceCumule[z,zz]-sumPseudotriangle[z])
                z=z+1
              }
              Paiementrplus1
              Paiementrplus11=matrix(NA,1,6)
              M=5
              for (i in 1:6) {
                Paiementrplus11[i]=rpois(M,Paiementrplus1[i])[1]
              }
              Paiementrplus11 #PAIMENT PREDIT POUR L'ANNEE 2007
              INC2=INC#NOUVELLE MATRICE DES PAIEMENTS
              z=2
              #Paiementrplus1=matrix(NA,1,6)
              for (zz in 7:2)
              {
                INC2[z,zz]=Paiementrplus11[z-1]
                z=z+1
              }
              INC2
              #CALCUL DES RESERVE DE LA MATRICE A L4ANNEE R+1 PAR CHAIN LADDER
              C=INC2
              D = INC2
              for (i in 1:nc){D[i,] =cumsum(INC2[i,])} 
              D=replace(D,is.na(D),0)
              #facteurs de développement
              LAMBDA = rep(NA,nc-1)
              for(k in 1:(nc-1))
              {
                LAMBDA[k]=(sum(D[1:(7-k+1),k+1])/sum(D[1:(7-k+1),k]))
              }
              LAMBDA
              D
              a=0 #VALEUR DE PARCOURS
              for (i in 3:7)
              {
                for (j in (7-a):7)
                {
                  D[i,j]=LAMBDA[j-1]*D[i,j-1] 
                }
                a=a+1
              }
              D
              #Calcul des réserves PAR ANNEE DE SURVENANCE
              reserves=matrix(data=0,1,5) #INITIALISATION
              reserves[1]=0
              for (i in 3:7)
              {
                reserves[i-2]=D[i,7]-D[i,7+2-i]
              }
              #CALCUL DE LA RESERVE TOTAL A EFFECTUER
              reservetotale[w]=sum(reserves)
}
#*******************************************************************
#*******************************************************************
#CALCUL DU PREMIER T.
T=Rtot-sum(Paiementrplus11)-reservetotale

###########################################################################################
#ON REPREND LE MEME PROCEDE QUE LE PRECEDENT A LA MATRICE AVEC LES PAIEMENT DE 2007 ESTIMER CEUX DE 2008
###########################################################################################
Y = as.vector(INC2)

ligne = rep(1:nl, each=nc) 
colonne = rep(1:nc, nl)

col = as.factor(ligne)
lig = as.factor(colonne)
#Regression lineaire entre les paiements effectuer et les année de survenance + de developpement
CL=glm(formula = Y ~ lig + col -1, family = poisson(link="log")) #APPLICATION D4UN GLM POUR L4OBTENTION DES ALPHA ET DES BETA
summary(CL,cor=F)

##Le vecteur ainsi que la matrice des prédictions st donnés par:
#Creation d'une matrice dite de Prediction à partir de la regression lineaire généralisée
Ypred = predict(CL,newdata=data.frame(lig,col),type="response")
MatricePredictionRegression = matrix (data=Ypred,nrow=nc)
#Matrice de prediction obtenue à partir de la régression en paiements et années lignes et colonnes
MatricePredictionRegression

V= (residus=residuals(CL,type="pearson")) #résidus de pearson du glm
R=sqrt((nc*(nc+1)/2)/((nc*(nc+1)/2)-(2*nc-1)))*V #Residus de pearson réajustés

reservetotale2=matrix(NA,1,5)
for (q in 1:5)
{
      #Reechantillonnage non parametrique des residus
      Rechantillon=sample(R,34,replace=TRUE) #REECHANTILLONNAGE DES RESIDU DE PEARSON REAJUSTE
      ####Pseudo triangle
      B=rep(NA,nl*nc)
      B[is.na(Y)==FALSE]=Rechantillon 
      ResidusRechantillon=matrix(B,nl,nc)#MATRICE TRIANGULAIRE DES RESIDUS 
      Pseudotriangle=round(ResidusRechantillon*sqrt(MatricePredictionRegression)+MatricePredictionRegression)
      Pseudotriangle=replace(Pseudotriangle,is.na(Pseudotriangle),0)#on remplace les na du triangle par 0
      
      C=Pseudotriangle
      D = Pseudotriangle
      for (i in 1:nc){D[i,] =cumsum(INC2[i,])} 
      D=replace(D,is.na(D),0)
      #facteurs de développement
      LAMBDA = rep(NA,nc-1)
      for(k in 1:(nc-1))
      {
        LAMBDA[k]=(sum(D[1:(7-k),k+1])/sum(D[1:(7-k),k]))
      }
      LAMBDA
      a=0 #VALEUR DE PARCOURS
      for (i in 3:7)
      {
        for (j in (7-a):7)
        {
          D[i,j]=LAMBDA[j-1]*D[i,j-1] 
        }
        a=a+1
      }
      #Calcul des pseudo Paiement de r+2
      sumPseudotriangle=rowSums(Pseudotriangle)
      PseudoMatriceCumule=D
      
      z=3
      Paiementrplus2=matrix(NA,1,5)
      for (zz in 7:3)
      {
        Paiementrplus2[z-2]=round(PseudoMatriceCumule[z,zz]-sumPseudotriangle[z])
        z=z+1
      }
      Paiementrplus2
      Paiementrplus22=matrix(NA,1,5)
      M=5
      for (i in 1:5) {
        Paiementrplus22[i]=rpois(M,abs(Paiementrplus2[i]))[1]
      }
      Paiementrplus22 #paiement prevues pour 2008
      
       #Paiementrplus22q[q]=Paiementrplus22
      INC3=INC2#NOUVELLE MATRICE DES PAIEMENTS
      z=3
      #Paiementrplus1=matrix(NA,1,6)
      for (zz in 7:3)
      {
        INC3[z,zz]=Paiementrplus22[z-2]
        z=z+1
      }
      INC3
      
      #CALCUL DES RESERVE DE LA MATRICE A L4ANNEE R+1 PAR CHAIN LADDER
      C=INC3
      D = INC3
      for (i in 1:nc){D[i,] =cumsum(INC3[i,])} 
      D=replace(D,is.na(D),0)
      #facteurs de développement
      LAMBDA = rep(NA,5)
      for(k in 2:(nc-1))
      {
        LAMBDA[k-1]=(sum(D[1:(7-k+1),k+1])/sum(D[1:(7-k+1),k]))
      }
      LAMBDA
      a=0 #VALEUR DE PARCOURS
      for (i in 4:7)
      {
        for (j in (7-a):7)
        {
          D[i,j]=LAMBDA[j-2]*D[i,j-1] 
        }
        a=a+1
      }
      #Calcul des réserves PAR ANNEE DE SURVENANCE
      reserves=matrix(data=0,1,5) #INITIALISATION
      reserves[1]=0
      for (i in 4:7)
      {
        reserves[i-2]=D[i,7]-D[i,7+3-i]
      }
      #CALCUL DE LA RESERVE TOTAL A EFFECTUER
      reservetotale2[q]=sum(reserves)
}
reservetotale #reserve totale 2007
reservetotale2 #¤reserve totale 2008
TT=reservetotale-reservetotale2-sum(Paiementrplus22) #???calcul des pertes pour 2008
TT
