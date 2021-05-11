####Biblioteca####
  library(ggplot2)
  library(tidyverse)
  library(MASS)
  library(ggpubr)
  library(plotly)
####Crabs####

#1. Importe o conjunto de dados para o R

  data(crabs)
  is.data.frame(crabs)
  crabs
    
#2. Calcule, para cada espécie, a média, desvio padrão 
#   e o coeficiente de variação da variável
#   tamanho do lobo e tamanho da carapaça.

###Especie B###
  #Lobo#

  BMedLob=
    mean(crabs$FL[crabs$sp=="B"]) #14.056
  BDPLob=
    sd(crabs$FL[crabs$sp=="B"]) #3.01961
  BCVLob=
    (BMedLob/BDPLob)*100 #21.48271
  
  #Carapaca#
    BMedCar=
      mean(crabs$CL[crabs$sp=="B"]) #30.058
    BDPCar=
      sd(crabs$CL[crabs$sp=="B"]) #6.902703
    BCVCar=
      (BDPCar/BMedCar)*100 #22.96461

###Especie O###
  #Lobo#
    OMedLob=
      mean(crabs$FL[crabs$sp=="O"]) #17.11
    ODPLobo=
      sd(crabs$FL[crabs$sp=="O"]) #3.275575
    OCVLobo=
      (ODPLobo/OMedLob)*100 #19.14422

  #Carapaca#
    OMedCar=
      mean(crabs$CL[crabs$sp=="O"]) #34.153
    ODPCar=
      sd(crabs$CL[crabs$sp=="O"]) #6.764262
    OCVCar=
    (ODPCar/OMedCar)*100 #19.80576

#3. Compare estes valores. Para cada espécie qual a variável mais heterogênea, o lobo ou a
#   carapaça? Justifique.
#   >> Para a especie B, a carapaca e mais heterogenea
#      Ja para a especie O, o lobo e mais heterogeneo
#      Basta olhar para o coeficiente de variacao de cada um, que por definição e o quanto uma variável e heterogênea
#      Na especie B, o lobo tem 21.48271% de variacao, ja a carapaca 22.96461%. Carapaca possui um valor maior. 
#      Na especie O, o lobo tem 19.14422% de variacao, ja a carapaca 19.80576%. Carapaca possui um valor maior. 

#4. Construa, para cada espécie, um gráfico boxplot comparando o tamanho do lobo frontal
#   entre machos e fêmeas.


#Gráficos#

#Grafico 1, BoxPlot, Espécie B, Lobo Frontal MxF#
    
  ggplotly(
    crabs%>%
    filter(sp=="B")%>%
    ggplot(aes(y=FL,x=sex,colour=sex))+
    geom_boxplot()+
    stat_summary(fun=mean, geom="point", shape=4, size=4)+
    geom_point()
  ) 

#Gráfico 2, Espécie O, Lobo Frontal MxF#
    
  ggplotly(
    crabs%>%
    filter(sp=="O")%>%
      ggplot(aes(y=FL,x=sex,colour=sex))+
      geom_boxplot()+
      labs(y="Lobo Frontal", x="Sexo")+
      geom_point()
  )

#5. Construa, para cada espécie, um gráfico boxplot comparando o tamanho do carapaca
#   entre machos e fêmeas.

#Gráfico 3, Espécie B, Carapaça MxF#

  ggplotly(
    crabs%>%
    filter(sp=="B")%>%
      ggplot(aes(y=CL,x=sex,colour=sex))+
      geom_boxplot()+
      labs(y="Carapaça",x="Sexo")+
      geom_point()
  )

#Gráfico 4, Espécie O, Carapaça MxF#

  ggplotly(
    crabs%>%
    filter(sp=="O")%>%
      ggplot(aes(y=CL,x=sex,colour=sex))+
      geom_boxplot()+
      labs(y="Carapaça", x="Sexo")+
      geom_point()
  )

#6. Para cada espécie plote um gráfico de dispersão tamanho da carapaça x tamanho do lobo
#   frontal.

#Criar o Grafico de dispersão Especie B#
 
  ggplotly(
    crabs %>%
    filter(sp=="B")%>%
      ggplot(aes(x=FL,y=CL))+
      geom_point()+
      labs(
        title="Grafico 5",
        subtitle= "Espécie B Lobo x Carapaca",
        x= "Lobo",
        y= "Carapaca"
        )+
      geom_point(colour = "Blue") +
      geom_smooth(method = "lm", col="Red")
  )
  
#Criar o Grafico de dispersão Especie O#
 
  ggplotly(
    crabs %>%
      filter(sp=="O")%>%
        ggplot(aes(x=FL,y=CL))+
        geom_point()+
        labs(
          title="Grafico 5",
          subtitle= "Espécie O Lobo x Carapaca",
          x= "Lobo",
          y= "Carapaca"
        )+
        geom_point(colour = "Purple") +
        geom_smooth(method = "lm", col="Brown")
  ) 
  
#7. Para cada espécie defina a equação da reta, baseados no método dos mínimos quadrados,
#   do gráfico de dispersão do item 6.

#Especie B#
  
  Modelo_Linear_B=
    lm(crabs$CL[crabs$sp=="B"]~crabs$FL[crabs$sp=="B"])
  #CL=2.275*FL - 1.916 

#Especie O#
    
  Modelo_Linear_O=
    lm(crabs$CL[crabs$sp=="O"]~crabs$FL[crabs$sp=="O"])
    #CL= 2.0413*FL - 0.7731

#8. Para cada espécie determine o coeficiente de determinação do modelo linear ajustado no
#   item 6. 

    #Especie B# 
    (cor(crabs$FL[crabs$sp=="B"],
         crabs$CL[crabs$sp=="B"]))^2 #0.9902459
    
    summary(Modelo_Linear_B)
    #R^2 é 0.9902
    
    #Especie O#
    (cor(crabs$FL[crabs$sp=="O"],
         crabs$CL[crabs$sp=="O"]))^2 #0.9770911
    
    summary(Modelo_Linear_O)
    #R^2 é 0.9771
    
####CrabsExtra####
#Existe uma curva maior ou menor de densidade do tamanho do 
#lobo e carapaça em masculino x feminino?

    #lobo
    
ggplotly(
crabs%>%
select( sex,FL)%>%
ggdensity(x = "FL",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("magenta", "gold"))
)
    #carapaca

ggplotly(
crabs%>%
  select( sex,CL)%>%
  ggdensity(x = "CL",
            add = "mean", rug = TRUE,
            color = "sex", fill = "sex",
            palette = c("slateblue1", "forestgreen"))
)


  #
####Iris####

  # Conjunto de dados flores
  
  #1. Importe o conjunto de dados para o R
  
  data(iris)
  iris
  is.data.frame(iris)
  
  #2. Calcule, para cada espécie, a média, desvio padrão e o coeficiente de variação da variável
  #   Length e Width da pétalas.
  
  #EspSetosa#
  #comprimento(L)# Length
  SetMedComp=
    mean(iris$Petal.Length[iris$Species=="setosa"])
  SetDesPadComp=
    sd(iris$Petal.Length[iris$Species=="setosa"])
  SetCoefVarComp=
    (SetDesPadComp/SetMedComp)*100
  
  #largura(W)# Width
  SetMedLarg= #0.246
    mean(iris$Petal.Width[iris$Species=="setosa"])
  SetDesPadLarg= #0.1053856
    sd(iris$Petal.Width[iris$Species=="setosa"])
  SetCoefVarLarg= #42.83967
    (SetDesPadLarg/SetMedLarg)*100
  
  #EspVersicolor#
  #comprimento(L)# Length
  VerMedComp= #4.26
    mean(iris$Petal.Length[iris$Species=="versicolor"])
  VerDesPadComp= #0.469911
    sd(iris$Petal.Length[iris$Species=="versicolor"])
  VerCoefVarComp= #11.03077
    (VerDesPadComp/VerMedComp)*100
  
  #largura(W)# Width
  VerMedLarg= #1.326
    mean(iris$Petal.Width[iris$Species=="versicolor"])
  VerDesPadLarg= #0.1977527
    sd(iris$Petal.Width[iris$Species=="versicolor"])
  VerCoefVarLarg= #14.91348
    (VerDesPadLarg/VerMedLarg)*100
  
  #EspVirginica#
  #comprimento(L)# Length
  VirMedComp= #5.552
    mean(iris$Petal.Length[iris$Species=="virginica"])
  VirDesPadComp= #0.5518947
    sd(iris$Petal.Length[iris$Species=="virginica"])
  VirCoefVarComp= #9.940466
    (VirDesPadComp/VirMedComp)*100
  
  #largura(W)# Width
  VirMedLarg= #2.026
    mean(iris$Petal.Width[iris$Species=="virginica"])
  VirDesPadLarg= #0.2746501
    sd(iris$Petal.Width[iris$Species=="virginica"])
  VirCoefVarLarg= #13.55627
    (VirDesPadLarg/VirMedLarg)*100
  
  #3. Compare estes valores. Para cada espécie qual a variável mais heterogênea, o comprimento(L) ou
  #   largura(W)? Justifique.
  
  #Setosa#
  #   A variável mais heterogênea é a largura(W) da pétala,"Petal.Width", 
  #   pois o coeficiente de variação é 42,83%, enquanto o a comprimento(L) da pétala é 11,87%
  
  #Versicolor#
  #   A variável mais heterogênea é a largura(W) da pétala,"Petal.Width", 
  #   pois o coeficiente de variação é 14,91%, enquanto o a comprimento(L) da pétala é 11,03%
  
  #Virginca#
  #   A variável mais heterogênea é a largura(W) da pétala,"Petal.Width"
  #   pois o coeficiente de variação é 13,55%, enquanto o a comprimento(L) da pétala é 9,94%
  
  #4. Construa um gráfico boxplot comparando, entre as espécies, o comprimento(L) das pétalas.
  
  #Commprimento(L) das Pétalas#
 
    iris%>%
      select(Petal.Length, Species)%>%
        ggplot(aes(y=Petal.Length, x=Species ,colour=Species))+
        geom_boxplot(alpha=0.5)+
        geom_dotplot(binaxis = "y", binwidth = 0.08,
                     stackdir= "center")+
        labs(y="Largura", x="Especies")
  
  #5. Construa um gráfico boxplot comparando, entre as espécies, a largura(W) das pétalas.
  
  #Largura(W) das Pétalas#
   
    iris%>%
       select(Petal.Width, Species)%>%
         ggplot(aes(y=Petal.Width, x=Species ,colour=Species))+
         geom_boxplot(alpha=0.5)+
         geom_dotplot(binaxis = "y", binwidth = 0.03,
                      stackdir= "center")+
         labs(y="Largura", x="Especies")
     
  #6. Para cada espécie plote um gráfico de dispersão comprimento(L) x largura(W).
  
  #Setosa#
    
  ggplotly(
   iris%>%
    filter(Species=="setosa")%>%
      ggplot(aes(x=Petal.Width,y=Petal.Length)) +
      stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
      geom_point(colour = "white") +
      geom_smooth(method = "lm", col="Red") +
      labs(
        title="Espécie Setosa",
        subtitle= "largura(W) x comprimento(L)",
        x= "Largura(W)",
        y= "Comprimento (L)")
  ) 
  
  #Versicolor#
  
  ggplotly( 
    iris%>%
     filter(Species=="versicolor")%>%
      ggplot(aes(x=Petal.Width,y=Petal.Length)) +
      geom_point(colour = "Black") +
      geom_smooth(method = "lm", col="Red") +
      labs(
        title="Espécie Versicolor",
        subtitle= "largura(W) x comprimento(L)",
        x= "Largura(W)",
        y= "Comprimento(L)")
  )
  
  #Virginica#
  
  ggplotly( 
    iris%>%
      filter(Species=="virginica")%>%
        ggplot(aes(x=Petal.Width,y=Petal.Length)) +
        geom_point(colour = "Black") +
        geom_smooth(method = "lm", col="Red") +
        labs(
          title="Espécie Virginica",
          subtitle= "largura(W) x comprimento(L)",
          x= "Largura(W)",
          y= "Comprimento(L)")
  )
  
  #7. Para cada espécie defina a equação da reta, baseados no método dos mínimos quadrados,
  #do gráfico de dispersão do item 6.
  
  #Setosa#
  SetModLin=iris$Petal.Width[iris$Species=="setosa"]~
            iris$Petal.Length[iris$Species=="setosa"]
  lm(SetModLin)
  #y= 0.20125x 0.04822
  
  #Versicolor
  VerModLin=iris$Petal.Width[iris$Species=="versicolor"]~
            iris$Petal.Length[iris$Species=="versicolor"]
  lm(VerModLin)
  #y= 0.33105x  0.08429
  
  #Virginca#
  VirModLin=iris$Petal.Length[iris$Species=="virginica"]~
            iris$Petal.Width[iris$Species=="virginica"]
  lm(VirModLin)
  #y= 0.6473x + 4.2407
  
  #8. Para cada espécie determine o coeficiente de determinação do modelo linear ajustado no
  #item 6.
  #Setosa
  cor(iris$Petal.Width[iris$Species=="setosa"],
      iris$Petal.Length[iris$Species=="setosa"])
  0.33163**2 #R2 >> 0.1099785
  
  #Versicolor#
  cor(iris$Petal.Width[iris$Species=="versicolor"],
      iris$Petal.Length[iris$Species=="versicolor"])
  0.7866681**2 #R2 >> 0.6188467
  
  #Virginica
  cor(iris$Petal.Width[iris$Species=="virginica"],
      iris$Petal.Length[iris$Species=="virginica"])
  0.3221082**2 #R2 >> 0.1037537