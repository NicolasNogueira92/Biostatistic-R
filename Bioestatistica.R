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
  crabs=tibble(crabs)
  print(crabs)
  
#2. Calcule, para cada espécie, a média, desvio padrão 
#   e o coeficiente de variação da variável
#   tamanho do lobo e tamanho da carapaça.

###Especie B###
  #Lobo#

    BMedLob=
      mean(crabs$FL[crabs$sp=="B"]) 
    BMedLob #14.056
    
    BDPLob=
      sd(crabs$FL[crabs$sp=="B"]) 
    BDPLob #3.01961
    
    BCVLob=
      (BMedLob/BDPLob)*100
    BCVLob #21.48271
    
  #Carapaca#
   
    BMedCar=
      mean(crabs$CL[crabs$sp=="B"]) 
    BMedCar #30.058
    
    BDPCar=
      sd(crabs$CL[crabs$sp=="B"]) 
    BDPCar #6.902703
    
    BCVCar=
      (BDPCar/BMedCar)*100 
    BCVCar #22.96461
    
###Especie O###
  #Lobo#
    OMedLob=
      mean(crabs$FL[crabs$sp=="O"])
    OMedLob #17.11
    
    ODPLobo=
      sd(crabs$FL[crabs$sp=="O"])
    ODPLobo #3.275575
    
    OCVLobo=
      (ODPLobo/OMedLob)*100 
    OCVLobo #19.14422

  #Carapaca#
    OMedCar=
      mean(crabs$CL[crabs$sp=="O"]) 
    OMedCar #34.153
    
    ODPCar=
      sd(crabs$CL[crabs$sp=="O"]) 
    ODPCar #6.764262
    
    OCVCar=
    (ODPCar/OMedCar)*100 
    OCVCar #19.80576
    
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
      stat_summary(fun=mean, geom="point", shape=4, size=4)+
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
      stat_summary(fun=mean, geom="point", shape=4, size=4)+
      geom_point()
  )

#Gráfico 4, Espécie O, Carapaça MxF#

  ggplotly(
    crabs%>%
    filter(sp=="O")%>%
      ggplot(aes(y=CL,x=sex,colour=sex))+
      geom_boxplot()+
      labs(y="Carapaça", x="Sexo")+
      stat_summary(fun=mean, geom="point", shape=4, size=4)+
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
  
  ModLinB=
  lm(crabs$CL[crabs$sp=="B"]~crabs$FL[crabs$sp=="B"])
  #CL=2.275*FL - 1.916 
  CrabsModLinB = function(x){2.275*x-1.916}
  
#Especie O#
    
  ModLinO=
  lm(crabs$CL[crabs$sp=="O"]~crabs$FL[crabs$sp=="O"])
  #CL= 2.0413*FL - 0.7731
  CrabsModLinO = function(x){2.0413*x-0.7731}
  
#8. Para cada espécie determine o coeficiente de determinação do modelo linear ajustado no
#   item 6. 

  #Espécie B#
  
  SSE_B = sum((predict(ModLinB) - crabs$CL[crabs$sp=="B"])^2)
  SSR_B = sum((predict(ModLinB) - mean(crabs$CL[crabs$sp=="B"]))^2)
  SST_B = SSR_B + SSE_B
  SSR_B / SST_B #0.9902459 >> R^2
  summary(ModLinB)[[8]] #R^2 é 0.9902459
    
  #Especie O#
  
  SSE_O = sum((predict(ModLinO) - crabs$CL[crabs$sp=="O"])^2)
  SSR_O = sum((predict(ModLinO) - mean(crabs$CL[crabs$sp=="O"]))^2)
  SST_O = SSR_O + SSE_O
  SSR_O / SST_O #0.9770911 >> R^2
  summary(ModLinO)[[8]] #R^2 é 0.9770911
  
    
####CrabsExtra####
#Alternativa aos Boxplot
    
    #lobo B
    
    ggplotly(
      crabs%>%
        filter(sp=="B")%>%
        select(sex,FL)%>%
        ggdensity(x = "FL",
                  add = "mean", rug = TRUE,
                  color = "sex", fill = "sex",
                  palette = c("magenta", "gold"))
    )
    
    #carapaca B
    
    ggplotly(
      crabs%>%
        filter(sp=="B")%>%
        select( sex,CL)%>%
        ggdensity(x = "CL",
                  add = "mean", rug = TRUE,
                  color = "sex", fill = "sex",
                  palette = c("slateblue1", "forestgreen"))
    )
    
    #Lobo sp O
    
    ggplotly(
      crabs%>%
        filter(sp=="O")%>%
        select(sex,FL)%>%
        ggdensity(x = "FL",
                  add = "mean", rug = TRUE,
                  color = "sex", fill = "sex",
                  palette = c("magenta", "gold"))
    )
    
    #carapaca O
    
    ggplotly(
      crabs%>%
        filter(sp=="O")%>%
        select( sex,CL)%>%
        ggdensity(x = "CL",
                  add = "mean", rug = TRUE,
                  color = "sex", fill = "sex",
                  palette = c("hotpink", "cyan"))
    )
    
####Iris####

  # Conjunto de dados flores
  
  #1. Importe o conjunto de dados para o R
  
  data(iris)
  is.data.frame(iris)
  iris=tibble(iris)
  print(iris)
  
  #2. Calcule, para cada espécie, a média, desvio padrão e o coeficiente de variação da variável
  #   Length e Width da pétalas.
  
  #EspSetosa#
  #Comprimento(L)# Length
  
  SetMedComp= 
    mean(iris$Petal.Length[iris$Species=="setosa"])
  SetMedComp #1.462

  SetDesPadComp= 
    sd(iris$Petal.Length[iris$Species=="setosa"])
  SetDesPadComp #0.173664
  
  SetCoefVarComp=
    (SetDesPadComp/SetMedComp)*100
  SetCoefVarComp #11.87852
  
  #Largura(W)# Width
  
  SetMedLarg= 
    mean(iris$Petal.Width[iris$Species=="setosa"])
  SetMedLarg #0.246
  
  SetDesPadLarg= 
    sd(iris$Petal.Width[iris$Species=="setosa"])
  SetDesPadLarg #0.1053856
  
  SetCoefVarLarg= 
    (SetDesPadLarg/SetMedLarg)*100
  SerCoefVarlarg #42.83967
  
  #EspVersicolor#
  #Comprimento(L)# Length
  
  VerMedComp=
    mean(iris$Petal.Length[iris$Species=="versicolor"])
  VerMedComp #4.26
  
  VerDesPadComp= 
    sd(iris$Petal.Length[iris$Species=="versicolor"])
  VerDesPadComp #0.469911
  
  VerCoefVarComp=
    (VerDesPadComp/VerMedComp)*100
  VerCoefVarComp #11.03077
  
  #Largura(W)# Width
  
  VerMedLarg= 
    mean(iris$Petal.Width[iris$Species=="versicolor"])
  VerMedLarg #1.326
  
  VerDesPadLarg=
    sd(iris$Petal.Width[iris$Species=="versicolor"])
  VerDesPadLarg #0.1977527
  
  VerCoefVarLarg= 
    (VerDesPadLarg/VerMedLarg)*100
  VerCoefVarLarg #14.91348
  
  #EspVirginica#
  #comprimento(L)# Length
  
  VirMedComp= 
    mean(iris$Petal.Length[iris$Species=="virginica"])
  VirMedComp #5.552
  
  VirDesPadComp= 
    sd(iris$Petal.Length[iris$Species=="virginica"])
  VirDesPadComp #0.5518947
  
  VirCoefVarComp= 
    (VirDesPadComp/VirMedComp)*100
  VirCoefVarComp #9.940466
  
  #Largura(W)# Width
  VirMedLarg= 
    mean(iris$Petal.Width[iris$Species=="virginica"])
  VirMedLarg #2.026
  
  VirDesPadLarg= 
    sd(iris$Petal.Width[iris$Species=="virginica"])
  VirMedLarg #0.2746501
  
  VirCoefVarLarg= 
    (VirDesPadLarg/VirMedLarg)*100
  VirCoefVarLarg #13.55627
  
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
      ggplot(aes(y=Petal.Width,x=Petal.Length)) +
      geom_point(colour = "black",alpha=0.3) +
      geom_smooth(method = "lm", col="Red") +
      labs(
        title="Espécie Versicolor",
        subtitle= "largura(W) x comprimento(L)",
        y= "Largura(W)",
        x= "Comprimento(L)")+
      geom_rug(col="black",alpha=0.1, size=1.5)
  )
  
  #Versicolor#
  
  ggplotly( 
    iris%>%
     filter(Species=="versicolor")%>%
      ggplot(aes(y=Petal.Width,x=Petal.Length)) +
      geom_point(colour = "blue", alpha=0.4) +
      geom_smooth(method = "lm", col="Red") +
      labs(
            title="Espécie Versicolor",
            subtitle= "largura(W) x comprimento(L)",
            y= "Largura(W)",
            x= "Comprimento(L)")+
      geom_rug(col="blue",alpha=0.1, size=1.5)
  )
  
  #Virginica#
  
  ggplotly( 
    iris%>%
      filter(Species=="virginica")%>%
      ggplot(aes(y=Petal.Width,x=Petal.Length)) +
      geom_point(colour = "darkmagenta",alpha=0.7) +
      geom_smooth(method = "lm", col="Red") +
      labs(
          title="Espécie Virginica",
          subtitle= "largura(W) x comprimento(L)",
          y= "Largura(W)",
          x= "Comprimento(L)")+
      geom_rug(col="darkmagenta",alpha=0.1, size=1.5)
  )

  #7. Para cada espécie defina a equação da reta, baseados no método dos mínimos quadrados,
  #do gráfico de dispersão do item 6.
  
  #Setosa#
  ModLinSet=
  lm(iris$Petal.Width[iris$Species=="setosa"]~
     iris$Petal.Length[iris$Species=="setosa"])
  #y= 0.20125x 0.04822
  IrisModLinSetosa = function(x){0.2012*x-0.04822}
  
  #Versicolor
  ModLinVer=
  lm(iris$Petal.Width[iris$Species=="versicolor"]~
     iris$Petal.Length[iris$Species=="versicolor"])
  #y= 0.33105x-0.08429
  IrisModLinVersicolor = function(x){0.33105*x-0.08429}
  
  #Virginca#
  ModLinVir=
  lm(iris$Petal.Width[iris$Species=="virginica"]~
     iris$Petal.Length [iris$Species=="virginica"])
  #y= 0.1603x + 1.1360
  IrisModLinVirginica = function(x){0.1673*x+1.1360}
  
  #8. Para cada espécie determine o coeficiente de determinação do modelo linear ajustado no
  #item 6.
  
  #Setosa
  SSE_Set = sum((predict(ModLinSet) - iris$Petal.Width[iris$Species=="setosa"])^2)
  SSR_Set = sum((predict(ModLinSet) - mean(iris$Petal.Width[iris$Species=="setosa"]))^2)
  SST_Set = SSR_Set + SSE_Set
  SSR_Set / SST_Set #0.1099785 >> R^2
  summary(ModLinSet)[[8]] #0.11 >> R^2
  
  #Versicolor#
  SSE_Ver = sum((predict(ModLinVer) - iris$Petal.Width[iris$Species=="versicolor"])^2)
  SSR_Ver = sum((predict(ModLinVer) - mean(iris$Petal.Width[iris$Species=="versicolor"]))^2)
  SST_Ver = SSR_Ver + SSE_Ver
  SSR_Ver / SST_Ver #0.6188467 >> R^2
  summary(ModLinVer)[[8]] #0.6188467 >> R^2
  
  #Virginica
  SSE_Vir = sum((predict(ModLinVir) - iris$Petal.Width[iris$Species=="virginica"])^2)
  SSR_Vir = sum((predict(ModLinVir) - mean(iris$Petal.Width[iris$Species=="virginica"]))^2)
  SST_Vir = SSR_Vir + SSE_Vir
  SSR_Vir / SST_Vir #0.1037537 >> R^2
  summary(ModLinVir)[[8]] #0.1037537 >> R^2
  
