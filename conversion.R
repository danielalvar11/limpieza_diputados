
library(dplyr)
library(tidyr) 
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(plotly)
library(plotrix)

diputados<-read.csv2("diputados.txt",encoding = "latin1")
academico<-read.csv2("nivel_academico.txt",encoding = "latin1")

diputados<-mutate(diputados, NIVEL.ACADEMICO=academico$N.ACADEMICO)


####################################################################################
#   Realizaremos una limpieza de datos, para adecuar la tabla a nuestros intereses##
#   a fin de facilitar la legibilidad de codigo y de calculos estadistico         ##
####################################################################################

diputados<-select(diputados, NOMBRE:CARGO  )#Elimino las columnas espureas

################################################
###LIMPIEZA DE G.PARLAMENTARIO####################
##################################################
#renombro los nombres de los partidos
diputados<-mutate(diputados, G.PARLAMENTARIO=ifelse(G.PARLAMENTARIO ==  "Grupo Popular" ,"PP",ifelse(G.PARLAMENTARIO=="Grupo Ciudadanos","Ciudadanos",ifelse(G.PARLAMENTARIO=="Grupo Confederal de Unidos Podemos-En Comú Podem-En Marea (GCUP-EC-EM)","Podemos",ifelse(G.PARLAMENTARIO=="Grupo Esquerra Republicana","Esquerra Republicana", ifelse(G.PARLAMENTARIO=="Grupo Mixto Bildu","Bildu",ifelse(G.PARLAMENTARIO=="Grupo Mixto Foro" , "Foro(G.Mixto)",ifelse(G.PARLAMENTARIO=="Grupo Mixto C-P-EUPV" , "C-P-EUPV",ifelse(G.PARLAMENTARIO=="Grupo Mixto Coalición Canaria" , "Coalicion Canaria",ifelse(G.PARLAMENTARIO=="Grupo Mixto Nueva Canarias" , "Nueva Canarias",ifelse(G.PARLAMENTARIO=="Grupo Mixto PdeCat" , "PdeCat",ifelse(G.PARLAMENTARIO=="Grupo Mixto UPN-PP","UPN-PP",ifelse(G.PARLAMENTARIO=="Grupo Socialesta", "PSOE",ifelse(G.PARLAMENTARIO=="Grupo Vasco EAJ-PNV" , "EAJ-PNV",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ifelse( G.PARLAMENTARIO=="Grupo Popuar","PP",ifelse(G.PARLAMENTARIO=="Grupo Popular ","PP",ifelse(G.PARLAMENTARIO=="Grupo CIudadanos","Ciudadanos",ifelse(G.PARLAMENTARIO=="Grupo Confederal de Unidos Podemos-En Comú Podem-En Marea (GCUP-EC-EM) ","Podemos",ifelse(G.PARLAMENTARIO=="Grupo Confederal de Unidos Podemos-En Comú Podem-En Marea (GCUP-EC-EM)  ","Podemos",ifelse(G.PARLAMENTARIO=="Grupo Mixto EH Bildu","Bildu",ifelse(G.PARLAMENTARIO=="Grupo Mixto PdeCat ","PdeCat",ifelse(G.PARLAMENTARIO=="Grupo Socialista","PSOE",ifelse(G.PARLAMENTARIO=="Grupo Socialista","PSOE",ifelse(G.PARLAMENTARIO=="Grupo Vasco EAJ-PNV ","EAJ-PNV",ifelse(G.PARLAMENTARIO=="Grupo Vasco EZJ-PNV ","EAJ-PNV","NO0000000000000000000000000000"))) ))))))))))))))))))))))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

diputados$G.PARLAMENTARIO[65]="PP"#diputados concretos puestos a mano
diputados$G.PARLAMENTARIO[260]="PSOE"
diputados$G.PARLAMENTARIO[312]="EAJ-PNV"
diputados$G.PARLAMENTARIO<-factor(diputados$G.PARLAMENTARIO)#el renombramiento no esta en un valor factor, sino caracter, poreso convertimos la variable a factor de nuevo

#############################################
###########LIMPIEZA DE SEXO##################
#############################################

diputados<-mutate(diputados,SEXO =ifelse(SEXO=="Muer","Mujer",ifelse(SEXO=="Mujer","Mujer",ifelse(SEXO=="Mujer ","Mujer",ifelse(SEXO ==" Hombre","Hombre",ifelse(SEXO=="Hombre","Hombre",ifelse(SEXO=="Hombre ","Hombre","Nunca se deja de morir")))))))
diputados$SEXO<-factor(diputados$SEXO)

Lugar<-c("Congreso","Congreso","España","España")#Dataframe para comparar el sexo de la poblacion española con el sexo del congreso
Sexo<-c("Hombres","Mujeres","Hombres","Mujeres")
Porcentaje<-c(60.57,39.43,49.03,50.97)
df<-data.frame(Lugar,Sexo,Porcentaje)



######################################
#####Limpieza de Estado Civil#########
######################################
diputados<-mutate(diputados, ECIVIL=ifelse(ECIVIL=="Casada","Casado/a",ifelse(ECIVIL=="Casado","Casado/a",ifelse(ECIVIL=="Casado ","Casado/a",ifelse(ECIVIL=="Divorciada","Divorciado/a",ifelse(ECIVIL=="Divorciado","Divorciado/a",ifelse(ECIVIL=="Pareja de hecho ","Pareja de hecho",ifelse(ECIVIL=="Separada","Divorciado/a",ifelse(ECIVIL=="Separada legalmente","Divorciado/a",ifelse(ECIVIL=="Separado","Divorciado/a",ifelse(ECIVIL=="Soltera","Soltero/a",ifelse(ECIVIL=="Soltero","Soltero/a",ifelse(ECIVIL=="Viuda","Viudo/a",ifelse(ECIVIL=="Viudo","Viudo/a","No Consta") ))))) ))))))))
diputados$ECIVIL<-factor(diputados$ECIVIL)



#####################################
#####Limpieza de Edad################
#####################################

for(i in 1:350) { 

  if(diputados$AÑO.DE.NACIMIENTO[i]<=1999&&diputados$AÑO.DE.NACIMIENTO[i]>=1995){diputados$RANGO.EDAD[i]="20-24"
  }else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1994&&diputados$AÑO.DE.NACIMIENTO[i]>=1990){diputados$RANGO.EDAD[i]="25-29"}
    else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1989&&diputados$AÑO.DE.NACIMIENTO[i]>=1985){diputados$RANGO.EDAD[i]="30-34"}
      else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1984&&diputados$AÑO.DE.NACIMIENTO[i]>=1980){diputados$RANGO.EDAD[i]="35-39"}
        else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1979&&diputados$AÑO.DE.NACIMIENTO[i]>=1975){diputados$RANGO.EDAD[i]="40-44"}
          else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1974&&diputados$AÑO.DE.NACIMIENTO[i]>=1970){diputados$RANGO.EDAD[i]="45-49"}
            else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1969&&diputados$AÑO.DE.NACIMIENTO[i]>=1965){diputados$RANGO.EDAD[i]="50-54"}
              else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1964&&diputados$AÑO.DE.NACIMIENTO[i]>=1960){diputados$RANGO.EDAD[i]="55-59"}
                else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1959&&diputados$AÑO.DE.NACIMIENTO[i]>=1955){diputados$RANGO.EDAD[i]="60-64"}
                  else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1954&&diputados$AÑO.DE.NACIMIENTO[i]>=1950){diputados$RANGO.EDAD[i]="65-69"}
                    else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1949&&diputados$AÑO.DE.NACIMIENTO[i]>=1945){diputados$RANGO.EDAD[i]="70-74"}
                      else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1944&&diputados$AÑO.DE.NACIMIENTO[i]>=1940){diputados$RANGO.EDAD[i]="75-79"}
                        else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1939&&diputados$AÑO.DE.NACIMIENTO[i]>=1935){diputados$RANGO.EDAD[i]="80-84"}
                          else{if(diputados$AÑO.DE.NACIMIENTO[i]<=1934&&diputados$AÑO.DE.NACIMIENTO[i]>=1930){diputados$RANGO.EDAD[i]="85-89"}
                            else{diputados$RANGO.EDAD[i]="No"
                            
                            }
                            
                          }
                          
                        }
                        
                      }
                      
                    }
                    
                  }
                  
                }
                
              }
              
            }
            
          }
          
        }
        
      }
      
    }
  }
}
#convertimos la variable RANGO.EDAD a factor
diputados$RANGO.EDAD<-factor(diputados$RANGO.EDAD)


##############################################
####Limpieza de Origen#######################
############################################
for(j in 1:350) { 
  if (diputados$ORIGEN[j]=="Las Palmas ") {diputados$ORIGEN[j]="Las Palmas"
  }else{
    if (diputados$ORIGEN[j]=="Madrid ") {diputados$ORIGEN[j]="Madrid"
    }else{
      if (diputados$ORIGEN[j]=="Santa Cruz de Tenerife ") {diputados$ORIGEN[j]="Santa Cruz de Tenerife"
      }else{
        if (diputados$ORIGEN[j]== "Sevilla ") {diputados$ORIGEN[j]= "Sevilla"
        }else{
          if (diputados$ORIGEN[j]== "Toledo " ) {diputados$ORIGEN[j]= "Toledo" 
          }
          
          
        }
      }
    }
    
  }
  
}
#volvemos a convertir ORIGEN en factor
diputados$ORIGEN<-factor(diputados$ORIGEN)
levels(diputados$ORIGEN)
#####Porcentaje de sexo del total del congreso##
test<-CrossTable(diputados$SEXO,diputados$G.PARLAMENTARIO,prop.chisq=FALSE)

####################################
##LIMPIEZA NIVEL ACADEMICO##########
####################################

diputados<-mutate(diputados,NIVEL.ACADEMICO =ifelse(NIVEL.ACADEMICO=="2ª Etapa de Educación Secundaria con orientación profesional (educación postsecundaria no superior)","2ª Etapa E.Secundaria con O.Profesional",ifelse(NIVEL.ACADEMICO=="Cursando estudios","Cursando estudios",ifelse(NIVEL.ACADEMICO=="Educación Superior","Educación Superior",ifelse(NIVEL.ACADEMICO=="No consta","No consta","ERROR")))))
diputados$NIVEL.ACADEMICO<-factor(diputados$NIVEL.ACADEMICO)

CrossTable(diputados$NIVEL.ACADEMICO,diputados$G.PARLAMENTARIO,prop.chisq=FALSE)



#############################
####GRAFICAS NIVEL ACADEMICO#
#############################
ggplot(data=diputados, aes(x=NIVEL.ACADEMICO,fill=G.PARLAMENTARIO,y=100*..count../350   )) + 
  geom_bar(stat="count")+ylab("Porcentaje")+xlab("Nivel Academico") +scale_fill_manual(values=c("green", "darkred","orange","yellow","darkgreen","#b27503","white","grey","blue","purple","#33d7ff","red","darkblue"))


ggplot(data=diputados, aes(x=G.PARLAMENTARIO,y=100*..count../350 ,fill=NIVEL.ACADEMICO)) + 
  geom_bar(position="dodge")+ylab("Porcentaje")+xlab("Partido Politico")


ggplot(data=diputados, aes(x=NIVEL.ACADEMICO,y=100*..count../350   )) + 
  geom_bar(stat="count")+ylab("Porcentaje")+xlab("Nivel Academico") 


############################3
##Generamos la tabla limpia#
#############################
test<-CrossTable(diputados$SEXO,diputados$G.PARLAMENTARIO,prop.chisq=FALSE)
write.csv2(test,file="test.txt",row.names = FALSE)

write.csv2(diputados,file="diputados_final.txt",row.names = FALSE)

