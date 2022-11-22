dadosMineracao1Semestre = read.csv(file.choose())
dadosMineracao2Semestre = read.csv(file.choose())
newDataFrame = data.frame()
aux = data.frame()

for(i in 1:length(dadosMineracao2Semestre$nome)){
  
  for(c in 1:length(dadosMineracao1Semestre$nome)){
    if((dadosMineracao1Semestre[c,"nome"] == dadosMineracao2Semestre[i,"nome"]) && dadosMineracao1Semestre[c,"Status"] == 0){
      
      newDataFrame = rbind(newDataFrame, dadosMineracao2Semestre[i,])

    }
    
  }

}
setwd("C:\\Users\\IFBA\\Desktop\\Concomitante\\2 semestre")
write.csv(newDataFrame, file = "(Concomitante)DadosMineracao2Semestre_Filtrado(05-10-22).csv")


