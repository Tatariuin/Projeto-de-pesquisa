dados1Semestre = read.csv(file.choose())
dados2Semestre = read.csv(file.choose())
dados1Semestre$Status = NA


dados1 = alunosLog
dados2 = alunosLog
dados3 = alunosLog
dados4 = alunosLog


Semestre3 = c(dados1,dados2,dados3,dados4)

summary(Semestre3)
Semestre3
dataFrame = data.frame(nome = character())

dataFrame = Semestre3

state = TRUE

for (i in 1 : length(dados1Semestre$nome)) {
  state = TRUE
  
  for(c in 1: length(Semestre3)){
    
    if(dados1Semestre$nome[i] == Semestre3[c]){
      state = FALSE
    }
    
  }
  
  dados1Semestre$Status[i] = state
  
}

setwd("C:\\Users\\IFBA\\Desktop\\subsequente\\2 semestre")
write.csv(dados1Semestre, file = "Full(StatusTeste)_11-10-22.csv")
