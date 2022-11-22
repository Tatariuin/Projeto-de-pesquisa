notas = read.csv(file.choose())
notas$Discente[30]
notas = notasLog

for (i in 1: length(notas$nome)) {
  if(str_detect(notas$nome[i],notas$Discente[i]) != TRUE){
    print(notas$nome[i])
    print(i)
  }
}
"Leide Souza da Silva" == "Leide Sousa da Silva"

