
library(stringr)
library(microbenchmark)

dadosMineracao = read.csv(file.choose());

dadosMineracaoDiscretizado = data.frame(
  nome = character(),
  quantAcessoForum = character(),
  quantAcessoSlides = character(),
  quantAcessoChat = character(),
  quantAcessoVideos = character(),
  quantiAcessoDiscussaoForum = character(),
  quantAcessoSemestre = character(),
  quantArquivosEnviados = character(),
  quantAcessosAtividades = character(),
  mediaSemestre = character(),
  quantAcessoArquivos = character(),
  Status = logical())

media = str_replace(dadosMineracao$mediaSemestre, "[,]",".")
media
dadosMineracao$mediaSemestre = media

dadosMineracao$mediaSemestre  = as.numeric(dadosMineracao$mediaSemestre)
sum(dadosMineracao$mediaSemestre)
summary(dadosMineracao$mediaSemestre)

max(dadosMineracao$quantAcessoForum)
min(dadosMineracao$quantAcessoForum)

AdicionaLinha = data.frame(nome = NA,
                           quantAcessoForum = NA,
                           quantAcessoSlides = NA,
                           quantAcessoChat = NA,
                           quantAcessoVideos = NA,
                           quantiAcessoDiscussaoForum = NA,
                           quantAcessoSemestre = NA,
                           quantArquivosEnviados = NA,
                           quantAcessosAtividades = NA,
                           mediaSemestre = NA,
                           quantAcessoArquivos = NA,
                           Status = NA)


alunosDiscretos = vector()
alunosDiscretos = dadosMineracao$nome  
c = numeric()


for(c in 1: length(alunosDiscretos)){
  
  dadosMineracaoDiscretizado = rbind(dadosMineracaoDiscretizado,AdicionaLinha)
  
}

dadosMineracaoDiscretizado$nome = dadosMineracao$nome
dadosMineracaoDiscretizado$quantAcessoForum = dadosMineracao$quantAcessoForum
dadosMineracaoDiscretizado$quantAcessoSlides = dadosMineracao$quantAcessoSlides
dadosMineracaoDiscretizado$quantAcessoChat = dadosMineracao$quantAcessoChat
dadosMineracaoDiscretizado$quantAcessoVideos = dadosMineracao$quantAcessoVideos
dadosMineracaoDiscretizado$quantiAcessoDiscussaoForum = dadosMineracao$quantiAcessoDiscussaoForum
dadosMineracaoDiscretizado$quantAcessoSemestre = dadosMineracao$quantAcessoSemestre
dadosMineracaoDiscretizado$quantArquivosEnviados = dadosMineracao$quantArquivosEnviados
dadosMineracaoDiscretizado$quantAcessosAtividades = dadosMineracao$quantAcessosAtividades
dadosMineracaoDiscretizado$quantAcessoArquivos = dadosMineracao$quantAcessoArquivos

for(c in 1: length(alunosDiscretos)){
  
  
  #mediaSemestre
  #Foi decidido usar um metodo diferente para a disccretização da nota do aluno
  if(dadosMineracao[c,"mediaSemestre"] <= 4.9){
    dadosMineracaoDiscretizado[c,"mediaSemestre"] = "baixo_rendimento";
  }
  if(dadosMineracao[c,"mediaSemestre"] > 4.9 && dadosMineracao[c,"mediaSemestre"] <= 7.9){
    dadosMineracaoDiscretizado[c,"mediaSemestre"] = "bom_rendimento";
  }
  if(dadosMineracao[c,"mediaSemestre"] > 7.9){
    dadosMineracaoDiscretizado[c,"mediaSemestre"] = "alto_rendimento";
  }
  
  #status
  if(dadosMineracao[c,"Status"] == 0){
    dadosMineracaoDiscretizado[c,"Status"] = FALSE;
  }
  if(dadosMineracao[c,"Status"] == 1){
    dadosMineracaoDiscretizado[c,"Status"] = TRUE;
  }
  
  
  
}

setwd("C:\\Users\\IFBA\\Desktop\\Concomitante\\1 semestre")
write.csv(dadosMineracaoDiscretizado, file = "(Apenas média discretizada)dadosMineracao_completo.csv")
