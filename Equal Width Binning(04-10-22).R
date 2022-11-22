
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

dadosMineracaoDiscretizado$Status = dadosMineracao$Status

for(c in 1: length(alunosDiscretos)){
  
  dadosMineracaoDiscretizado[c,"nome"] = alunosDiscretos[c]
  
  #quantAcessoForum
  w = (max(dadosMineracao$quantAcessoForum) - min(dadosMineracao$quantAcessoForum))/3
  
  baixo = min(dadosMineracao$quantAcessoForum)+w
  normal = min(dadosMineracao$quantAcessoForum)+2*w
  alto = min(dadosMineracao$quantAcessoForum)+3*w
  
  if(dadosMineracao[c,"quantAcessoForum"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessoForum"] > baixo && dadosMineracao[c,"quantAcessoForum"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "normal";
  }
  if(dadosMineracao[c,"quantAcessoForum"] > normal && dadosMineracao[c,"quantAcessoForum"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "alto";
  }
  
  #quantAcessoSlides
  
  w = (max(dadosMineracao$quantAcessoSlides) - min(dadosMineracao$quantAcessoSlides))/3
  
  baixo = min(dadosMineracao$quantAcessoSlides)+w
  normal = min(dadosMineracao$quantAcessoSlides)+2*w
  alto = min(dadosMineracao$quantAcessoSlides)+3*w
  
  if(dadosMineracao[c,"quantAcessoSlides"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessoSlides"] > baixo && dadosMineracao[c,"quantAcessoSlides"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "normal";
  }
  if(dadosMineracao[c,"quantAcessoSlides"] > normal && dadosMineracao[c,"quantAcessoSlides"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "alto";
  }
  
  #quantAcessoChat
  
  w = (max(dadosMineracao$quantAcessoChat) - min(dadosMineracao$quantAcessoChat))/3
  
  baixo = min(dadosMineracao$quantAcessoChat)+w
  normal = min(dadosMineracao$quantAcessoChat)+2*w
  alto = min(dadosMineracao$quantAcessoChat)+3*w
  
  if(dadosMineracao[c,"quantAcessoChat"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessoChat"] > baixo && dadosMineracao[c,"quantAcessoChat"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "normal";
  } 
  if(dadosMineracao[c,"quantAcessoChat"] > normal && dadosMineracao[c,"quantAcessoChat"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "alto";
  }
  
  #quantAcessoVideos
  
  w = (max(dadosMineracao$quantAcessoVideos) - min(dadosMineracao$quantAcessoVideos))/3
  
  baixo = min(dadosMineracao$quantAcessoVideos)+w
  normal = min(dadosMineracao$quantAcessoVideos)+2*w
  alto = min(dadosMineracao$quantAcessoVideos)+3*w
  
  if(dadosMineracao[c,"quantAcessoVideos"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessoVideos"] > baixo && dadosMineracao[c,"quantAcessoVideos"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "normal";
  }
  if(dadosMineracao[c,"quantAcessoVideos"] > normal && dadosMineracao[c,"quantAcessoVideos"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "alto";
  }
  
  #quantiAcessoDiscussaoForum
  
  w = (max(dadosMineracao$quantiAcessoDiscussaoForum) - min(dadosMineracao$quantiAcessoDiscussaoForum))/3
  
  baixo = min(dadosMineracao$quantiAcessoDiscussaoForum)+w
  normal = min(dadosMineracao$quantiAcessoDiscussaoForum)+2*w
  alto = min(dadosMineracao$quantiAcessoDiscussaoForum)+3*w
  
  if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "baixo";
  }
  if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] > baixo && dadosMineracao[c,"quantiAcessoDiscussaoForum"] <= normal){
    dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "normal";
  }
  if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] > normal && dadosMineracao[c,"quantiAcessoDiscussaoForum"] <= alto){
    dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "alto";
  }
  
  #quantAcessoSemestre
  
  w = (max(dadosMineracao$quantAcessoSemestre) - min(dadosMineracao$quantAcessoSemestre))/3
  
  baixo = min(dadosMineracao$quantAcessoSemestre)+w
  normal = min(dadosMineracao$quantAcessoSemestre)+2*w
  alto = min(dadosMineracao$quantAcessoSemestre)+3*w
  
  if(dadosMineracao[c,"quantAcessoSemestre"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessoSemestre"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessoSemestre"] > baixo && dadosMineracao[c,"quantAcessoSemestre"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessoSemestre"] = "normal";
  }
  if(dadosMineracao[c,"quantAcessoSemestre"] > normal && dadosMineracao[c,"quantAcessoSemestre"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessoSemestre"] = "alto";
  }
  
  #quantArquivosEnviados
  
  w = (max(dadosMineracao$quantArquivosEnviados) - min(dadosMineracao$quantArquivosEnviados))/3
  
  baixo = min(dadosMineracao$quantArquivosEnviados)+w
  normal = min(dadosMineracao$quantArquivosEnviados)+2*w
  alto = min(dadosMineracao$quantArquivosEnviados)+3*w
  
  if(dadosMineracao[c,"quantArquivosEnviados"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "baixo";
  }
  if(dadosMineracao[c,"quantArquivosEnviados"] > baixo && dadosMineracao[c,"quantArquivosEnviados"] <= normal){
    dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "normal";
  }
  if(dadosMineracao[c,"quantArquivosEnviados"] > normal && dadosMineracao[c,"quantArquivosEnviados"] <= alto){
    dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "alto";
  }
  
  #quantAcessosAtividades
  
  w = (max(dadosMineracao$quantAcessosAtividades) - min(dadosMineracao$quantAcessosAtividades))/3
  
  baixo = min(dadosMineracao$quantAcessosAtividades)+w
  normal = min(dadosMineracao$quantAcessosAtividades)+2*w
  alto = min(dadosMineracao$quantAcessosAtividades)+3*w
  
  if(dadosMineracao[c,"quantAcessosAtividades"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessosAtividades"] > baixo && dadosMineracao[c,"quantAcessosAtividades"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "normal";
  }
  if(dadosMineracao[c,"quantAcessosAtividades"] > normal && dadosMineracao[c,"quantAcessosAtividades"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "alto";
  }
  
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
  
  #quantAcessoArquivos
  
  w = (max(dadosMineracao$quantAcessoArquivos) - min(dadosMineracao$quantAcessoArquivos))/3
  
  baixo = min(dadosMineracao$quantAcessoArquivos)+w
  normal = min(dadosMineracao$quantAcessoArquivos)+2*w
  alto = min(dadosMineracao$quantAcessoArquivos)+3*w
  
  if(dadosMineracao[c,"quantAcessoArquivos"] <= baixo){
    dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "baixo";
  }
  if(dadosMineracao[c,"quantAcessoArquivos"] > baixo && dadosMineracao[c,"quantAcessoArquivos"] <= normal){
    dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "normal";
  }
  if(dadosMineracao[c,"quantAcessoArquivos"] > normal && dadosMineracao[c,"quantAcessoArquivos"] <= alto){
    dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "alta";
  }
  
  
  
}

setwd("C:\\Users\\IFBA\\Desktop\\Concomitante\\2 semestre\\testesDiscretização\\StatusTeste")
write.csv(dadosMineracaoDiscretizado, file = "(Equal width binning)Full2Semestre(11-10-22).csv")
