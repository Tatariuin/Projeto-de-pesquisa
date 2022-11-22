
library(stringr)
library(microbenchmark)

dadosMineracao = read.csv(file.choose())

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
  Status = character())

media = str_replace(dadosMineracao$mediaSemestre, "[,]",".")
media
dadosMineracao$mediaSemestre = media

dadosMineracao$mediaSemestre  = as.numeric(dadosMineracao$mediaSemestre)
sum(dadosMineracao$mediaSemestre)
summary(dadosMineracao$mediaSemestre)



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
    MED = mean(dadosMineracao$quantAcessoForum);
    MED
    if(dadosMineracao[c,"quantAcessoForum"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessoForum"] > (MED*0.3333) && dadosMineracao[c,"quantAcessoForum"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "normal";
    }
    if(dadosMineracao[c,"quantAcessoForum"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "alto";
    }
    
    #quantAcessoSlides
    MED = mean(dadosMineracao$quantAcessoSlides);
    
    if(dadosMineracao[c,"quantAcessoSlides"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessoSlides"] > (MED*0.3333) && dadosMineracao[c,"quantAcessoSlides"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "normal";
    }
    if(dadosMineracao[c,"quantAcessoSlides"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "alto";
    }
    
    #quantAcessoChat
    MED = mean(dadosMineracao$quantAcessoChat);
    
    if(dadosMineracao[c,"quantAcessoChat"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessoChat"] > (MED*0.3333) && dadosMineracao[c,"quantAcessoChat"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "normal";
    } 
    if(dadosMineracao[c,"quantAcessoChat"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "alto";
    }
    
    #quantAcessoVideos
    MED = mean(dadosMineracao$quantAcessoVideos);
    
    if(dadosMineracao[c,"quantAcessoVideos"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessoVideos"] > (MED*0.3333) && dadosMineracao[c,"quantAcessoVideos"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "normal";
    }
    if(dadosMineracao[c,"quantAcessoVideos"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "alto";
    }
    
    #quantiAcessoDiscussaoForum
    MED = mean(dadosMineracao$quantiAcessoDiscussaoForum);
    
    if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "baixo";
    }
    if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] > (MED*0.3333) && dadosMineracao[c,"quantiAcessoDiscussaoForum"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "normal";
    }
    if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "alto";
    }
    
    #quantAcessoSemestre
    MED = mean(dadosMineracao$quantAcessoSemestre);
    
    if(dadosMineracao[c,"quantAcessoSemestre"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessoSemestre"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessoSemestre"] > (MED*0.3333) && dadosMineracao[c,"quantAcessoSemestre"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoSemestre"] = "normal";
    }
    if(dadosMineracao[c,"quantAcessoSemestre"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoSemestre"] = "alto";
    }
    
    #quantArquivosEnviados
    MED = mean(dadosMineracao$quantArquivosEnviados);
    MED
    if(dadosMineracao[c,"quantArquivosEnviados"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "baixo";
    }
    if(dadosMineracao[c,"quantArquivosEnviados"] > (MED*0.3333) && dadosMineracao[c,"quantArquivosEnviados"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "normal";
    }
    if(dadosMineracao[c,"quantArquivosEnviados"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "alto";
    }
    
    #quantAcessosAtividades
    MED = mean(dadosMineracao$quantAcessosAtividades);
    
    if(dadosMineracao[c,"quantAcessosAtividades"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessosAtividades"] > (MED*0.3333) && dadosMineracao[c,"quantAcessosAtividades"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "normal";
    }
    if(dadosMineracao[c,"quantAcessosAtividades"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "alto";
    }
    
    #mediaSemestre
    MED = mean(dadosMineracao$mediaSemestre);
  
   if(dadosMineracao[c,"mediaSemestre"] <= (5.9)){
      dadosMineracaoDiscretizado[c,"mediaSemestre"] = "baixo_rendimento";
    }
    if(dadosMineracao[c,"mediaSemestre"] > (5.9) && dadosMineracao[c,"mediaSemestre"] <= (7.9)){
      dadosMineracaoDiscretizado[c,"mediaSemestre"] = "bom_rendimento";
    }
    if(dadosMineracao[c,"mediaSemestre"] > (7.9)){
      dadosMineracaoDiscretizado[c,"mediaSemestre"] = "alto_rendimento";
    } 
    
    #quantAcessoArquivos
    MED = mean(dadosMineracao$quantAcessoArquivos);
    
    if(dadosMineracao[c,"quantAcessoArquivos"] <= (MED*0.3333)){
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "baixo";
    }
    if(dadosMineracao[c,"quantAcessoArquivos"] > (MED*0.3333) && dadosMineracao[c,"quantAcessoArquivos"] <= (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "normal";
    }
    if(dadosMineracao[c,"quantAcessoArquivos"] > (MED*0.6666)){
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "alta";
    }
   
    
    
    
  
  
  
}
#data = dadosMineracaoDiscretizado
#data = rbind(data,dadosMineracaoDiscretizado)


setwd("C:\\Users\\IFBA\\Desktop\\concomitante\\3 semestre")
write.csv(dadosMineracaoDiscretizado, file = "(Med33%Conc)Full3Semestre(18-11-22).csv")
