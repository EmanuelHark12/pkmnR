#' poke_leaders
#' @description This is the function that collects the gym leaders (name, type and city) of a certain region.
#'
#' @param geracao The name of a pokemon region.
#'
#' @return The dataframe with basics information of gym leaders  (name, type and city).
#'
#'@references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://www.serebii.net/.
#' @export
#'
poke_leaders <- function(geracao=''){
  `%>%` <- magrittr::`%>%`
  jogo <- get_pkmn_region(geracao)
  print(jogo)
  base = 'https://www.serebii.net/'
  lideres = c()
  cidades = c()
  types = c()
  if(jogo == 'sunmoon' || jogo == 'ultrasunultramoon'){
    end = paste0(jogo,'/trialcaptains.shtml')
    pagina = paste0(base,end)
    r_pokemon <- xml2::read_html(pagina) %>%
      xml2::xml_child(2) %>%
      xml2::xml_child(3) %>%
      xml2::xml_child(5) %>%
      xml2::xml_child(2) %>%
      xml2::xml_child(2)
    ginasios = r_pokemon %>% xml2::xml_find_all('//td[@class = "foocontent"]') %>%
      xml2::xml_find_all('text()')
  }
  else{
    end = paste0(jogo,'/gyms.shtml')
    pagina = paste0(base,end)
    print(pagina)
    if(jogo == 'rb' || jogo == 'yellow' || jogo == 'gs' || jogo == 'crystal' || jogo == 'fireredleafgreen'){
      r_pokemon <- xml2::read_html(pagina) %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(3) %>%
        xml2::xml_child(5) %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(2)
      ginasios = r_pokemon %>% xml2::xml_find_all('//td[@class = "foocontent"]') %>%
        xml2::xml_find_all('//p') %>%
        xml2::xml_find_all('text()')
      n = length(ginasios)
      y = (n-3)%/%7 + 1
      jogos <- rep(jogo,y)
      print(y)
      for(i in seq(3,n,7)){
        if(i >= 59){
          cidades = c(cidades,stringr::str_split(xml2::xml_text(ginasios[[i+1]]),':')[[1]][2])
          lideres = c(lideres,stringr::str_split(xml2::xml_text(ginasios[[i+2]]),':')[[1]][2])
          types = c(types,stringr::str_replace_all(stringr::str_split(xml2::xml_text(ginasios[[i+3]]),':')[[1]][2]," ",""))
        }
        else{
          cidades = c(cidades,stringr::str_split(xml2::xml_text(ginasios[[i]]),':')[[1]][2])
          lideres = c(lideres,stringr::str_split(xml2::xml_text(ginasios[[i+1]]),':')[[1]][2])
          types = c(types,stringr::str_replace_all(stringr::str_split(xml2::xml_text(ginasios[[i+2]]),':')[[1]][2]," ",""))
        }
      }
      gym_df = data.frame(cidades,lideres,types,jogos)
      colnames(gym_df) <- c('Cidades','Lideres','Tipos','Jogo')
      return(gym_df)
    }
    else if(jogo == 'rubysapphire'){
      r_pokemon <- xml2::read_html(pagina)  %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(3) %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(1)
      ginasios = r_pokemon %>% xml2::xml_find_all('//td[@class = "fooinfo"]') %>% xml2::xml_find_all('//p') %>% xml2::xml_find_all('text()')
      n = length(ginasios)
      y = (n-3)%/%7 + 1
      jogos <- rep(jogo,y)
      print(n)
      for(i in seq(3,n,7)){
          cidades = c(cidades,stringr::str_split(xml2::xml_text(ginasios[[i]]),':')[[1]][2])
          lideres = c(lideres,stringr::str_split(xml2::xml_text(ginasios[[i+1]]),':')[[1]][2])
          types = c(types,stringr::str_replace_all(stringr::str_split(xml2::xml_text(ginasios[[i+2]]),':')[[1]][2]," ",""))
      }
      gym_df = data.frame(cidades,lideres,types,jogos)
      colnames(gym_df) <- c('Cidades','Lideres','Tipos','Jogo')
      return(gym_df)
    }
    else if(jogo == 'emerald'){
      end = paste0(jogo,'/gym.shtml')
      pagina = paste0(base,end)
      r_pokemon <- xml2::read_html(pagina)  %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(3) %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(2) %>%
        xml2::xml_child(1)
      ginasios = r_pokemon  %>% xml2::xml_find_all('//table') %>%
        xml2::xml_find_all('//tr') %>%
        xml2::xml_find_all('//td') %>%
        xml2::xml_find_all('//table')

      bora_ginasios = ginasios[seq(5,12,1)] %>%
        xml2::xml_text() %>%
        stringr::str_replace_all('\r',' ') %>%
        stringr::str_replace_all('\n',' ') %>%
        stringr::str_replace_all('\t',' ') %>%
        stringr::str_trim() %>%
        stringr::str_split('                ')
      n = length(bora_ginasios)
      lideres = c()
      cidades = c()
      types = c('Rock-type','Fighting-type','Electri-type','Fire-type','Normal-type','Flying-type','Psychic-type','Water-type')
      jogos <- rep('jogo',n)
      for(i in seq(1,n,1)){
        certas = bora_ginasios[[i]][1] %>% stringr::str_split('     ')
        corretas = certas[[1]][1] %>% stringr::str_split('Leader ')
        name_leader = corretas[[1]][2]
        cidade = certas[[1]][2]
        cidades = c(cidades,cidade)
        lideres = c(lideres,name_leader)
        #cidades = c(cidades,str_split(xml_text(ginasios[[i]]),':')[[1]][2])
        #lideres = c(lideres,str_split(xml_text(ginasios[[i+1]]),':')[[1]][2])
        #types = c(types,str_replace_all(str_split(xml_text(ginasios[[i+2]]),':')[[1]][2]," ",""))
      }
      gym_df = data.frame(cidades,lideres,types,jogos)
      colnames(gym_df) <- c('Cidades','Lideres','Tipos','Jogo')
      return(gym_df)
    }
  }
  return('kkk')
}
#' poke_all_leaders
#' @description This is the function that collects the dataframes of
#' gym leaders (name, type and city) of all region.
#'
#'
#' @return The dataframe with basics information of all gym leaders  (name, type and city).
#'
#'@references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://www.serebii.net/.
#' @export
#'
poke_all_leaders <- function(){
  kanto = poke_leaders('kanto')
  johto = poke_leaders('johto')
  hoenn = poke_leaders('hoenn')
  total = rbind(kanto,johto,hoenn)
  return(total)
}

#' get_pkmn_region
#' @description This is the function that collects the name of game of pokemon given the region of pokemon for collect information of gym leaders.
#'
#' @param regiao The name of region
#' @return The name of pokemon game used for collect information of gym leaders.
#'
#'@references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://www.serebii.net/.
#' @export
#'
get_pkmn_region <- function(regiao=''){
  if(regiao == ''){
    regiao = readline(prompt = 'Qual a regiao que voce quer?')
  }
  if(regiao == 'kanto'){
    aviso = cat('Jogos da 1\u00AA Geracao: Red & Blue: rb
             Yellow: yellow
             Fire Red & Leaf Green: fireredleafgreen
             ')
    jogo = readline(prompt = 'Qual desses jogos?')
    if(jogo == 'Red & Blue'){
      return('rb')
    }
    else if(jogo == 'Yellow'){
      return('yellow')
    }
    else if(jogo == 'Fire Red & Leaf Green')
      return('fireredleafgreen')
  }
  if(regiao == 'johto'){
    aviso = cat('Jogos da 2\u00AA Geracao: Gold & Silver: gs
                 Crystal: crystal
             ')
    jogo = readline(prompt = 'Qual desses jogos?')
    if(jogo == 'Crystal'){
      return('crystal')
    }
    else if(jogo == 'Gold & Silver'){
      return('gs')
    }

  }
  if(regiao == 'hoenn'){
    aviso = cat('Jogos da 3\u00AA Geracao: Ruby & Sapphire: rubysapphire
                Emerald: emerald
             ')
    jogo = readline(prompt = 'Qual desses jogos?')
    if(jogo == 'Ruby &  Sapphire'){
      return('rubysapphire')
    }
    else if(jogo == 'Emerald'){
      return('emerald')
    }

  }
}
