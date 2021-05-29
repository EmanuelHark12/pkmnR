#' poke_info
#' @description This is the function that collects the basics information about a pokemon.
#'
#' @param pokemon The name of pokemon
#' @param show_moves A boolean that allow the function make a list of moves of pokemon chosen.
#'
#' @return The list of dataframes with basics information of pokemons (types,base-status,height,weight,region) and
#' a list of moves(name,accuracy,damage class,power,description,pp and type) with show_moves = TRUE.
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#'
#' @export
#'
poke_info <- function(pokemon,show_moves = FALSE){
  `%>%` <- magrittr::`%>%`
  u_base <- "https://pokeapi.co/api/v2/pokemon/"
  barra <- ''
  end_point = paste0(pokemon,barra)
  u_pokemon <- paste0(u_base,end_point)
  r_pokemon <- httr::GET(u_pokemon)
  dados = r_pokemon %>%
    httr::content("parsed", simplifyDataFrame = TRUE)
  id = dados$id
  if(dplyr::between(id,0,899) |  dplyr::between(id,899,10032) |
     dplyr::between(id,10061,10061) | dplyr::between(id,10086,10086)
     | dplyr::between(id,10091,10092) | dplyr::between(id,10100,10121)
     | dplyr::between(id,10123,10127)
     | dplyr::between(id,10130,10142) | dplyr::between(id,10147,10147)
     | dplyr::between(id,10151,10152) | dplyr::between(id,10155,10185)){
    if(show_moves == TRUE){
      if(dplyr::between(id,808,899)  | dplyr::between(id,10158,10220)){

      }
      else{
        movimentos = poke_move_list(dados)
      }
    }
    tipos = poke_get_type(dados)
    status = poke_status(dados)
    regiao = poke_region(pokemon)
    peso = dados$weight
    altura = dados$height
    df = data.frame(pokemon,regiao,t(tipos),t(status[[1]]),peso,altura,id)
    colnames(df) <- c('Nome','Regiao','Tipo Principal','Tipo Secundario',status[[2]],'Peso','Altura','id')
    if(show_moves == TRUE){
      if(dplyr::between(id,808,899)  | dplyr::between(id,10158,10220)){
        aviso = list('Esse pokemon e de galar')
        resultado = list(df,aviso)
        return(resultado)
      }
      resultado = list(df,movimentos)
      return(resultado)
    }
    return(df)
  }
  else(
    return('Esse pokemon ou e mega ou e totem ou e gigantamax')
  )
}
#' poke_move
#' @description This is the function that collects the basics information about a certain Pokemon move.
#'
#' @param x The dataframe who contains information about a certain move.
#' @param Movimentos The name of move.
#'
#' @return The dataframe with basics information of pokemon move (name,accuracy,damage class,power,description,pp and type)
#' that are learned by level in Pokemon Ultra Sun & Ultra Moon.
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_move <- function(x,Movimentos){
  detalhes = x
  golpe = dplyr::filter(x,x[['move_learn_method']] == 'level-up' & x[['version_group']] == 'ultra-sun-ultra-moon')
  if (dim(golpe)[1] >= 1){
    more <- poke_move_info(Movimentos)
    Level <-  golpe[['level_learned_at']][1]
    df = data.frame(Movimentos,Level)
    df = cbind(df,more)
    return(df)
  }
}
#' poke_move_list
#' @description This is the function that organize the basics information about all Pokemon moves filter by a function
#' poke_move.
#'
#' @param x The list of dataframes that contains informations of a pokemon.
#'
#'
#' @return The dataframe with basics information of pokemon moves (name,accuracy,damage class,power,description,pp and type)
#' that are learned by level in Pokemon Ultra Sun & Ultra Moon.
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/
#'
#' @export
#'
poke_move_list <- function(x){
  `%>%` <- magrittr::`%>%`
  l <- purrr::map2(x$moves$version_group_details,x$moves$move$name,poke_move) %>%
    purrr::compact() %>%
    dplyr::bind_rows()
  lw <- l %>% dplyr::arrange(l['Level'])
  return(lw)
}
#' poke_move_info
#' @description This is the function that collect the the especif  information about certain Pokemon move.
#'
#' @param move The name of a pokemon move.
#'
#' @return The dataframe with basics information of pokemon move (accuracy,damage class,power,description,pp and type).
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_move_info <- function(move){
  `%>%` <- magrittr::`%>%`
  start <- 'https://pokeapi.co/api/v2/move/'
  barra <- '/'
  end <- paste0(move,barra)
  link <- paste0(start,end)
  r_pokemon <- httr::GET(link)
  dados = r_pokemon %>%
    httr::content("parsed", simplifyDataFrame = TRUE)
  acuracia = dados$accuracy
  if(is.null(acuracia)){
    acuracia = NA
  }
  tipos = dados$type$name
  modo = dados$damage_class$name
  pp = dados$pp
  descricao = dados$effect_entries$effect
  if(modo == "status"){
    poder = NA
    resultado = data.frame(tipos,poder,pp,acuracia,descricao,modo)
    colnames(resultado) = c('Tipo','Poder','Uso','Acuracia','Descricao','Modo')
    return(resultado)
  }
  else{
    poder = dados$power
    if(is.null(poder)){
      poder = NA
    }
    resultado = data.frame(tipos,poder,pp,acuracia,descricao,modo)
    colnames(resultado) = c('Tipo','Poder','Uso','Acuracia','Descricao','Modo')
    return(resultado)
  }
}
#' poke_get_type
#' @description This is the function that collect the type of a pokemon.
#'
#' @param x The list of dataframes that contains informations of a pokemon.
#'
#'
#' @return The array with the types of a pokemon.
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_get_type <- function(x){

  tipos = x$types$type$name
  if (length(tipos) == 1){
    return(c(x$types$type$name,'NA'))
  }
  else{
    return(c(x$types$type$name[1],x$types$type$name[2]))
  }
}
#' poke_status
#' @description This is the function that collect the base status (hp, attack, defense, special attack, special defense and speed) of
#' a certain pokemon.
#'
#' @param x The list of dataframes that contains informations of a pokemon.
#'
#' @return The list of values of status and the name of status.
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_status <- function(x){
  status = x$stats$base_stat
  names = x$stats$stat$name
  resultado = list(status,names)
  return(resultado)
}
#' poke_region
#' @description This is the function that collect the region of
#' a certain pokemon.
#'
#' @param pokemon The name of a pokemon.
#'
#' @return The  name of region of origin of a pokemon .
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_region <- function(pokemon){
  `%>%` <- magrittr::`%>%`
  barra = ''
  base = 'https://pokeapi.co/api/v2/pokemon/'
  end_reg = paste0(pokemon,barra)
  total = paste0(base,end_reg)
  r_pokemon <- httr::GET(total)
  dados = r_pokemon %>% httr::content("parsed", simplifyDataFrame = TRUE)
  id = dados$id

  regiao = dplyr::case_when(dplyr::between(id,1,151)~'kanto',
                            dplyr::between(id,152,251)~'johto',
                            dplyr::between(id,252,386) | dplyr::between(id,10001,10003) |
                            dplyr::between(id,10077,10078) | dplyr::between(id,10013,10015) ~ 'hoenn',
                            dplyr::between(id,387,493) | dplyr::between(id,10004,10012) ~ 'sinnoh',
                            dplyr::between(id,494,649) | dplyr::between(id,10016,10024) ~ 'unova',
                            dplyr::between(id,650,721) | dplyr::between(id,10025,10032) |
                            dplyr::between(id,10061,10061) ~ 'kalos',
                            dplyr::between(id,722,809) | dplyr::between(id,10086,10086)
                     | dplyr::between(id,10091,10092) | dplyr::between(id,10100,10121)
                     | dplyr::between(id,10123,10127)
                     | dplyr::between(id,10130,10142) | dplyr::between(id,10147,10147)
                     | dplyr::between(id,10151,10152) | dplyr::between(id,10155,10157) ~ 'alola',
                     dplyr::between(id,808,899) | dplyr::between(id,10158,10185) ~ 'galar')
  return(regiao)
}
