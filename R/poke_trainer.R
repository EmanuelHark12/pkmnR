#' poke_trainer
#' @description This is the function that collects the  information about a pokemon team chosen from the user.
#'
#'
#' @return The list of dataframes with basics information of pokemons (types,base-status,height,weight,region and level).
#'
#'@references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_trainer <- function(){
  `%>%` <- magrittr::`%>%`
  nome = readline(prompt = 'Qual e o seu nome? ')
  cidade = readline(prompt = 'Qual e a sua cidade? ')
  numero_de_pokemon = as.numeric( readline(prompt = 'Quantos Pokemons? '))
  poke_list = list()
  for(i in seq(1,numero_de_pokemon)){
    moveset = c()
    pokemon = as.character(readline(prompt = 'Qual e o seu pokemon? '))
    info = poke_info(pokemon,show_moves=TRUE)
    level = as.numeric(readline(prompt = 'Qual e o nivel de seu pokemon? '))
    moves = info[[2]] %>% dplyr::filter(info[[2]]['Level'] <= level) %>% dplyr::select('Movimentos')
    print(moves)
    if(dim(moves)[1] > 4){
      for(j in seq(1,4)){
        escolhido <- readline(prompt = 'Qual sera o golpe? ')
        if(is.element(escolhido,moves$Movimentos)){
          pp <- as.numeric(info[[2]] %>% dplyr::filter(info[[2]]['Movimentos'] == escolhido) %>% dplyr::select('Uso'))
          moveset <- c(moveset,c(escolhido,pp))
        }
      }
    }
    else{
      for(j in seq(1,dim(moves)[1])){
        escolhido <- as.character(readline(prompt = 'Qual sera o golpe? '))
        if(is.element(escolhido,moves$Movimentos)){
          pp <- as.numeric(info[[2]] %>% dplyr::filter(info[[2]]['Movimentos'] == escolhido) %>% dplyr::select('Uso'))
          moveset <-c(moveset,c(escolhido,pp))
        }
      }
    }
    infos = c(pokemon,level)
    df = data.frame(t(infos))
    move_final = data.frame(matrix(moveset,ncol=2,byrow = TRUE))
    colnames(df) <- c('Moves','pp')
    df = rbind(df,move_final)
    if(i != 1){
      poke_list[[i]] = df
    }
    else{
      poke_list[[i]] = df
    }
  }
  return(poke_list)
}
#' poke_print
#' @description This is the function that print the pokemon team.
#'
#' @param dados The list of dataframes of pokemon used by a trainer collected by poke_trainer.
#'
#' @return The html Table with the pokemon's used by a trainer.
#'@references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/.
#' @export
#'
poke_print <- function(dados){
  w = length(dados)
  file = c()
  for(i in seq(1,length(dados))){
    unset = unlist(dados[[i]],use.names = FALSE)
    n = length(unset)
    df = htmlTable::htmlTable(dados[[i]][-1,],cgroup = c(paste0(dados[[i]][1,1],'- Lvl - ',dados[[i]][2,2]),''),n.cgroup = c(1))
    file = c(file,df)
  }
  return(htmlTable::htmlTable(file))
}
