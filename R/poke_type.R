#' poke_type
#' @description This is the function that collects the  information about all pokemon of certain type.
#'
#' @param tipo the pokemon type.
#' @param show_moves A boolean that allow the function make a list of moves of pokemon chosen.
#' @param show_trainers A boolean that allow the function make a list of trainers of certain moves.
#' @return The list of dataframes with basics information of  all pokemons (types,base-status,height,weight,region) of
#' certain type and a list of moves (name,accuracy,damage class,power,description,pp and type) with show_moves = TRUE.
#'
#' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/
#' @export
#'
poke_type <- function(tipo,show_moves = FALSE,show_trainers = FALSE){
  `%>%` <- magrittr::`%>%`
  u_base = 'https://pokeapi.co/api/v2/type/'
  u_pokemon = paste0(u_base,tipo)
  r_pokemon <- httr::GET(u_pokemon)
  gym = tipo %>% stringr::str_to_title() %>% stringr::str_trim()
  dados = r_pokemon %>%
    httr::content("parsed", simplifyDataFrame = TRUE)

  pokemons = dados$pokemon$pokemon$name
  if(show_trainers == TRUE){
    trainers = gym_type(gym)
    estudo = lapply(pokemons,function(x) poke_info(x,show_moves))
    resultado = list(estudo,trainers)
    return(resultado)
  }
  else{
    estudo = lapply(pokemons,function(x) poke_info(x,show_moves))
    return(estudo)
  }
}
#' gym_type
#' @description This is the function that filter the all gym leaders of certain type that was collected by the function
#' poke_all_leaders.
#'
#' @param tipo the pokemon type.
#'
#' @return The  dataframe of gym leader of certain type.
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://www.serebii.net/.
#' @export
#'
gym_type <- function(tipo){
  `%>%` <- magrittr::`%>%`
  type_correct = paste0(tipo,'-type')
  trainers = poke_all_leaders()
  result = trainers %>% dplyr::filter(trainers$Tipos == type_correct)
  return(result)
}



