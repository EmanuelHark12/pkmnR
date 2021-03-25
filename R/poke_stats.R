#' poke_df
#' @description This is the function that collects the basics information about a pokemon.
#'
#' @param data The list of dataframes that contains informations of a pokemon.
#' @param show_moves A boolean that allow the function make a list of moves of pokemon chosen.
#' @param show_trainers A boolean that allow the function make a list of trainers of certain moves.
#'
#' @return The dataframes with basics information of pokemons (types,base-status,height,weight,region)
#'
#' #' @references Pokémon and Pokémon character names are trademarks of Nintendo. The data collect from https://pokeapi.co/
#' @export
#'
poke_df <- function(data,show_moves=FALSE,show_trainers = FALSE){
  if(show_trainers == TRUE){
    n = length(data[[1]])
    df = data.frame(Nome = character(), Regiao = character(), Tipo1 = character(),
                    Tipo2 = character(), Vida = double(), Ataque = double(),
                    Defesa = double(),SpAtaque = double(), SpDefesa = double(),
                    Velocidade = double())
    print(n)
    colnames(df) <- c('Nome','Regiao','Tipo Principal','Tipo Secundario','Vida','Ataque','Defesa',
                      'Ataque Especial','Defesa Especial','Velocidade')
    for(i in seq(1,n)){
      if(show_moves == FALSE){
        if (typeof(data[[1]][[i]]) == 'list'){
          df = rbind(df,data[[1]][[i]][1,])
        }
        else{

        }
      }
      else{
        if (typeof(data[[1]][[i]][[1]]) == 'list'){
          df = rbind(df,data[[1]][[i]][[1]])
        }
        else{

        }
      }
    }
    return(df)
  }
  else{
    n = length(data)
    df = data.frame(Nome = character(), Regiao = character(), Tipo1 = character(),
                    Tipo2 = character(), Vida = double(), Ataque = double(),
                    Defesa = double(),SpAtaque = double(), SpDefesa = double(),
                    Velocidade = double())
    print(n)
    colnames(df) <- c('Nome','Regiao','Tipo Principal','Tipo Secundario','Vida','Ataque','Defesa',
                      'Ataque Especial','Defesa Especial','Velocidade')
    for(i in seq(1,n)){
      if(show_moves == FALSE){
          if (typeof(data[[i]]) == 'list'){
            df = rbind(df,data[[i]][1,])
          }
          else{

          }
        }
      else{
        if (typeof(data[[1]][[i]]) == 'list'){
          df = rbind(df,data[[1]][[i]])
        }
        else{

        }
      }
    }
    return(df)
  }

}
