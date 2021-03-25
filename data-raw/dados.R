normalize <- poke_type('normal')
normal <- poke_df(normalize)
usethis::use_data(normal, overwrite = TRUE)

fightingize <- poke_type('fighting')
fighting <- poke_df(fightingize)
usethis::use_data(fighting, overwrite = TRUE)

iceize <- poke_type('ice')
ice <- poke_df(iceize)
usethis::use_data(ice, overwrite = TRUE)
