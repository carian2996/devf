palindromo <- function(p) {
      for(i in 1:floor(nchar(p)/2) ) {
            r <- nchar(p) - i + 1
            if ( substr(p, i, i) != substr(p, r, r) ) return("Intentalo de nuevo")
      }
      "Felicidades! Tienes un palindromo en tus manos!"
}