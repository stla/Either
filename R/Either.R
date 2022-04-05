#' @title New 'Left' value
#' @description Creates a new \code{\link{Either}} object of type 'Left'.
#'
#' @param element the content of the 'Left' value
#'
#' @return A 'Left' \code{\link{Either}} object.
#' @export
#'
#' @examples
#' Left("abc")
Left <- function(element){
  Either$new(side = "left", element = element)
}

#' @title New 'Right' value
#' @description Creates a new \code{\link{Either}} object of type 'Right'.
#'
#' @param element the content of the 'Right' value
#'
#' @return A 'Right' \code{\link{Either}} object.
#' @export
#'
#' @examples
#' Right("abc")
Right <- function(element){
  Either$new(side = "right", element = element)
}

#' @title 'Either' object from 'maybe' object
#' @description Creates a new \code{\link{Either}} object from a 'maybe' object.
#'
#' @param a content of the potential 'Left' output value
#' @param mb a 'maybe' value; if \code{nothing}, the function returns the
#'   'Left' value containing \code{a}, otherwise the function returns the
#'   'Right' value containing the content of the 'Just' \code{mb} value
#'
#' @return An \code{\link{Either}} object.
#' @export
#'
#' @importFrom maybe is_maybe is_nothing
#'
#' @examples
#' library(maybe)
#' eitherFromMaybe("abc", nothing())
#' eitherFromMaybe("abc", just(123))
eitherFromMaybe <- function(a, mb){
  stopifnot(is_maybe(mb))
  if(is_nothing(mb)){
    Left(a)
  }else{
    Right(mb[["content"]])
  }
}

#' @title 'Either' objects
#' @description R6 class to create and manipulate 'Either' values. An 'Either'
#'   value is given by a side (the container), 'Left' or 'Right', and an
#'   element assigned to this side.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @importFrom maybe nothing just
Either <- R6Class(
  "Either",

  private = list(
    .isLeft = NULL,
    .element = NULL
  ),

  public = list(

    #' @description Creates a new 'Either' object.
    #'
    #' @param side the container, \code{"Left"} or \code{"Right"} (lower case
    #'   is accepted)
    #' @param element the element to wrap in the container
    #'
    #' @return An 'Either' object.
    #'
    #' @examples
    #' Either$new("left", 123)
    #'
    #' @note Alternatively, you can use the functions \code{\link{Left}} or
    #'   \code{\link{Right}}.
    initialize = function(side, element){
      side <- match.arg(tolower(side), c("left", "right"))
      private[[".isLeft"]] <- side == "left"
      private[[".element"]] <- element
      invisible(self)
    },

    #' @description Prints an 'Either' object (this method is automatically
    #'   called).
    #'
    #' @param ... ignored
    print = function(...){
      if(private[[".isLeft"]]){
        cat("Left\n")
      }else{
        cat("Right\n")
      }
      print(private[[".element"]])
    },

    #' @description Checks whether the reference 'Either' object is a 'Left'
    #'   value.
    #'
    #' @return A Boolean value.
    #'
    #' @examples
    #' Either$new("right", 999)$isLeft()
    isLeft = function(){
      private[[".isLeft"]]
    },

    #' @description Checks whether the reference 'Either' object is a 'Right'
    #'   value.
    #'
    #' @return A Boolean value.
    #'
    #' @examples
    #' Either$new("right", 999)$isRight()
    isRight = function(){
      !private[[".isLeft"]]
    },

    #' @description Get the content of a 'Left' value.
    #'
    #' @param default a default element to return in case if the reference
    #'   'Either' object is not a 'Left' value.
    #'
    #' @return The content of the reference 'Either' object if it is a 'Left'
    #'   value, otherwise the default element.
    #'
    #' @examples
    #' Either$new("right", 999)$getLeft("abc")
    getLeft = function(default){
      if(private[[".isLeft"]]){
        private[[".element"]]
      }else{
        default
      }
    },

    #' @description Get the content of a 'Right' value.
    #'
    #' @param default a default element to return in case if the reference
    #'   'Either' object is not a 'Right' value.
    #'
    #' @return The content of the reference 'Either' object if it is a 'Right'
    #'   value, otherwise the default element.
    #'
    #' @examples
    #' Either$new("right", 999)$getRight("abc")
    getRight = function(default){
      if(private[[".isLeft"]]){
        default
      }else{
        private[[".element"]]
      }
    },

    #' @description Converts an 'Either' object to a 'maybe' object.
    #'
    #' @return The 'nothing' value if the reference 'Either' object is a 'Left'
    #'   value, otherwise the 'Just' value with the same content as the
    #'   reference 'Either' object.
    #'
    #' @examples
    #' Either$new("right", 999)$toMaybe()
    #' Either$new("left", 999)$toMaybe()
    toMaybe = function(){
      if(private[[".isLeft"]]){
        nothing()
      }else{
        just(private[[".element"]])
      }
    },

    #' @description Apply a function to the content of the reference 'Either'
    #'   object if it is a 'Left' value.
    #'
    #' @param f a function
    #'
    #' @return If the reference 'Either' object is a 'Left' value, this returns
    #'   the 'Left' value whose content is the result of \code{f} applied to
    #'   the original content. Otherwise this returns a clone of the reference
    #'   'Either' object.
    #'
    #' @examples
    #' Either$new("left", 999)$mapLeft(is.numeric)
    #' Either$new("right", 999)$mapLeft(is.numeric)
    mapLeft = function(f){
      if(private[[".isLeft"]]){
        Left(f(private[[".element"]]))
      }else{
        self$clone(deep = TRUE)
      }
    },

    #' @description Apply a function to the content of the reference 'Either'
    #'   object if it is a 'Right' value.
    #'
    #' @param f a function
    #'
    #' @return If the reference 'Either' object is a 'Right' value, this returns
    #'   the 'Right' value whose content is the result of \code{f} applied to
    #'   the original content. Otherwise this returns a clone of the reference
    #'   'Either' object.
    #'
    #' @examples
    #' Either$new("left", 999)$mapRight(is.numeric)
    #' Either$new("right", 999)$mapRight(is.numeric)
    mapRight = function(f){
      if(private[[".isLeft"]]){
        self$clone(deep = TRUE)
      }else{
        Right(f(private[[".element"]]))
      }
    },

    #' @description Apply a function to the content of the reference 'Either'
    #'   object.
    #'
    #' @param f function to apply to the content of the reference 'Either'
    #'   object if it is a 'Left' value
    #' @param g function to apply to the content of the reference 'Either'
    #'   object if it is a 'Right' value
    #'
    #' @return An 'Either' object.
    #'
    #' @examples
    #' Either$new("left", 999)$either(is.numeric, is.character)
    #' Either$new("right", 999)$either(is.numeric, is.character)
    either = function(f, g){
      if(private[[".isLeft"]]){
        Left(f(private[[".element"]]))
      }else{
        Right(g(private[[".element"]]))
      }
    }

  )
)

# private[["."]]
