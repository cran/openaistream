#' DataStream Class
#'
#' A R6 class to manage data streams.

DataStream <- R6::R6Class(
  "DataStream",
  private = list(
    requery = NULL,
    status = NULL,
    iterator = NULL,
    num = 2,
    check = function() {
      if (is.null(private$requery)) {
        return("completed or requery is NULL_")
      }
      if (!inherits(private$requery, "curl")) {
        return("is not curl")
      }
      is_valid <- TRUE
      tryCatch({
        isOpen(private$requery)
        is_valid <- FALSE
      }, error = function(e) {
        is_valid <- TRUE
      })
      if (is_valid) {
        return("httr2_invalid")
      }
      if (summary(private$requery)$opened == "opened") {
        return("httr2_open")
      } else {
        return("httr2_close")
      }
    },
    destroy = function(status="close") {
      try({
        if (!is.null(private$requery) && inherits(private$requery, "curl")) {
          base::close(private$requery)
        }
      },silent = T)
      private$requery <- NULL
      private$status <- status
      private$iterator <- NULL
      private$num <- NULL
      gc()
    },
    data_source = function() {
      private$status <- private$check()
      if (private$status == "httr2_close") {
        tryCatch({
          open(private$requery, "rbf")
          private$status = "httr2_open"
        }, error = function(e) {
          private$status <- paste0(private$status, " open is fail")
          return(private$status)
        })
      }
      if (private$status =="httr2_open") {
        buf <- readLines(private$requery, private$num * 2)
        lstr <- lapply(buf, function(v) {
          if (nchar(v) < 20) {
            if ("data: [DONE]" == v) {
              private$destroy("complete")
            }
            return("")
          } else {
            return(gsub(v, replacement = "", pattern = "data: "))
          }
        })
        lstr_cleaned <- lstr[nchar(unlist(lstr)) > 1]
        vres <- unlist(lapply(lstr_cleaned, function(v) {
          choices <- fromJSON(v)$choices
          if (is.null(choices$text)) {
            return(choices)
          } else {
            return(choices$text)
          }
        }))
        return(list(all_resp = lstr_cleaned, vres = vres))
      } else {
        return(private$status)
      }
    }
  ),
  public = list(
    #' @description Initialize the DataStream object
    #' @param requery The requery object, usually of class 'curl'
    #' @param num Numeric. Defines the number of lines to read from the data source.
    initialize = function(requery, num = 2) {
      private$requery <- requery
      private$status <- "initialized"
      private$num <- num
      private$iterator <- iter(private$data_source)
    },
    #' @description Close the DataStream
    #'
    #' This method tries to destroy the object, and closes the requery if opened.
    #' @return A character message indicating the status of the close operation.
    close = function(){
      tryCatch({
        private$destroy()
        return("close success")
      }, error = function(e) {
        return(e)
      })
    },
    #' @description Get the status of the DataStream
    #'
    #' @return A character string indicating the current status of the DataStream.
    get_state =function(){
      return(private$status)
    }

  ),
  active = list(
    #' @field next_value The next data value from the DataStream or an error message.
    next_value = function() {
      tryCatch({
        if(is.null(private$iterator)){
          return(private$status)
        }
        nextElem(private$iterator)
      }, error = function(e) {
        return(e)
      })
    }
  )
)
