suppressPackageStartupMessages(library(plumber))
#suppressPackageStartupMessages(library(Rook))  # For parsing multipart http requests
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(httpuv))
suppressPackageStartupMessages(library(rjson))

args <- commandArgs(TRUE)

if (length(args) == 1) {
  port <- as.integer(args[1])
} else {
  port <- 8080
}
#pr <- plumber::plumb("docker-apps-plumber.R")


#pr$run(host='0.0.0.0', port=8080) # Listen to the specified port on all interfaces

options(warn=1)

source("docker-apps-plumber.R")

# Not used
multiplexer <- function(req) {
  body <- paste0("Time: ", Sys.time(), "<br>Path requested: ", req$PATH_INFO)
  
  cat("moi\n")
  #body <- hb_predictor(req)
  cat("hei\n")
  list(
    status = 200L,
    headers = list('Content-Type' = 'text/html'),
    body = body
  )
}

cat(sprintf("Open address http://localhost:%i/hb-predictor in your browser.\n", port))
cat("Press control-c to kill the server\n")

cat("Here\n")

s <- runServer("0.0.0.0", port,
                 list(
                   call = function(req) {
                     cat(sprintf("Got http request %s\n", req$PATH_INFO))
                     print(names(req))
                     cat("User agent is:\n")
                     print(req$HTTP_USER_AGENT)
                     print(req$.bodyData)  # NULL
                     cat("moi\n")
                     if (req$PATH_INFO == "/hb-predictor") {
                       body <- hb_predictor(req)
                     } else if (req$PATH_INFO == "/hb-predictor2") {
                       body <- rjson::toJSON(hb_predictor2(req))
                       return(list(status = 200L,
                                   headers = list('Content-Type' = 'application/json'),
                                   body = body))
                     } else {
                       body <- paste0("Time: ", Sys.time(), "<br>Path requested: ", req$PATH_INFO)
                     }
                     cat("hei\n")
                     list(
                       status = 200L,
                       headers = list('Content-Type' = 'text/html'),
                       body = body
                     )
                   },
                   onWSOpen = function(ws) {
                     # The ws object is a WebSocket object
                     cat("Server websocket connection opened.\n")

                     # ws$send("First message")
                     # Sys.sleep(1000)
                     # ws$send("Second message")
                     # Sys.sleep(1000)
                     # ws$send("Third message")

                     ws$onMessage(function(binary, message) {
                       cat("Server received message:", message, "\n")
                       #result <- list()
                       #result <- withCallingHandlers(

                       result <- tryCatch(
                           error = function(cnd) {
                             message("In error handler of docker-server-plumber.R\n")
                             error_messages <- c(sprintf("Error in %s call:", "hb_predictor3"), cnd$message)
                             cat(paste(error_messages, collapse="\n"))
                             #traceback(1)
                             #print(rlang::trace_back())
                             cat("\n")
                             result <- list(type="final", error_messages=error_messages)
                             return(result)
                           },
                           expr = {
                             withCallingHandlers(
                               error=function(cnd){
                                 traceback()
                                 print(rlang::trace_back())
                               }, 
                               {
                                 hb_predictor3(ws)
                               }
                             )
                           }
                       )
                       #if ("error_messages" %in% names(result)) {
                         #message("Täällä ollaan!!!!!!!!!!!!!!!!")
                         #traceback()
                         #print(rlang::last_trace())
                       #}
                       # if (!is.null(error_messages)) {
                       #   cat(paste0(error_messages))
                       #   result <- list(type="final", error_messages=error_messages)
                       # }
                       ws$send(rjson::toJSON(result))
                     })
                     
                     ws$onClose(function() {
                       cat("Server websocket connection closed.\n")
                     })
                   },
                   staticPaths = list("/static" = "../static/",
                                      "/output" = "../output/")
                 )
)

cat("And here\n")
Sys.sleep(Inf)