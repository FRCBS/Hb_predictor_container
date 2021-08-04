suppressPackageStartupMessages(library(plumber))
#suppressPackageStartupMessages(library(Rook))  # For parsing multipart http requests
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(httpuv))
suppressPackageStartupMessages(library(rjson))

#pr <- plumber::plumb("docker-apps-plumber.R")

cat("Open address http://localhost:8080/hb-predictor in your browser.\n")
cat("Press control-c to kill the server\n")

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

cat("Here\n")

s <- runServer("0.0.0.0", 8080,
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
                       result <- tryCatch(
                         error = function(cnd) {
                           message("In error handler of docker-server-plumber.R\n")
                           error_messages <- c(sprintf("Error in %s call:", "hb_predictor3"), cnd$message)
                           cat(paste(error_messages, collapse="\n"))
                           cat("\n")
                           result <- list(type="final", error_messages=error_messages)
                           return(result)
                         },
                         {
                           hb_predictor3(ws)
                         }
                       )
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