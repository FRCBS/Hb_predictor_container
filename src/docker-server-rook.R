suppressPackageStartupMessages(library(Rook))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))

R.server <- Rhttpd$new()


R.server$add(app = File$new(getwd()), name = "pic_dir")
#print(R.server)

iter <- 1

Rook.app <- function(env) {
    iter <<- iter + 1

    request <- Request$new(env)
    post <- Multipart$parse(env)
    response <- Response$new()

    write.initial.HTML(response, iter)
    write.icebreaker.HTML(request, response, post, R.server)
    #write.iris.HMTL(request, response, R.server)
    write.final.HTML(response)

    response$finish()
}

write.initial.HTML <- function(response, iter) {

    response$write("<h1>A Simple Web Application</h1>")
    #response$write(paste("<i>***Times through the Rook.app function:", as.character(iter), "***</i>"))
    response$write('<form enctype="multipart/form-data" method="POST">')
    response$write("<h3>Hb predictor</h3>")
    #response$write("First name: <input type=\"text\" name=\"firstname\"><br><br>")
    #response$write("Favorite number: <input type=\"text\" name=\"favnumber\"><br>")
    response$write('Upload a file: <input type=file name=fileUpload>')
    response$write("<h3>What do you want to do?</h3>")
    response$write('<input type="submit" value="Generate summary and plot" name="submit_button">')
    #response$write("<button value=\"Plot\" name=\"iris_button\">Plot the Iris data set </button> <br>")

}

write.icebreaker.HTML <- function(request, response, post, server) {

#    if ("firstname" %in% names(request$POST()) & "favnumber" %in% names(request$POST())) {
    if ("fileUpload" %in% names(request$POST()) ) {

        response$write("<h3>Results are</h3>")
        #response$write(paste("Hi, my name is ", request$POST()$firstname, ".", 
        #    sep = ""))

        #fav.number = as.numeric(request$POST()$favnumber)

        #response$write(paste("<br>My favorite number, plus 1, is ", as.character(fav.number + 
        #    1), ". <br>", sep = ""))
	x <- post[["fileUpload"]]
        response$write(c('<pre>You uploaded file ',x[["filename"]],'</pre>'))
        response$write(c('<pre>whose content type is ',x[["content_type"]],'</pre>'))
        response$write(c('<pre>which is temporarily stored in',x[["tempfile"]],'</pre>'))
	
	myiris = read.csv(file = x[["tempfile"]], row.names = NULL)
#        irissum <- iris %>% group_by(Species) %>% summarise(Sepal.Length=mean(Sepal.Length),
#	                                                    Petal.Length=mean(Petal.Length))
        irissum <- iris %>% group_by(Species) %>% summarise_at(c("Sepal.Length", "Petal.Length", "Sepal.Width",
	"Petal.Width"), mean)

	table_string = kable(irissum, format="html")
	response$write(table_string)
        png(file.path(getwd(), "iris.png"))
	plot(myiris)
        dev.off()

        response$write(paste("<img src =", server$full_url("pic_dir"), "/iris.png", 
            ">", sep = ""))
    }
}

#write.iris.HMTL <- function(request, response, server) {
#    if ("iris_button" %in% names(request$POST())) {
#        png(file.path(getwd(), "iris.png"))
#        plot(iris)
#        dev.off()
#
#        response$write(paste("<img src =", server$full_url("pic_dir"), "/iris.png", 
#            ">", sep = ""))
#    }
#}

write.final.HTML <- function(response) {
    response$write("</form>")
}


# Add your Rook app to the Rhttp object
R.server$add(app = Rook.app, name = "hb-predictor")
R.server$start(listen="127.0.0.1", port=8080, quiet=TRUE) # Rook can listen only localhost (127.0.0.1)
#print(R.server)


# view your web app in a browser
#R.server$browse("hb-predictor")

cat("Open address http://localhost:8080/custom/hb-predictor in your browser.\n")
cat("Press control-c to kill the server\n")
#sys.sleep(Inf)
tryCatch(Sys.sleep(Inf), interrupt=function(e) "Got ctrl-c")
#invisible(scan("stdin", character(), nlines=1, quiet=TRUE))
cat("Exiting application!\n")
R.server$remove(all=TRUE)
rm(R.server)
