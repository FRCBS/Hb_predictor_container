#!/usr/bin/Rscript

library(stringr)

options(warn = 1)

args = commandArgs(TRUE)

myparse <- function(args) {
    L <- list()

    for (a in args) {
        x <- str_split(a, "=", simplify=TRUE)
        L[[ x[1] ]] <- x[2]
    }
    return(L)
}

## gender <- args[1]
## sample_fraction <- as.numeric(args[2])
## hlen <- as.integer(args[3])
## output_file=args[4]
## extra_id=args[5]
## hlen_exactly=args[6]

myparams <- myparse(args)

output_file=myparams$output_file
myparams["output_file"] <- NULL

if (is.null(myparams$input_file)) {
    myparams[["input_file"]] = '/home/toivoja/FRCBS/interval_prediction/data/full_data_preprocessed-2020-05-25-train.rdata'
}

if ("sample_fraction" %in% names(myparams)) {
    myparams$sample_fraction <- as.numeric(myparams$sample_fraction)
}
if ("hlen" %in% names(myparams)) {
    myparams$hlen <- as.numeric(myparams$hlen)
}


print(myparams)
## cat(sprintf("Using gender=%s, sample_fraction=%.2f, hlen=%s, extra_id=%s\n",
##             gender, sample_fraction, hlen, extra_id))

rmarkdown::render('jarkko_subset_analyses.Rmd',
                  output_file=output_file,
		  output_format='md_document',
		  output_dir='../output',
                  params = myparams)

                                        #		  params = list(input_file='/home/toivoja/FRCBS/interval_prediction/data/full_data_preprocessed-2020-06-08-train-with-donor-specific-enriched.rdata',
		  ## params = list(input_file='/home/toivoja/FRCBS/interval_prediction/data/full_data_preprocessed-2020-05-25-train.rdata',
                  ##               gender=gender,
                  ##               method='icp-fix',
                  ##               sample_fraction=sample_fraction,
                  ##               hlen=hlen,
                  ##               extra_id=extra_id,
                  ##               hlen_exactly=ifelse(is.null(hlen_exactly), FALSE, hlen_exactly),
                  ##               skip_train=FALSE,
                  ##               create_datasets_bool=TRUE))
