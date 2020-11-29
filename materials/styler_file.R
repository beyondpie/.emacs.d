library(styler)
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1 ) {
  stop("Please insert one and only one file")
}

styler::style_file(path=args[1])
