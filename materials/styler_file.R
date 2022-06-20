library(styler)
args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Please insert one and only one file")
}

## https://styler.r-lib.org/reference/tidyverse_style.html
styler::style_file(path = args[1], 
                   style = tidyverse_style,
                   strict = TRUE,
                   scope = "tokens",
                   indent_by = 2,
                   start_comments_with_one_space = FALSE,
                   reindention = tidyverse_reindention(),
                   math_token_spacing = tidyverse_math_token_spacing())
