# Return stars for pvalue
stars <- function(estimate, pvalue) {
    stars <- ifelse(pvalue <= .01, '***',
                    ifelse(pvalue <= .05, '**',
                           ifelse(pvalue <= .1, '*', '')
                           )
                    )
    output <- paste0(estimate, stars)
    return(output)
}

source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}