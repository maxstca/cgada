#Render current report
source <- getwd()
out <- paste0(getwd(), "/", "docs")
rmarkdown::render(input = paste0(source, "/", "CGA-0.14.Rmd"),
                  output_format = "html_document",
                  output_file = "CGA-0.14.html",
                  output_dir = out)

#Legacy reports
source <- getwd()
out <- paste0(getwd(), "/", "docs")
rmarkdown::render(input = paste0(source, "/", "CGA-Playtest-Analysis.Rmd"),
                  output_format = "html_document",
                  output_file = "CGA Playtest Analysis.html",
                  output_dir = out)

source <- getwd()
out <- paste0(getwd(), "/", "docs")
rmarkdown::render(input = paste0(source, "/", "CGA-0.13.Rmd"),
                  output_format = "html_document",
                  output_file = "CGA-0.13.html",
                  output_dir = out)

source <- getwd()
out <- paste0(getwd(), "/", "docs")
rmarkdown::render(input = paste0(source, "/", "CGA-0.13.1.rmd"),
                  output_format = "html_document",
                  output_file = "CGA-0.13.1.html",
                  output_dir = out)