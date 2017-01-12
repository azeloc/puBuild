#'Concatena Capítulos De Um Power Up
#'
#'Concatena os capítulos de um power up em um único arquivo index.html.
#'
#'@importFrom magrittr %>%
#'@par dir diretório em que encontram-se os capítulos
#'@par titulo título do arquivo resposta
#'@par output_file nome do arquivo de saída
#'@par output_dir nome do diretório saída
#'
#'@export
Rmd_bind <- function(dir = ".", titulo, output_file = NULL,
                     output_dir = 'content/'){

  # cria o header do novo arquivo
  book_header = sprintf("---\ntitle: %s\ndate: '%s'\n---\n", titulo, Sys.Date()) %>%
    textConnection() %>%
    readLines()

  # lista e copia os diretórios do PU
  dir_list <- list.dirs(path = dir, recursive = F)
  suppressWarnings(dir.create(output_dir))
  for(d in dir_list){
    copy_dir(d, sprintf("%s/%s", output_dir, basename(d)))
  }

  # Avisa se já existir um arquivo index.Rmd
  if(length(grep("index.Rmd", list.files(dir, full.names = T))) > 0){
    warning("index.Rmd already exists")
  }

  # lista todos arquivos .Rmd que tem no PU
  cfiles <- list.files(dir, pattern = "*.Rmd", full.names = T)

  # Cria um arquivo .Rmd com o header
  f <- sprintf("%s/index.Rmd", dir)
  write(book_header, file = f)

  # adiciona um child para cada arquivo
  for(i in 1:length(cfiles)){
    text <- sprintf('```{r child = "%s"} \n```\n', cfiles[i])
    write(text, sep = "", file = f, append = T)
  }

  ezknitr::ezknit(file = f, out_dir = output_dir, fig_dir = "figures", keep_html = FALSE)

  # limpar o resultado...
  # por causa da função go_interactive do tutorial, ele insere algumas
  # coisas antes da header do markdown

  linhas <- readLines(sprintf("%s/index.md", output_dir))
  indice <- min((1:length(linhas))[stringr::str_detect(linhas, '---')])

  if(indice != 1)
    linhas <- linhas[-c(1:(indice-1))]

  writeLines(linhas, sprintf("%s/index.md", output_dir))
}

copy_dir <- function(from, to){
  system(paste("cp -r", from, to))
}
