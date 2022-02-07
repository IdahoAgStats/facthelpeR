X <- read.csv("https://raw.githubusercontent.com/IdahoAgStats/small-grains-curation/main/controlled_vocab/agronomic_trt_trial.csv?token=GHSAT0AAAAAABHVP36KVSS63X5O25QICQQYYPY7CJQ")

library(readr)
X <- read_csv("https://raw.githubusercontent.com/IdahoAgStats/small-grains-curation/main/controlled_vocab/agronomic_trt_trial.csv?token=GHSAT0AAAAAABHVP36KVSS63X5O25QICQQYYPY7CJQ")




library(httr)
https://github.com/KZPS/Spotkania/tree/master/Matteo/literature
f2 <- "https://api.github.com/repos/KZPS/Spotkania/git/trees/master?recursive=1"

f2 <- "https://github.com/IdahoAgStats/facthelpeR/tree/main/inst/extdata/OSU_2019_example/auxiliary_files"
f2 <- "https://api.github.com/repos/IdahoAgStats/facthelpeR/tree/main/inst/extdata/OSU_2019_example/auxiliary_files"
req <- GET(f2)
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
grep("Matteo/literature/", filelist, value = TRUE, fixed = TRUE)


script <-
  GET(
    url = "https://api.github.com/repos/IdahoAgStats/facthelpeR/contents/"
  )



script <-
  GET(
    url = "https://api.github.com/repos/IdahoAgStats/facthelpeR/inst/extdata/OSU_2019_example/auxiliary_files/contents/"
  )

stop_for_status(script)
filelist <- unlist(lapply(content(script)$tree, "[", "path"), use.names = F)

filelist <- unlist(lapply(content(script)$path, "[", "path"), use.names = F)


grep("Matteo/literature/", filelist, value = TRUE, fixed = TRUE)

