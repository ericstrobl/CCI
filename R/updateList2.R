updateList2 <- function (path, set, old.list) 
{
  c(old.list, lapply(set, function(s) c(path, s)))
}