# 404

You can set the 404 page in two ways.

```r
app$not_found <- function(req, res){
  res$send(htmltools::h2("404"), status = 404L)
}

app$set_404(function(req, res){
  res$send(htmltools::h2("Not found"), status = 404L)
})
```
