heat.colors <- 
function (n, alpha = 1) 
{
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n%/%4
    i <- n - j
    c(rainbow(i, start = 0, end = 1/6, alpha = alpha), if (j > 
                                                           0) hsv(h = 1/6, s = seq.int(from = 1 - 1/(2 * j), 
                                                                                       to = 1/(2 * j), length.out = j), v = 1, alpha = alpha))
  }
  else character()
}

cool.colors <-
function (n) 
{
  if ((n <- as.integer(n[1])) > 0) {
    j <- n%/%4
    i <- n - j
    c(if (j > 0) hsv(h = 3/6, s = seq(to = 1 - 1/(2 * j), 
                                      from = 1/(2 * j), length = j), v = 1), rainbow(i, 
                                                                                     start = 3/6, end = 4/6))
  }
  else character(0)
}

cool2warm.colors <- 
function (n, middle = "#FFFFFF") 
{
  n <- as.integer(n[1])
  if (n > 0) {
    j <- n%/%2
    rev(c(if (j > 0) heat.colors(j), if (2 * j < n) c(middle), 
      if (j > 0) cool.colors(j)))
  }
  else character(0)
}