max_chroma <- function(h, l, floor = FALSE) {
  ## align h and l
  n <- max(c(length(h), length(l)))
  h <- rep_len(h, n)
  l <- rep_len(l, n)

  ## assure h in [0, 360]
  while(any(h < 0)) h[h < 0] <- h[h < 0] + 360
  while(any(h >= 360)) h[h >= 360] <- h[h >= 360] - 360

  ## assure l in [0, 100]
  l <- pmin(100, pmax(0, l))

  ## obtain surrounding h/l coordinates
  hmin <- floor(h + 1e-8)
  hmax <- ceiling(h + 1e-8)
  lmin <- floor(l + 1e-8)
  lmax <- ceiling(l + 1e-8)

  ## average
  c <- (hmax - h) * (lmax - l) * colorspace::max_chroma_table[paste(hmin, lmin, sep = "-")] + 
       (hmax - h) * (l - lmin) * colorspace::max_chroma_table[paste(hmin, lmax, sep = "-")] + 
       (h - hmin) * (lmax - l) * colorspace::max_chroma_table[paste(hmax, lmin, sep = "-")] + 
       (h - hmin) * (l - lmin) * colorspace::max_chroma_table[paste(hmax, lmax, sep = "-")]

  ## catch border cases
  c <- as.numeric(c) # pmin(c, 100)
  c[l <= 0 | l >= 100] <- 0
  
  ## take floor to be "on the safe side"
  if(floor) c <- floor(c)
  
  return(c)
}
