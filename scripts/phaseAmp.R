phaseAmp <- function(loadings,pcs){
  eof.real <- Re(loadings)
  eof.im <- Im(loadings)
  pc.real <- Re(pcs)
  pc.im <- Im(pcs)
  
  S.amp <- Re(loadings * Conj(loadings))
  ph.s <- atan2(eof.im,eof.real)
  ph.s <- ph.s*180/pi
  
  t.amp <- Re(pcs * Conj(pcs))
  ph.t <- atan2(pc.im,pc.real)
  ph.t <- ph.t*180/pi
  
  list(spatial.amp = S.amp, spatial.phase = ph.s, temp.amp = t.amp, temp.phase = ph.t)
}