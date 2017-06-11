eof.latlon <- function(lat, data, neof=30, alpha=0.05) {
  
  # COMPUTE PRINCIPAL COMPONENTS OF A DATA ARRAY.  
  
  # INPUT:
  #	LON[NLON]: VECTOR SPECIFYING LONGITUDES
  #	LAT[NLAT]: VECTOR SPECIFYING LATITUDES
  #	DATA.ARRAY[NLON,NLAT,NTOT] OR DATA.ARRAY[NLON*NLAT,NTOT]:  
  #	  THE DATA ARRAY IN [SPACE1,SPACE2,TIME] OR [SPACE,TIME] FORMAT 
  #	NEOF = NUMBER OF SPATIAL EOFS (WITH MASK) TO BE OUTPUTTED.  DEFAULT = 30
  #   IDATE = A VECTOR INDICATING THE STARTING TIME AND INCREMENT (E.G., IDATE = C(1, "JAN", 1979, "1MO") )
  #   DNAME = NAME OF DATA (E.G., HADSST, ERSSTV, NCEP/NCAR REAN TEMP, ETC)
  #   ALPHA = SIZE OF THE CONFIDENCE INTERVAL OF EXPLAINED VARIANCES: (1-ALPHA)*100%
  
  # OUTPUT LIST:
  #	$EOF[NLON*NLAT,NEOF]: THE FIRST NEOF SCALED EOFS 
  #	$PC[NTOT,NEOF]: THE PCS, NORMALIZED TO UNIT VARIANCE 
  #	$FEXPVAR: FRACTION OF EXPLAINED VARIANCE FOR EACH EOF (ALL OF THEM).
  #	$FEXPVAR.CI: CONFIDENCE INTERVALS FOR FRACTION OF EXPLAINED VARIANCE.
  #	$EOFI[NLON*NLAT,NEOF]: PSEUDO INVERSE OF EOF (I.E.,T(EOFI) %*% EOF = I)
  #	$NEOF: MINIMUM OF (NEOF IN ARGUMENT LIST, RANK OF DATA.ARRAY)
  #	$BAD[NLON*NLAT]: LOGICAL ARRAY INDICATING POINTS DROPPED FROM THE ANALYSIS
  #	$WEIGHT[NLON*NLAT]: THE WEIGHTS USED TO COMPUTE THE EOF
  #	$LON: THE LONGITUDES OF THE SPATIAL FIELD
  #	$LAT: THE LATITUDES OF THE SPATIAL FIELD
  #	$SVAL: THE SINGULAR VALUES OF THE SCALED ANOMALIES
  
  # DEFINE PARAMETERS
  #nlon = length(lon)
  #nlat = length(lat)
  ntot <- dim(data)[2] #length(data.array)/(nlon*nlat)
  #print(ntot)
  #if ( length(data.array) %% nlon*nlat != 0 ) stop("data.array dimension not integral multiple of nlon*nlat")
  
  # DEFINE WEIGHTING
  #lat <- attr(data,"lat")[attr(data,"iys")]
  weight <- matrix(sqrt(cos(lat*pi/180)),nrow=length(lat),ncol=ntot)
  #print(dim(weight))
  
  
  # REMOVE CLIMATOLOGY
  clim <- rowMeans(data)
  data <- data - clim
  
  # NORMALIZE/WEIGHT THE VARIABLES
  
  
  data <- data * weight
  
  # COMPUTE SVD
  mmin = min(c(dim(data),neof))
  data.svd = svd(data,nu=mmin,nv=mmin)
  
  # COMPUTE FEXPVAR
  fexpvar = data.svd$d^2/sum(data.svd$d^2)
  
  # COMPUTE CONFIDENCE INTERVAL OF FEXPVAR
  stderr     = qnorm(alpha/2,lower.tail=FALSE) * sqrt(2/ntot)
  fexpvar.ci = cbind (fexpvar * (1 - stderr) , fexpvar * (1 + stderr) )
  
  # COMPUTE PCS
  pc = sqrt(ntot-1)*data.svd$v[,(1:mmin)]
  
  # FILL IN MISSING POINTS IN EOF 
  eof  = array(NA,dim=c(dim(data)[1],mmin))
  eofi = array(NA,dim=c(dim(data)[1],mmin))        
  for ( n in 1:mmin ) eof[,n] = data.svd$u[,n]/weight[,1]*data.svd$d[n]/sqrt(ntot-1)
  for ( n in 1:mmin ) eofi[,n] = data.svd$u[,n]*weight[,1]/data.svd$d[n]*sqrt(ntot-1)
  
  list(eof=eof,pc=pc,
       fexpvar=fexpvar,fexpvar.ci=fexpvar.ci,
       eofi=eofi,neof=mmin,weight=weight,
       sval=data.svd$d,clim=clim)
}

