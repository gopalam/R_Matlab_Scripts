Coherence <- function(signal1,signal2,Dt) {
  
  # input signal1 , signal1, vectors to analyze for coherence.
  # Dt, sampling interval (in time units  of time, e.g 15 min sampling interval, Dt=1/96.)
  #Frequency-dependent coherence on a pair of variables
  # Dt= sampling interval in units of days
  # coherence can be determined for  pairs of tims series variables
  # output= coherence (c) and frequency (f) vectors
  # 
  # written as described in  Menke, W., and J. Menke (2009), Environmental Data Analysis with
  # MATLAB, 288 pp., Elsevier, New York.
  

  
  N=length(signal1);
  
  # round off to even number of points
  N=floor(N/2)*2;
  signal1=signal1[1:N];
  signal2=signal2[1:N];
  
  
  
  N=2*floor(N/2);
  Nf = N/2+1;
  fny = 1/(2*Dt);
  Df = fny/(N/2);
  f1 = Df
  f2=0:(Nf-1);
  f=f1*f2;
  
  # bandwidth factors
  lowside = 0.75;
  highside =1.25;
  
  
  # initialize matrix
  C = matrix( 0 ,nrow=Nf, ncol=1) 
  
  
  # compute cross spectral density and power spectral density
  # no need to normalize, since all normalizations cancel
    u = fft( signal1[1:N] ); # fft
    v = fft( signal2[1:N] ); # fft
  
  u = u[1:Nf]; # delete negative frequencies
  v = v[1:Nf];
  usv = Conj(u) * v; # cross spectral density of u and v
  usu = Conj(u) * u; # power spectral density of u
  vsv = Conj(v) * v; # power spectral density of v
  
  # average over band
  
  usva=matrix( 0 ,nrow=Nf, ncol=1) 
  usua=matrix( 0 ,nrow=Nf, ncol=1) 
  vsva=matrix( 0 ,nrow=Nf, ncol=1) 
  
  
  
  
  for (i in 1:Nf) {
    
    fi = Df*(i-1); # center frequency
    flow = lowside*fi;
    fhigh = highside*fi;
    ilow = floor(flow/Df)+1;
    ihigh = floor(fhigh/Df)+1;
    
    
    
    if (ilow < 1){ 
      ilow=1;
       }
    else {
      if (ilow > Nf) {
    ilow=Nf;
      }
    }
    
    
    if (ihigh < 1) {
      ihigh=1;
    }
    else {
      if (ihigh > Nf) {
    ihigh=Nf;
      }
    }
    
    # integral over frequency range
    usva = mean( usv[ilow:ihigh] );
    usua = mean( usu[ilow:ihigh] );
    vsva = mean( vsv[ilow:ihigh] );
    C[i] = (Conj(usva)*usva) / (usua * vsva) ;
    
    C=Re(C);
    
    
  }
  
  
  Ch=cbind(f,C)
  
  
  return(Ch)

  
  
  
  
  
  
  
  
  
  
  
  
}
