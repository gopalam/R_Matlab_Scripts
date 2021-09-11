SignalFilter <- function(data,Dt,bandlimits) {

# %% a simple digital filter 
# %input 
# % data = signal. note, the length of the signal is truncated to nearest even number.
# % Dt=  sampling frequency, in unit of days, example 15 min == 1/96
# % bandlimits, band of frequency within which to pass the signal. in units of time ( not frequency!)
# % eg.  bandlimits=c(0, 2/24) ; filter for signal by keeping periodicities within 0 days and 2 hours
# % eg. bandlimits=c(33/24,Inf); filter for signal by keeping periodicities within 33 hours  and infinity
# usage  Rec=SignalFilter(data,Dt,bandlimits)
# 
# % Gopal Mulukutla, November 2014.


#   % determine the length of the variable
N=length(data);

# % round off to even number of points
# % remember that FFT runs on even number of samples

N=floor(N/2)*2;
# % limit the data to N
data=data[1:N];

# %%  time/frequency set up
# % Determine Nyquist frequency (fmax)
fmax=1/(2.0*Dt);

# % determine the sampling frequency
Df=fmax/(N/2);
# % Create a vector with sampling frequencies
f1a=(0:(N/2));
f1b=(((-N/2)+1):-1);
# bind
f1=c(f1a, f1b);
f=Df*(f1);
Nf=(N/2)+1;
# %% Filtering
# % in units of time (in this case days)
# %Fourier transform and associated frequencies

amplitudes = fft(data);
# %Generate a list of frequencies, all positive values

frequencies=abs(f);

# %Computes periods and decide of the ones to keep

periods=1./frequencies;

PeriodsToPass=which(periods >= bandlimits[1] & periods <= bandlimits[2]);
# % create empty field to write new amplitudes
FiltAmps=amplitudes*0;
# %Keep the desired periods (amplitudes)
FiltAmps[PeriodsToPass]=amplitudes[PeriodsToPass];
# %Reconstruct the signal
Recon=fft(FiltAmps, inverse = TRUE)/length(FiltAmps)
Recon=Re(Recon)
return (Recon)
}
