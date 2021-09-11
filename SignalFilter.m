function [Recon] = SignalFilter(data,Dt,bandlimits)
%% a simple digital filter 
%input 
% data = signal. note, the length of the signal is truncated to nearest even number.
% Dt=  sampling frequency, in unit of days, example 15 min == 1/96
% bandlimits, band of frequency within which to pass the signal. in units of time ( not frequency!)
% eg. [0  2/24] ; filter for signal by keeping periodicities within 0 days and 2 hours
% eg. [33/24  inf] ; filter for signal by keeping periodicities within 33 hours  and infinity

% Gopal Mulukutla, November 2014.

% edit
% modified to return the same length as the signal, by padding the signal with zeros to the next even number and returning only the values
% for the original signal
% December 2014

%%
% determine the length of the variable
N1=length(data);

if mod(N1,2) == 0
  %number is even
  % do nothing
else
  %number is odd
 data=[data;0]; 
end
N=length(data);
% % round off to even number of points
% % remember that FFT runs on even number of samples
% N=floor(N/2)*2;
% % limit the data to N
% data=data(1:N);
%%  time/frequency set up
% Determine Nyquist frequency (fmax)
fmax=1/(2.0*Dt);
% determine the sampling frequency
Df=fmax/(N/2);
% Create a vector with sampling frequencies
f=Df*[0:N/2,-N/2+1:-1]'; 
%% Filtering
% in units of time (in this case days)
%Fourier transform and associated frequencies
amplitudes = fft(data);
%Generate a list of frequencies, all positive values
frequencies=abs(f);
%Computes periods and decide of the ones to keep
periods=1./frequencies;
PeriodsToPass=(periods>=bandlimits(1) & periods<=bandlimits(2));
% create empty field to write new amplitudes
FiltAmps=amplitudes*0;
%Keep the desired periods (amplitudes)
FiltAmps(PeriodsToPass)=amplitudes(PeriodsToPass);
%Reconstruct the signal
Recon=ifft(FiltAmps);

if mod(N1,2) == 0
  %number is even
  % do nothing
else
  %number is odd
 Recon=Recon(1:N1); 
end

end
