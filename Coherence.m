 function [c,f]=coherence(AllData,Dt)
%Frequency-dependent coherence on a pair of variables
% input signal: with 2 columns  AllData=[sig1 sig], 
% sig1= variable 1 , sig 2= variable 2
% Dt= sampling interval in units of days
% coherence can be determined for  pairs of variables
%output= coherence (c) and frequency (f) vectors
% 
% Modified function as described by  
% Menke, W., and J. Menke (2009), Environmental Data Analysis with
% MATLAB, 288 pp., Elsevier, New York.
 
%Gopal Mululukutla, 
% December 2020
% example usage
%  sig1= WaterTemp, sig2=AirTemp
% Dt=1/48, sampling interval = 30 minutes
%sig=[sig1 sig2]
%[f,c]=coherence(sig,Dt)

%% Attribution—
%If you use it or modify it, please credit this author and Menke, W., and J. Menke (2009). 

%%
D = AllData;
[N, Ncols] = size(D);
N=2*floor(N/2);
Nf = N/2+1;
fny = 1/(2*Dt);
Df = fny/(N/2);
f = Df*[0:Nf-1];
%%
% bandwidth factors
lowside = 0.75;
highside =1.25;

ifig=1;

% loop over all pairs of columns, computing coherence for each
for icol = [1:(Ncols)]
for jcol = [icol+1:(Ncols)]

c = zeros(Nf,1); % coherence

% compute cross spectral density and power spectral density
% no need to normalize, since all normalizations cancel
u = fft( D(1:N,icol) ); % fft
v = fft( D(1:N,jcol) );
u = u(1:Nf); % delete negative frequencies
v = v(1:Nf);
usv = conj(u) .* v; % cross spectral density of u and v
usu = conj(u) .* u; % power spectral density of u
vsv = conj(v) .* v; % power spectral density of v

% average over band
usva = zeros(Nf,1);
usua = zeros(Nf,1);
vsva = zeros(Nf,1); 
for i = 1:Nf
    fi = Df*(i-1); % center frequency
    flow = lowside*fi;
    fhigh = highside*fi;
    ilow = floor(flow/Df)+1;
    ihigh = floor(fhigh/Df)+1;
    if( ihigh==ilow)
        ihigh=ilow+1;
    end
    if (ilow < 1)
        ilow=1;
    elseif (ilow > Nf)
        ilow=Nf;
    end
    if (ihigh < 1)
        ihigh=1;
    elseif (ihigh > Nf)
        ihigh=Nf;
    end
    usva = mean( usv(ilow:ihigh) );
    usua = mean( usu(ilow:ihigh) );
    vsva = mean( vsv(ilow:ihigh) );
    c(i) = (conj(usva)*usva) / (usua * vsva) ;
end

% % plot coherence
% figure(ifig);
% % axis( [0, 1/5, 0, 1.1] );
% plot( f, c, 'k-', 'LineWidth', 2 );
% set(gca,'LineWidth',2);
% 
% xlabel('frequency, cycles per day','Fontsize',20);
% ylabel('coherence','Fontsize',20);
% set(gca,'Fontsize',20)
% % title(sprintf('%s and %s',char(names(icol)),char(names(jcol))));
% 
% ifig=ifig+1;

end
end




