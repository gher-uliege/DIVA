

% load files
%-----------

divacova=dlmread([dir.output,'covariance.dat'],' ');
divacovafit=dlmread([dir.output,'covariancefit.dat'],' ');
FID=fopen([dir.output,'paramfit.dat'],'r');


% read in paramfit.dat
%---------------------

FF=fgetl(FID);
RLS=fgetl(FID);
FF=fgetl(FID);
VARS=fgetl(FID);
FF=fgetl(FID);
VARS=fgetl(FID);
fclose(FID);



plot(divacova(:,1),divacova(:,2),divacova(:,1),divacova(:,2)+divacova(:,4),'r-.',divacova(:,1),divacova(:,2)-divacova(:,4),'r-.')
hold on
plot(divacovafit(:,1),divacovafit(:,2),'o',divacovafit(:,1),divacovafit(:,3))
%legend('data used for fitting','fitted curve')

relweight=divacovafit(:,4)/max(divacovafit(:,4))*divacovafit(1,2)

plot(divacovafit(:,1),relweight,'g.')
legend('data covariance','error on covariance',' ','data used for fitting','fitted curve', 'relative weight during fitting')

hold on


VAR=str2num(VARS)
RL=str2num(RLS)
plot(divacova(:,1),VAR*divacova(:,1)/RL.*BesselK(1,divacova(:,1)/RL))

%axis([0 max(divacova(:,1))*0.8 -divacovafit(1,2)*2 divacovafit(1,2)*2] )
