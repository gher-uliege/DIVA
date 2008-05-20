% ---------------------------------
% PROGRAM diva_contour (CL version)
% plot the data and the contour
% c. troupin, december 2006
% ---------------------------------

this_directory = pwd;

% Name of the output file ?

Name = 'output'

% OUTPUTS
% -------
% - fileout1 is the name of the contour plot
% - fileout2 is the name of the (contour + data) plot
% - format_out --> format of the plot:
% format_out = 1 --> jpeg format
% format_out = 2 --> eps format

format_out = 1;

fileout1 = [Name,'_contour'];
fileout2 = [Name,'_data_contour'];


% ---------------------------------------------------------------
% ---------------------------------------------------------------

cd ../../../divastripped/

cd ./input
data = dlmread('data.dat');
cont = dlmread('coast.cont');


% contour plotting
% ----------------

display('contour plotting...')

ncont=cont(1,1);
ioff=2;

xmin = cont(3,1);
xmax = cont(3,1);
ymin = cont(3,2);
ymax = cont(3,2);

for i=1:ncont
    np=cont(ioff,1);
    xx=cont(ioff+1:ioff+np,1);
    yy=cont(ioff+1:ioff+np,2);

    % to close the contours
    xx = [xx;xx(1)];
    yy = [yy;yy(1)];

    ioff=ioff+np+1;
%    hold on
    pause
    np=np
    plot(xx,yy,'k','LineWidth',1.5)
%    if (i>1)
         fill(xx,yy,[.9,.9,.9])
%    end

    if (min(xx) < xmin),
        xmin = min(xx);
    end;
    if (max(xx) > xmax),
        xmax = max(xx);
    end;
    if (min(yy) < ymin),
        ymin = min(yy);
    end;
    if (max(yy) > ymax),
        ymax = max(yy);
    end;
    
    clear np xx yy
end
hold on


% xlabel('Longitude ( ^{\circ} E)','fontsize',14)
% ylabel('Latitude ( ^{\circ} N)','fontsize',14)
title('Contour','fontsize',14)
axis([xmin xmax ymin ymax]);
grid on;
hold on


% --------------------------------------------------------
display('Your plots are finished')
cd(this_directory);
%clear, close all
