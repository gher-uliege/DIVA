% -------------------------------------------------------------------------
% diva_data.m
% --------------
%
% plot the data
%
%
% c. troupin, December 2006
% modified January 2008
% -------------------------------------------------------------------------

fileout_data = [dir.figures,casename,'_data'];


% contour plotting
%-----------------

diva_contour


% load the data file
%----------------------

data = dlmread([dir.input,'data.dat']);

% check if no more than one data at each location
%------------------------------------------------

[position,i1,j1] = unique(data(:,1:2),'rows');
lon_u = data(i1,1);
lat_u = data(i1,2);
data_u = data(i1,3);

% plotting
%---------

display('data plotting');

if (is_mmap == 1),
    [xx,yy]=m_ll2xy(lon_u,lat_u);
    scatter(xx,yy,2,data_u,'filled');
else
    scatter(lon_u,lat_u,50,data_u,'filled');
end;

h=colorbar;
set(get(h,'title'),'string',[var.abbrev,' ',var.units],'FontSize',14);


% print(fig.opt,[fileout1,fig.ext]);

display('plot finished');

print(fig.opt,[fileout_data,fig.ext]);