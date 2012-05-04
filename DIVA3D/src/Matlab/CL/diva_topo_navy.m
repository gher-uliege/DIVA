% diva_topo_navy
%---------------

clear, close all, clc

Name = 'Corsica';                    % case name 
fileout = [Name,'_topo_navy'];       % name of the output figure

% load the files
%---------------

[flag,c4,imax,jmax,kmax,valex,nbmots] = uread('topo.grd');
gridinfo = textread('topoInfo.dat');

% analysis plotting
% -----------------

display('analysed field plotting ...')
gg=reshape(c4,imax,jmax);
for i=1:imax
    for j=1:jmax
        if gg(i,j)==valex 
            gg(i,j)=NaN;
        end
    end
end

% grid parameters

lonmin = gridinfo(1);
latmin = gridinfo(2);
dlon = gridinfo(3);
dlat = gridinfo(4);
lonmax = lonmin+(gridinfo(5)-1)*dlon;
latmax = latmin+(gridinfo(6)-1)*dlat;

lon = [lonmin:dlon:lonmax];
lat = [latmin:dlat:latmax];


% projection 
%-----------
figure
m_proj('mercator',...
       'lon',[lonmin lonmax],...
       'lat',[latmin latmax]);


m_pcolor(lon,lat,-gg');
shading interp

% extraction of coastlines
%-------------------------

m_gshhs_h('patch',[.7 .7 .7],'linewidth',1.5,'edgecolor','k');
hold on;

% gridding
%---------

m_grid('box','fancy','tickdir','in');
h=colorbar;
set(get(h,'title'),'string','Depth (m)','FontSize',14);



% printing
%---------

print('-djpeg100',fileout)
print('-depsc2',fileout)
close