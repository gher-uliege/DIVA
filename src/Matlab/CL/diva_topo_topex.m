% -------------------------------------------------------------------------
% diva_topo_topex.m
% -----------------
%
% Plot bathymetry created by using
% individual measurements from TOPEX
% and diva tool 'divatopo'
%
%
% ctroupin, July 2007
% Adapted: January 2008
% -------------------------------------------------------------------------

file_in = [dir.output,'topo.grd'];
fileout_topo = [dir.figures,casename,'_topo_topex'];


% load the topography
%--------------------

[flag,c4,imax,jmax,kmax,valex,nbmots] = uread(file_in);

% grid parameters
%----------------

gridinfo = textread([dir.output,'TopoInfo.dat']);
xorigin = gridinfo(1);
yorigin = gridinfo(2);
dx = gridinfo(3);
dy = gridinfo(4);
xend = xorigin+(gridinfo(5)-1)*dx;
yend = yorigin+(gridinfo(6)-1)*dy;

x = [xorigin:dx:xend];
y = [yorigin:dy:yend];


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



% plot 
%-----

figure

if (is_mmap == 1)
    m_proj('mercator',...
           'lon',[xorigin xend],...
           'lat',[yorigin yend]);
    m_pcolor(x,y,gg');
    hold on;
    pause(.2)
    warning('off'); shading('interp');
    m_grid('box','fancy','tickdir','in');
    [xx,yy] = m_ll2xy(x,y); 
    contour(xx,yy,gg',[0 0],'linecolor','k','linewidth',2);
else
    pcolor(x,y,gg');
    pause(.2)
    warning('off'); shading('interp');
    contour(x,y,gg',[0 0],'linecolor','k','linewidth',2);   
end;

h=colorbar;
set(get(h,'title'),'string',[var.abbrev,' ',var.units],'FontSize',14);
hold on

xlabel('Longitude','fontsize',14);
ylabel('Latitude','fontsize',14);

if (abs(x(end)-x(1)) == abs(y(end)-y(1)) )
    axis square;
end;

% printing
%---------

print(fig.opt,[fileout_topo,fig.ext]);
pause(.5);
close;

