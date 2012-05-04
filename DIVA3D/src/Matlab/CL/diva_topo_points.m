% -------------------------------------------------------------------------
% diva_topo_points.m
% -----------------
%
% Plot points of bathymetry measurements from TOPEX
%
%
% ctroupin, July 2007
% Adapted: January 2008
% -------------------------------------------------------------------------

file_in = [dir.input,'topo.dat'];
fileout_topo = [dir.figures,casename,'_topo_points'];


% load the points
%----------------

data = load(file_in);
x = data(:,1);
y = data(:,2);
depth = data(:,3);

% limits of the domain
%---------------------

xorigin = min(x);
xend = max(x);
yorigin = min(y);
yend = max(y);

% plot 
%-----

figure

if (is_mmap == 1)
    m_proj('mercator',...
           'lon',[xorigin xend],...
           'lat',[yorigin yend]);
    [xx,yy]=m_ll2xy(x,y);
    scatter(xx,yy,5,depth,'filled');
    m_grid('box','fancy','tickdir','in');
    %m_contour(xx,yy,depth,[0 0],'linecolor','k','linewidth',2);
else
    scatter(x,y,5,depth,'filled');
    %contour(x,y,depth,[0 0],'linecolor','k','linewidth',2);   
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




