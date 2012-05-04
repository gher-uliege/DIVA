% -------------------------------------------------------------------------
% diva_topo_gebco.m
% -----------------
%
% Writes a GEBCO bathymetry into the GHER format
%
%
% F. Lenartz, November 2007
% Adapted: ctroupin, January 2008
% -------------------------------------------------------------------------


fname = char(ls([dir.input,'*gebco*']));
file_in = [dir.input,fname];
fileout_topo = [dir.figures,casename,'_topo_gebco'];


% open file and read in it
%-------------------------

fid=fopen(file_in);

for kk=1:44
    eval(['A_' num2str(kk)  '=fscanf(fid,' '''' '%s' '''' ',1);']);
end

x=[str2num(A_22):1/60:str2num(A_29)]';
y=[str2num(A_36):1/60:str2num(A_43)]';

clc

data=textread(file_in,'','headerlines',11);
a=fliplr(reshape(data(:,3),size(x,1),size(y,1)));
a = -a;         % to have the good sign;
fclose(fid);

% write into gher-format file
%----------------------------

uwrite([dir.input,'topo.grd'],a,size(x,1),size(y,1),1,Inf,size(x,1));


% inverse topography sign 
%------------------------

%diva_inversetopo(dir.input);

% construct TopoInfo.dat
%-----------------------

fidinfo=fopen([dir.input,'TopoInfo.dat'],'W');
fprintf(fidinfo,'%f\n',str2num(A_22));
fprintf(fidinfo,'%f\n',str2num(A_36));
fprintf(fidinfo,'%f\n',1/60);
fprintf(fidinfo,'%f\n',1/60);
fprintf(fidinfo,'%i\n',length(x));
fprintf(fidinfo,'%i\n',length(y));

clear A_*;

fclose(fidinfo);

% plot the topography
%--------------------

% grid parameters
%----------------

gridinfo = textread([dir.input,'TopoInfo.dat']);
xorigin = gridinfo(1);
yorigin = gridinfo(2);
dx = gridinfo(3);
dy = gridinfo(4);
xend = xorigin+(gridinfo(5)-1)*dx;
yend = yorigin+(gridinfo(6)-1)*dy;

x = [xorigin:dx:xend];
y = [yorigin:dy:yend];


figure

if (is_mmap == 1)
    m_proj('mercator',...
           'lon',[xorigin xend],...
           'lat',[yorigin yend]);
    m_pcolor(x,y,-a');        % -a: to have positive altitude on the plot
    hold on;
    pause(.2)
    warning('off'); shading('interp');
    [xx,yy] = m_ll2xy(x,y);
    contour(xx,yy,-a',[0 0],'linecolor','k','linewidth',2);
    m_grid('box','fancy','tickdir','in');
else
    pcolor(x,y,-a');        % -a: to have positive altitude on the plot
    hold on;
    pause(.2)
    warning('off'); shading('interp');
    contour(x,y,-a',[0 0],'linecolor','k','linewidth',2);
end;

h=colorbar;
set(get(h,'title'),'string',[var.abbrev,' ',var.units],'FontSize',14);
hold on


xlabel('Longitude','fontsize',14);
ylabel('Latitude','fontsize',14);

if (abs(x(end)-x(1)) == abs(y(end)-y(1)) )
    axis square;
end;


% and print it
print(fig.opt,[fileout_topo,fig.ext]);
pause(.5);
close;