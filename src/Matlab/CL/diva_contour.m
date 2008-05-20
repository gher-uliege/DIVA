% -------------------------------------------------------------------------
% diva_contour.m
% --------------
%
% plot the contour
%
%
% c. troupin, December 2006
% modified January 2008
% -------------------------------------------------------------------------

fileout_cont = [dir.figures,casename,'_contour'];

% grid parameters
%----------------


gridinfo = textread([dir.output,'GridInfo.dat']);
xorigin = gridinfo(1);
yorigin = gridinfo(2);
dx = gridinfo(3);
dy = gridinfo(4);
xend = xorigin+(gridinfo(5)-1)*dx;
yend = yorigin+(gridinfo(6)-1)*dy;

x = [xorigin:dx:xend];
y = [yorigin:dy:yend];


% load the contour file
%----------------------

cont=dlmread([dir.input,'coast.cont']);

% figure initialization
%----------------------

if is_mmap == 1,
    figure;
    m_proj('mercator',...
           'lon',[xorigin xend],...
           'lat',[yorigin yend]);
else
    figure;
end

% plotting
%---------

display('contour plotting ...')
ncont=cont(1,1);
ioff=2;
xxx = [];
yyy = [];

for i=1:ncont
    np=cont(ioff,1);
    xx=cont(ioff+1:ioff+np,1);
    yy=cont(ioff+1:ioff+np,2);

    % to close the contours
    xx = [xx;xx(1)];
    yy = [yy;yy(1)];
    
    xxx = [xxx;NaN;xx];
    yyy = [yyy;NaN;yy];

    ioff=ioff+np+1;    
    clear np xx yy
end
hold on;

if (is_mmap == 1),
        m_plot(xxx,yyy,'k','LineWidth',1.5);
        m_grid('box','fancy','tickdir','in');
else
    plot(xxx,yyy,'k','LineWidth',1.5);
end;

xlabel('Longitude','fontsize',14);
ylabel('Latitude','fontsize',14);

if (xend-xorigin == yend-yorigin )
    axis square
end;


print(fig.opt,[fileout_cont,fig.ext]);

display('plot finished');