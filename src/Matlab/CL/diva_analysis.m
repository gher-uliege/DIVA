% -------------------------------------------------------------------------
% diva_analysis.m
% ---------------
%
% plot the analyzed field
%
%
% c. troupin, December 2006
% modified January 2008
% -------------------------------------------------------------------------

fileout_ana = [dir.figures,casename,'_analysis'];

data = dlmread([dir.input,'data.dat']);
[flag,c4,imax,jmax,kmax,valex,nbmots] = uread([dir.output,'fieldgher.anl']);


% contour plotting
%-----------------

diva_contour

% analysis plotting
% -----------------

display('analyzed field plotting ...')

gg=reshape(c4,imax,jmax);
filter = find(gg==valex);
gg(filter) = NaN;

if (is_mmap == 1);
    m_pcolor(x,y,gg');
    warning('off'); shading('interp');
    m_gshhs_h('patch',[.7 .7 .7],'linewidth',1.5,'edgecolor','k');
    hold on;
    m_grid('box','fancy','tickdir','in');
else
    pcolor(x,y,gg');
    warning('off'); shading('interp');
    axis([xorigin xend yorigin yend]);
end;

xlabel('Longitude','fontsize',14);
ylabel('Latitude','fontsize',14);

h1=colorbar;
set(get(h1,'title'),'string',[var.abbrev,' ',var.units],'FontSize',14);

if (xend-xorigin == yend-yorigin )
    axis square
end;

print(fig.opt,[fileout_ana,fig.ext]);

display('plot finished');
