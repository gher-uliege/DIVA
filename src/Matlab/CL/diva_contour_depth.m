% -------------------------------------------------------------------------
% diva_contour_depth 
% ------------------
%
% plot the contour for each depth
%
% c. troupin, July 2007
% adapted January 2008
% ---------------------------------------



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




% load contour and depth files
%-----------------------------

depth = load([dir.input,'contour.depth']);
Ndepth = length(depth);


% projection
%-----------

figure(Ndepth+1);

if (is_mmap == 1)
    m_proj('mercator',...
           'lon',[xorigin xend],...
           'lat',[yorigin yend]);
end;

tic

%--begin loop on the depths       
for k = 1:Ndepth,
    
    % projection
    %-----------

    
    figure(k)
    
    if (is_mmap == 1)
        m_proj('mercator',...
           'lon',[xorigin xend],...
           'lat',[yorigin yend]);
    end;
   
       
    if k<10,
        contour_file_name = [dir.output,'coast.cont.1000',num2str(k)];
    else
        contour_file_name = [dir.output,'coast.cont.100',num2str(k)];
    end;
    
    display('loading contour...');
    cont = dlmread(contour_file_name);

    % contour plotting
    % ----------------

    display('contour plotting...')
    
    ncont=cont(1,1);
    ioff=2;
    xxx = [];
    yyy = [];
    
%-----begin loop on the contours
    for i=1:ncont
        
        
        np=cont(ioff,1);
        xx=cont(ioff+1:ioff+np,1);
        yy=cont(ioff+1:ioff+np,2);

        % to close the contours
        xx = [xx;xx(1)];
        yy = [yy;yy(1)];

        ioff=ioff+np+1;
        
        xxx = [xxx;NaN;xx];
        yyy = [yyy;NaN;yy]; 
                  
        clear np xx yy
    end
       
%-----end loof on the contours

    % plotting
    %---------
    
    figure(k)
    if (is_mmap == 1),
        m_plot(xxx,yyy,'k','LineWidth',1);
        m_grid('box','fancy','tickdir','in');
    else
        plot(xxx,yyy,'k','LineWidth',1);
    end;
        
    xlabel('Longitude','fontsize',14);
    ylabel('Latitude','fontsize',14);

    % prepare axes
    if (xend-xorigin == yend-yorigin )
        axis square
    end;
    
    % write a title
    titlename = ['Depth = ',num2str(depth(k)),' m'];
    title(titlename,'fontsize',14);
    
    fileout = [dir.figures,casename,'_contour_',num2str(depth(k))]
    print(fig.opt,[fileout,fig.ext]);
    close
    
    % figure with all the contours
    figure(Ndepth+1);
    if (is_mmap == 1),
        m_plot(xxx,yyy,'k','LineWidth',1);
        m_grid('box','fancy','tickdir','in');
    else
        plot(xxx,yyy,'k','LineWidth',1);
    end;
    hold on;

end    
%--end loop on the depths      


figure(Ndepth+1);
xlabel('Longitude','fontsize',14);
ylabel('Latitude','fontsize',14);

% prepare axes
if (xend-xorigin == yend-yorigin )
    axis square
end;

fileout = [dir.figures,casename,'_contour_all']
print(fig.opt,[fileout,fig.ext]);
close all;
    
    
toc
display('plots finished');
