% -----------------------------------
% PROGRAM diva_analysis (GUI version)
% plot the analysed field
% c. troupin, december 2006
% -----------------------------------

this_directory = pwd;

% what is the name of the analysis file?

Name = 'adriatic'


% INPUTS
% ------
% - ContFileName is the name of the contour file
% - AnalysisFileName is the name of the data file
% - GridInfo.dat contains the grid parameters:
%   x,y originin, dx, dy, x, y end.

% Modify the file names if necessary

ContFileName = [Name,'.cont'];
NameMesh = [Name,'.mesh'];
NameMesh2 = [Name,'.mesh.mh4'];
AnalysisFileName = [Name,'.anl'];

% OUTPUTS
% -------
% - fileout is the name of the analysis field plot
% - format_out --> format of the plot:
% format_out = 1 --> jpeg format
% format_out = 2 --> eps format

format_out = 1;

% name of the analysis output files 
fileout1 = [Name,'_analysis'];
fileout2 = [Name,'_analysis_mesh'];




% ---------------------------------------------------------------
% ---------------------------------------------------------------

cd ../../../GUIwork
cd ../ANALYSIS
[flag,c4,imax,jmax,kmax,valex,nbmots] = uread(AnalysisFileName);
gridinfo = textread('GridInfo.dat');
cd ../CONT
cont =  dlmread(ContFileName);
cd ../MESH
mesh=dlmread(NameMesh);
ndon=dlmread(NameMesh2);
inode=mesh(1:ndon(1),1);
xnode=mesh(1:ndon(1),2);
ynode=mesh(1:ndon(1),3);
cd ../PLOT


% contour plotting
% ----------------

display('contour plotting ...')

ncont=cont(1,1);

ioff=2;
for i=1:ncont
    np=cont(ioff,1);
    xx=cont(ioff+1:ioff+np,1);
    yy=cont(ioff+1:ioff+np,2);

    % to close the contours
    xx = [xx;xx(1)];
    yy = [yy;yy(1)];

    ioff=ioff+np+1;
    hold on
    plot(xx,yy,'k','LineWidth',2)
    fill(xx,yy,[0.7,0.7,.7])
    clear np xx yy
end
hold on;


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

xorigin = gridinfo(1)
yorigin = gridinfo(2)
dx = gridinfo(3)
dy = gridinfo(4)
xend = gridinfo(5)
yend = gridinfo(6)

x = [xorigin:dx:xend];
y = [yorigin:dx:yend];


pcolor(x,y,gg')
shading('flat')
xlabel('Longitude ( ^{\circ} E)','fontsize',14)
ylabel('Latitude ( ^{\circ} N)','fontsize',14)
title('Analysis','fontsize',14)
axis([xorigin xend yorigin yend])
colorbar
colormap(jet)



if (format_out == 1),
    print('-djpeg',[fileout1,'.jpg'])
else 
    print('-depsc',[fileout1,'.eps'])
end;

% --------------------
% plot the mesh 
% --------------------

display('mesh plotting')

ioff=ndon(1)+ndon(2);
for i=1:ndon(3)
    i1=mesh(ioff+i,1);
    i2=mesh(ioff+i,3);
    i3=mesh(ioff+i,5);
    xt(1)=xnode(i1);
    yt(1)=ynode(i1);
    xt(2)=xnode(i2);
    yt(2)=ynode(i2);
    xt(3)=xnode(i3);
    yt(3)=ynode(i3);
    xt(4)=xnode(i1);
    yt(4)=ynode(i1);
    hold on
    plot(xt,yt,'k','LineWidth',1)
end
title('Analysis and Mesh','fontsize',14);

if (format_out == 1),
    print('-djpeg',[fileout2,'.jpg'])
else 
    print('-depsc2',[fileout2,'.eps'])
end;


% --------------------------------------------
display('your plots are finished')
cd(this_directory);
clear, close all
