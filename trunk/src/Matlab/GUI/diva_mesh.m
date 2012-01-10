% -------------------------------
% PROGRAM diva_mesh (GUI version)
% plot the data and the mesh
% c. troupin, december 2006
% -------------------------------

this_directory = pwd;

% what is the name of the mesh file?

Name = 'adriatic'

% INPUTS
% ------
% - DataFileName is the name of the data file
% - ContFileName is the name of the contour file
% - NameMesh is the name of the mesh file

% Modify the file name if necessary

DataFileName = [Name,'.dat'];
ContFileName = [Name,'.cont'];
NameMesh = [Name,'.mesh'];
NameMesh2 = [Name,'.mesh.mh4'];

% OUTPUTS
% -------
% - fileout1 is the name of the mesh plot
% - fileout2 is the name of the (mesh + data) plot
% - format_out --> format of the plot:
% format_out = 1 --> jpeg format
% format_out = 2 --> eps format

format_out = 1;

fileout1 = [Name,'_mesh'];
fileout2 = [Name,'_mesh_data'];


% ---------------------------------------------------------------
% ---------------------------------------------------------------

cd ../../../GUIwork
cd ./CONT
cont=dlmread(ContFileName);
cd ../MESH
mesh=dlmread(NameMesh);
ndon=dlmread(NameMesh2);
inode=mesh(1:ndon(1),1);
xnode=mesh(1:ndon(1),2);
ynode=mesh(1:ndon(1),3);
cd ../DATA
data =  dlmread(DataFileName);
cd ../PLOT



% --------------------
% plot of the contour
% --------------------
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
     if (i>1),
         fill(xx,yy,[0.7,0.7,.7])
     end;
    clear np xx yy
end

% --------------------
% plot the mesh 
% --------------------

display('mesh plotting')

figure(1)
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
    axis([min(xnode) max(xnode) min(ynode) max(ynode)])
end

xlabel('Longitude (E)','fontsize',14)
ylabel('Latitude (N)','fontsize',14)
title('Mesh','fontsize',14)
hold on

if (format_out == 1),
    print('-djpeg',[fileout1,'.jpg'])
else 
    print('-depsc2',[fileout1,'.eps'])
end;


% --------------------
% plot the data
% --------------------

display('data plotting');

scatter(data(:,1),data(:,2),15,data(:,3),'filled')
xlabel('Longitude ( ^{\circ} E)','fontsize',14)
ylabel('Latitude ( ^{\circ} N)','fontsize',14)
title('Mesh and Data','fontsize',14)
colorbar

if (format_out == 1),
    print('-djpeg',[fileout2,'.jpg'])
else 
    print('-depsc2',[fileout2,'.eps'])
end;

% --------------------------------------------------------
display('Your plots are finished')
cd(this_directory);
clear, close all
