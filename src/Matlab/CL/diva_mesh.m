% -------------------------------------------------------------------------
% diva_mesh.m
% -----------
%
% plot contour and mesh
%
%
% c. troupin, December 2006
% modified: f. lenartz, November 2007
% adapted:  January 2008
% -------------------------------------------------------------------------

fileout_mesh = [dir.figures,casename,'_mesh'];

% contour plotting
%-----------------

diva_contour

% mesh plotting
% -------------

display('loading mesh files')

ndon=dlmread([dir.output,'meshtopo.dat']);
mesh=dlmread([dir.output,'mesh.dat']);

inode=mesh(1:ndon(1),1);
xnode=mesh(1:ndon(1),2);
ynode=mesh(1:ndon(1),3);

display('mesh plotting')

ioff=ndon(1)+ndon(2);
xt=zeros(3,ndon(3));
yt=zeros(3,ndon(3));
i=[1:ndon(3)]';
i1=mesh(ioff+i,1);
i2=mesh(ioff+i,3);
i3=mesh(ioff+i,5);
xt(1,i)=xnode(i1);
yt(1,i)=ynode(i1);
xt(2,i)=xnode(i2);
yt(2,i)=ynode(i2);
xt(3,i)=xnode(i3);
yt(3,i)=ynode(i3);


if (is_mmap == 1);
    [xt2,yt2] = m_ll2xy(xt,yt);
    patch(xt2,yt2,ones(3,ndon(3)),'w');
    m_grid('box','fancy','tickdir','in');
else
    patch(xt,yt,ones(3,ndon(3)),'w');
end;
hold on;    

if (xend-xorigin == yend-yorigin )
    axis square
end;

print(fig.opt,[fileout_mesh,fig.ext]);

display('plot finished');

