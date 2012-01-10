function[topo_out] = diva_inversetopo(input_dir)
% diva_inversetopo
% inverse the sign of the topography
% in order to be compatible with DIVA conventions
% 
% input: name of the file containing the topography to invert
% output: name of the file 
%
% F. Lenartz, November 2007
% Adapted: ctroupin, January 2008
%--------------------------------------------------------------------------


if (nargin == 0),
    display('Inverting topography file topo.grd');
    display('and writing it into topo.grd.new');
    topo_in = '.\';
else 
    if (nargin > 2),
    display('Example of use:')
    display('  ')
    display('diva_inversetopo(directory_name)')
    end;
end;

if (nargout == 0),
    topo_out = 'topo.grd';
end;
    
    
[flag,c4,imax,jmax,kmax,valex,nbmots] = uread([input_dir,'topo.grd']);
gridinfo=textread([input_dir,'TopoInfo.dat']);
gg=reshape(c4,imax,jmax);
for i=1:imax
    for j=1:jmax
        if gg(i,j)==valex 
            gg(i,j)=NaN;
        end
    end
end

gg=-gg;
uwrite([input_dir,topo_out],gg,imax,jmax,kmax,Inf,imax);


%eof