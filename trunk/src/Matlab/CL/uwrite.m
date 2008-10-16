% -----------------------------------------------------
% ------------ GHER file : write function --------------
% ------------ for MATLAB routines --------------------
% ------------ M. Rixen 2000 --------------------------

function [flag]=uwrite(file,c4,imax,jmax,kmax,valex,nbmots)

flag=0;
file;
dummy=0;
dummyf=0.;
dummy24=24;
iprec=4;
fid=fopen(file,'w','native');
if fid~=-1
    ind=find(isnan(c4));
    c4(ind)=valex;
    if nbmots==-1
        nbmots=imax*jmax*kmax
    end
    for i=1:10
        fwrite(fid,dummy,'int32');
        fwrite(fid,dummy,'int32');
    end
    nl=fix((imax*jmax*kmax)/nbmots);
    ir=imax*jmax*kmax-nbmots*nl;
    dummyval2=4*nbmots;
    fwrite(fid,dummy24,'int32');
    fwrite(fid,imax,'int32');
    fwrite(fid,jmax,'int32');
    fwrite(fid,kmax,'int32');
    fwrite(fid,iprec,'int32');
    fwrite(fid,nbmots,'int32');
    fwrite(fid,valex,'single');
    fwrite(fid,dummy24,'int32');
    if imax<0 | jmax<0 | kmax<0
        nl=0;
        ir=4;
        disp('Degenerated matrix');
    end
    ide=1;
    for kl=1:nl
        fwrite(fid,4*nbmots,'int32');
        fwrite(fid,c4(ide:ide+nbmots-1),'single');
        fwrite(fid,4*nbmots,'int32');
        ide=ide+nbmots;
    end
    fwrite(fid,4*ir,'int32');
    fwrite(fid,c4(ide:ide+ir-1),'single');
    fwrite(fid,4*ir,'int32');
    flag=1;
    fclose(fid);
end

