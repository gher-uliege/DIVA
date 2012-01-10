function fid=gzfopen(fname,permisson,machineformat)

global GZ_FILE_IDS GZ_FILE_NAMES

if isempty(GZ_FILE_NAMES)
  GZ_FILE_NAMES={};
end

zipped = 0;

if length(fname) > 3
  if strcmp(fname(end-2:end),'.gz') 
     zipped = 1;
  end
end

if zipped
  tmp = ['/tmp/gzfopen.' num2str(floor(rand*1e7),'%7.7i')];
  eval(['!cp ' fname '  ' tmp '.gz;   gunzip ' tmp '.gz; ']);
  fid = fopen(tmp,permisson,machineformat);

  GZ_FILE_IDS(end+1) = fid;
  GZ_FILE_NAMES{end+1} = tmp;
else
  fid = fopen(fname,permisson,machineformat);
end

