function gzfclose(fid)

global GZ_FILE_IDS GZ_FILE_NAMES

fclose(fid);

index = [];
if ~isempty(GZ_FILE_IDS)
  index = find(GZ_FILE_IDS == fid);
end

if ~isempty(index)
  tmp = GZ_FILE_NAMES{index};
  eval(['!rm  ' tmp ]);

  GZ_FILE_IDS(index)=[];
  GZ_FILE_NAMES={GZ_FILE_NAMES{1:index-1}  GZ_FILE_NAMES{index+1:end}};
end

