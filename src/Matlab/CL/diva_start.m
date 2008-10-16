%--------------------------------------------------------------------------
% divastart.m
%------------
%
% define names, directories and toolboxes for plotting diva outputs
% To be executed AFTER 'divasave'
%
%
%
% ctroupin, 2 January 2008
%
%--------------------------------------------------------------------------

clear all, close all, clc

% names of case and variable 
%---------------------------

% variable = 1 (temperature)
%            2 (salinity)
%            3 (depth)
%            0 (nothing)
             
casename = 'Climato14';
variable = 3;

% directories
%------------

% dir.name = name of your      input/output     directories called with 
%                           divaload/divasave
% dir.figures = where you want the figures to be saved

dir.name    = 'D:\diva-4.2.0\Climato\14';
%dir.figures = 'D:\diva-4.2.0\examples\topoghir\figures\';
dir.figures = 'D:\diva-4.2.0\Climato\figures\';

% figure output format
%---------------------

% fig_format = 1 --> jpeg
%            = 2 --> eps
             
fig_format = 1;

% m_map:
%-------

% = 1   if you have the m_map toolbox installed
% = 0   if not

is_mmap = 1; 

if (is_mmap == 1),
    display('m_map toolbox available');
end

%--------------------------------------------------------------------------
% end of user's parameters
%--------------------------------------------------------------------------


% check directories


if (  (~strcmp(dir.name(end),'/'))  &  (~strcmp(dir.name(end),'\'))  )
    dir.name = [dir.name,'\'];
end;

if (  (~strcmp(dir.figures(end),'/'))  &  (~strcmp(dir.figures(end),'\'))  )
    dir.figures = [dir.figures,'\'];
end;

dir.input   = [dir.name,'input\'];
dir.output  = [dir.name,'output\'];

if (~exist(dir.name,'dir')),
    display(['Directory ',dir.name,' not existing']);
    display(['Making directory ',dir.name]);
    mkdir(dir.name);
end;

if (~exist(dir.figures,'dir')),
    display(['Directory ',dir.figures,' not existing']);
    display(['Making directory ',dir.figures]);
    mkdir(dir.figures);
end;

if (~exist(dir.input,'dir')),
    display(['Directory ',dir.input,' not existing']);
    display(['Making directory ',dir.input]);
    mkdir(dir.input);
end;

if (~exist(dir.output,'dir')),
    display(['Directory ',dir.output,' not existing']);
    display(['Making directory ',dir.output]);
    mkdir(dir.output);
end;


% figure format

switch fig_format
    case 1
        display('Figures in jpeg format')
        fig.ext = '.jpg';
        fig.opt = '-djpeg100';
    case 2
        display('Figures in eps format')
        fig.ext = '.eps';
        fig.opt = '-depsc2 ';
    otherwise
        display('Default format: jpeg')
        fig.ext = '.jpg';
        fig.opt = '-djpeg100';
end;



switch variable
    case 1
        var.name = 'Temperature';
        var.abbrev = 'T';
        var.units = '(^{\circ}C)';
    case 2
        var.name = 'Salinity';
        var.abbrev = 'S';
        var.units = '(PSU)';
    case 3
        var.name = 'Depth';
        var.abbrev = 'Depth';
        var.units = '(m)';
    otherwise
        var.name = 'none';
        var.abbrev = [];
        var.units = [];
end;

display(['Plot for the variable: ',var.name])
display(['in the directory:  ' ,dir.figures])
