#######################################################
# Initializes  dialog boxes datas
#######################################################


set Data(VarList) {DBDescrname OutGenName yearmin yearmax permin permax \
                 ContDescr ContDescrName depthlist latmin latmax \
                 lonmin lonmax ctd bottle bt TS GlobalFlagMin GlobalFlagMax \
                 FlagMin FlagMax}

set Diva(VarList) {femdim DataName DepthName MeshName BathName OutName error Varbak dt RBTcl clref muref cl snr 
                alph0 alph1 mu xorig yorig dx dy xend yend valexcl SelDepth \
                ov femdim}

set Fem(VarList) {dim contname meshname batname depthname unif rlen srcoef \
                nbsm}

set GrData(VarList) {dim name DepthName ContDescr ContDescrName seldepth title \
			xname yname}

set GrFem(VarList) {femdim name depthname seldepth title xname yname}

set GrDiva(VarList) {resname seldepth dispdepth title xname yname clrlv \
                   depthscale}

set Dir(VarList) {mesh data analysis temp database options description \
		bathymetry contour depth work}

set Coordinates(VarList) { system lambda}

set Preference(VarList) { language }

set Panel(VarList) { data mesh diva grdata grmesh grdiva coord dir language }

#######################################################
# Sets defaults path for window selection
#######################################################


set Fs(data_base) $env(DIVA_WORK_DIR)/BASE
set Fs(data_base_filter) "*.pdf"
set Fs(data_base_des) $env(DIVA_WORK_DIR)/DES
set Fs(data_base_filter) "*.des"
set Fs(data_name) $env(DIVA_WORK_DIR)/DATA
set Fs(data_name_filter) "*.data*"
set Fs(data_cont) $env(DIVA_WORK_DIR)/CONT
set Fs(data_cont_filter) "*.cont"
set Fs(data_conf) $env(DIVA_WORK_DIR)/CONFIG
set Fs(data_conf_filter) "*.data.conf"

set Fs(mesh_cont) $env(DIVA_WORK_DIR)/CONT
set Fs(mesh_cont_filter) "*.cont"
set Fs(mesh_name) $env(DIVA_WORK_DIR)/MESH
set Fs(mesh_name_filter) "*.mesh"
set Fs(mesh_bat) $env(DIVA_WORK_DIR)/BAT
set Fs(mesh_bat_filter) "*.bat"
set Fs(mesh_depth) $env(DIVA_WORK_DIR)/DEPTH
set Fs(mesh_depth_filter) "*.depth"
set Fs(mesh_conf) $env(DIVA_WORK_DIR)/CONFIG 
set Fs(mesh_conf_filter) "*.mesh.conf"

set Fs(diva_data) $env(DIVA_WORK_DIR)/DATA 
set Fs(diva_data_filter) "*.data*"
set Fs(diva_data_depth) $env(DIVA_WORK_DIR)/DEPTH
set Fs(diva_data_depth_filter) "*.depth*"
set Fs(diva_mesh) $env(DIVA_WORK_DIR)/MESH 
set Fs(diva_mesh_filter) "*.mesh"
set Fs(diva_bath) $env(DIVA_WORK_DIR)/BAT 
set Fs(diva_bat_filter) "*.bat"
set Fs(diva_name) $env(DIVA_WORK_DIR)/ANALYSIS
set Fs(diva_res_filter) "*.anl"
set Fs(diva_conf) $env(DIVA_WORK_DIR)/CONFIG
set Fs(diva_conf_filter) "*.diva.conf"

set Fs(gr_data_name) $env(DIVA_WORK_DIR)/DATA 
set Fs(gr_data_name_filter) "*.data*"
set Fs(gr_data_name_depth) $env(DIVA_WORK_DIR)/DEPTH
set Fs(gr_data_name_depth_filter) "*.depth"
set Fs(gr_data_cont) $env(DIVA_WORK_DIR)/CONT 
set Fs(gr_data_cont_filter) "*.cont"
set Fs(gr_data_conf) $env(DIVA_WORK_DIR)/CONFIG
set Fs(gr_data_conf_filter) "*.grdata.conf"

set Fs(gr_mesh_name) $env(DIVA_WORK_DIR)/MESH 
set Fs(gr_mesh_name_filter) "*.mesh"
set Fs(gr_mesh_depth) $env(DIVA_WORK_DIR)/DEPTH 
set Fs(gr_mesh_depth_filter) "*.depth"
set Fs(gr_mesh_conf) $env(DIVA_WORK_DIR)/CONFIG
set Fs(gr_mesh_conf_filter) "*.grmesh.conf"

set Fs(gr_diva_name) $env(DIVA_WORK_DIR)/ANALYSIS 
set Fs(gr_diva_name_res) "*.anl"
set Fs(gr_diva_conf) $env(DIVA_WORK_DIR)/CONFIG
set Fs(gr_diva_conf_filter) "*.grdiva.conf"

set Fs(dir_conf) $env(DIVA_WORK_DIR)/CONFIG 
set Fs(dir_conf_filter) "*.dir.conf"

set PlplotDev "dp"

#######################################################
# Sets the different  FORTRAN executables path
#######################################################


set Data(extr_exec) "$env(DIVA_BIN_DIR)/extract.a"

set Fem(fem3d_exec) "$env(DIVA_BIN_DIR)/fem3d.a"
set Fem(gener_exec) "$env(DIVA_BIN_DIR)/gener.a"
set Fem(reorg_exec) "$env(DIVA_BIN_DIR)/reorg.a"
set Fem(coord_exec) "$env(DIVA_BIN_DIR)/coord.a"
set Fem(generopt_exec) "$env(DIVA_BIN_DIR)/generopt.a"

set Diva(diva_exec) $env(DIVA_BIN_DIR)/diva.a
set Diva(concat_exec) "$env(DIVA_BIN_DIR)/concat.a"
set Diva(stiff_exec) "$env(DIVA_BIN_DIR)/stiff.a"
set Diva(mask_exec) "$env(DIVA_BIN_DIR)/mask.a"
set Diva(coord_exec) "$env(DIVA_BIN_DIR)/coord.a"
set Diva(sum_exec)   "$env(DIVA_BIN_DIR)/sum.a"
set Diva(substref_exec) "$env(DIVA_BIN_DIR)/substref.a"

set Gra(header_exec) "$env(DIVA_BIN_DIR)/header2.a"
set Gra(visu_exec) "$env(DIVA_BIN_DIR)/visu"


# set Trash C:/msys/dev/null
set Trash ./null


LoadVar
