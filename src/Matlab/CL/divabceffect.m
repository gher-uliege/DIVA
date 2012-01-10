divacova=dlmread('../../../divastripped/output/valatxyascii.anl.center',' ')
divacovabc=dlmread('../../../divastripped/output/valatxyascii.anl.bc',' ')
plot(divacova(:,1),divacova(:,3),divacovabc(:,1),divacovabc(:,3))
