divacova=dlmread('../../../divastripped/output/valatxyascii.anl',' ')
RL=1
plot(divacova(:,1),divacova(:,1)/RL.*BesselK(1,divacova(:,1)/RL),divacova(:,1),divacova(:,3),'.')
