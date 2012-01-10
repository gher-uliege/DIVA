        xi=1
        
        read(5,*,END=99) RL, xi
 99     continue
        write(12,*) 1/RL**4
        write(12,*) 2*xi/RL**2
        stop
        end
        
