       read(13,*) xori
       read(13,*) yori
       read(13,*) dx
       read(13,*) dy
       read(13,*) nx
       read(13,*) ny
       read(13,*) valex
       rewind(13)
       write(13,*) xori-dx
       write(13,*) yori-dy
       write(13,*) dx
       write(13,*) dy
       write(13,*) nx
       write(13,*) ny
       write(13,*) valex
       stop
       end
