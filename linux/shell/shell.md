``` shell
# !/bin/bash
```

To run a unexecutable shell file:
```
    $ sh fname.sh
```

```
    #!/bin/bash
    tar cvfz homeuser.tgz /home/lev/

    #!/bin/bash
    BU=homeuser$(date +%Y%m%d).tgz
    tar cvfz $BU $(pwd)
```
