run:
	docker run -d -i --name test_db -p 5431:5432 -v `pwd`:/data postgresql:1.0
attach:
	docker exec -it test_db bash

stop:
	docker stop test_db

clear:
	docker rm -f test_db

