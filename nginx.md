# Nginx
## [Beginner's Guide](http://nginx.org/en/docs/beginners_guide.html)
By default, the configuration file is named nginx.conf and placed in the directory
/usr/local/nginx/conf, /etc/nginx, or /usr/local/etc/nginx.
### Starting, Stopping, and Reloading Configuration
To start nginx, run the executable file. Once nginx is started, it can be controlled by invoking the executable with the -s parameter. Use the following syntax:  

    nginx -s signal
Where signal may be one of the following:
- stop — fast shutdown
- quit — graceful shutdown
- reload — reloading the configuration file
- reopen — reopening the log files
Note: Commands should be executed under the same user that started nginx.  

Changes made in the configuration file will not be applied until the command to reload configuration is sent to nginx or it is restarted. To reload configuration, execute:  

    nginx -s reload

    #under centos could also be:
    systemctl reload nginx
Once the master process receives the signal to reload configuration, it checks the syntax validity
of the new configuration file and tries to apply the configuration provided in it. If this is a
success, the master process starts new worker processes and sends messages to old worker processes,
requesting them to shut down. Otherwise, the master process rolls back the changes and continues
to work with the old configuration. Old worker processes, receiving a command to shut down, stop
accepting new connections and continue to service current requests until all such requests are
serviced. After that, the old worker processes exit.

    ps -ax | grep nginx
    kill -s QUIT 1628

### Configuration File's Structure
nginx consists of modules which are controlled by directives specified in the configuration file.
Directives are divided into simple directives and block directives. A simple directive consists of
the name and parameters separated by spaces and ends with a semicolon (;). A block directive has
the same structure as a simple directive, but instead of the semicolon it ends with a set of
additional instructions surrounded by braces ({ and }). If a block directive can have other
directives inside braces, it is called a context (examples: events, http, server, and location).  

Directives placed in the configuration file outside of any contexts are considered to be in the
main context. The events and http directives reside in the main context, server in http, and
location in server.  

The rest of a line after the # sign is considered a comment..  

### Serving Static Content
An important web server task is serving out files (such as images or static HTML pages). You will
implement an example where, depending on the request, files will be served from different local
directories: /data/www (which may contain HTML files) and /data/images (containing images). This
will require editing of the configuration file and setting up of a server block inside the http
block with two location blocks.  

    http {
        server {
        }
    }

Generally, the configuration file may include several server blocks distinguished by ports on which
they listen to and by server names. Once nginx decides which server processes a request, it tests
the URI specified in the request’s header against the parameters of the location directives defined
inside the server block.  

Add the following location block to the server block:

    location / {
          root /data/www;
    }
This location block specifies the “/” prefix compared with the URI from the request. For matching
requests, the URI will be added to the path specified in the root directive, that is, to /data/www,
to form the path to the requested file on the local file system.  
If there are several matching location blocks nginx selects the one with the longest prefix.
The location block above provides the shortest prefix, of length one, and so only if all other
location blocks fail to provide a match, this block will be used.  

Next, add the second location block:

    location /images/ {
          root /data;
    }
It will be a match for requests starting with /images/ (location / also matches such requests, but
has shorter prefix).  

The resulting configuration of the server block should look like this:

    server {
        location / {
            root /data/www;
        }

        location /images/ {
            root /data;
        }
    }
This is already a working configuration of a server that listens on the standard port 80 and is
accessible on the local machine at http://localhost/.  

To apply the new configuration, start nginx if it is not yet started or send the reload signal to
the nginx’s master process, by executing:  

    nginx -s reload
In case something does not work as expected, you may try to find out the reason in access.log and
error.log files in the directory /usr/local/nginx/logs or /var/log/nginx.  

### Settting Up a Simple Proxy Server
One of the frequent uses of nginx is setting it up as a proxy server, which means a server that
receives requests, passes them to the proxied servers, retrieves responses from them, and sends them to the clients.  

    server {
        listen 8080;
        root /data/up1;

        location / {
        }
    }
This will be a simple server that listens on the port 8080 (previously, the listen directive has
not been specified since the standard port 80 was used) and maps all requests to the /data/up1
directory on the local file system.  

Note that the root directive is placed in the server context. Such root directive is used when the
location block selected for serving a request does not include own root directive.  

    server {
        location / {
            proxy_pass http://localhost:8080;
        }

        location /images/ {
            root /data;
        }
    }
We will modify the second location block, which currently maps requests with the /images/ prefix to
the files under the /data/images directory, to make it match the requests of images with typical
file extensions. The modified location block looks like this:  

    location ~ \.(gif|jpg|png)$ {
        root /data/images;
    }

### Setting Up FastCGI Proxying
The most basic nginx configuration to work with a FastCGI server includes using the fastcgi_pass
directive instead of the proxy_pass directive, and fastcgi_param directives to set parameters
passed to a FastCGI server.  

In PHP, the SCRIPT_FILENAME parameter is used for determining the script name, and the QUERY_STRING
parameter is used to pass request parameters. The resulting configuration would be:  

    server {
        location / {
            fastcgi_pass  localhost:9000;
            fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
            fastcgi_param QUERY_STRING    $query_string;
        }

        location ~ \.(gif|jpg|png)$ {
            root /data/images;
        }
    }
