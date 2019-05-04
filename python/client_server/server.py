#!/usr/bin/python

import socket

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
host = socket.gethostname()
print('host:', host)
port = 12345
sock.bind((host, port))

sock.listen(5)

while True:
    # Wait for a connection
    client, addr = sock.accept()

    try:
        print("Got conn from", addr)

        while True:
            data = client.recv(512)
            print("received", data)

            if data:
                print('sending data back to the client...')
                client.sendall(b'Thank you for sending' + data)
    finally:
        client.close()
