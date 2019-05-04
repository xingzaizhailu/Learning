#!/usr/bin/python

import socket
import time

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

host = socket.gethostname()
port = 12345

sock.connect((host, port))

try:
    # Send data
    message = 'This is the message.'
    print('sending message: ', message)
    sock.sendall(message)

    # Look for the response
    amount_received = 0

    while True:
        data = sock.recv(512)
        amount_received += len(data)
        print("\nClient received: ", data)
finally:
    print("Client - closing socket...")
    sock.close()
