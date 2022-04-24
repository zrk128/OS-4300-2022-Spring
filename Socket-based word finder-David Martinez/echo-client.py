from socket import *
#Socket connection stuff
serverName = 'localhost'
serverPort = 12000
clientSocket = socket(AF_INET, SOCK_STREAM)

clientSocket.connect((serverName,serverPort))

sentence = input("Input query (format is a*a or *a or a*):") #take query
clientSocket.send(sentence.encode()) #encode query and send
    
#loop for receiving words
while True:
    word = clientSocket.recv(1024) #receive word
    if len(word.decode()) == 0: #check to make sure word is not empty
        break #if so, break from loop
    print(word.decode()) #print word
clientSocket.close() #close socket connection
