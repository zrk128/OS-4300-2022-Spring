from socket import *
import time
## server init related stuff
serverPort = 12000
serverSocket = socket(AF_INET,SOCK_STREAM)
serverSocket.bind(("",serverPort))
serverSocket.listen(1)
print("The server is ready to receive")

i = 0
while 1:
    connectionSocket, addr = serverSocket.accept() #accept connection
    #print connnection time
    print("Server connected to", addr, "on", time.ctime(time.time())) 
    f = open("wordlist.txt",'r') #open word list
    wordlist = f.readlines() #read wordlist
    
    query = connectionSocket.recv(1024)#receive query
    command = query.decode()#decode query
    command = command.split('*')#split query by *
    front = str(command[0]) #set query front
    back = str(command[1]) #set query back
    fcount = 0 #iterator that counts number of matching front characters
    bcount = 0 #iterator that counts number of matching back characters
    words = [] #list of words that pass conditions
    for word in wordlist: 
        word = word.strip('\n') #strip null 
        if word == '' or len(word) < len(front) or len(word) < len(back):
            continue #edge case
        for i in range(len(front)):
            if front[i] == word[i]:
                fcount += 1 #iterate if character matches
        for i in range(len(back)):
            if back[len(back)-i-1] == word[len(word)-i-1]:
                bcount += 1 #iterate if character matches
        if fcount == len(front) and bcount == len(back):
              words.append(word)#append if word passes
        fcount = bcount = 0 #reset counts
    for w in words:
        connectionSocket.send(w.encode()) #encode word to be sent back
        time.sleep(0.1)#delay for words to be buffered correctly
    print("all words have been sent!")
    connectionSocket.close() #close connection
