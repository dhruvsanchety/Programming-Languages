import sys
import time
import aiohttp
import asyncio
import argparse
import json

servers = { "Hill": 12165, "Jaquez": 12166, "Smith": 12167, "Singleton": 12168, "Campbell": 12169 }
connections = { "Hill": ["Jaquez", "Smith"], "Jaquez": ["Hill", "Singleton"], "Smith": ["Hill", "Singleton", "Campbell"],  "Singleton": ["Campbell", "Jaquez", "Smith"], "Campbell": ["Singleton", "Smith"] }
KEY = "AIzaSyB55kTlh_FCqUlLHl5AHQTCQlpdpoWeCvU"

class Server:
    def __init__(self, name, ip, port):
        self.name = name
        self.ip = ip
        self.port = port
        self.client_timestamps = {}
        self.client_message = {}
 
    async def handle_echo(self, reader, writer):
        while not reader.at_eof():  
            data = await reader.readline()
            message = data.decode()
            sendback_message = None
            if message == "":
                continue
             
            log_file.write("{} recieved: {}\n".format(self.name, message))
            words = message.split()
            if len(words) == 6 and words[0] == "AT":
                await self.AT(message,words)
            elif words[0] == "IAMAT":
                sendback_message = await self.IAMAT(message,words)
            elif words[0] == "WHATSAT":
                sendback_message = await self.WHATSAT(message,words)
            else:
                sendback_message = "? " + message

            if sendback_message != None:
                log_file.write("{} send: {}".format(self.name, sendback_message))
                writer.write(sendback_message.encode())
                await writer.drain()

        log_file.write("close the client socket\n")
        writer.close()

    async def AT(self,message,words):
        if (not (words[3] in self.client_timestamps)) or ( float(words[5])  > self.client_timestamps[words[3]] ):
            self.client_timestamps[words[3]] = float(words[5])
            self.client_message[words[3]] = message
            await self.flood(message)
        return None
    
    async def WHATSAT(self,message,words):
        if float(words[2]) > 50 or float(words[2]) < 0 or int(words[3]) > 20 or int(words[3]) < 0 or words[1] not in self.client_timestamps:
            sendback_message = "? " + message
        else:
            s = self.client_message[words[1]].split()[4]
            nearby = ""
            async with aiohttp.ClientSession() as session:
                s = self.latitudelongitude(s)
                google = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}'.format(s, words[2], KEY)
                nearby = await self.fetch(session, google)
            minimum = min(len(nearby["results"]),int(words[3]))
            nearby["results"] = nearby["results"][:(int(minimum))]
            nearby =  json.dumps(nearby, indent=4)
            sendback_message = "{}\n{}\n\n".format(self.client_message[words[1]], str(nearby))
        return sendback_message

    async def fetch(self,session, google):
        async with session.get(google) as response:
            return await response.json()

    async def IAMAT(self,message,words):
        if len(words) == 4:
            difference = time.time() - float(words[3])
            if difference>0:
                difference="+"+str(difference)
            else:
                difference=str(difference)
            sendback_message = "AT " + self.name + " " + difference + " " + words[1] + " " + words[2] + " " + words[3]
            self.client_timestamps[words[1]] = float(words[3])
            self.client_message[words[1]] = sendback_message
            await self.flood(sendback_message)
        else:
            sendback_message = "? " + message
        return sendback_message

    async def run_forever(self):
        log_file.write('{} starting\n'.format(self.name))
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)
        async with server:
            await server.serve_forever()   
        log_file.write('{} closing\n'.format(self.name))
        server.close()

    async def flood(self, message):
        for server in connections[self.name]:
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', servers[server])
                log_file.write("{} send to {}: {}\n".format(self.name, server, message))
                writer.write(message.encode())
                await writer.drain()
                log_file.write("Connection closed: {}\n".format(server))
                writer.close()
                await writer.wait_closed()
            except:
                log_file.write("Cannot connect: {}\n".format(server))

    def latitudelongitude(self, s):                                                                   
        latitude = s[0]
        if not (latitude == '+' or latitude == '-'):
            return None
        i=1
        while i<len(s) and not  (s[i]=='+' or s[i]=='-'):
            latitude=latitude+s[i]
            i=i+1
        longitude = ""
        if i==len(s):
               return None
        while i<len(s):
            longitude=longitude+s[i]
            i=i+1
        return "{},{}".format(latitude,longitude)
    
def main():
    parser = argparse.ArgumentParser('CS131 Project Argument Parser')
    parser.add_argument('server_name', type=str, help='Need arg')
    args = parser.parse_args()
    if not args.server_name in servers:
        print("Invalid Server Argument. Please retry.\n")
        sys.exit()
    global log_file 
    log_file = open(args.server_name+".log","a")
    server = Server(args.server_name, '127.0.0.1', servers[args.server_name])
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()
