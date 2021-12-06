using System;
using System.Net;
using System.Net.Sockets;
using System.Text;


namespace Scale
{
    class Program
    {
        //List<string> ls = new List<string>();
        static void Main(string[] args)
        {
            //Console.WriteLine();
            //Console.WriteLine("Hello World!");
            StartServer();
            //Console.WriteLine(host.AddressList[0]);
        }
        public static void StartServer()
        {
            string connectIpAddress = "127.0.0.1";
            int connectPort = 1701;
            string userName = "Admin";
            string userPassword = "Admin";
            // Get Host IP Address that is used to establish a connection
            // In this case, we get one IP address of localhost that is IP : 127.0.0.1
            // If a host has multiple addresses, you will get a list of addresses
            //IPHostEntry host = Dns.GetHostEntry("localhost");
            //IPAddress ipAddress = host.AddressList[0];
            IPAddress ipAddress = IPAddress.Parse(connectIpAddress);
            //Console.WriteLine(ipAddress);
            IPEndPoint localEndPoint = new IPEndPoint(ipAddress, connectPort);

            try
            {

                // Create a Socket that will use Tcp protocol
                Socket listener = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
                // A Socket must be associated with an endpoint using the Bind method
                listener.Bind(localEndPoint);
                // Specify how many requests a Socket can listen before it gives Server busy response.
                // We will listen 10 requests at a time
                listener.Listen(10);

                Console.WriteLine("Waiting for a connection..." );
                Socket handler = listener.Accept();

                // Incoming data from the client.
                string data = null;
                byte[] bytes = null;
                byte[] msg = null;
                string responseCode = "";
                string sdsCode = "";
                string responseString = "";
                bool UnexpectedResponse = false;
                while (true)
                {
                    data = null;
                    bytes = null;
                    msg = null;
                    responseCode = "";
                    sdsCode = "";
                    responseString = "";
                    UnexpectedResponse = false;
                    bytes = new byte[1024];
                    msg = new byte[1024];
                    int bytesRec = handler.Receive(bytes);
                    data = Encoding.ASCII.GetString(bytes, 0, bytesRec);
                    Console.WriteLine(data);
                    if(data!="")
                    {
                        responseCode = data.Substring(3,2);
                        //Console.WriteLine(responseCode);
                        switch(responseCode)
                        {
                            case "00":
                                sdsCode = data.Substring(5, 1);
                                if(sdsCode== "C")
                                {
                                    responseString = data.Substring(17, 12);
                                    responseString = responseString.Trim();
                                    ParseOutWinSockDataString(data);// here insert parseoutsockdatastring method call..
                                    Console.WriteLine("Weight1 ," + responseString);
                                }
                                else if(sdsCode=="R")
                                {
                                    responseString = data.Substring(10, 8);
                                    responseString = responseString.Trim();
                                    Console.WriteLine("Weight1 ," + responseString);
                                }
                                else if(sdsCode=="B")
                                {
                                    responseString = data.Substring(10, 8);
                                    responseString = responseString.Trim();
                                    Console.WriteLine("Weight1 ,"  + responseString);
                                }
                                break;
                            case "53":
                                msg = Encoding.ASCII.GetBytes("User "+userName);
                                listener.Send(msg);
                                break;
                            case "51":
                                msg = Encoding.ASCII.GetBytes("pass "+userPassword);
                                listener.Send(msg);
                                break;
                            case "12":
                                msg = Encoding.ASCII.GetBytes("callback wt0101");
                                listener.Send(msg);
                                msg = Encoding.ASCII.GetBytes("callback wt0201");
                                listener.Send(msg);
                                break;
                            case "83":
                                UnexpectedResponse = true;
                                break;
                            case "93":
                                Console.WriteLine("Invalid Password WARNING");
                                UnexpectedResponse = true;
                                break;
                            default:
                                Console.WriteLine("default");
                                UnexpectedResponse = true;
                                break;
                        }
                    }
                    if(UnexpectedResponse==true)
                    {
                        Console.WriteLine("Received unexpected response from 780 ERROR");
                        listener.Shutdown(SocketShutdown.Receive);
                        listener.Close();
                        break;
                    }

                    //if (data.IndexOf("<EOF>") > -1)
                    //{
                    //    break;
                    //}
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }

            Console.WriteLine("\n Press any key to continue...");
            Console.ReadKey();
        }
        //public static void AddItemToList(string Verbage)
        //{
            //List1[List1.Length] = Verbage;
        //}

        public static void ParseOutWinSockDataString(string str)
        {
            string findString = "wt0101=";
            int startPos = 1;
            int foundString = -1;
            int p1 = 0;
            int p2 = 0;
            string w1 = "";
            string w2 = "";
            while(foundString>0)
            {
                foundString = str.IndexOf(findString, startPos);
                if(foundString!=0)
                {
                    Console.WriteLine("Weight1 Caption " + str.Substring(foundString + 7, 12).Trim());
                    startPos = foundString + 8;
                }
            }
            p1 = str.IndexOf("wt0101=", 1);
            if(p1!=0)
            {
                w1 = str.Substring(p1 + 7, 12);
                Console.WriteLine("Weight1 Caption " + w1.Trim());
            }
            p2 = str.IndexOf("wt0201=", 1);
            if(p2!=0)
            {
                w2 = str.Substring(p2 + 7, 12);
                Console.WriteLine("weight2 Caption " + w2.Trim());
            }
        }
    }
}
