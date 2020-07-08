using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace startup
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateHostBuilder(args).Build().Run();
        }

        public static IHostBuilder CreateHostBuilder(string[] args) =>
            // the fowllowing are run inorder
            // but they don't have to be registered inorder
            Host.CreateDefaultBuilder(args)
                .ConfigureWebHostDefaults(webBuilder =>
                {
                    Console.Write("1. ConfigureWebHostDefaults: ");
                    Console.WriteLine("Register essential components of the APP, like config, container.");
                    webBuilder.UseStartup<Startup>();
                    // Startup Class is not compulsory, it can be replaced by the following:
                    /*
                        webBuilder.ConfigureServices(services =>
                        {
                            services.AddControllers();
                        });
                        webBuilder.Configure(app =>
                        {
                            // what's in `Startup.Configure()`
                        });
                    */
                })
                .ConfigureHostConfiguration(builder =>
                {
                    Console.Write("2. ConfigureHostConfiguration: ");
                    Console.WriteLine("Register essential components to start up, like listen to what ports or URLs.");
                })
                .ConfigureAppConfiguration(builder =>
                {
                    Console.Write("3. ConfigureAppConfiguration: ");
                    Console.WriteLine("to add our own configure for the APP to read from.");
                })
                .ConfigureServices(service =>
                {
                    Console.WriteLine("4. ConfigureServices, ConfigureLogging, Startup, Startup.ConfigureServices.");
                    Console.WriteLine("5. Startup.Configure: register middlewares.");
                });
    }
}
