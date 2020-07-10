using System.IO;
using Microsoft.EntityFrameworkCore;
using Packt.Shared;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace NorthwindWeb
{
    public class Startup
    {
        // This method gets called by the runtime. Use this method to add services to the container.
        // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddRazorPages();

            string databasePath = Path.Combine("..", "Northwind.db");
            services.AddDbContext<Northwind>(options =>
                options.UseSqlite($"Data Source={databasePath}"));
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // $ dotnet dev-certs https --trust
                // to enable https
                app.UseHsts();
                // If a website specifies hsts and a browser supports it
                // it forces all comm over HTTPS
            }

            app.UseRouting();

            app.UseHttpsRedirection();  // redirect HTTP requests to HTTPS

            app.UseDefaultFiles();      // index.html, default.html and so on
            app.UseStaticFiles();       // UseDefaultFiles must before useStaticFiles

            app.UseEndpoints(endpoints =>
            {
                // endpoints.MapGet("/", async context =>
                // {
                //     await context.Response.WriteAsync("Hello World!");
                // });
                endpoints.MapRazorPages();
            });
        }
    }
}
